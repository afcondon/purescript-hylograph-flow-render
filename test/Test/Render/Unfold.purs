-- | The unfolding engine and the continuity of its layouts, on flows made
-- | by rule (`Test.Render.Generated`): every cut of every flow lawful, and
-- | every pair of cuts that differ by one group held to the laws of the
-- | pair, both directions.
module Test.Render.Unfold (suite, tally) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl, for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), ElementKey(..), GroupId, StepId(..), project)
import Hylograph.Flow.Grouping (collapse)
import Hylograph.Flow.Render.Continuity (Continuity(..), change, continuity)
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Layout, PlacedOutline, plainPicture, run)
import Hylograph.Flow.Render.Laws (check)
import Hylograph.Flow.Render.Sankey (Metrics, sankeyToward)
import Hylograph.Flow.Render.Unfold (unfolding)
import Test.Render.Fixtures (expect, expectNone, sankeyMetrics)
import Test.Render.Generated (Made, cutsOf, made, pairsOf, quantity)

metrics :: Metrics
metrics = sankeyMetrics { nodeBreadth = 16.0 }

seeds :: Array Int
seeds = Array.range 1 60

type Cut = Set.Set GroupId

settingOf :: Direction -> Made -> { layoutOf :: Cut -> Layout (Array Number), faultsOf :: Cut -> Array String, plainOf :: Cut -> Layout (Array Number) }
settingOf direction subject =
  let
    projectionOf closed = project Steps (collapse subject.grouping closed subject.flow)
    engine = unfolding direction metrics quantity { grouping: subject.grouping, finest: projectionOf Set.empty }
    pictureOf closed = plainPicture { groups = Just { grouping: subject.grouping, closed } }
  in
    { layoutOf: \closed -> run engine (pictureOf closed) (projectionOf closed)
    , faultsOf: \closed -> map (\fault -> show fault.law <> ": " <> fault.words) (check engine (pictureOf closed) (projectionOf closed) (run engine (pictureOf closed) (projectionOf closed)))
    , plainOf: \closed -> run (sankeyToward direction metrics quantity) (pictureOf closed) (projectionOf closed)
    }

-- | The outlines of the groups inside a group's.
innerOutlines :: forall wire. GroupId -> Layout wire -> Array PlacedOutline
innerOutlines group laid = Array.filter (\outline -> ancestry outline) laid.outlines
  where
  ancestry outline = case outline.parent of
    Just parent -> parent == group || Array.any ancestry (Array.filter (\other -> other.group == parent) laid.outlines)
    Nothing -> false

-- | Whether an outline's members are in more than one tier.
spansTiers :: forall wire. Layout wire -> PlacedOutline -> Boolean
spansTiers laid outline = Array.length (Array.nub (Array.mapMaybe (\index -> Array.index laid.nodes index >>= _.layer) outline.members)) > 1

-- | A rung checked again, by another road than `change` takes: straight
-- | from the boxes. `Held`: every node in both layouts has one box.
-- | `Nested` and better: every node of the open layout that the shut one
-- | has not is inside the span, across the flow, of the group's bar.
keeps :: forall wire. Direction -> GroupId -> Continuity -> Layout wire -> Layout wire -> Boolean
keeps direction group rung shut open = case rung of
  Held -> nestedHere && Array.all (\n -> Array.any (\m -> m.node.key == n.node.key && m.box == n.box) open.nodes || not (Array.any (\m -> m.node.key == n.node.key) open.nodes)) shut.nodes
  Nested -> nestedHere
  _ -> true
  where
  span box = case direction of
    LeftToRight -> { from: box.y, to: box.y + box.height }
    TopDown -> { from: box.x, to: box.x + box.width }
  bars = Array.filter (\n -> n.node.key == StepKey (GroupStep group)) shut.nodes
  fresh = Array.filter (\n -> not (Array.any (\m -> m.node.key == n.node.key) shut.nodes)) open.nodes
  nestedHere = Array.null fresh || Array.any (\bar -> Array.all (\n -> (span n.box).from >= (span bar.box).from - 1.0e-6 && (span n.box).to <= (span bar.box).to + 1.0e-6) fresh) bars

-- | The second layout with two nodes made to change places across the
-- | flow: two that are in both layouts, in one tier of each, and apart in
-- | each. Nothing where there are no two such nodes.
swapped :: forall wire. Direction -> Layout wire -> Layout wire -> Maybe (Layout wire)
swapped direction shut open = do
  pair <- Array.head do
    a <- open.nodes
    b <- open.nodes
    if a.index < b.index && isJust a.layer && a.layer == b.layer && apart a.box b.box && alsoApart a.node.key b.node.key then [ { a, b } ] else []
  pure open { nodes = open.nodes <#> \n -> if n.index == pair.a.index then n { box = pair.b.box } else if n.index == pair.b.index then n { box = pair.a.box } else n }
  where
  middle box = case direction of
    LeftToRight -> box.y + box.height / 2.0
    TopDown -> box.x + box.width / 2.0
  apart one other = middle one /= middle other
  alsoApart one other = case Array.find (\n -> n.node.key == one) shut.nodes, Array.find (\n -> n.node.key == other) shut.nodes of
    Just a, Just b -> isJust a.layer && a.layer == b.layer && apart a.box b.box
    _, _ -> false

turnsBack :: forall wire. Layout wire -> Boolean
turnsBack laid = Array.any (\e -> e.course == Backward) laid.edges

-- | What the pairs of a direction come to, counted.
tally :: Direction -> { flows :: Int, cuts :: Int, pairs :: Int, held :: Int, nested :: Int, ordered :: Int, rearranged :: Int, fit :: Int, misfit :: Int, misfitForward :: Int, exact :: Int, plainRearranged :: Int, unkept :: Int, spoiled :: Int, spoiledCaught :: Int, faults :: Array String }
tally direction = foldl add { flows: 0, cuts: 0, pairs: 0, held: 0, nested: 0, ordered: 0, rearranged: 0, fit: 0, misfit: 0, misfitForward: 0, exact: 0, plainRearranged: 0, unkept: 0, spoiled: 0, spoiledCaught: 0, faults: [] } seeds
  where
  add acc seed =
    let
      subject = made seed
      setting = settingOf direction subject
      cuts = cutsOf subject.grouping
      lawFaults = cuts >>= \cut -> map (\words -> "seed " <> show seed <> ", " <> show (Array.fromFoldable cut) <> ": " <> words) (setting.faultsOf cut)
      pairs = pairsOf subject.grouping <#> \pair ->
        let
          shut = setting.layoutOf pair.shut
          open = setting.layoutOf pair.open
          found = change shut open
          fits = all (\bar -> bar.slot <= bar.breadth + 1.0e-6) found.bars
        in
          { rung: found.rung
          , fits
          , forward: not (turnsBack shut) && not (turnsBack open) && not (Array.any (spansTiers open) (innerOutlines pair.group open))
          , exact: not (Array.null found.bars) && all (\bar -> bar.abreast >= bar.breadth - 1.0e-6) found.bars
          , kept: keeps direction pair.group found.rung shut open
          , spoiled: case swapped direction shut open of
              Just wrong -> Just (continuity shut wrong)
              Nothing -> Nothing
          , plain: continuity (setting.plainOf pair.shut) (setting.plainOf pair.open)
          , words: map (\words -> "seed " <> show seed <> ", " <> show pair.group <> " of " <> show (Array.fromFoldable pair.shut) <> ": " <> words) found.disordered
          }
      count f = Array.length (Array.filter f pairs)
    in
      { flows: acc.flows + 1
      , cuts: acc.cuts + Array.length cuts
      , pairs: acc.pairs + Array.length pairs
      , held: acc.held + count (\p -> p.rung == Held)
      , nested: acc.nested + count (\p -> p.rung == Nested)
      , ordered: acc.ordered + count (\p -> p.rung == Ordered)
      , rearranged: acc.rearranged + count (\p -> p.rung == Rearranged)
      , fit: acc.fit + count _.fits
      , misfit: acc.misfit + count (not <<< _.fits)
      , misfitForward: acc.misfitForward + count (\p -> not p.fits && p.forward)
      , exact: acc.exact + count _.exact
      , plainRearranged: acc.plainRearranged + count (\p -> p.plain == Rearranged)
      , unkept: acc.unkept + count (not <<< _.kept)
      , spoiled: acc.spoiled + count (\p -> isJust p.spoiled)
      , spoiledCaught: acc.spoiledCaught + count (\p -> p.spoiled == Just Rearranged)
      , faults: acc.faults <> lawFaults <> Array.concatMap _.words pairs
      }

suite :: Effect Unit
suite = do
  log "render: the unfolding engine, and what its layouts keep between cuts"
  for_ [ LeftToRight, TopDown ] \direction -> do
    let found = tally direction
    log ("  " <> show direction <> ": " <> show found.flows <> " made flows, " <> show found.cuts <> " cuts, " <> show found.pairs <> " pairs of cuts that differ by one group: Held " <> show found.held <> ", Nested " <> show found.nested <> ", Ordered " <> show found.ordered <> ", Rearranged " <> show found.rearranged)
    log ("    the parts fit a slot of the bar's breadth in " <> show found.fit <> " pairs and not in " <> show found.misfit <> " (" <> show found.misfitForward <> " of those with no ribbon turned back and no open group that spans tiers inside the opened one); in " <> show found.exact <> " some tier of parts adds up to the bar exactly")
    log ("    the Sankey engine on the same pairs: Rearranged in " <> show found.plainRearranged)
    expectNone (show direction <> ": every cut's layout keeps the general laws and the engine's promises, and no pair breaks order") (Array.take 12 found.faults)
    expect (show direction <> ": Ordered, always: no pair is Rearranged") (found.rearranged == 0)
    expect (show direction <> ": Nested, for breadth: the parts fit a slot as broad as the bar in every pair where no ribbon is turned back and no open group inside the opened one spans tiers") (found.misfitForward == 0)
    expect (show direction <> ": no pair is given a rung it does not keep, checked again straight from the boxes") (found.unkept == 0)
    log ("    the open layout with two nodes of a tier made to change places: " <> show found.spoiled <> " pairs so spoiled, " <> show found.spoiledCaught <> " of them Rearranged")
    expect (show direction <> ": a layout that reorders fails the law: every pair so spoiled is Rearranged") (found.spoiled > 0 && found.spoiledCaught == found.spoiled)
    expect (show direction <> ": all the rungs occur among the pairs") (found.held > 0 && found.nested > 0 && found.ordered > 0)
