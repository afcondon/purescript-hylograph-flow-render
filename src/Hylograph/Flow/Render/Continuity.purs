-- | What is kept between two layouts of two cuts of one flow, measured
-- | (0.6).
-- |
-- | When a group opens or closes, the reader must not lose their place.
-- | There is a ladder of what a pair of pictures can promise, best first;
-- | this module computes the three rungs that are laws of the pair, from
-- | the two layouts and nothing else, so that a page says which rung it is
-- | on and adds other signals accordingly. It claims a rung only where it
-- | has measured it.
-- |
-- | - `Held`: every node the change does not touch keeps its box exactly,
-- |   and the pair is `Nested`.
-- | - `Nested`: each opened group's parts lie within the breadth its
-- |   closed bar had, where the bar stood: every part's span across the
-- |   flow is inside the bar's span. And the pair is `Ordered`.
-- | - `Ordered`: things move, but no two nodes of a tier and no two
-- |   ribbons at a node change order.
-- | - `Rearranged`: none of these. (The ladder's other rungs, a watched
-- |   transition and signals in colour and outline, are then all the
-- |   reader has.)
-- |
-- | HOW A CHANGE IS READ FROM TWO LAYOUTS. A node is *untouched* when its
-- | key is in both. A *bar* is a closed group's node (its step, or its
-- | summary) that is in one layout and not the other; its *parts* are the
-- | members of that group's outline in the other layout, which is why the
-- | layouts must have been made with the picture told of its groups. A
-- | node in one layout only that is neither a bar with parts nor a part of
-- | a bar is *unaccounted for*, and a pair with such a node is not called
-- | `Nested`. Nothing here is told which layout is the finer: several
-- | groups may open and others close between the two, and each bar is
-- | paired with its parts wherever they are.
-- |
-- | ORDER, exactly. Two untouched nodes in one tier of both layouts are
-- | across the flow from one another the same way in both. A bar is to an
-- | untouched node of its tier as each of its parts is to that node, where
-- | they share a tier. On each side of an untouched node, the ribbons to
-- | (or from) two different other ends stand the same way in both, a
-- | ribbon to a part counting as a ribbon to its bar; and so the ribbons
-- | to the parts of one group stand together. Two things at one place (a
-- | node of no breadth, a ribbon of none) are in order either way. Nodes
-- | with no tier (the shelf) are not compared.
module Hylograph.Flow.Render.Continuity
  ( Continuity(..)
  , Bar
  , Change
  , Opened
  , opened
  , change
  , continuity
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl, sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (abs)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (ElementKey(..), GroupId, ItemId(..), StepId(..))
import Hylograph.Flow.Render.Engine (Box, Direction(..), Layout, PlacedNode)
import Hylograph.Flow.Render.Route as Route

-- | The best rung a pair keeps; see the module's header.
data Continuity = Rearranged | Ordered | Nested | Held

derive instance Eq Continuity
derive instance Ord Continuity

instance Show Continuity where
  show = case _ of
    Rearranged -> "Rearranged"
    Ordered -> "Ordered"
    Nested -> "Nested"
    Held -> "Held"

-- | A closed group's bar in one layout and its parts in the other.
-- | `breadth`: the bar's, across the flow. `slot`: from the start of the
-- | first part to the end of the last, in any tier (so the parts fit a
-- | slot of the bar's breadth when `slot <= breadth`). `abreast`: the most
-- | the parts of one tier come to, added up. `inPlace`: every part's span
-- | is inside the span the bar had.
type Bar = { group :: GroupId, breadth :: Number, slot :: Number, abreast :: Number, parts :: Int, inPlace :: Boolean }

-- | Everything measured of a pair. `moved`: the untouched nodes whose box
-- | is not what it was. `unaccounted`: see the header. `disordered`: each
-- | breach of order, in words.
type Change =
  { rung :: Continuity
  , moved :: Array ElementKey
  , bars :: Array Bar
  , unaccounted :: Array ElementKey
  , disordered :: Array String
  }

-- | The best rung two layouts of two cuts of one flow keep.
continuity :: forall wire. Layout wire -> Layout wire -> Continuity
continuity before after = (change before after).rung

groupOfKey :: ElementKey -> Maybe GroupId
groupOfKey = case _ of
  StepKey (GroupStep group) -> Just group
  ItemKey (GroupItem group) -> Just group
  _ -> Nothing

close :: Number
close = 1.0e-6

-- | A closed group's bar in one layout, and its parts in another.
type Opened = { group :: GroupId, bar :: PlacedNode, parts :: Array PlacedNode, measured :: Bar }

-- | The bars of the first layout that the second has not, each with its
-- | parts in the second: the members there of its group's outline. (A bar
-- | whose group has no outline in the second layout is not among them.)
opened :: forall wire. Layout wire -> Layout wire -> Array Opened
opened here there = do
  node <- here.nodes
  case groupOfKey node.node.key of
    Just group | not (Array.any (\n -> n.node.key == node.node.key) there.nodes) -> case Array.find (\outline -> outline.group == group) there.outlines of
      Just outline ->
        let
          parts = Array.mapMaybe (\index -> Array.find (\n -> n.index == index) there.nodes) outline.members
          tiers = Array.nub (map _.layer parts)
          first = foldl (\least part -> min least (start part.box)) (1.0 / 0.0) parts
          last = foldl (\most part -> max most (start part.box + breadth part.box)) (-1.0 / 0.0) parts
        in
          if Array.null parts then []
          else
            [ { group
              , bar: node
              , parts
              , measured:
                  { group
                  , breadth: breadth node.box
                  , slot: last - first
                  , abreast: foldl (\most tier -> max most (sum (map (breadth <<< _.box) (Array.filter (\part -> part.layer == tier) parts)))) 0.0 tiers
                  , parts: Array.length parts
                  , inPlace: first >= start node.box - close && last <= start node.box + breadth node.box + close
                  }
              }
            ]
      Nothing -> []
    _ -> []
  where
  start box = case here.direction of
    LeftToRight -> box.y
    TopDown -> box.x
  breadth box = case here.direction of
    LeftToRight -> box.height
    TopDown -> box.width

-- | What is kept between two layouts, and what is not.
change :: forall wire. Layout wire -> Layout wire -> Change
change one other =
  { rung
  , moved
  , bars: map _.bar paired
  , unaccounted
  , disordered
  }
  where
  sameWay = one.direction == other.direction

  start :: Box -> Number
  start box = case one.direction of
    LeftToRight -> box.y
    TopDown -> box.x

  breadth :: Box -> Number
  breadth box = case one.direction of
    LeftToRight -> box.height
    TopDown -> box.width

  middle :: Box -> Number
  middle box = start box + breadth box / 2.0

  across :: Route.Point -> Number
  across point = case one.direction of
    LeftToRight -> point.y
    TopDown -> point.x

  byKey :: Layout wire -> Map ElementKey PlacedNode
  byKey laid = foldl (\m n -> if Map.member n.node.key m then m else Map.insert n.node.key n m) Map.empty laid.nodes

  inOne = byKey one
  inOther = byKey other

  untouched = Array.filter (\n -> Map.member n.node.key inOther) (Array.fromFoldable (Map.values inOne))

  sameBox a b = abs (a.x - b.x) <= close && abs (a.y - b.y) <= close && abs (a.width - b.width) <= close && abs (a.height - b.height) <= close

  moved = untouched # Array.mapMaybe \n -> Map.lookup n.node.key inOther >>= \m -> if sameBox n.box m.box then Nothing else Just n.node.key

  -- ---- bars and their parts -----------------------------------------------------
  pairedOne = opened one other <#> \found -> { bar: found.measured, node: found.bar, parts: found.parts, inFirst: true }
  pairedOther = opened other one <#> \found -> { bar: found.measured, node: found.bar, parts: found.parts, inFirst: false }
  paired = pairedOne <> pairedOther

  -- every node in one layout only is a bar with parts, or a part of a bar
  explained = Set.fromFoldable (paired >>= \found -> [ found.node.node.key ] <> map _.node.key found.parts)
  unaccounted =
    Array.filter (\key -> not (Map.member key inOther) && not (Set.member key explained)) (Array.fromFoldable (Map.keys inOne))
      <> Array.filter (\key -> not (Map.member key inOne) && not (Set.member key explained)) (Array.fromFoldable (Map.keys inOther))

  -- ---- order ---------------------------------------------------------------------------
  -- which way two things stand: -1, 1, or 0 where they are at one place
  way :: Number -> Number -> Int
  way a b = if a < b - close then (-1) else if a > b + close then 1 else 0

  agree :: Int -> Int -> Boolean
  agree a b = a == 0 || b == 0 || a == b

  nodeFaults = do
    a <- untouched
    b <- untouched
    if a.index >= b.index then []
    else case Map.lookup a.node.key inOther, Map.lookup b.node.key inOther of
      Just a', Just b' | isJust a.layer && a.layer == b.layer && isJust a'.layer && a'.layer == b'.layer ->
        if agree (way (middle a.box) (middle b.box)) (way (middle a'.box) (middle b'.box)) then []
        else [ a.node.label <> " and " <> b.node.label <> " change order in their tier" ]
      _, _ -> []

  barFaults = paired >>= \found ->
    let
      -- the untouched nodes, as they are beside the bar and beside its parts
      beside = if found.inFirst then untouched <#> (\n -> { withBar: n, withParts: fromMaybe n (Map.lookup n.node.key inOther) })
        else untouched <#> (\n -> { withBar: fromMaybe n (Map.lookup n.node.key inOther), withParts: n })
    in
      do
        pair <- beside
        part <- found.parts
        if isJust found.node.layer && pair.withBar.layer == found.node.layer && isJust part.layer && pair.withParts.layer == part.layer
          && not (agree (way (middle found.node.box) (middle pair.withBar.box)) (way (middle part.box) (middle pair.withParts.box))) then [ pair.withBar.node.label <> " changes sides of " <> found.node.node.label <> " as it becomes " <> part.node.label ]
        else []

  -- a part's bar's key, or a node's own: what a ribbon's other end is,
  -- so that a ribbon to a part is a ribbon to its bar
  standsFor :: Map ElementKey ElementKey
  standsFor = Map.fromFoldable (paired >>= \found -> map (\part -> Tuple part.node.key found.node.node.key) found.parts)

  canonical :: ElementKey -> ElementKey
  canonical key = fromMaybe key (Map.lookup key standsFor)

  -- on a side of a node: for each other end, where its ribbons stand
  sideOf :: Layout wire -> ElementKey -> Boolean -> Map ElementKey { least :: Number, most :: Number }
  sideOf laid key leaving = foldl
    ( \m e ->
        if leaving && e.edge.from == key then put (canonical e.edge.to) (across (Route.start e.route)) m
        else if not leaving && e.edge.to == key then put (canonical e.edge.from) (across (Route.end e.route)) m
        else m
    )
    Map.empty
    laid.edges
    where
    put end at = Map.insertWith (\a b -> { least: min a.least b.least, most: max a.most b.most }) end { least: at, most: at }

  rangeWay :: { least :: Number, most :: Number } -> { least :: Number, most :: Number } -> Maybe Int
  rangeWay a b =
    if a.most <= b.least + close && b.most <= a.least + close then Just 0
    else if a.most <= b.least + close then Just (-1)
    else if b.most <= a.least + close then Just 1
    else Nothing

  ribbonFaults = do
    node <- untouched
    leaving <- [ true, false ]
    let
      here = sideOf one node.node.key leaving
      there = sideOf other node.node.key leaving
      ends = Array.filter (\end -> Map.member end there) (Array.fromFoldable (Map.keys here))
    a <- ends
    b <- ends
    if a >= b then []
    else case Map.lookup a here, Map.lookup b here, Map.lookup a there, Map.lookup b there of
      Just a1, Just b1, Just a2, Just b2 -> case rangeWay a1 b1, rangeWay a2 b2 of
        Just first, Just second | agree first second -> []
        _, _ -> [ "at " <> node.node.label <> ", the ribbons " <> (if leaving then "to " else "from ") <> show a <> " and " <> show b <> " change order, or are mixed" ]
      _, _, _, _ -> []

  disordered = if sameWay then nodeFaults <> barFaults <> ribbonFaults else [ "the two layouts run different ways" ]

  -- ---- the rung ---------------------------------------------------------------------------
  ordered = Array.null disordered
  nested = ordered && Array.null unaccounted && all (_.bar.inPlace) paired
  rung
    | nested && Array.null moved = Held
    | nested = Nested
    | ordered = Ordered
    | otherwise = Rearranged
