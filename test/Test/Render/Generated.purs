-- | Flows made by rule from a seed: layered, every wire carrying a
-- | quantity, with a nested grouping whose groups take their members from
-- | any tiers. For holding a pair of layouts to the laws of continuity on
-- | every pair of cuts that differ by one group.
module Test.Render.Generated
  ( Made
  , made
  , cutsOf
  , pairsOf
  , groupsOf
  , quantity
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Flow, GroupId(..), StepId(..), mapPayloads)
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Sankey (Widths(..))
import Test.Render.Fixtures (flowOf)

type Made = { seed :: Int, flow :: Flow Unit Unit (Array Number), grouping :: Array Grouping, nodes :: Int, edges :: Int }

-- | An edge is as wide as what its last wire carries.
quantity :: Widths (Array Number)
quantity = Sankey \edge -> sum (NonEmptyArray.last edge.wires).payload

-- a linear congruential generator; multiplication wraps as Int does
next :: Int -> Int
next s = s * 1664525 + 1013904223

-- | A number from 0 to below `n`, and the next state.
draw :: Int -> Int -> { value :: Int, state :: Int }
draw n s = let s' = next s in { value: (Int.quot s' 65536 `mod` 32768) `mod` max 1 n, state: s' }

type Link = { from :: String, to :: String, carries :: Number }

made :: Int -> Made
made seed = { seed, flow, grouping, nodes: Array.length names, edges: Array.length links }
  where
  d0 = draw 3 (seed * 7919 + 17)
  tierCount = 3 + d0.value

  -- how many nodes each tier has
  tiers = foldl
    (\acc _ -> let d = draw 3 acc.state in { state: d.state, sizes: Array.snoc acc.sizes (1 + d.value) })
    { state: d0.state, sizes: [] }
    (Array.range 1 tierCount)

  nameAt tier place = "t" <> show tier <> "n" <> show place
  namesOf tier = Array.range 0 (fromMaybe 1 (Array.index tiers.sizes tier) - 1) <#> nameAt tier
  names = Array.concatMap namesOf (Array.range 0 (tierCount - 1))

  pick :: Array String -> Int -> { value :: Maybe String, state :: Int }
  pick from s = let d = draw (Array.length from) s in { value: Array.index from d.value, state: d.state }

  -- every node after the first tier is fed from the tier before, once or
  -- twice, and one in four from two tiers before as well (a bypass)
  fed = foldl
    ( \acc tier -> foldl
        ( \inner to ->
            let
              a = pick (namesOf (tier - 1)) inner.state
              wa = draw 9 a.state
              b = draw 2 wa.state
              second = pick (namesOf (tier - 1)) b.state
              wb = draw 9 second.state
              skip = draw 4 wb.state
              far = pick (namesOf (tier - 2)) skip.state
              wf = draw 9 far.state
              one = Array.fromFoldable (a.value <#> \from -> { from, to, carries: Int.toNumber (1 + wa.value) })
              two = if b.value == 0 && second.value /= a.value then Array.fromFoldable (second.value <#> \from -> { from, to, carries: Int.toNumber (1 + wb.value) }) else []
              three = if skip.value == 0 && tier >= 2 then Array.fromFoldable (far.value <#> \from -> { from, to, carries: Int.toNumber (1 + wf.value) }) else []
            in
              { state: wf.state, links: inner.links <> one <> two <> three }
        )
        acc
        (namesOf tier)
    )
    { state: tiers.state, links: [] }
    (Array.range 1 (tierCount - 1))

  -- and every node before the last tier that feeds nothing feeds something
  drained = foldl
    ( \acc tier -> foldl
        ( \inner from ->
            if Array.any (\link -> link.from == from) inner.links then inner
            else
              let
                a = pick (namesOf (tier + 1)) inner.state
                wa = draw 9 a.state
              in
                { state: wa.state, links: inner.links <> Array.fromFoldable (a.value <#> \to -> { from, to, carries: Int.toNumber (1 + wa.value) }) }
        )
        acc
        (namesOf tier)
    )
    fed
    (Array.range 0 (tierCount - 2))

  links :: Array Link
  links = drained.links

  -- one item to a transfer
  itemOf link = link.from <> ">" <> link.to
  flow = mapPayloads identity identity (\carried -> [ carried ]) $ flowOf []
    ( names <#> \name ->
        { step: name
        , reads: Array.filter (\link -> link.to == name) links <#> \link -> { item: itemOf link, carries: link.carries }
        , writes: Array.filter (\link -> link.from == name) links <#> \link -> { item: itemOf link, carries: link.carries }
        }
    )

  -- the nodes in an order made from the seed
  shuffled = map _.name $ Array.sortWith _.key $ (foldl (\acc name -> let d = draw 1000 acc.state in { state: d.state, found: Array.snoc acc.found { key: Tuple d.value name, name } }) { state: drained.state, found: [] } names).found

  -- up to three top-level groups of two to five nodes; in each, sometimes,
  -- a group of some of them, and in that, sometimes, another
  grouping = (foldl topLevel { state: next drained.state, left: shuffled, groups: [], count: 0 } (Array.range 1 3)).groups

  topLevel acc place =
    let
      size = draw 4 acc.state
      taken = Array.take (2 + size.value) acc.left
      wanted = draw 4 size.state
    in
      if Array.length taken < 2 || (place > 1 && wanted.value == 0) then acc { state = wanted.state }
      else
        let
          inner = nest ("G" <> show place) 1 taken wanted.state
        in
          acc { state = inner.state, left = Array.drop (Array.length taken) acc.left, groups = Array.snoc acc.groups (Group (GroupId ("G" <> show place)) ("G" <> show place) inner.members) }

  nest :: String -> Int -> Array String -> Int -> { members :: Array Grouping, state :: Int }
  nest name depth within s =
    let
      wanted = draw 2 s
      size = draw (Array.length within) wanted.state
      inside = Array.take (1 + size.value) within
      outside = Array.drop (1 + size.value) within
    in
      if depth >= 3 || wanted.value == 0 || Array.length within < 2 then { members: map (Member <<< StepId) within, state: size.state }
      else
        let
          deeper = nest (name <> "i") (depth + 1) inside size.state
        in
          { members: [ Group (GroupId (name <> "i")) (name <> "i") deeper.members ] <> map (Member <<< StepId) outside, state: deeper.state }

-- | Every group of a grouping, outermost first.
groupsOf :: Array Grouping -> Array GroupId
groupsOf = Array.concatMap case _ of
  Group group _ inner -> [ group ] <> groupsOf inner
  _ -> []

-- | Every cut: every set of closed groups.
cutsOf :: Array Grouping -> Array (Set GroupId)
cutsOf grouping = foldl (\cuts group -> cuts <> map (Set.insert group) cuts) [ Set.empty ] (groupsOf grouping)

-- | Every pair of cuts that differ by one group: the cut with the group
-- | closed, and the cut with it open.
pairsOf :: Array Grouping -> Array { group :: GroupId, shut :: Set GroupId, open :: Set GroupId }
pairsOf grouping = do
  cut <- cutsOf grouping
  group <- Array.fromFoldable cut
  pure { group, shut: cut, open: Set.delete group cut }
