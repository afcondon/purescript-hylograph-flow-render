-- | Room made down the page: for nodes taller than their edges make them,
-- | and for the outlines of open groups.
-- |
-- | The Sankey layout gives every box (a node, or the waypoint of a long
-- | edge) a column and a place down it, with one gap between any two boxes
-- | of a column. It has no way to be told that this box needs to be taller,
-- | or that these boxes are to stand in a frame that nothing else enters.
-- | This module moves boxes down, and only down, until both hold.
-- |
-- | What it is given is a forest of *blocks* in the order they are to stand
-- | in, top to bottom: a block is one box, or a cluster of blocks (an open
-- | group). A block's *extent* is the columns it spans, first to last, and
-- | its top and bottom; a cluster's is the extent of what is in it, with
-- | `padding` all round and `header` more above. The blocks of one level
-- | are taken in order, and each is moved down, rigidly, by the least that
-- | puts its top `gap` under the bottom of every earlier block of its level
-- | in **every column of its extent**, whether or not it has a box in that
-- | column. So the extents of the blocks of a level are disjoint
-- | rectangles (in columns by top and bottom), which is what makes an
-- | outline hold its members and nothing else.
-- |
-- | That works only when the order given agrees, in every column, with the
-- | order the boxes already stand in (a block never moves up past another).
-- | The caller sees to that: it holds the Sankey layout to the same order
-- | with `nodeSort`.
-- |
-- | A box with no place yet (`top: Nothing`: a member of an open group that
-- | no edge touches, which the Sankey layout would put on a shelf far from
-- | its group) is stood under what its level already has in its column.
module Hylograph.Flow.Render.Internal.Bands
  ( Leaf
  , Block(..)
  , Spacing
  , Frame
  , Arranged
  , arrange
  , leavesOf
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Hylograph.Flow (GroupId)

-- | One box. `top`: where the Sankey layout put it; `Nothing` for a box it
-- | did not place. `height`: how tall it is to be.
type Leaf = { name :: String, layer :: Int, top :: Maybe Number, height :: Number }

data Block
  = Single Leaf
  | Cluster GroupId (Array Block)

-- | `gap`: between two blocks of a level. `padding`: from a cluster's
-- | contents to its frame. `header`: more room above, for the group's name.
type Spacing = { gap :: Number, padding :: Number, header :: Number }

-- | A cluster as placed. `depth`: how many clusters it is inside.
-- | `levels`: how many levels of cluster are inside it. `leaves`: every box
-- | in it, however deep.
type Frame =
  { group :: GroupId
  , parent :: Maybe GroupId
  , depth :: Int
  , levels :: Int
  , top :: Number
  , bottom :: Number
  , firstLayer :: Int
  , lastLayer :: Int
  , leaves :: Array String
  }

-- | `tops`: each box's top, by name. `frames`: outermost first. `bottom`:
-- | the lowest bottom of anything (0 where there is nothing).
type Arranged = { tops :: Map String Number, frames :: Array Frame, bottom :: Number }

data Settled
  = SettledLeaf { name :: String, layer :: Int, top :: Number, height :: Number }
  | SettledCluster { group :: GroupId, top :: Number, bottom :: Number, firstLayer :: Int, lastLayer :: Int, within :: Array Settled }

type Extent = { top :: Number, bottom :: Number, firstLayer :: Int, lastLayer :: Int }

extentOf :: Settled -> Extent
extentOf = case _ of
  SettledLeaf leaf -> { top: leaf.top, bottom: leaf.top + leaf.height, firstLayer: leaf.layer, lastLayer: leaf.layer }
  SettledCluster cluster -> { top: cluster.top, bottom: cluster.bottom, firstLayer: cluster.firstLayer, lastLayer: cluster.lastLayer }

moved :: Number -> Settled -> Settled
moved by = case _ of
  SettledLeaf leaf -> SettledLeaf leaf { top = leaf.top + by }
  SettledCluster cluster -> SettledCluster cluster { top = cluster.top + by, bottom = cluster.bottom + by, within = map (moved by) cluster.within }

-- | The names of every box of a block.
leavesOf :: Block -> Array String
leavesOf = case _ of
  Single leaf -> [ leaf.name ]
  Cluster _ within -> Array.concatMap leavesOf within

arrange :: Spacing -> Array Block -> Arranged
arrange spacing blocks =
  { tops: Map.fromFoldable (Array.concatMap tops settled)
  , frames: Array.concatMap (frames Nothing 0) settled
  , bottom: foldl (\lowest block -> max lowest (extentOf block).bottom) 0.0 settled
  }
  where
  -- nothing of the picture stands above its top
  settled = settle (Just 0.0) blocks

  tops :: Settled -> Array (Tuple String Number)
  tops = case _ of
    SettledLeaf leaf -> [ Tuple leaf.name leaf.top ]
    SettledCluster cluster -> Array.concatMap tops cluster.within

  names :: Settled -> Array String
  names = case _ of
    SettledLeaf leaf -> [ leaf.name ]
    SettledCluster cluster -> Array.concatMap names cluster.within

  levelsIn :: Settled -> Int
  levelsIn = case _ of
    SettledLeaf _ -> 0
    SettledCluster cluster -> 1 + foldl (\most inner -> max most (levelsIn inner)) 0 cluster.within

  frames :: Maybe GroupId -> Int -> Settled -> Array Frame
  frames parent depth = case _ of
    SettledLeaf _ -> []
    whole@(SettledCluster cluster) ->
      [ { group: cluster.group
        , parent
        , depth
        , levels: levelsIn whole - 1
        , top: cluster.top
        , bottom: cluster.bottom
        , firstLayer: cluster.firstLayer
        , lastLayer: cluster.lastLayer
        , leaves: names whole
        }
      ]
        <> Array.concatMap (frames (Just cluster.group) (depth + 1)) cluster.within

  -- The blocks of one level, each as far down as the earlier ones push it.
  -- `floor`: the least top anything may have where no earlier block says.
  settle :: Maybe Number -> Array Block -> Array Settled
  settle floor level = (foldl step { skyline: Map.empty, done: [] } level).done
    where
    step :: { skyline :: Map Int Number, done :: Array Settled } -> Block -> { skyline :: Map Int Number, done :: Array Settled }
    step state block =
      let
        rough = roughly block
        extent = extentOf rough.settled
        columns = Array.range extent.firstLayer extent.lastLayer
        under = foldl
          ( \lowest column -> case Map.lookup column state.skyline, lowest of
              Just floorHere, Just known -> Just (max known (floorHere + spacing.gap))
              Just floorHere, Nothing -> Just (floorHere + spacing.gap)
              Nothing, _ -> lowest
          )
          Nothing
          columns
        least = case under of
          Just required -> Just required
          Nothing -> floor
        by =
          if rough.placed then case least of
            Just required -> max 0.0 (required - extent.top)
            Nothing -> 0.0
          else case least of
            Just required -> required - extent.top
            -- nothing in its column yet: level with the highest of its level
            Nothing -> fromMaybe 0.0 (Array.head (Array.sort (map (_.top <<< extentOf) state.done))) - extent.top
        final = moved by rough.settled
        bottom = (extentOf final).bottom
      in
        { skyline: foldl (\skyline column -> Map.insert column bottom skyline) state.skyline columns
        , done: Array.snoc state.done final
        }

  roughly :: Block -> { settled :: Settled, placed :: Boolean }
  roughly = case _ of
    Single leaf -> case leaf.top of
      Just top -> { settled: SettledLeaf { name: leaf.name, layer: leaf.layer, top, height: leaf.height }, placed: true }
      Nothing -> { settled: SettledLeaf { name: leaf.name, layer: leaf.layer, top: 0.0, height: leaf.height }, placed: false }
    Cluster group within ->
      let
        inner = settle Nothing within
        extents = map extentOf inner
        top = fromMaybe 0.0 (Array.head (Array.sort (map _.top extents)))
        bottom = foldl (\lowest extent -> max lowest extent.bottom) top extents
        firstLayer = fromMaybe 0 (Array.head (Array.sort (map _.firstLayer extents)))
        lastLayer = foldl (\last extent -> max last extent.lastLayer) firstLayer extents
      in
        { settled: SettledCluster { group, top: top - spacing.padding - spacing.header, bottom: bottom + spacing.padding, firstLayer, lastLayer, within: inner }
        , placed: Array.any hasPlace within
        }

  hasPlace :: Block -> Boolean
  hasPlace = case _ of
    Single leaf -> case leaf.top of
      Just _ -> true
      Nothing -> false
    Cluster _ within -> Array.any hasPlace within
