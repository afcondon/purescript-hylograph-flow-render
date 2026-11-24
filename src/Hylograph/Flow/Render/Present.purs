-- | Present mode: nothing hidden, the folded parts drawn quiet (0.6).
-- |
-- | An alternative to folding. The picture is the layout with every group
-- | open, whatever the cut; the groups the cut closes are *quiet*: their
-- | nodes are still there, at their breadths, with their ribbons, and are
-- | made to take less of the eye.
-- |
-- | THE RULE IN A SANKEY: BREADTH IS DATA AND IS NEVER REDUCED. A quiet
-- | node may lose length along the flow, its colour, and its words; it may
-- | not get narrower, and neither may a ribbon. So de-emphasis is two
-- | things and no others: `present` shortens a quiet node along the flow
-- | (about its middle, its ribbons following its sides), and `quietly`
-- | marks quiet nodes and the ribbons between two of them with the class
-- | `flow-quiet`, for a stylesheet to mute. Every node is as broad across
-- | the flow, and every ribbon as broad, as in the layout it was given.
-- |
-- | What it does not do: draw the tiers of a quiet group nearer together.
-- | That would move everything downstream of it, which the layout engine
-- | would have to be told of; here only the nodes are shortened.
module Hylograph.Flow.Render.Present
  ( quietKeys
  , present
  , quietly
  , quietClass
  ) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Hylograph.Flow (ElementKey, GroupId)
import Hylograph.Flow.Render.Engine (Direction(..), Layout)
import Hylograph.Flow.Render.Reshape (reshaped)
import Hylograph.Flow.Render.Tree (Decoration)

-- | The nodes of a layout that are in a quiet group: the members of those
-- | groups' outlines. (The layout must have been made with the picture told
-- | of its groups, and with them open.)
quietKeys :: forall wire. Set GroupId -> Layout wire -> Set ElementKey
quietKeys quiet laid = Set.fromFoldable do
  outline <- Array.filter (\found -> Set.member found.group quiet) laid.outlines
  index <- outline.members
  Array.fromFoldable (Array.find (\n -> n.index == index) laid.nodes) <#> _.node.key

-- | A layout with the nodes of the quiet groups shortened along the flow to
-- | a share of what they were (`length`, from 0 to 1), about their middles;
-- | nothing is made narrower.
present :: forall wire. Number -> Set GroupId -> Layout wire -> Layout wire
present length quiet laid = reshaped
  { node: \n ->
      if Set.member n.node.key keys then case laid.direction of
        LeftToRight -> n.box { x = n.box.x + n.box.width * (1.0 - share) / 2.0, width = n.box.width * share }
        TopDown -> n.box { y = n.box.y + n.box.height * (1.0 - share) / 2.0, height = n.box.height * share }
      else n.box
  , outline: _.box
  , width: laid.width
  , height: laid.height
  }
  laid
  where
  keys = quietKeys quiet laid
  share = max 0.0 (min 1.0 length)

-- | The class a quiet mark is given.
quietClass :: String
quietClass = "flow-quiet"

-- | A decoration with `flow-quiet` added to the class of every quiet node
-- | and of every edge between two quiet nodes.
quietly :: forall wire. Set ElementKey -> Decoration wire -> Decoration wire
quietly keys decoration = decoration
  { node = \node -> let mark = decoration.node node in if Set.member node.key keys then mark { className = join mark.className } else mark
  , edge = \edge -> let line = decoration.edge edge in if Set.member edge.from keys && Set.member edge.to keys then line { className = join line.className } else line
  }
  where
  join name = if name == "" then quietClass else name <> " " <> quietClass
