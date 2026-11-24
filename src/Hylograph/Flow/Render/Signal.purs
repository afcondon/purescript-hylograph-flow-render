-- | Signals of identity that do not depend on place (0.6).
-- |
-- | When a change of cut moves things (`Continuity` says the pair is only
-- | `Ordered`, or less), another channel has to carry the identity that
-- | position lost. Two are offered to a page as decorations:
-- |
-- | - **hue**: a group has a hue, its closed bar is drawn in it, and so are
-- |   its parts when it is open (`hued`). A group inside a hued group that
-- |   has no hue of its own takes the hue of the group over it.
-- | - **the outline bearing the closed name**: the layout engines draw it
-- |   already (`PlacedOutline.name`), for every open group.
-- |
-- | The page decides when to use them; `Continuity.continuity` is what it
-- | decides by.
module Hylograph.Flow.Render.Signal
  ( groupsOver
  , hueOf
  , hued
  ) where

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Hylograph.Flow (ElementKey(..), GroupId, ItemId(..), StepId(..))
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Tree (Decoration)

-- | Every group over each element a grouping names, outermost first,
-- | whichever are open; a closed group's step and summary are under the
-- | groups over it and under itself.
groupsOver :: Array Grouping -> ElementKey -> Array GroupId
groupsOver grouping = \key -> case key of
  StepKey (GroupStep group) -> fromMaybe [ group ] (Map.lookup group found.ofGroup)
  ItemKey (GroupItem group) -> fromMaybe [ group ] (Map.lookup group found.ofGroup)
  _ -> fromMaybe [] (Map.lookup key found.ofKey)
  where
  found :: { ofKey :: Map ElementKey (Array GroupId), ofGroup :: Map GroupId (Array GroupId) }
  found = foldl (walk []) { ofKey: Map.empty, ofGroup: Map.empty } grouping
  walk chain known = case _ of
    Member step -> known { ofKey = Map.insert (StepKey step) chain known.ofKey }
    Holds item -> known { ofKey = Map.insert (ItemKey item) chain known.ofKey }
    Group group _ inner -> foldl (walk (Array.snoc chain group)) (known { ofGroup = Map.insert group (Array.snoc chain group) known.ofGroup }) inner

-- | An element's hue: that of the innermost group over it that has one.
hueOf :: Array Grouping -> (GroupId -> Maybe String) -> ElementKey -> Maybe String
hueOf grouping hue = \key -> foldl (\found group -> hue group <|> found) Nothing (over key)
  where
  over = groupsOver grouping

-- | A decoration with every node in a hued group drawn in its group's hue,
-- | closed or open, and every edge that leaves such a node likewise (an
-- | edge that leaves a node of no hue takes the hue of the node it enters).
hued :: forall wire. Array Grouping -> (GroupId -> Maybe String) -> Decoration wire -> Decoration wire
hued grouping hue decoration = decoration
  { node = \node -> let mark = decoration.node node in mark { colour = of' node.key <|> mark.colour }
  , edge = \edge -> let line = decoration.edge edge in line { colour = of' edge.from <|> of' edge.to <|> line.colour }
  }
  where
  of' = hueOf grouping hue
