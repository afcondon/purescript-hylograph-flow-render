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
  , palette
  , shade
  , coloured
  ) where

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Hylograph.Flow (ElementKey(..), GroupId, ItemId(..), Projection, StepId(..))
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Tree (Decoration)
import Data.Int (floor, hexadecimal, round, toNumber, toStringAs)
import Data.Int as Int
import Data.String as String
import Data.Tuple (Tuple(..))
import Prelude

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

-- | The hues of colour by default (0.7): five, in a fixed order.
-- |
-- | In a flow drawing any two ribbons may cross, so every *pair* of hues
-- | has to be told apart, not only neighbours. These five are the largest
-- | set found, among the eight hues of a general-purpose categorical
-- | palette and Okabe and Ito's eight, that passes a palette validator on
-- | all pairs, against a white surface: every pair at least 8 apart in
-- | OKLab (times 100) under simulated protanopia and deuteranopia (the
-- | worst is 9.1, yellow and aqua, protan), at least 15 apart in full
-- | colour (the worst 15.6, green and aqua), all in one band of lightness.
-- | Under simulated tritanopia the worst pair is 6.7 apart. No sixth hue of
-- | those tried could be added: so a picture with more than five things to
-- | tell apart reuses a hue, and `Measures.overlapsInOneColour` says where
-- | that matters. (Okabe and Ito's set whole fails the same check on its
-- | yellow's lightness; with its yellow left out its worst pair passes, but
-- | no more than five of it and the other set together pass all pairs.)
-- |
-- | Blue, aqua, yellow, green, sky.
palette :: Array String
palette = [ "#2a78d6", "#1baf7a", "#eda100", "#008300", "#56b4e9" ]

-- | A colour made lighter (towards 1) or darker (towards -1): mixed with
-- | white or with black by that share. A colour not written as `#rrggbb`
-- | is itself.
shade :: Number -> String -> String
shade by colour = case channels of
  [ Just r, Just g, Just b ] | String.length colour == 7 -> "#" <> hex (mix r) <> hex (mix g) <> hex (mix b)
  _ -> colour
  where
  channels = [ 1, 3, 5 ] <#> \at -> Int.fromStringAs hexadecimal (String.take 2 (String.drop at colour))
  towards = if by >= 0.0 then 255.0 else 0.0
  share = min 1.0 (if by >= 0.0 then by else -by)
  mix channel = round (toNumber channel + (towards - toNumber channel) * share)
  hex n = let written = toStringAs hexadecimal (max 0 (min 255 n)) in if String.length written < 2 then "0" <> written else written

-- | Colour by default (0.7): a decoration with every node that gives
-- | anything, and every ribbon, in a hue.
-- |
-- | The flow's top-level blocks that give something (a top-level group, or
-- | a node no group holds; one that only receives stays in the ink) take
-- | the `palette`'s hues in the order they are given, round and round. A
-- | group's parts keep the group's hue in shades, from darker to lighter
-- | in the order given, so that two parts of one group can be told apart
-- | and are still seen to be of one group; a closed group's bar is the
-- | group's hue itself. A ribbon takes the colour of where it comes from,
-- | in one of three tints by where it goes (see `tinted` below).
-- | So a ribbon keeps its hue whatever is open, and an overlap is grey on
-- | grey only where two blocks share a hue (more than five give) or two
-- | ribbons leave one node.
-- |
-- | `finest`: the flow's projection with nothing closed, in the order the
-- | hues are to be given (the unfolding engine's one order, say: its nodes
-- | sorted by `Unfold.rankOf`). Text is not coloured: the tree writes
-- | every label in the ink.
coloured :: forall wire. Array Grouping -> Projection wire -> Decoration wire -> Decoration wire
coloured grouping finest decoration = decoration
  { node = \node -> let mark = decoration.node node in mark { colour = of' node.key <|> mark.colour }
  , edge = \edge -> let line = decoration.edge edge in line { colour = (tinted edge.to <$> of' edge.from) <|> line.colour }
  }
  where
  over = groupsOver grouping
  -- Two ribbons that leave one node have one colour by the rule above, and
  -- they do cross one another (a long ribbon and a short one from one
  -- source). So a ribbon is its source's colour in one of three tints, by
  -- where it goes: as it is, a little lighter, a little darker, by the
  -- place of its far end in the order given. The hue is the source's
  -- still; two ribbons of one source to two ends are told apart unless
  -- their ends are three places apart.
  tinted to colour = case placeOf to `mod` 3 of
    1 -> shade 0.28 colour
    2 -> shade (-0.22) colour
    _ -> colour
  placeOf key = fromMaybe 0 case key of
    StepKey (GroupStep group) -> firstIn group
    ItemKey (GroupItem group) -> firstIn group
    _ -> Array.findIndex (\node -> node.key == key) finest.nodes
  firstIn group = Array.findIndex (\node -> Array.elem group (over node.key)) finest.nodes
  gives = foldl (\known edge -> Map.insert edge.from true known) Map.empty finest.edges
  -- the top-level block of an element: its outermost group, or itself
  blockOf key = case Array.head (over key) of
    Just group -> Left' group
    Nothing -> Right' key
  -- each element of the finest flow with its block, in the order given
  placed = finest.nodes <#> \node -> { key: node.key, block: blockOf node.key, giving: isJust (Map.lookup node.key gives) }
  blocks = Array.nubEq (map _.block (Array.filter _.giving placed))
  hueOfBlock block = Array.elemIndex block blocks >>= \place -> Array.index palette (place `mod` Array.length palette)
  membersOf block = Array.filter (\found -> found.block == block) placed
  -- an element's own colour: its block's hue, shaded by its place in the block
  colours :: Map ElementKey String
  colours = Map.fromFoldable $ blocks >>= \block -> case hueOfBlock block of
    Nothing -> []
    Just hue ->
      let
        members = membersOf block
        count = Array.length members
      in
        Array.mapWithIndex (\place found -> Tuple found.key (if count < 2 then hue else shade (-0.3 + 0.75 * toNumber place / toNumber (count - 1)) hue)) members
  -- a closed group's step or summary: the hue of its block, unshaded where
  -- the group is the block, and otherwise the shade of the middle of its members
  ofGroup group =
    let
      inside = Array.filter (\found -> Array.elem group (over found.key)) placed
      middle = Array.index inside (floor (toNumber (Array.length inside) / 2.0))
    in
      case Array.head inside of
        Just first | first.block == Left' group -> hueOfBlock first.block
        _ -> middle >>= \found -> Map.lookup found.key colours
  of' key = case key of
    StepKey (GroupStep group) -> ofGroup group
    ItemKey (GroupItem group) -> ofGroup group
    _ -> Map.lookup key colours

-- | A top-level block: a group, or a node no group holds.
data Block = Left' GroupId | Right' ElementKey

derive instance Eq Block
