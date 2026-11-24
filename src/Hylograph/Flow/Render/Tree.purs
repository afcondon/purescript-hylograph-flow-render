-- | A layout as a HATS tree: one SVG, edges under nodes.
-- |
-- | The tree draws a `Layout` (`Hylograph.Flow.Render.Engine`) and does not
-- | know which engine made it: every engine's layout is drawn by this one
-- | function.
-- |
-- | An item (what holds a value) is an open box; a step (what computes) is
-- | a filled lozenge. An edge is one stroked path along its route
-- | (`Route.svgPath`): a ribbon as its ink says (a band as broad as it is,
-- | or a hairline down its middle), a polyline or a spline as a hairline; an
-- | edge turned back to close a cycle is dashed where it is a hairline. A
-- | port the engine placed is a small square on its node's side, open for
-- | an input and filled for an output, over the edges, so that an edge's
-- | end is seen to meet it.
-- |
-- | The layout's direction is the root's `data-direction`. A node's words
-- | are set from its left side where the flow runs left to right (its
-- | edges enter and leave by its ends, and the words read along it), and
-- | about its middle where the flow runs top-down (its edges enter and
-- | leave above and below its middle); an item's swatch is along the side
-- | the flow enters by.
-- |
-- | Everything else about an element is the caller's: its label, a short
-- | note beside the label, a class and a colour (`Decoration`), and
-- | whatever an `Overlay` knows of it (`withOverlay`). Clicks and the
-- | pointer's coming and going are told to the caller by element.
-- |
-- | A closed group (`Hylograph.Flow.Grouping.collapse`) is a step and, where
-- | it provides anything, a summary item, and is drawn as any step and any
-- | item are. Whether it should look otherwise is the caller's to say too:
-- | `withGroups` changes the marks of a group's step and summary, by group.
-- |
-- | An open group, where the layout was told of groups, is an outline under
-- | everything else: a quiet box round its members with its name in a strip
-- | at the top. The name is what takes gestures (a tab behind it, as wide
-- | as the name), told as `PointedGroup`, so a page can shut a group by a
-- | click on its name; the box itself takes none, and what is inside it is
-- | reached through it.
-- |
-- | A node's inside (0.4): where the decoration draws a node's inside
-- | (`Decoration.inside`), the tree draws nothing of the node but that and
-- | its ports: no box, no label, no words, no swatch, no note. The inside
-- | is a HATS tree in the node's own coordinates, its box's top left at
-- | 0,0, placed by a `g.flow-inside` translated to the box. It is for a node
-- | the caller sized (`Picture.fixed`), such as a table of rows, whose
-- | inside only the caller knows how to draw; the tree still places it,
-- | keys it, and tells its gestures.
-- |
-- | The keyboard (0.5): a mark the decoration calls a button
-- | (`Decoration.button`) can be reached by the Tab key and says what it is
-- | to a screen reader (`tabindex="0"`, `role="button"`, and the name given
-- | as its `aria-label`); the others are as they were. HATS tells clicks and
-- | the pointer and no keys, so pressing a button is the page's to arrange
-- | once: `Hylograph.Flow.Render.Keys.pressByKeys` makes Enter and Space on
-- | a focused button a click on it, which is then told as any click is.
-- |
-- | Words: where the layout set a node's words (`Picture.words`), those
-- | lines are drawn, the title's and then the detail's, and the mark's
-- | `label` is not used; its `note` stands at the right of the first line.
-- | Where it did not, the mark's `label` is written on one line, as in 0.2.
-- |
-- | Legibility (0.7; `treeWith` and its `Dress`, none of it drawn by `tree`
-- | or `treeLabelled` unless said):
-- |
-- | - a ribbon that runs against the flow carries marks of its direction
-- |   along it, and no other ribbon does (`Dress.marks`);
-- | - a ribbon drawn broader than it is (`Ink.Least`) is dashed and has the
-- |   class `flow-not-to-scale` in any tree, and with `Dress.marks` a break
-- |   mark across its middle;
-- | - a closed group's bar shows its seams (`Dress.seams`, which the
-- |   unfolding engine's `seams` gives): a line across the bar where two
-- |   parts divide, and beside a part that only receives a wall on the side
-- |   nothing leaves by, beside one that only gives a wall on the side
-- |   nothing enters by;
-- | - a group's name is a heading (`Dress.headings`): a closed top-level
-- |   group's name stands over the start of its bar, where its outline's
-- |   name stands when it is open, in the same letters, so the name is in
-- |   one place closed and open; and the labels of the nodes inside an open
-- |   group are lighter and smaller than it;
-- | - a group that has several outlines (the unfolding engine's tied
-- |   outlines: one for each run of its members) has its name on each and a
-- |   tie drawn from each to the next.
-- |
-- | What is drawn, for a stylesheet or a test to find:
-- |
-- | ```
-- | svg.flow[data-direction=left-to-right|top-down]
-- |   g.flow-outlines > g.flow-outline[data-group][data-depth][data-run]
-- |                       > rect.flow-outline-box, rect.flow-outline-tab, text.flow-outline-label[.flow-group-name]
-- |   g.flow-ties? > path.flow-tie[data-group]                      (where a group has several outlines)
-- |   g.flow-marks? > path.flow-direction[data-edge] | path.flow-scale-break[data-edge]   (Dress.marks)
-- |   (in a node, Dress.seams:) rect.flow-seam*, rect.flow-only[data-only=receives|gives][data-part]*
-- |   (Dress.headings:) text.flow-label.flow-group-name on a closed top-level group; .flow-part-name on a part's label
-- |   g.flow-edges > path.flow-edge.flow-ribbon|.flow-polyline|.flow-spline
-- |                    .flow-band|.flow-hairline[.flow-backward][.flow-swept][data-edge][data-from][data-to]
-- |   g.flow-nodes > g.flow-node.flow-item|.flow-step[.flow-lettered][data-key]
-- |                    > rect.flow-box, rect.flow-swatch?, text.flow-label,
-- |                      text.flow-label-more*, text.flow-detail*, text.flow-note?,
-- |                      rect.flow-port.flow-port-in|.flow-port-out[data-port]*
-- |                  | g.flow-node.flow-drawn > g.flow-inside[transform] > (the caller's),
-- |                      rect.flow-port.flow-port-in|.flow-port-out[data-port]*
-- | ```
module Hylograph.Flow.Render.Tree
  ( Mark
  , Line
  , Gesture(..)
  , Pointed(..)
  , Bands(..)
  , Labels(..)
  , Decoration
  , plainDecoration
  , withOverlay
  , withGroups
  , groupOf
  , pointedBy
  , keyName
  , groupName
  , hairline
  , strokeOf
  , directionName
  , portSide
  , tree
  , treeLabelled
  , Dress
  , plainDress
  , treeWith
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Hylograph.Flow (Edge, ElementKey(..), GroupId(..), ItemId(..), Node, StepId(..), WireId(..), itemLabel, portLabel, stepLabel)
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Layout, Only(..), PlacedEdge, PlacedNode, PlacedOutline, PlacedPort, Seamed, portPoint, runsOf)
import Hylograph.Flow.Render.Engine as Engine
import Hylograph.Flow.Render.Route (Ink(..), Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Overlay (Overlay)
import Hylograph.Flow.Overlay as Overlay
import Data.Int (toNumber)
import Data.Int as Int
import Data.Number (abs, sqrt)
import Hylograph.Flow.Render.Text (defaultLettering, estimateWidth, heavier, lineHeight)
import Hylograph.HATS (ElementType(..), Tree, elem, forEach, onClick, onMouseEnter, onMouseLeave, staticStr, withBehaviors)
import Hylograph.HATS.Friendly as Friendly

-- | A node's dress. `note`: a few letters beside the label (an operation, a
-- | count). `className`: added to the node's classes. `colour`: a step's
-- | fill, an item's swatch; `Nothing` for the ink.
type Mark = { label :: String, note :: Maybe String, className :: String, colour :: Maybe String }

-- | An edge's dress.
type Line = { className :: String, colour :: Maybe String }

data Gesture = Clicked | Entered | Left

derive instance Eq Gesture

instance Show Gesture where
  show = case _ of
    Clicked -> "Clicked"
    Entered -> "Entered"
    Left -> "Left"

-- | What a gesture was made on. A node is its element. An edge is a wire
-- | where it is exactly one (`Both`, and any edge between a drawn item and
-- | a drawn step); otherwise it is the path it stands for, with the undrawn
-- | elements it passes through. An open group's outline is its group.
data Pointed
  = PointedElement ElementKey
  | PointedPath { from :: ElementKey, to :: ElementKey, via :: Array ElementKey }
  | PointedGroup GroupId

derive instance Eq Pointed

instance Show Pointed where
  show = case _ of
    PointedElement key -> "(PointedElement " <> show key <> ")"
    PointedPath p -> "(PointedPath " <> show p.from <> " " <> show p.via <> " " <> show p.to <> ")"
    PointedGroup group -> "(PointedGroup " <> show group <> ")"

-- | How a ribbon that is a band is drawn (0.5).
-- |
-- | - `Stroked`: one path along its centre line, stroked as broad as the
-- |   ribbon is, as since 0.1. Right where a ribbon runs nearly straight
-- |   along the flow. Where it slants it is too thin, and its ends are not
-- |   flat on its nodes, because a stroke is broad across the line and not
-- |   across the flow: a ribbon 200 pixels broad that shifts 150 across in
-- |   a run of 90 bulges past both nodes.
-- | - `Swept`: the area the ribbon's span on its source sweeps to its span
-- |   on its target (`Route.sweptPath`), filled and not stroked: flat on
-- |   both nodes, and as broad across the flow all the way as the ribbon
-- |   is. Its colour is then its `fill`, which is what a stylesheet must
-- |   set. A band turned back to close a cycle, which runs both ways along
-- |   the flow, is stroked either way; so is a ribbon too thin to be drawn
-- |   as a band (3 pixels or less), which is a line.
data Bands = Stroked | Swept

derive instance Eq Bands

-- | Where a node's label is written (0.6; `treeLabelled`).
-- |
-- | - `Within`: in its box, as since 0.1.
-- | - `Beside least`: beside its box, for a node that is a thin bar and as
-- |   broad as its ribbons, no more (the unfolding engine's): after it
-- |   along the flow where the picture has room and before it where it has
-- |   not, left to right; over it, or under it in the first row, top-down.
-- |   In the ink's colour, since it is on the paper. A node less than
-- |   `least` pixels across the flow has no label drawn (its mark has the
-- |   class `flow-unlabelled`): labels beside bars that touch would run
-- |   into one another. And a step is then a bar with square corners, so
-- |   that parts that stand with no gap between them tile.
data Labels = Within | Beside Number

derive instance Eq Labels

-- | `inside` (0.4): a node's inside, drawn by the caller in the node's own
-- | coordinates, for the nodes it draws; see the module's header.
-- | `button` (0.5): the marks that are buttons for the keyboard, each with
-- | the name it is announced by; `Nothing` for a mark that is none.
-- | `bands` (0.5): how a ribbon that is a band is drawn.
type Decoration wire =
  { node :: Node -> Mark
  , edge :: Edge wire -> Line
  , notify :: Gesture -> Pointed -> Effect Unit
  , inside :: Node -> Maybe Tree
  , button :: Pointed -> Maybe String
  , bands :: Bands
  }

-- | Each node under its own label, nothing else said, gestures ignored, no
-- | inside drawn by the caller, no mark a button, bands stroked.
plainDecoration :: forall wire. Decoration wire
plainDecoration =
  { node: \node -> { label: node.label, note: Nothing, className: "", colour: Nothing }
  , edge: \_ -> { className: "", colour: Nothing }
  , notify: \_ _ -> pure unit
  , inside: const Nothing
  , button: const Nothing
  , bands: Stroked
  }

-- | A decoration changed by what an overlay knows: of a node, by its key;
-- | of an edge that is one wire, by the wire's.
withOverlay :: forall a wire. Overlay a -> (a -> Mark -> Mark) -> (a -> Line -> Line) -> Decoration wire -> Decoration wire
withOverlay known onNode onEdge decoration = decoration
  { node = \node -> case Overlay.lookup node.key known of
      Just fact -> onNode fact (decoration.node node)
      Nothing -> decoration.node node
  , edge = \edge -> case pointedBy edge of
      PointedElement key | Just fact <- Overlay.lookup key known -> onEdge fact (decoration.edge edge)
      _ -> decoration.edge edge
  }

-- | The closed group an element is: its step or its summary item.
groupOf :: ElementKey -> Maybe GroupId
groupOf = case _ of
  StepKey (GroupStep group) -> Just group
  ItemKey (GroupItem group) -> Just group
  _ -> Nothing

-- | A decoration changed for closed groups: the mark of a group's step and
-- | of its summary item, by the group. (A click on either is told as any
-- | click is, by its element; `groupOf` says which group that is, for a
-- | page that opens a group where it stands.)
withGroups :: forall wire. (GroupId -> Mark -> Mark) -> Decoration wire -> Decoration wire
withGroups onGroup decoration = decoration
  { node = \node -> case groupOf node.key of
      Just group -> onGroup group (decoration.node node)
      Nothing -> decoration.node node
  }

-- | What an edge is, for a gesture or an overlay: the wire it is, where it
-- | stands for one wire, by the identity the edge carries.
pointedBy :: forall wire. Edge wire -> Pointed
pointedBy edge = case NonEmptyArray.toArray edge.wires of
  [ wire ] -> PointedElement (WireKey wire.id)
  _ -> PointedPath { from: edge.from, to: edge.to, via: edge.via }

-- | An element's key as one string, for `data-key` and for HATS's keys.
-- | A closed group's step is `group:` and the group's id where a source's
-- | step is `step:` and its name, so a step and a group never share a key;
-- | likewise a closed group's summary item is `summary:` and the group's id
-- | where a source's item is `item:` and its name. Inside a wire's key the
-- | item, the step and the port are written as they read (`itemLabel`,
-- | `stepLabel`, `portLabel`).
keyName :: ElementKey -> String
keyName = case _ of
  ItemKey (ItemId item) -> "item:" <> item
  ItemKey (GroupItem (GroupId group)) -> "summary:" <> group
  StepKey (StepId name) -> "step:" <> name
  StepKey (GroupStep (GroupId group)) -> "group:" <> group
  WireKey (Reads item step port) -> "reads:" <> itemLabel item <> ">" <> stepLabel step <> "." <> portLabel port
  WireKey (Writes step port item) -> "writes:" <> stepLabel step <> "." <> portLabel port <> ">" <> itemLabel item

-- | A group's id as one string, for an outline's `data-group`.
groupName :: GroupId -> String
groupName (GroupId group) = group

ink :: String
ink = "#161616"

quiet :: String
quiet = "#8a8a8a"

paper :: String
paper = "#ffffff"

-- | The stroke of an edge that is drawn as a line and whose route says no
-- | stroke of its own: a polyline, a spline.
hairline :: Number
hairline = 1.25

-- | How wide an edge is stroked: a ribbon as its ink says, any other route
-- | as a `hairline`.
strokeOf :: Route -> Number
strokeOf = case _ of
  Ribbon ribbon -> case ribbon.ink of
    Band -> ribbon.breadth
    Hairline stroke -> stroke
    Least stroke -> stroke
  Polyline _ -> hairline
  Spline _ -> hairline

-- | A direction as the root's `data-direction` says it.
directionName :: Direction -> String
directionName = case _ of
  LeftToRight -> "left-to-right"
  TopDown -> "top-down"

-- | The side of a port's mark: 5 pixels, centred on the port.
portSide :: Number
portSide = 5.0

tree :: forall wire. Decoration wire -> Layout wire -> Tree
tree = treeLabelled Within

-- | What a tree draws beside a decoration (0.7); see the module's header.
-- | (Not fields of `Decoration`, so that no decoration written out whole
-- | has to change.)
type Dress =
  { labels :: Labels
  , marks :: Boolean
  , seams :: Array Seamed
  , headings :: Boolean
  }

-- | Labels within their boxes, and nothing of 0.7's: `tree`'s own dress.
plainDress :: Dress
plainDress = { labels: Within, marks: false, seams: [], headings: false }

-- | The tree with each node's label written where it is told (0.6):
-- | `tree` is `treeLabelled Within`. (Not a field of `Decoration`, so that
-- | no decoration written out whole before 0.6 has to change.)
treeLabelled :: forall wire. Labels -> Decoration wire -> Layout wire -> Tree
treeLabelled labels = treeWith plainDress { labels = labels }

-- | The tree, dressed (0.7): `treeLabelled labels` is `treeWith plainDress
-- | { labels = labels }`.
treeWith :: forall wire. Dress -> Decoration wire -> Layout wire -> Tree
treeWith dress decoration laid =
  elem SVG
    [ staticStr "class" "flow"
    , staticStr "data-direction" (directionName laid.direction)
    , Friendly.viewBox (-2.0) (-2.0) (laid.width + 4.0) (laid.height + 4.0)
    , Friendly.width (laid.width + 4.0)
    , Friendly.height (laid.height + 4.0)
    ]
    ( [ elem Group [ staticStr "class" "flow-outlines" ]
          [ forEach "flow-outlines" Group runs (\found -> groupName found.outline.group <> (if found.run == 0 then "" else "#" <> show found.run)) outlineMark ]
      ]
        <> (if Array.null ties then [] else [ elem Group [ staticStr "class" "flow-ties" ] [ forEach "flow-ties" Path ties _.key tieMark ] ])
        <>
          [ elem Group [ staticStr "class" "flow-edges" ]
              [ forEach "flow-edges" Path laid.edges (show <<< _.index) edgeMark ]
          ]
        <> (if dress.marks then [ elem Group [ staticStr "class" "flow-marks" ] [ forEach "flow-marks" Path marked (show <<< _.index) markOf ] ] else [])
        <>
          [ elem Group [ staticStr "class" "flow-nodes" ]
              [ forEach "flow-nodes" Group laid.nodes (\n -> show n.index <> " " <> keyName n.node.key) nodeMark ]
          ]
    )
  where
  labels = dress.labels

  -- each outline with which of its group's it is
  runs = runsOf laid.outlines

  -- a tie from each outline of a group to the next of the same group
  ties = runs # Array.mapMaybe \found ->
    Array.find (\other -> other.outline.group == found.outline.group && other.run == found.run + 1) runs <#> \next ->
      { key: groupName found.outline.group <> "#" <> show found.run, group: found.outline.group, from: found.outline.box, to: next.outline.box }

  tieMark :: { key :: String, group :: GroupId, from :: Engine.Box, to :: Engine.Box } -> Tree
  tieMark tie =
    let
      -- from the middle of the side of one outline that faces the next, to
      -- the middle of the side of the next that faces it
      ends = case laid.direction of
        LeftToRight -> { a: { x: tie.from.x + tie.from.width, y: tie.from.y + tie.from.height / 2.0 }, b: { x: tie.to.x, y: tie.to.y + tie.to.height / 2.0 } }
        TopDown -> { a: { x: tie.from.x + tie.from.width / 2.0, y: tie.from.y + tie.from.height }, b: { x: tie.to.x + tie.to.width / 2.0, y: tie.to.y } }
      bend = case laid.direction of
        LeftToRight -> let middle = (ends.a.x + ends.b.x) / 2.0 in { leaving: { x: middle, y: ends.a.y }, arriving: { x: middle, y: ends.b.y } }
        TopDown -> let middle = (ends.a.y + ends.b.y) / 2.0 in { leaving: { x: ends.a.x, y: middle }, arriving: { x: ends.b.x, y: middle } }
      at point = show point.x <> "," <> show point.y
    in
      elem Path
        [ staticStr "class" "flow-tie"
        , staticStr "data-group" (groupName tie.group)
        , Friendly.d ("M" <> at ends.a <> " C" <> at bend.leaving <> " " <> at bend.arriving <> " " <> at ends.b)
        , Friendly.fill "none"
        , Friendly.stroke quiet
        , Friendly.strokeWidth 1.0
        , staticStr "stroke-dasharray" "2 3"
        , staticStr "pointer-events" "none"
        ]
        []

  -- the ribbons that carry a mark: turned back, or not to scale
  marked = Array.filter (\e -> e.course == Backward || notToScale e.route) laid.edges

  notToScale :: Route -> Boolean
  notToScale = case _ of
    Ribbon ribbon -> case ribbon.ink of
      Least _ -> true
      _ -> false
    _ -> false

  -- A ribbon turned back: arrowheads along its line, pointing the way it
  -- runs. A ribbon not to scale and not turned back: two short strokes
  -- across its middle, as a break in an axis is drawn.
  markOf :: PlacedEdge wire -> Tree
  markOf e =
    let
      points = Route.flatten e.route
      legs = Array.zipWith (\a b -> { a, b, long: Route.distance a b }) points (Array.drop 1 points)
      whole = Array.foldl (\total leg -> total + leg.long) 0.0 legs
      -- the point so far along the line, and the way the line runs there
      placeAt far = (Array.foldl
        ( \state leg -> case state.found of
            Just _ -> state
            Nothing ->
              if leg.long > 0.0 && state.gone + leg.long >= far then
                let part = (far - state.gone) / leg.long
                in state { found = Just { at: { x: leg.a.x + (leg.b.x - leg.a.x) * part, y: leg.a.y + (leg.b.y - leg.a.y) * part }, way: { x: (leg.b.x - leg.a.x) / leg.long, y: (leg.b.y - leg.a.y) / leg.long } } }
              else state { gone = state.gone + leg.long }
        )
        { gone: 0.0, found: Nothing }
        legs).found
      size = max 3.5 (min 9.0 (0.3 * strokeOf e.route))
      pitch = max 30.0 (6.0 * size)
      count = max 1 (Int.floor (whole / pitch))
      at point = show point.x <> "," <> show point.y
      arrow found =
        let
          tip = { x: found.at.x + found.way.x * size, y: found.at.y + found.way.y * size }
          wing side = { x: found.at.x - found.way.x * size + side * found.way.y * size, y: found.at.y - found.way.y * size - side * found.way.x * size }
        in
          "M" <> at (wing 1.0) <> " L" <> at tip <> " L" <> at (wing (-1.0))
      break found =
        let
          slant = { x: (found.way.x - found.way.y) / sqrt 2.0, y: (found.way.y + found.way.x) / sqrt 2.0 }
          stroke off = let c = { x: found.at.x + found.way.x * off, y: found.at.y + found.way.y * off } in "M" <> at { x: c.x - slant.x * 5.0, y: c.y - slant.y * 5.0 } <> " L" <> at { x: c.x + slant.x * 5.0, y: c.y + slant.y * 5.0 }
        in
          stroke (-2.0) <> " " <> stroke 2.0
      backward = e.course == Backward
      drawn =
        if backward then Array.intercalate " " (Array.mapMaybe (\index -> arrow <$> placeAt ((toNumber index + 0.5) * whole / toNumber count)) (Array.range 0 (count - 1)))
        else Array.intercalate " " (Array.fromFoldable (break <$> placeAt (whole / 2.0)))
    in
      elem Path
        [ staticStr "class" (if backward then "flow-direction" else "flow-scale-break")
        , staticStr "data-edge" (show e.index)
        , Friendly.d drawn
        , Friendly.fill "none"
        , Friendly.stroke ink
        , Friendly.strokeWidth (if backward then 1.75 else 1.25)
        , staticStr "stroke-linecap" "round"
        , staticStr "stroke-linejoin" "round"
        , staticStr "pointer-events" "none"
        ]
        []

  -- the nodes inside an open group's outline: its parts
  partsOfOpen :: Array Int
  partsOfOpen = Array.concatMap _.members laid.outlines

  seamsOf :: PlacedNode -> Maybe Seamed
  seamsOf n = Array.find (\found -> found.node == n.index) dress.seams

  gestures pointed =
    [ onClick (decoration.notify Clicked pointed)
    , onMouseEnter (decoration.notify Entered pointed)
    , onMouseLeave (decoration.notify Left pointed)
    ]

  -- what makes a mark a button for the keyboard, where the decoration
  -- says it is one
  buttonOf pointed = case decoration.button pointed of
    Just name -> [ staticStr "tabindex" "0", staticStr "role" "button", staticStr "aria-label" name ]
    Nothing -> []

  -- where an outline's name is written, down the page: in the middle of
  -- its strip, or on its top edge where it has none
  nameLine :: PlacedOutline -> Number
  nameLine found = if found.header <= 0.0 then found.box.y else found.box.y + found.header / 2.0

  -- where an outline's name ends, across the page
  nameEnd :: PlacedOutline -> Number
  nameEnd found = found.box.x + (if found.header <= 0.0 then indentOf found else 0.0) + 16.0 + heavier * estimateWidth 11.0 found.name

  -- how far in a strip-less outline's name is set: past the end of the
  -- name of the nearest outline over it that is written on its line
  indentOf :: PlacedOutline -> Number
  indentOf found = go (found.parent >>= \parent -> Array.find (\other -> other.group == parent) laid.outlines)
    where
    go = case _ of
      Just above ->
        if abs (nameLine above - nameLine found) < 11.0 then max 0.0 (nameEnd above - found.box.x)
        else go (above.parent >>= \parent -> Array.find (\other -> other.group == parent) laid.outlines)
      Nothing -> 0.0

  -- an open group: its box takes no gesture, its name does
  outlineMark :: { run :: Int, outline :: PlacedOutline } -> Tree
  outlineMark { run, outline } =
    let
      nameSize = if dress.headings then 12.0 else 11.0
      stripless = outline.header <= 0.0
      nameWidth named = 16.0 + heavier * estimateWidth nameSize named
      -- The name takes gestures through its tab. An outline with no strip
      -- for its name (the unfolding engine's inner outlines, which take
      -- no room across the flow) has its name somewhere else (0.6; an
      -- outline with a strip is as it was):
      --
      -- * up its own margin, where the flow runs left to right and the
      --   outline has a margin before its first member broad enough for a
      --   line of the name: read from the bottom up, from the outline's
      --   top down as far as it goes. No other outline's name and no node
      --   is in that margin, so the name can always be pointed at;
      -- * otherwise on its top edge, after the names of the outlines over
      --   it that are on that same line.
      margin = case laid.direction of
        LeftToRight -> Array.foldl (\least index -> case Array.find (\n -> n.index == index) laid.nodes of
          Just n -> min least (n.box.x - outline.box.x)
          Nothing -> least) outline.box.width outline.members
        TopDown -> 0.0
      upright = stripless && margin >= nameSize - 2.0
      tabHeight = if upright then max (nameSize + 1.0) (min outline.box.height (nameWidth outline.name)) else if stripless then nameSize + 1.0 else outline.header
      tabWidth = if upright then margin else if stripless then nameWidth outline.name else min outline.box.width (nameWidth outline.name)
      indent = if stripless && not upright then indentOf outline else 0.0
      labelX = if upright then outline.box.x + margin / 2.0 else outline.box.x + indent + 8.0
      labelY = if upright then outline.box.y + 4.0 else if dress.headings then outline.box.y + outline.header / 2.0 else outline.box.y + outline.header / 2.0 + 1.0
    in
      withBehaviors (gestures (PointedGroup outline.group)) $
        elem Group
          ( [ staticStr "class" "flow-outline"
            , staticStr "data-group" (groupName outline.group)
            , staticStr "data-depth" (show outline.depth)
            ]
              <> (if dress.headings || run > 0 then [ staticStr "data-run" (show run) ] else [])
              <> buttonOf (PointedGroup outline.group)
          )
          [ elem Rect
              [ staticStr "class" "flow-outline-box"
              , Friendly.x outline.box.x
              , Friendly.y outline.box.y
              , Friendly.width outline.box.width
              , Friendly.height outline.box.height
              , staticStr "rx" "3"
              , Friendly.fill "none"
              , Friendly.stroke quiet
              , Friendly.strokeWidth 1.0
              , staticStr "pointer-events" "none"
              ]
              []
          , elem Rect
              [ staticStr "class" "flow-outline-tab"
              , Friendly.x (outline.box.x + indent)
              , Friendly.y (if stripless && not upright then outline.box.y - tabHeight / 2.0 else outline.box.y)
              , Friendly.width tabWidth
              , Friendly.height tabHeight
              , Friendly.fill paper
              , staticStr "fill-opacity" "0"
              , staticStr "pointer-events" "all"
              ]
              []
          , elem Text
              ( [ staticStr "class" (classes [ "flow-outline-label", if upright then "flow-outline-label-upright" else "", if dress.headings then "flow-group-name" else "" ])
                , Friendly.x labelX
                , Friendly.y labelY
                , Friendly.textAnchor (if upright then "end" else "start")
                , staticStr "dominant-baseline" "central"
                , Friendly.fontSize (show nameSize)
                , Friendly.fontWeight (if dress.headings then "700" else "600")
                , Friendly.fill (if dress.headings then ink else quiet)
                , staticStr "pointer-events" "none"
                , staticStr "textContent" outline.name
                ]
                  <> (if upright then [ staticStr "transform" ("rotate(-90 " <> show labelX <> " " <> show labelY <> ")") ] else [])
              )
              []
          ]

  -- a band lets what it crosses show through; a hairline is solid
  isBand :: PlacedEdge wire -> Boolean
  isBand e = strokeOf e.route > 3.0

  isBandRibbon :: Route -> Boolean
  isBandRibbon = case _ of
    Ribbon ribbon -> ribbon.ink == Band
    _ -> false

  kindOf :: Route -> String
  kindOf = case _ of
    Ribbon _ -> "flow-ribbon"
    Polyline _ -> "flow-polyline"
    Spline _ -> "flow-spline"

  edgeMark :: PlacedEdge wire -> Tree
  edgeMark e =
    let
      line = decoration.edge e.edge
      backward = e.course == Backward
      swept = decoration.bands == Swept && isBand e && not backward && isBandRibbon e.route
      -- one pixel across the flow
      across = case laid.direction of
        LeftToRight -> { x: 0.0, y: 1.0 }
        TopDown -> { x: 1.0, y: 0.0 }
    in
      withBehaviors (gestures (pointedBy e.edge)) $
        elem Path
          ( [ staticStr "class" (classes [ "flow-edge", kindOf e.route, if backward then "flow-backward" else "", if isBand e then "flow-band" else "flow-hairline", if swept then "flow-swept" else "", if notToScale e.route then "flow-not-to-scale" else "", line.className ])
            , staticStr "data-edge" (show e.index)
            , staticStr "data-from" (keyName e.edge.from)
            , staticStr "data-to" (keyName e.edge.to)
            ]
              <>
                ( if swept then
                    [ Friendly.d (Route.sweptPath across e.route)
                    , Friendly.fill (fromMaybe ink line.colour)
                    , Friendly.fillOpacity "0.22"
                    , Friendly.stroke "none"
                    ]
                  else
                    [ Friendly.d (Route.svgPath e.route)
                    , Friendly.fill "none"
                    , Friendly.stroke (fromMaybe ink line.colour)
                    , Friendly.strokeWidth (strokeOf e.route)
                    , Friendly.strokeOpacity (if isBand e then "0.22" else "1")
                    ]
                )
              <> (if notToScale e.route then [ staticStr "stroke-dasharray" "3 2" ] else if backward && not (isBand e) then [ staticStr "stroke-dasharray" "5 4" ] else [])
              <> buttonOf (pointedBy e.edge)
          )
          []

  nodeMark :: PlacedNode -> Tree
  nodeMark n =
    let
      mark = decoration.node n.node
      isStep = case n.node.key of
        StepKey _ -> true
        _ -> false
      middle = n.box.y + n.box.height / 2.0
      -- where a line of words is set from, and how it is anchored there
      setting = case laid.direction of
        LeftToRight -> { anchor: "start", x: \inset -> n.box.x + inset }
        TopDown -> { anchor: "middle", x: \_ -> n.box.x + n.box.width / 2.0 }
      lettering = if isStep then paper else ink
      text className anchor x y size weight content =
        elem Text
          [ staticStr "class" className
          , Friendly.x x
          , Friendly.y y
          , Friendly.textAnchor anchor
          , staticStr "dominant-baseline" "central"
          , Friendly.fontSize size
          , Friendly.fontWeight weight
          , Friendly.fill lettering
          , staticStr "pointer-events" "none"
          , staticStr "textContent" content
          ]
          []
      -- words the layout set: the lines one under another, the block of
      -- them in the middle of the box (at the layout's default sizes: the
      -- tree is not told another lettering)
      titleLine = lineHeight defaultLettering defaultLettering.titleSize
      detailLine = lineHeight defaultLettering defaultLettering.detailSize
      besideLeast = case labels of
        Beside least -> Just least
        Within -> Nothing
      acrossFlow = case laid.direction of
        LeftToRight -> n.box.height
        TopDown -> n.box.width
      unlabelled = case besideLeast of
        Just least -> acrossFlow < least
        Nothing -> false
      -- a part of an open group, whose label is subordinate to the group's
      -- name; and a closed group no group is open over, whose name is a
      -- heading over the start of its bar (left to right)
      isPart = dress.headings && Array.elem n.index partsOfOpen
      isHeaded = dress.headings && laid.direction == LeftToRight && isJust (groupOf n.node.key) && not (Array.elem n.index partsOfOpen)
      -- a label beside its bar, on the paper
      besideLabel =
        let
          long = heavier * estimateWidth 12.0 mark.label
          plain className anchor x y = elem Text
            [ staticStr "class" (classes [ className, if isPart then "flow-part-name" else "", if isHeaded then "flow-group-name" else "" ])
            , Friendly.x x
            , Friendly.y y
            , Friendly.textAnchor anchor
            , staticStr "dominant-baseline" "central"
            , Friendly.fontSize (if isPart then "11" else "12")
            , Friendly.fontWeight (if isPart then "400" else if isHeaded then "700" else "600")
            , Friendly.fill ink
            , staticStr "pointer-events" "none"
            , staticStr "textContent" mark.label
            ]
            []
        in
          case laid.direction of
            -- in the middle of a strip of 14 pixels over the bar, and as far
            -- in as an outline's name is: where its open name will be
            LeftToRight | isHeaded -> plain "flow-label flow-label-heading" "start" (n.box.x + 2.0) (n.box.y - 7.0)
            LeftToRight ->
              if n.box.x + n.box.width + 6.0 + long <= laid.width then plain "flow-label flow-label-beside" "start" (n.box.x + n.box.width + 6.0) middle
              else plain "flow-label flow-label-beside" "end" (n.box.x - 6.0) middle
            TopDown ->
              if n.box.y >= 16.0 then plain "flow-label flow-label-beside" "middle" (n.box.x + n.box.width / 2.0) (n.box.y - 8.0)
              else plain "flow-label flow-label-beside" "middle" (n.box.x + n.box.width / 2.0) (n.box.y + n.box.height + 9.0)
      written = case n.words of
        Nothing | isJust besideLeast -> { lines: if unlabelled && not isHeaded then [] else [ besideLabel ], first: middle }
        Nothing -> { lines: [ text "flow-label" setting.anchor (setting.x 13.0) middle "12" "600" mark.label ], first: middle }
        Just set ->
          let
            block = toNumber (Array.length set.title) * titleLine + toNumber (Array.length set.detail) * detailLine
            blockTop = middle - block / 2.0
            titleAt i = blockTop + (toNumber i + 0.5) * titleLine
            detailAt i = blockTop + toNumber (Array.length set.title) * titleLine + (toNumber i + 0.5) * detailLine
          in
            { lines:
                Array.mapWithIndex (\i line -> text (if i == 0 then "flow-label" else "flow-label-more") setting.anchor (setting.x defaultLettering.inset) (titleAt i) (show defaultLettering.titleSize) "600" line) set.title
                  <> Array.mapWithIndex (\i line -> text "flow-detail" setting.anchor (setting.x defaultLettering.inset) (detailAt i) (show defaultLettering.detailSize) "400" line) set.detail
            , first: titleAt 0
            }
      box =
        elem Rect
          ( [ staticStr "class" "flow-box"
            , Friendly.x n.box.x
            , Friendly.y n.box.y
            , Friendly.width n.box.width
            , Friendly.height n.box.height
            , Friendly.stroke (if isStep then fromMaybe ink mark.colour else ink)
            , Friendly.strokeWidth 1.25
            , Friendly.fill (if isStep then fromMaybe ink mark.colour else paper)
            ]
              <> (if isStep && not (isJust besideLeast) then [ staticStr "rx" (show (min 13.0 (n.box.height / 2.0))) ] else [])
          )
          []
      swatch = case mark.colour of
        Just colour | not isStep ->
          [ elem Rect
              [ staticStr "class" "flow-swatch"
              , Friendly.x n.box.x
              , Friendly.y n.box.y
              , Friendly.width (case laid.direction of
                  LeftToRight -> 5.0
                  TopDown -> n.box.width)
              , Friendly.height (case laid.direction of
                  LeftToRight -> n.box.height
                  TopDown -> 5.0)
              , Friendly.fill colour
              , staticStr "pointer-events" "none"
              ]
              []
          ]
        _ -> []
      -- a closed group's seams: a line of paper across the bar where two
      -- parts divide, and a wall of ink beside a part that only receives
      -- (on the side nothing leaves by) or only gives (on the side nothing
      -- enters by)
      seamMarks = case seamsOf n of
        Nothing -> []
        Just seamed ->
          let
            flat = laid.direction == LeftToRight
            strip className from to before after extra = elem Rect
              ( [ staticStr "class" className
                , Friendly.x (if flat then before else n.box.x + from)
                , Friendly.y (if flat then n.box.y + from else before)
                , Friendly.width (if flat then after - before else to - from)
                , Friendly.height (if flat then to - from else after - before)
                , staticStr "pointer-events" "none"
                ]
                  <> extra
              )
              []
            near = if flat then n.box.x else n.box.y
            far = if flat then n.box.x + n.box.width else n.box.y + n.box.height
            divisions = Array.drop 1 seamed.parts <#> \part -> strip "flow-seam" (part.from - 0.75) (part.from + 0.75) near far [ Friendly.fill paper ]
            walls = seamed.parts # Array.mapMaybe \part -> part.only <#> \only ->
              let
                inset = min 1.5 ((part.to - part.from) / 4.0)
                side = case only of
                  OnlyReceives -> { before: far + 2.0, after: far + 5.0, name: "receives" }
                  OnlyGives -> { before: near - 5.0, after: near - 2.0, name: "gives" }
              in
                strip "flow-only" (part.from + inset) (part.to - inset) side.before side.after [ Friendly.fill ink, staticStr "data-only" side.name, staticStr "data-part" part.name ]
          in
            divisions <> walls
      note = case mark.note of
        Just words -> [ text "flow-note" "end" (n.box.x + n.box.width - 10.0) written.first "9" "500" words ]
        Nothing -> []
      drawn = case decoration.inside n.node of
        Just inside ->
          { drawnClass: "flow-drawn"
          , parts: [ elem Group [ staticStr "class" "flow-inside", staticStr "transform" ("translate(" <> show n.box.x <> "," <> show n.box.y <> ")") ] [ inside ] ]
          }
        Nothing -> { drawnClass: "", parts: [ box ] <> swatch <> seamMarks <> written.lines <> note }
    in
      withBehaviors (gestures (PointedElement n.node.key)) $
        elem Group
          ( [ staticStr "class" (classes [ "flow-node", if isStep then "flow-step" else "flow-item", if isJust n.words then "flow-lettered" else "", if unlabelled then "flow-unlabelled" else "", drawn.drawnClass, mark.className ])
            , staticStr "data-key" (keyName n.node.key)
            ]
              <> buttonOf (PointedElement n.node.key)
          )
          (drawn.parts <> map portMark n.ports)
      where
      portMark :: PlacedPort -> Tree
      portMark port =
        let
          point = portPoint n.box port
        in
          elem Rect
            [ staticStr "class" case port.flow of
                Engine.Input -> "flow-port flow-port-in"
                Engine.Output -> "flow-port flow-port-out"
            , staticStr "data-port" (portLabel port.name)
            , Friendly.x (point.x - portSide / 2.0)
            , Friendly.y (point.y - portSide / 2.0)
            , Friendly.width portSide
            , Friendly.height portSide
            , Friendly.fill case port.flow of
                Engine.Input -> paper
                Engine.Output -> ink
            , Friendly.stroke ink
            , Friendly.strokeWidth 1.0
            , staticStr "pointer-events" "none"
            ]
            []

  classes = Array.intercalate " " <<< Array.filter (_ /= "")
