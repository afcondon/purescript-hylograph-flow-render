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
-- | Words: where the layout set a node's words (`Picture.words`), those
-- | lines are drawn, the title's and then the detail's, and the mark's
-- | `label` is not used; its `note` stands at the right of the first line.
-- | Where it did not, the mark's `label` is written on one line, as in 0.2.
-- |
-- | What is drawn, for a stylesheet or a test to find:
-- |
-- | ```
-- | svg.flow[data-direction=left-to-right|top-down]
-- |   g.flow-outlines > g.flow-outline[data-group][data-depth]
-- |                       > rect.flow-outline-box, rect.flow-outline-tab, text.flow-outline-label
-- |   g.flow-edges > path.flow-edge.flow-ribbon|.flow-polyline|.flow-spline
-- |                    .flow-band|.flow-hairline[.flow-backward][data-edge][data-from][data-to]
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
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Hylograph.Flow (Edge, ElementKey(..), GroupId(..), ItemId(..), Node, StepId(..), WireId(..), itemLabel, portLabel, stepLabel)
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Layout, PlacedEdge, PlacedNode, PlacedOutline, PlacedPort, portPoint)
import Hylograph.Flow.Render.Engine as Engine
import Hylograph.Flow.Render.Route (Ink(..), Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Overlay (Overlay)
import Hylograph.Flow.Overlay as Overlay
import Data.Int (toNumber)
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

-- | `inside` (0.4): a node's inside, drawn by the caller in the node's own
-- | coordinates, for the nodes it draws; see the module's header.
type Decoration wire =
  { node :: Node -> Mark
  , edge :: Edge wire -> Line
  , notify :: Gesture -> Pointed -> Effect Unit
  , inside :: Node -> Maybe Tree
  }

-- | Each node under its own label, nothing else said, gestures ignored, no
-- | inside drawn by the caller.
plainDecoration :: forall wire. Decoration wire
plainDecoration =
  { node: \node -> { label: node.label, note: Nothing, className: "", colour: Nothing }
  , edge: \_ -> { className: "", colour: Nothing }
  , notify: \_ _ -> pure unit
  , inside: const Nothing
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
tree decoration laid =
  elem SVG
    [ staticStr "class" "flow"
    , staticStr "data-direction" (directionName laid.direction)
    , Friendly.viewBox (-2.0) (-2.0) (laid.width + 4.0) (laid.height + 4.0)
    , Friendly.width (laid.width + 4.0)
    , Friendly.height (laid.height + 4.0)
    ]
    [ elem Group [ staticStr "class" "flow-outlines" ]
        [ forEach "flow-outlines" Group laid.outlines (groupName <<< _.group) outlineMark ]
    , elem Group [ staticStr "class" "flow-edges" ]
        [ forEach "flow-edges" Path laid.edges (show <<< _.index) edgeMark ]
    , elem Group [ staticStr "class" "flow-nodes" ]
        [ forEach "flow-nodes" Group laid.nodes (\n -> show n.index <> " " <> keyName n.node.key) nodeMark ]
    ]
  where
  gestures pointed =
    [ onClick (decoration.notify Clicked pointed)
    , onMouseEnter (decoration.notify Entered pointed)
    , onMouseLeave (decoration.notify Left pointed)
    ]

  -- an open group: its box takes no gesture, its name does
  outlineMark :: PlacedOutline -> Tree
  outlineMark outline =
    let
      nameSize = 11.0
      tabWidth = min outline.box.width (16.0 + heavier * estimateWidth nameSize outline.name)
    in
      withBehaviors (gestures (PointedGroup outline.group)) $
        elem Group
          [ staticStr "class" "flow-outline"
          , staticStr "data-group" (groupName outline.group)
          , staticStr "data-depth" (show outline.depth)
          ]
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
              , Friendly.x outline.box.x
              , Friendly.y outline.box.y
              , Friendly.width tabWidth
              , Friendly.height outline.header
              , Friendly.fill paper
              , staticStr "fill-opacity" "0"
              , staticStr "pointer-events" "all"
              ]
              []
          , elem Text
              [ staticStr "class" "flow-outline-label"
              , Friendly.x (outline.box.x + 8.0)
              , Friendly.y (outline.box.y + outline.header / 2.0 + 1.0)
              , Friendly.textAnchor "start"
              , staticStr "dominant-baseline" "central"
              , Friendly.fontSize (show nameSize)
              , Friendly.fontWeight "600"
              , Friendly.fill quiet
              , staticStr "pointer-events" "none"
              , staticStr "textContent" outline.name
              ]
              []
          ]

  -- a band lets what it crosses show through; a hairline is solid
  isBand :: PlacedEdge wire -> Boolean
  isBand e = strokeOf e.route > 3.0

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
    in
      withBehaviors (gestures (pointedBy e.edge)) $
        elem Path
          ( [ staticStr "class" (classes [ "flow-edge", kindOf e.route, if backward then "flow-backward" else "", if isBand e then "flow-band" else "flow-hairline", line.className ])
            , staticStr "data-edge" (show e.index)
            , staticStr "data-from" (keyName e.edge.from)
            , staticStr "data-to" (keyName e.edge.to)
            , Friendly.d (Route.svgPath e.route)
            , Friendly.fill "none"
            , Friendly.stroke (fromMaybe ink line.colour)
            , Friendly.strokeWidth (strokeOf e.route)
            , Friendly.strokeOpacity (if isBand e then "0.22" else "1")
            ]
              <> (if backward && not (isBand e) then [ staticStr "stroke-dasharray" "5 4" ] else [])
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
      written = case n.words of
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
              <> (if isStep then [ staticStr "rx" (show (min 13.0 (n.box.height / 2.0))) ] else [])
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
      note = case mark.note of
        Just words -> [ text "flow-note" "end" (n.box.x + n.box.width - 10.0) written.first "9" "500" words ]
        Nothing -> []
      drawn = case decoration.inside n.node of
        Just inside ->
          { drawnClass: "flow-drawn"
          , parts: [ elem Group [ staticStr "class" "flow-inside", staticStr "transform" ("translate(" <> show n.box.x <> "," <> show n.box.y <> ")") ] [ inside ] ]
          }
        Nothing -> { drawnClass: "", parts: [ box ] <> swatch <> written.lines <> note }
    in
      withBehaviors (gestures (PointedElement n.node.key)) $
        elem Group
          [ staticStr "class" (classes [ "flow-node", if isStep then "flow-step" else "flow-item", if isJust n.words then "flow-lettered" else "", drawn.drawnClass, mark.className ])
          , staticStr "data-key" (keyName n.node.key)
          ]
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
