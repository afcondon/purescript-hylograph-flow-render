-- | A layout as a HATS tree: one SVG, edges under nodes.
-- |
-- | An item (what holds a value) is an open box; a step (what computes) is
-- | a filled lozenge. An edge is one stroked path, as wide as the layout
-- | says: a hairline in a plain picture, a band in a Sankey; an edge turned
-- | back to close a cycle is dashed where it is a hairline.
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
-- | What is drawn, for a stylesheet or a test to find:
-- |
-- | ```
-- | svg.flow
-- |   g.flow-edges > path.flow-edge[.flow-backward][data-edge][data-from][data-to]
-- |   g.flow-nodes > g.flow-node.flow-item|.flow-step[data-key]
-- |                    > rect.flow-box, rect.flow-swatch?, text.flow-label, text.flow-note?
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
  , tree
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Hylograph.Flow (Edge, ElementKey(..), GroupId(..), ItemId(..), Node, StepId(..), WireId(..), itemLabel, portLabel, stepLabel)
import Hylograph.Flow.Overlay (Overlay)
import Hylograph.Flow.Overlay as Overlay
import Hylograph.Flow.Render.Layout (Course(..), Layout, PlacedEdge, PlacedNode)
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
-- | elements it passes through.
data Pointed
  = PointedElement ElementKey
  | PointedPath { from :: ElementKey, to :: ElementKey, via :: Array ElementKey }

derive instance Eq Pointed

instance Show Pointed where
  show = case _ of
    PointedElement key -> "(PointedElement " <> show key <> ")"
    PointedPath p -> "(PointedPath " <> show p.from <> " " <> show p.via <> " " <> show p.to <> ")"

type Decoration wire =
  { node :: Node -> Mark
  , edge :: Edge wire -> Line
  , notify :: Gesture -> Pointed -> Effect Unit
  }

-- | Each node under its own label, nothing else said, gestures ignored.
plainDecoration :: forall wire. Decoration wire
plainDecoration =
  { node: \node -> { label: node.label, note: Nothing, className: "", colour: Nothing }
  , edge: \_ -> { className: "", colour: Nothing }
  , notify: \_ _ -> pure unit
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

ink :: String
ink = "#161616"

paper :: String
paper = "#ffffff"

tree :: forall wire. Decoration wire -> Layout wire -> Tree
tree decoration laid =
  elem SVG
    [ staticStr "class" "flow"
    , Friendly.viewBox (-2.0) (-2.0) (laid.width + 4.0) (laid.height + 4.0)
    , Friendly.width (laid.width + 4.0)
    , Friendly.height (laid.height + 4.0)
    ]
    [ elem Group [ staticStr "class" "flow-edges" ]
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

  -- a band lets what it crosses show through; a hairline is solid
  isBand :: PlacedEdge wire -> Boolean
  isBand e = e.stroke > 3.0

  edgeMark :: PlacedEdge wire -> Tree
  edgeMark e =
    let
      line = decoration.edge e.edge
      backward = e.course == Backward
    in
      withBehaviors (gestures (pointedBy e.edge)) $
        elem Path
          ( [ staticStr "class" (classes [ "flow-edge", if backward then "flow-backward" else "", if isBand e then "flow-band" else "flow-hairline", line.className ])
            , staticStr "data-edge" (show e.index)
            , staticStr "data-from" (keyName e.edge.from)
            , staticStr "data-to" (keyName e.edge.to)
            , Friendly.d e.path
            , Friendly.fill "none"
            , Friendly.stroke (fromMaybe ink line.colour)
            , Friendly.strokeWidth e.stroke
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
      lettering = if isStep then paper else ink
      text className anchor x size weight content =
        elem Text
          [ staticStr "class" className
          , Friendly.x x
          , Friendly.y middle
          , Friendly.textAnchor anchor
          , staticStr "dominant-baseline" "central"
          , Friendly.fontSize size
          , Friendly.fontWeight weight
          , Friendly.fill lettering
          , staticStr "pointer-events" "none"
          , staticStr "textContent" content
          ]
          []
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
              , Friendly.width 5.0
              , Friendly.height n.box.height
              , Friendly.fill colour
              , staticStr "pointer-events" "none"
              ]
              []
          ]
        _ -> []
      note = case mark.note of
        Just words -> [ text "flow-note" "end" (n.box.x + n.box.width - 10.0) "9" "500" words ]
        Nothing -> []
    in
      withBehaviors (gestures (PointedElement n.node.key)) $
        elem Group
          [ staticStr "class" (classes [ "flow-node", if isStep then "flow-step" else "flow-item", mark.className ])
          , staticStr "data-key" (keyName n.node.key)
          ]
          ([ box ] <> swatch <> [ text "flow-label" "start" (n.box.x + 13.0) "12" "600" mark.label ] <> note)

  classes = Array.intercalate " " <<< Array.filter (_ /= "")
