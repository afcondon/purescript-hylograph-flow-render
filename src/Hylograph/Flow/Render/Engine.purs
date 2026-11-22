-- | What any layout engine makes, and an engine as a value.
-- |
-- | In 0.2 the renderer had one layout, the Sankey layout, and its types
-- | were that layout's: a `Layout` was what `layout` returned, a picture
-- | held Sankey `Metrics` and `Widths`, and the laws asserted Sankey facts.
-- | A trial that drew the same flows with a second engine had to pass its result off as a Sankey layout. This module
-- | is the layout result with the Sankey layout taken out of it, so that the
-- | tree draws a `Layout` and the laws and measures read one without
-- | knowing which engine made it.
-- |
-- | It is designed for three engines and no others: the Sankey layout
-- | (`Hylograph.Flow.Render.Sankey`), `layered-layout` (ports, right-angled
-- | routes, no widths; its adapter lives outside this package,
-- | which must not depend on it), and Stemma, a rewrite of Graphviz dot
-- | (clusters, splines, ranks, top-down by default).
-- |
-- | THE DECISIONS, each with its reason.
-- |
-- | - **A route is data** (`Hylograph.Flow.Render.Route`), and a placed
-- |   edge has no `path`, `start`, `end`, `width` or `stroke` of its own:
-- |   each is read from the route (`Route.svgPath`, `Route.start`, …), so
-- |   none can disagree with it.
-- | - **Ports are placed on nodes** (`PlacedNode.ports`). A placed port is
-- |   which port (its name, and whether it is one of the node's inputs or
-- |   its outputs: a step may have an input and an output of one name), the
-- |   side of the box it is on, and how far along that side: so it is on
-- |   the box's boundary by construction, and `portPoint` says where. An
-- |   engine places the ports it gives a place to and no others: the Sankey
-- |   layout stacks a node's edges by where they go, not by port, and
-- |   places none. Whether an edge meets a port is not a field of the edge:
-- |   the edge already names its ports (`Edge.fromPort`, `Edge.toPort`, the
-- |   core's), and the law is that where the node has that port placed the
-- |   route ends there.
-- | - **`direction` is the layout's**, not the tree's or the caller's: the
-- |   engine knows which way it ran. The laws read "forward" and "aligned"
-- |   by it, and the tree sets a node's words by it.
-- | - **`layer` stays a `Maybe`.** It is the layer (dot's rank) the engine
-- |   put a node in *of the whole picture*, and some placed nodes have
-- |   none: the Sankey layout's shelf of nodes no edge touches, and every
-- |   node of a layout made of several layerings (`layered-layout` run once
-- |   for each open group). Both are real, so the case is not designed
-- |   out; what is designed out is a layer that means something only
-- |   within a part of the picture.
-- | - **An engine is a value** (`Engine`): a name, the function, and what
-- |   it promises. A `Picture` is what every engine is told beside the
-- |   projection: what is written in a node, and which groups are open.
-- |   What only one engine can be told (the Sankey layout's `Metrics` and
-- |   `Widths`, the other's spacings) is that engine's own configuration,
-- |   given where the engine is made (`sankey metrics widths`), and is not
-- |   in `Picture`. Node breadth is among those: an engine may size a node
-- |   as it likes, and the law about words reads the breadth a node was
-- |   given from its own box.
-- | - **Promises are a closed ADT** (`Promise`), one constructor for each
-- |   law that holds of some engines and not of all. A promise carries what
-- |   its law needs and no layout holds: `WidthsConserved` carries the scale
-- |   and the edge's quantity. `Hylograph.Flow.Render.Laws` checks the
-- |   general laws of every layout and, of an engine's layout, exactly the
-- |   laws it promises: an engine that does not conserve widths is not
-- |   asked to, so nothing has to be suppressed.
module Hylograph.Flow.Render.Engine
  ( Box
  , Direction(..)
  , Side(..)
  , PortFlow(..)
  , PlacedPort
  , portPoint
  , Course(..)
  , PlacedNode
  , PlacedEdge
  , PlacedOutline
  , Layout
  , Groups
  , OutlineMetrics
  , defaultOutline
  , Picture
  , plainPicture
  , openPaths
  , Conservation
  , Promise(..)
  , Engine(..)
  , run
  , engineName
  , promises
  , module Exports
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Hylograph.Flow (Edge, ElementKey(..), GroupId(..), ItemId(..), Node, PortName, Projection, StepId(..))
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Route (Point, Route)
import Hylograph.Flow.Render.Route (Point) as Exports
import Hylograph.Flow.Render.Text (Lettering, SetWords, Words, defaultLettering)

type Box = { x :: Number, y :: Number, width :: Number, height :: Number }

-- | Which way the flow runs in a layout: its layers are columns
-- | (`LeftToRight`) or rows (`TopDown`).
data Direction = LeftToRight | TopDown

derive instance Eq Direction

instance Show Direction where
  show = case _ of
    LeftToRight -> "LeftToRight"
    TopDown -> "TopDown"

-- | A side of a box.
data Side = LeftSide | RightSide | TopSide | BottomSide

derive instance Eq Side
derive instance Ord Side

instance Show Side where
  show = case _ of
    LeftSide -> "LeftSide"
    RightSide -> "RightSide"
    TopSide -> "TopSide"
    BottomSide -> "BottomSide"

-- | Which of a node's ports a name is among: its `inputs` or its `outputs`.
data PortFlow = Input | Output

derive instance Eq PortFlow
derive instance Ord PortFlow

instance Show PortFlow where
  show = case _ of
    Input -> "Input"
    Output -> "Output"

-- | A port with a place. `along`: how far along `side` it is, in pixels
-- | from the side's top (a left or right side) or its left end (a top or
-- | bottom side).
type PlacedPort = { name :: PortName, flow :: PortFlow, side :: Side, along :: Number }

-- | Where on the picture a port of a box is.
portPoint :: Box -> PlacedPort -> Point
portPoint box port = case port.side of
  LeftSide -> { x: box.x, y: box.y + port.along }
  RightSide -> { x: box.x + box.width, y: box.y + port.along }
  TopSide -> { x: box.x + port.along, y: box.y }
  BottomSide -> { x: box.x + port.along, y: box.y + box.height }

-- | `Forward`: with the flow. `Backward`: an edge the engine turned back to
-- | close a cycle (a scan reading what it wrote).
data Course = Forward | Backward

derive instance Eq Course

instance Show Course where
  show = case _ of
    Forward -> "Forward"
    Backward -> "Backward"

-- | `index`: the node's place in the projection. `layer`: its layer in the
-- | whole picture, where the engine gave it one (see the module's header).
-- | `words`: what is written in it, as set, where the picture said
-- | (`Picture.words`); `Nothing` where it did not. `ports`: the ports the
-- | engine gave a place; none for an item, which has none.
type PlacedNode =
  { index :: Int
  , node :: Node
  , box :: Box
  , layer :: Maybe Int
  , words :: Maybe SetWords
  , ports :: Array PlacedPort
  }

-- | `index`: the edge's place in the projection. `edge`: the projection's
-- | own, with the wires it stands for, so what is known of a wire can be
-- | shown on it. `route`: the line it is drawn along, from its source to
-- | its target.
type PlacedEdge wire =
  { index :: Int
  , edge :: Edge wire
  , route :: Route
  , course :: Course
  }

-- | An open group's outline. `name`: the group's, from the grouping.
-- | `header`: the height of the strip at the top of `box` that is kept for
-- | the name. `depth`: how many outlines it is inside; `parent`: the
-- | nearest. `members`: the indices of the nodes inside it, however deep.
type PlacedOutline =
  { group :: GroupId
  , name :: String
  , box :: Box
  , header :: Number
  , depth :: Int
  , parent :: Maybe GroupId
  , members :: Array Int
  }

-- | Nodes and edges in the projection's order. An edge with an end that is
-- | no node of the projection has no place and is left out (`project` makes
-- | none). Outlines outermost first, so drawn in order an outline is over
-- | the one that holds it; none unless the picture told of groups.
type Layout wire =
  { direction :: Direction
  , nodes :: Array PlacedNode
  , edges :: Array (PlacedEdge wire)
  , outlines :: Array PlacedOutline
  , width :: Number
  , height :: Number
  }

-- | The grouping, and the groups that are closed: what `collapse` was given
-- | to make the flow that was projected. Every other group is open.
type Groups = { grouping :: Array Grouping, closed :: Set GroupId }

-- | `padding`: from an outline to what is in it. `header`: the strip for
-- | the group's name.
type OutlineMetrics = { padding :: Number, header :: Number }

defaultOutline :: OutlineMetrics
defaultOutline = { padding: 8.0, header: 18.0 }

-- | What every engine is told beside the projection. `words`: what is
-- | written in each node, for the engine to wrap to the breadth it gives
-- | the node and make room for; `Nothing` leaves every node as tall as the
-- | engine makes it and its label to the tree, on one line. `groups`: the
-- | open groups to outline; `Nothing` for none.
type Picture =
  { words :: Maybe (Node -> Words)
  , lettering :: Lettering
  , groups :: Maybe Groups
  , outline :: OutlineMetrics
  }

-- | No words set and no group outlined.
plainPicture :: Picture
plainPicture = { words: Nothing, lettering: defaultLettering, groups: Nothing, outline: defaultOutline }

-- | The open groups a drawn element is in, outermost first, and every
-- | group's name. A step is where `Member` puts it and an item where
-- | `Holds` does; a closed group's step and its summary are in the groups
-- | their group is in; an item a closed group holds that is drawn all the
-- | same (an open step writes it) is in the open groups over that group.
-- | An element no group holds is in none.
openPaths :: Groups -> { pathOf :: ElementKey -> Array GroupId, nameOf :: GroupId -> String }
openPaths groups =
  { pathOf: case _ of
      StepKey step -> fromMaybe [] (Map.lookup step found.steps)
      ItemKey item -> fromMaybe [] (Map.lookup item found.items)
      WireKey _ -> []
  , nameOf: \group@(GroupId written) -> fromMaybe written (Map.lookup group found.names)
  }
  where
  nothing = { steps: Map.empty, items: Map.empty, names: Map.empty }
  found = foldl (walk [] false) nothing groups.grouping

  -- what a grouping says outright (`Member`, `Holds`) stands over what is
  -- said of a group's own step and summary
  walk path shut known = case _ of
    Member step -> known { steps = Map.insert step path known.steps }
    Holds item -> known { items = Map.insert item path known.items }
    Group group name inner ->
      let
        isShut = shut || Set.member group groups.closed
        own = known
          { steps = Map.alter (keep path) (GroupStep group) known.steps
          , items = Map.alter (keep path) (GroupItem group) known.items
          , names = Map.insert group name known.names
          }
      in
        foldl (walk (if isShut then path else Array.snoc path group) isShut) own inner

  keep path = case _ of
    Just said -> Just said
    Nothing -> Just path

-- | What the law of conserved widths needs and no layout holds: the pixels
-- | to one unit of width, and an edge's width in units.
type Conservation wire = { pixelsPerUnit :: Number, unitsOf :: Edge wire -> Number }

-- | A law that holds of some engines' layouts and not of every layout; an
-- | engine lists the ones it keeps. What each says exactly is in
-- | `Hylograph.Flow.Render.Laws`.
-- |
-- | - `FlowsForward`: an edge not turned back runs with the flow, and where
-- |   the projection has no cycle no edge is turned back.
-- | - `LayersAligned`: the nodes of one layer stand in one line across the
-- |   flow.
-- | - `RoutesOrthogonal`: every route is straight legs along the axes.
-- | - `WidthsConserved`: every route is a ribbon as broad as its quantity
-- |   says, and at each node the ribbons of a side stand in the node, beside
-- |   one another, and the node is as tall as the broader side.
data Promise wire
  = FlowsForward
  | LayersAligned
  | RoutesOrthogonal
  | WidthsConserved (Conservation wire)

-- | A layout engine: what it is called, what it promises of every layout it
-- | makes, and the making.
newtype Engine wire = Engine
  { name :: String
  , promises :: Array (Promise wire)
  , layout :: Picture -> Projection wire -> Layout wire
  }

-- | An engine's layout of a projection.
run :: forall wire. Engine wire -> Picture -> Projection wire -> Layout wire
run (Engine engine) = engine.layout

engineName :: forall wire. Engine wire -> String
engineName (Engine engine) = engine.name

promises :: forall wire. Engine wire -> Array (Promise wire)
promises (Engine engine) = engine.promises
