-- | Where a projection's nodes and edges go. Pure, so it runs in node.
-- |
-- | One layout serves both pictures. Every edge has a width, in units; a
-- | node is as tall as the wider of what enters it and what leaves it, and
-- | its edges stack along its sides. A Sankey takes the widths from the
-- | wires' payloads and draws each edge as wide as it is. A plain DAG gives
-- | every edge one unit and draws each as a hairline down the middle of its
-- | place. The positions come from the same code either way: the Sankey
-- | layout in `hylograph-layout`, with its lanes for edges that span more
-- | than one layer. The flow runs left to right.
-- |
-- | That layout does all of the placing (`DataViz.Layout.Sankey.Whole`): the
-- | scale is fixed (`unit` pixels to one unit of width, the layers a fixed
-- | distance apart) and the picture is as large as that makes it; a node no
-- | edge touches stands on a shelf under the picture; and an edge that
-- | closes a cycle, an edge from a node to itself among them, leaves its
-- | source below the edges that go forward, runs back under the picture in a
-- | channel of its own, and enters its target likewise. What is left here is
-- | the translation: from a projection to rows and names, and from the
-- | layout's boxes and routes to placed nodes and centre lines, set in from
-- | the left by a margin with room for an edge to turn.
module Hylograph.Flow.Render.Layout
  ( Widths(..)
  , Metrics
  , defaultMetrics
  , Point
  , Box
  , Course(..)
  , PlacedNode
  , PlacedEdge
  , Layout
  , layout
  , widthOf
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (isFinite)
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Hierarchy.Link (linkBezierHorizontal)
import DataViz.Layout.Sankey.BackEdge (generateBackEdgePath)
import DataViz.Layout.Sankey.Path (findNode)
import DataViz.Layout.Sankey.Types (Alignment(..), LinkColorMode(..), SankeyConfig, defaultSankeyConfig)
import DataViz.Layout.Sankey.Whole (Sizing(..), WholeLayout, computeWholeLayout)
import Hylograph.Flow (Edge, ElementKey, Node, Projection)

-- | How wide an edge is. `Plain`: every edge one unit, drawn as a hairline.
-- | `Sankey`: the caller reads the width from the edge, which carries what
-- | its wires carry (`wires`: one for a wire, more where the edge crosses
-- | undrawn elements, and which of them is the quantity is the domain's to
-- | say). A width that is not a positive number is drawn as nothing.
data Widths wire
  = Plain
  | Sankey (Edge wire -> Number)

-- | `unit`: pixels to one unit of width. `hairline`: the stroke of a plain
-- | edge. `nodeBreadth`: a node's extent along the flow. `layerGap`: between
-- | one layer and the next. `nodeGap`: between two nodes of a layer.
-- | `restHeight`: how tall a node is that no edge gives a height.
type Metrics =
  { unit :: Number
  , hairline :: Number
  , restHeight :: Number
  , nodeBreadth :: Number
  , layerGap :: Number
  , nodeGap :: Number
  }

defaultMetrics :: Metrics
defaultMetrics = { unit: 26.0, hairline: 1.25, restHeight: 26.0, nodeBreadth: 112.0, layerGap: 72.0, nodeGap: 18.0 }

type Point = { x :: Number, y :: Number }

type Box = { x :: Number, y :: Number, width :: Number, height :: Number }

-- | `Forward`: from one layer to a later one. `Backward`: an edge that
-- | closes a cycle (a scan reading what it wrote), routed under the picture.
data Course = Forward | Backward

derive instance Eq Course

instance Show Course where
  show = case _ of
    Forward -> "Forward"
    Backward -> "Backward"

-- | `index`: the node's place in the projection. `layer`: `Nothing` for a
-- | node on the shelf, which no edge touches.
type PlacedNode = { index :: Int, node :: Node, box :: Box, layer :: Maybe Int }

-- | `index`: the edge's place in the projection. `width`: the room it takes
-- | on the side of each node it touches, in pixels. `stroke`: how wide it is
-- | drawn, along `path`, which runs from `start` to `end`.
type PlacedEdge wire =
  { index :: Int
  , edge :: Edge wire
  , width :: Number
  , stroke :: Number
  , start :: Point
  , end :: Point
  , path :: String
  , course :: Course
  }

-- | Nodes and edges in the projection's order. An edge with an end that is
-- | no node of the projection has no place and is left out (`project` makes
-- | none).
type Layout wire =
  { nodes :: Array PlacedNode
  , edges :: Array (PlacedEdge wire)
  , width :: Number
  , height :: Number
  }

-- | An edge's width in units under a choice of widths.
widthOf :: forall wire. Widths wire -> Edge wire -> Number
widthOf widths edge = case widths of
  Plain -> 1.0
  Sankey measure ->
    let w = measure edge
    in if isFinite w && w > 0.0 then w else 0.0

layout :: forall wire. Metrics -> Widths wire -> Projection wire -> Layout wire
layout metrics widths projection =
  { nodes: Array.sortWith _.index (layered <> shelved)
  , edges: Array.sortWith _.index (forward <> backward)
  , width: 2.0 * inset + foldl (\acc n -> max acc n.x1) placed.chart.x1 placed.isolated
  , height: placed.extent.y1
  }
  where
  -- the first node with a key stands for the key
  indexOf :: Map ElementKey Int
  indexOf = foldl (\m (Tuple i n) -> if Map.member n.key m then m else Map.insert n.key i m) Map.empty (Array.mapWithIndex Tuple projection.nodes)

  nameOf :: Int -> String
  nameOf i = "node:" <> show i

  -- the edges both of whose ends are nodes, as the Sankey layout's rows
  routable = Array.catMaybes $ Array.mapWithIndex
    ( \index edge -> ado
        from <- Map.lookup edge.from indexOf
        to <- Map.lookup edge.to indexOf
        in { index, edge, from, to }
    )
    projection.edges

  rows = routable <#> \r -> { s: nameOf r.from, t: nameOf r.to, v: widthOf widths r.edge }

  -- room on each side for an edge that turns back
  inset = metrics.layerGap / 2.0
  reach = inset * 0.9
  pitch = metrics.nodeBreadth + metrics.layerGap

  -- only the corner of the extent counts: the scale settles the rest
  config :: SankeyConfig
  config = (defaultSankeyConfig 1000.0 1000.0)
    { alignment = Left
    , linkColorMode = StaticColor ""
    , nodeWidth = metrics.nodeBreadth
    , nodePadding = metrics.nodeGap
    , extent = { x0: 0.0, y0: 0.0, x1: 1000.0, y1: 1000.0 }
    }

  placed :: WholeLayout
  placed = computeWholeLayout
    { sizing: AtScale { pixelsPerUnit: metrics.unit, columnPitch: pitch }
    , backEdges: { margin: metrics.nodeGap, gap: metrics.nodeGap / 2.0, reach }
    , shelf: { margin: 2.0 * metrics.nodeGap, minimumColumns: 4, columnPitch: pitch, nodeHeight: metrics.restHeight, rowGap: metrics.nodeGap }
    }
    (\_ _ -> true)
    (Array.mapWithIndex (\index _ -> nameOf index) projection.nodes)
    rows
    config

  boxes = placed.nodes <> placed.waypoints

  shift :: Point -> Point
  shift p = { x: p.x + inset, y: p.y }

  inLayers = Map.fromFoldable (placed.nodes <#> \n -> Tuple n.name n)
  onShelf = Map.fromFoldable (placed.isolated <#> \n -> Tuple n.name n)

  layered = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) inLayers <#> \n ->
        { index, node, box: { x: n.x0 + inset, y: n.y0, width: n.x1 - n.x0, height: n.y1 - n.y0 }, layer: Just n.layer }
    )
    projection.nodes

  shelved = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) onShelf <#> \n ->
        { index, node, box: { x: n.x0 + inset, y: n.y0, width: n.x1 - n.x0, height: n.y1 - n.y0 }, layer: Nothing }
    )
    projection.nodes

  -- an edge forward: one curve for each layer it crosses, straight across
  -- each waypoint between (an edge turned back has a route of no segments)
  forward = Array.catMaybes $ Array.zipWith
    ( \r route -> do
        spans <- for route.segments \link -> ado
          source <- findNode boxes link.sourceIndex
          target <- findNode boxes link.targetIndex
          in { from: shift { x: source.x1, y: link.y0 }, to: shift { x: target.x0, y: link.y1 }, width: link.width }
        first <- Array.head spans
        final <- Array.last spans
        let across = Array.zipWith (\a b -> linkBezierHorizontal a.to.x a.to.y b.from.x b.from.y) spans (Array.drop 1 spans)
        let curves = spans <#> \s -> linkBezierHorizontal s.from.x s.from.y s.to.x s.to.y
        pure
          { index: r.index
          , edge: r.edge
          , width: first.width
          , stroke: strokeOf first.width
          , start: first.from
          , end: final.to
          , path: joinWith " " (interleave curves across)
          , course: Forward
          }
    )
    routable
    placed.routes

  strokeOf width = case widths of
    Plain -> metrics.hairline
    Sankey _ -> width

  interleave :: forall a. Array a -> Array a -> Array a
  interleave xs ys = Array.concat (Array.zipWith (\x y -> [ x, y ]) xs ys) <> Array.drop (Array.length ys) xs

  -- an edge turned back: the layout says which row it is, where it stands
  -- on its two nodes, and which channel is its own
  backward = placed.backEdges # Array.mapMaybe \routed ->
    let
      start = shift { x: routed.backEdge.x0, y: routed.backEdge.y0 }
      end = shift { x: routed.backEdge.x1, y: routed.backEdge.y1 }
    in
      Array.index routable routed.backEdge.index <#> \r ->
        { index: r.index
        , edge: r.edge
        , width: routed.backEdge.width
        , stroke: strokeOf routed.backEdge.width
        , start
        , end
        , path: generateBackEdgePath reach routed { backEdge = routed.backEdge { x0 = start.x, x1 = end.x } }
        , course: Backward
        }
