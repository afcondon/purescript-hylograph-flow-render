-- | What must hold of any layout of any projection, as a function that
-- | returns what does not. Here, and not in a test directory, so that every
-- | adapter's tests can hold the layout of its own real flows to it.
module Hylograph.Flow.Render.Laws
  ( faults
  , layoutFaults
  , acyclic
  , varied
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, sum)
import Data.Int (toNumber)
import Data.Graph.Algorithms (isDAG, mkSimpleGraph)
import Data.Maybe (Maybe(..))
import Data.Number (abs, isFinite)
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Projection)
import Hylograph.Flow.Render.Layout (Course(..), Layout, Metrics, PlacedEdge, PlacedNode, Widths(..), layout, widthOf)

-- | No path of edges returns to where it began (an edge from a node to
-- | itself is such a path).
acyclic :: forall wire. Projection wire -> Boolean
acyclic p = isDAG (mkSimpleGraph (map _.key p.nodes) (p.edges <#> \e -> Tuple e.from e.to))

-- | The faults of the layout this package makes of a projection.
layoutFaults :: forall wire. Metrics -> Widths wire -> Projection wire -> Array String
layoutFaults metrics widths projection = faults metrics widths projection (layout metrics widths projection)

-- | Widths that differ from edge to edge and mean nothing, for holding the
-- | layout to its laws on a flow whose wires carry no quantity: the number
-- | of wires an edge stands for, and a half more where its source's key is
-- | below its target's.
varied :: forall wire. Widths wire
varied = Sankey \edge -> toNumber (NonEmptyArray.length edge.wires) + (if edge.from < edge.to then 0.5 else 0.0)

-- | Every fault in a layout of a projection, in words; none is the law.
-- |
-- | - each node of the projection has exactly one place, and each edge one;
-- | - every number is finite and every box is inside the picture;
-- | - no two node boxes overlap;
-- | - an edge that goes forward ends to the right of where it starts, in a
-- |   later layer; where the projection has no cycle, every edge does;
-- | - an edge is as wide as its width says, at `unit` pixels to the unit;
-- |   the edges on one side of a node stand inside the node and beside one
-- |   another, and the node is as tall as the wider side. (Checked where
-- |   every width is positive.)
faults :: forall wire. Metrics -> Widths wire -> Projection wire -> Layout wire -> Array String
faults metrics widths projection laid =
  places <> edgePlaces <> finite <> inside <> overlaps <> direction <> widthFaults
  where
  tolerance = 1.0e-6
  upTo count = if count <= 0 then [] else Array.range 0 (count - 1)
  near a b = abs (a - b) <= tolerance * max 1.0 (max (abs a) (abs b))
  describe :: PlacedNode -> String
  describe n = n.node.label <> " " <> show n.node.key
  describeEdge :: PlacedEdge wire -> String
  describeEdge e = "edge " <> show e.index <> " " <> show e.edge.from <> " -> " <> show e.edge.to

  places =
    if map _.index laid.nodes == upTo (Array.length projection.nodes) && all identity (Array.zipWith (\n p -> n.node.key == p.key) laid.nodes projection.nodes) then []
    else [ "the placed nodes are not the projection's nodes, each once, in order" ]

  edgePlaces =
    if map _.index laid.edges == upTo (Array.length projection.edges) then []
    else [ "the placed edges are not the projection's edges, each once, in order" ]

  boxNumbers n = [ n.box.x, n.box.y, n.box.width, n.box.height ]
  edgeNumbers e = [ e.width, e.stroke, e.start.x, e.start.y, e.end.x, e.end.y ]
  finite =
    (if all isFinite ([ laid.width, laid.height ] <> Array.concatMap boxNumbers laid.nodes <> Array.concatMap edgeNumbers laid.edges) then [] else [ "a number is not finite" ])
      <> (if all (\n -> n.box.width >= 0.0 && n.box.height >= 0.0) laid.nodes then [] else [ "a box has a negative side" ])

  inside = laid.nodes # Array.mapMaybe \n ->
    if n.box.x >= -tolerance && n.box.y >= -tolerance && n.box.x + n.box.width <= laid.width + tolerance && n.box.y + n.box.height <= laid.height + tolerance then Nothing
    else Just (describe n <> " is outside the picture")

  overlapping a b =
    a.box.x < b.box.x + b.box.width - tolerance && b.box.x < a.box.x + a.box.width - tolerance
      && a.box.y < b.box.y + b.box.height - tolerance
      && b.box.y < a.box.y + a.box.height - tolerance
  overlaps = do
    a <- laid.nodes
    b <- laid.nodes
    if a.index < b.index && overlapping a b then [ describe a <> " overlaps " <> describe b ] else []

  layerOf key = Array.find (\n -> n.node.key == key) laid.nodes >>= _.layer
  goesForward e = e.end.x > e.start.x && case layerOf e.edge.from, layerOf e.edge.to of
    Just from, Just to -> to > from
    _, _ -> false
  direction =
    Array.mapMaybe (\e -> if e.course == Forward && not (goesForward e) then Just (describeEdge e <> " is drawn forward and does not go forward") else Nothing) laid.edges
      <>
        if acyclic projection then Array.mapMaybe (\e -> if e.course == Backward then Just (describeEdge e <> " turns back in a projection with no cycle") else Nothing) laid.edges
        else []

  allPositive = all (\e -> widthOf widths e > 0.0) projection.edges

  -- the edges standing on one side of a node, top to bottom
  side :: (PlacedEdge wire -> Boolean) -> (PlacedEdge wire -> Number) -> Array { centre :: Number, width :: Number }
  side touches centre = Array.sortBy (comparing _.centre) (Array.filter touches laid.edges <#> \e -> { centre: centre e, width: e.width })

  sideFaults n name stand =
    let
      top s = s.centre - s.width / 2.0
      bottom s = s.centre + s.width / 2.0
      within = all (\s -> top s >= n.box.y - 1.0e-4 && bottom s <= n.box.y + n.box.height + 1.0e-4) stand
      apart = all identity (Array.zipWith (\a b -> top b >= bottom a - 1.0e-4) stand (Array.drop 1 stand))
    in
      (if within then [] else [ "an edge " <> name <> " " <> describe n <> " stands outside it" ])
        <> (if apart then [] else [ "two edges " <> name <> " " <> describe n <> " stand on one another" ])

  widthFaults =
    if not allPositive then []
    else
      Array.mapMaybe (\e -> if near e.width (metrics.unit * widthOf widths e.edge) then Nothing else Just (describeEdge e <> " is " <> show e.width <> " wide, its width says " <> show (metrics.unit * widthOf widths e.edge))) laid.edges
        <> Array.concatMap
          ( \n ->
              let
                entering = side (\e -> e.edge.to == n.node.key) _.end.y
                leaving = side (\e -> e.edge.from == n.node.key) _.start.y
                wider = max (sum (map _.width entering)) (sum (map _.width leaving))
              in
                if Array.null entering && Array.null leaving then []
                else
                  sideFaults n "into" entering <> sideFaults n "out of" leaving
                    <> (if near n.box.height wider then [] else [ describe n <> " is " <> show n.box.height <> " tall, its wider side is " <> show wider ])
          )
          laid.nodes
