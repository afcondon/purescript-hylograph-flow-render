-- | Measures of a drawn graph, whoever laid it out: the scoreboard of an
-- | earlier layout trial, here so that every engine is scored by one rule.
-- |
-- | Every measure is a function of a `Drawn` and nothing else: node boxes,
-- | each edge as the straight pieces it is drawn along, and the boxes of
-- | the open groups' outlines with their members. `drawnOf` makes a `Drawn`
-- | of any `Layout`, each route by `Hylograph.Flow.Render.Route.flatten`: a
-- | straight piece is its two ends, a cubic curve is `curveSteps` (12)
-- | straight pieces. So a ribbon, a dog-leg and a spline are measured by
-- | one rule, and no engine's own counts are used. A `Drawn` can also be
-- | made by hand, which is how the measures are held to figures worked out
-- | on paper. (The trial read each edge back from its SVG path; the pieces
-- | are the same, so its figures stand.)
-- |
-- | THE MEASURES, exactly, as the trial stated them.
-- |
-- | - `crossings`: the number of points at which a piece of one edge's
-- |   line properly crosses a piece of another edge's: each piece's two
-- |   ends strictly on opposite sides of the other piece. Touching does not
-- |   count: two edges that leave one point, an edge that turns exactly on
-- |   another, and two edges that run along one line (a shared trunk) cross
-- |   nowhere. Two edges that cross twice count twice. **Nor does meeting
-- |   at a shared end**: where the two edges have a node in common and the
-- |   point of crossing is within `skin` (1 pixel) of where each of them
-- |   ends at that node, they meet there, as two edges into one port do
-- |   when their ends are computed a hair apart; any other crossing of two
-- |   edges with a node in common counts. (Since 0.3.1; before, such a
-- |   meeting counted one.) A crossing exactly at a point where both lines
-- |   are cut into pieces is a touch by this rule and is not counted.
-- | - `edgeLength`: the sum of the lengths of every edge's line, pixels.
-- | - `bends`: total turning, in right angles: the sum over every interior
-- |   point of every line of the angle turned there, divided by 90
-- |   degrees. An orthogonal route scores one for each corner; a curve
-- |   scores by how far it swings (an S that reaches 45 degrees and comes
-- |   back scores one), and a straight line nothing.
-- | - `extent`: the box round every node, outline and edge point; `area` is
-- |   its width times its height, in pixels squared, and `aspect` its width
-- |   over its height.
-- | - `nodeOverlaps`: the pairs of node boxes with more than a touch in
-- |   common (more than `skin`, 1 pixel, each way).
-- | - `edgesThroughNodes`: the pairs of an edge and a node that is neither
-- |   of its ends, where the edge's line enters the node's box drawn in by
-- |   `skin` on every side.
-- | - `outlineTrespasses`: the pairs of an edge and an outline where neither
-- |   end of the edge is a member of the outline's group and the edge's
-- |   line enters the outline's box drawn in by `skin`. (An edge with an
-- |   end inside must cross the outline, and is not counted.)
-- | - `strangersIn`: the pairs of an outline and a node that is no member
-- |   of it and is in its box or partly in it: the general law
-- |   (`Hylograph.Flow.Render.Laws.strangers`), counted.
-- | - `unrouted`: the edges with no line (fewer than two points). No route
-- |   of a `Layout` is such (`Route` cannot be), so of a layout this is 0
-- |   unless a route's points are all one point; it is here for a `Drawn`
-- |   made otherwise. An edge an engine left out altogether is the laws'
-- |   to report (`Placed`), and `unplaced` counts them.
-- |
-- | Time to lay out is not here: it is measured by whoever runs the layout.
module Hylograph.Flow.Render.Measures
  ( Drawn
  , DrawnEdge
  , DrawnOutline
  , Score
  , drawnOf
  , score
  , scoreOf
  , crossings
  , edgeLength
  , bends
  , extent
  , area
  , aspect
  , nodeOverlaps
  , edgesThroughNodes
  , outlineTrespasses
  , strangersIn
  , unrouted
  , unplaced
  , skin
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pi)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Projection)
import Hylograph.Flow.Render.Engine (Box, Layout)
import Hylograph.Flow.Render.Route (Point, cross, distance, flatten, turning)

-- | `from`, `to`: the places of the edge's two nodes in `nodes`.
type DrawnEdge = { from :: Int, to :: Int, points :: Array Point }

type DrawnOutline = { box :: Box, members :: Array Int }

type Drawn =
  { nodes :: Array Box
  , edges :: Array DrawnEdge
  , outlines :: Array DrawnOutline
  }

type Score =
  { nodes :: Int
  , edges :: Int
  , crossings :: Int
  , edgeLength :: Number
  , bends :: Number
  , width :: Number
  , height :: Number
  , area :: Number
  , aspect :: Number
  , nodeOverlaps :: Int
  , edgesThroughNodes :: Int
  , outlineTrespasses :: Int
  , strangers :: Int
  , unrouted :: Int
  }

-- | How far inside a box an edge must come to be in it, and how much two
-- | boxes must share to overlap: 1 pixel.
skin :: Number
skin = 1.0

-- | A layout as drawn: its node boxes in the layout's order, its edges as
-- | their routes' straight pieces, its outlines.
drawnOf :: forall wire. Layout wire -> Drawn
drawnOf laid =
  { nodes: map _.box laid.nodes
  , edges: laid.edges <#> \placed ->
      { from: fromMaybe (-1) (Map.lookup placed.edge.from places)
      , to: fromMaybe (-1) (Map.lookup placed.edge.to places)
      , points: flatten placed.route
      }
  , outlines: laid.outlines <#> \outline -> { box: outline.box, members: positions outline.members }
  }
  where
  -- the first node with a key stands for the key, as in the engines
  places = foldl (\known (Tuple place node) -> if Map.member node.node.key known then known else Map.insert node.node.key place known) Map.empty (Array.mapWithIndex Tuple laid.nodes)
  byIndex = Map.fromFoldable (Array.mapWithIndex (\place node -> Tuple node.index place) laid.nodes)
  positions = Array.mapMaybe (\index -> Map.lookup index byIndex)

-- | Every measure of a layout.
scoreOf :: forall wire. Layout wire -> Score
scoreOf = score <<< drawnOf

-- | The edges of a projection that a layout of it gave no place.
unplaced :: forall wire. Projection wire -> Layout wire -> Int
unplaced projection laid = Array.length projection.edges - Array.length (Array.nub (map _.index laid.edges))

score :: Drawn -> Score
score drawn =
  { nodes: Array.length drawn.nodes
  , edges: Array.length drawn.edges
  , crossings: crossings drawn
  , edgeLength: edgeLength drawn
  , bends: bends drawn
  , width: box.width
  , height: box.height
  , area: area drawn
  , aspect: aspect drawn
  , nodeOverlaps: nodeOverlaps drawn
  , edgesThroughNodes: edgesThroughNodes drawn
  , outlineTrespasses: outlineTrespasses drawn
  , strangers: strangersIn drawn
  , unrouted: unrouted drawn
  }
  where
  box = extent drawn

-- ---------------------------------------------------------------------------
-- geometry
-- ---------------------------------------------------------------------------

type Piece = { edge :: Int, a :: Point, b :: Point, left :: Number, right :: Number, top :: Number, bottom :: Number }

piecesOf :: Drawn -> Array Piece
piecesOf drawn = Array.concat $ Array.mapWithIndex
  (\edge drawnEdge -> Array.zipWith (\a b -> { edge, a, b, left: min a.x b.x, right: max a.x b.x, top: min a.y b.y, bottom: max a.y b.y }) drawnEdge.points (Array.drop 1 drawnEdge.points))
  drawn.edges

properly :: Piece -> Piece -> Boolean
properly p q =
  opposite (cross p.a p.b q.a) (cross p.a p.b q.b) && opposite (cross q.a q.b p.a) (cross q.a q.b p.b)
  where
  opposite m n = (m > tiny && n < -tiny) || (m < -tiny && n > tiny)
  tiny = 1.0e-9

crossings :: Drawn -> Int
crossings drawn = sweep 0 0
  where
  edges = drawn.edges
  -- where an edge ends at a node, if it has an end there
  endAt node index = Array.index edges index >>= \drawnEdge ->
    if drawnEdge.from == node then Array.head drawnEdge.points
    else if drawnEdge.to == node then Array.last drawnEdge.points
    else Nothing
  nodesOf index = maybe [] (\drawnEdge -> Array.filter (_ >= 0) [ drawnEdge.from, drawnEdge.to ]) (Array.index edges index)
  -- the two pieces cross where the two edges meet at a node they share
  meeting piece other = case intersection piece other of
    Nothing -> false
    Just at -> Array.any
      ( \node -> case endAt node piece.edge, endAt node other.edge of
          Just one, Just two -> distance at one <= skin && distance at two <= skin
          _, _ -> false
      )
      (Array.filter (\node -> Array.elem node (nodesOf other.edge)) (nodesOf piece.edge))
  pieces = Array.sortWith _.left (piecesOf drawn)
  count = Array.length pieces
  sweep at found =
    if at >= count then found
    else case Array.index pieces at of
      Nothing -> found
      Just piece -> sweep (at + 1) (against piece (at + 1) found)
  -- every later piece that begins before this one ends
  against piece at found = case Array.index pieces at of
    Nothing -> found
    Just other ->
      if other.left > piece.right then found
      else against piece (at + 1)
        (if other.edge /= piece.edge && other.top <= piece.bottom && piece.top <= other.bottom && properly piece other && not (meeting piece other) then found + 1 else found)

-- | Where two pieces' lines cross, if they are not parallel.
intersection :: Piece -> Piece -> Maybe Point
intersection p q =
  let
    r = { x: p.b.x - p.a.x, y: p.b.y - p.a.y }
    d = { x: q.b.x - q.a.x, y: q.b.y - q.a.y }
    denominator = r.x * d.y - r.y * d.x
    t = ((q.a.x - p.a.x) * d.y - (q.a.y - p.a.y) * d.x) / denominator
  in
    if denominator == 0.0 then Nothing else Just { x: p.a.x + t * r.x, y: p.a.y + t * r.y }

edgeLength :: Drawn -> Number
edgeLength drawn = sum (piecesOf drawn <#> \piece -> distance piece.a piece.b)

bends :: Drawn -> Number
bends drawn = sum (map (turning <<< _.points) drawn.edges) / (pi / 2.0)

extent :: Drawn -> Box
extent drawn = case Array.uncons corners of
  Nothing -> { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
  Just { head, tail } ->
    let
      found = foldl (\acc point -> { left: min acc.left point.x, right: max acc.right point.x, top: min acc.top point.y, bottom: max acc.bottom point.y }) { left: head.x, right: head.x, top: head.y, bottom: head.y } tail
    in
      { x: found.left, y: found.top, width: found.right - found.left, height: found.bottom - found.top }
  where
  cornersOf box = [ { x: box.x, y: box.y }, { x: box.x + box.width, y: box.y + box.height } ]
  corners = Array.concatMap cornersOf drawn.nodes <> Array.concatMap (cornersOf <<< _.box) drawn.outlines <> Array.concatMap _.points drawn.edges

area :: Drawn -> Number
area drawn = let box = extent drawn in box.width * box.height

aspect :: Drawn -> Number
aspect drawn = let box = extent drawn in if box.height > 0.0 then box.width / box.height else 0.0

overlapping :: Box -> Box -> Boolean
overlapping a b =
  a.x < b.x + b.width - skin && b.x < a.x + a.width - skin
    && a.y < b.y + b.height - skin
    && b.y < a.y + a.height - skin

nodeOverlaps :: Drawn -> Int
nodeOverlaps drawn = sum $ Array.mapWithIndex
  (\place box -> Array.length (Array.filter (overlapping box) (Array.drop (place + 1) drawn.nodes)))
  drawn.nodes

-- | A box drawn in by the skin on every side; nothing where that leaves none.
within :: Box -> Maybe Box
within box =
  if box.width > 2.0 * skin && box.height > 2.0 * skin then Just { x: box.x + skin, y: box.y + skin, width: box.width - 2.0 * skin, height: box.height - 2.0 * skin }
  else Nothing

-- | Whether a straight piece has a point in a box (Liang and Barsky's
-- | clipping: the piece's parameter range inside all four sides is not empty).
enters :: Box -> Point -> Point -> Boolean
enters box a b = fromMaybe false do
  first <- clip (-dx) (a.x - box.x) { from: 0.0, to: 1.0 }
  second <- clip dx (box.x + box.width - a.x) first
  third <- clip (-dy) (a.y - box.y) second
  fourth <- clip dy (box.y + box.height - a.y) third
  pure (fourth.from <= fourth.to)
  where
  dx = b.x - a.x
  dy = b.y - a.y
  clip p q range =
    if p == 0.0 then (if q < 0.0 then Nothing else Just range)
    else
      let
        r = q / p
      in
        if p < 0.0 then (if r > range.to then Nothing else Just range { from = max range.from r })
        else (if r < range.from then Nothing else Just range { to = min range.to r })

passes :: Box -> Array Point -> Boolean
passes box points = case within box of
  Nothing -> false
  Just inner -> Array.any identity (Array.zipWith (enters inner) points (Array.drop 1 points))

-- | The box round a polyline, for telling quickly that it is nowhere near.
reach :: Array Point -> Box
reach points = extent { nodes: [], edges: [ { from: -1, to: -1, points } ], outlines: [] }

near :: Box -> Box -> Boolean
near a b = a.x <= b.x + b.width && b.x <= a.x + a.width && a.y <= b.y + b.height && b.y <= a.y + a.height

edgesThroughNodes :: Drawn -> Int
edgesThroughNodes drawn = sum $ drawn.edges <#> \drawnEdge ->
  let
    round = reach drawnEdge.points
  in
    if Array.length drawnEdge.points < 2 then 0
    else Array.length $ Array.filter identity $ Array.mapWithIndex
      (\place box -> place /= drawnEdge.from && place /= drawnEdge.to && near round box && passes box drawnEdge.points)
      drawn.nodes

outlineTrespasses :: Drawn -> Int
outlineTrespasses drawn = sum $ drawn.outlines <#> \outline ->
  let
    members = Set.fromFoldable outline.members
  in
    Array.length $ Array.filter
      (\drawnEdge -> not (Set.member drawnEdge.from members) && not (Set.member drawnEdge.to members) && Array.length drawnEdge.points >= 2 && near (reach drawnEdge.points) outline.box && passes outline.box drawnEdge.points)
      drawn.edges

-- | The pairs of an outline and a node that is no member of it and is in
-- | its box or partly in it, to a millionth of a pixel: the count of what
-- | `Hylograph.Flow.Render.Laws.strangers` lists.
strangersIn :: Drawn -> Int
strangersIn drawn = sum $ drawn.outlines <#> \outline ->
  let
    members = Set.fromFoldable outline.members
  in
    Array.length $ Array.filter identity $ Array.mapWithIndex (\place box -> not (Set.member place members) && touching box outline.box) drawn.nodes
  where
  tolerance = 1.0e-6
  touching a b =
    a.x < b.x + b.width - tolerance && b.x < a.x + a.width - tolerance
      && a.y < b.y + b.height - tolerance
      && b.y < a.y + a.height - tolerance

unrouted :: Drawn -> Int
unrouted drawn = Array.length (Array.filter (\drawnEdge -> Array.length drawnEdge.points < 2) drawn.edges)
