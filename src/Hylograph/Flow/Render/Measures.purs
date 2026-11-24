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
-- |   meeting counted one.) **A crossing at a point where a line is cut
-- |   into pieces counts too (0.7)**: where one line has a corner on the
-- |   other (on a corner of it, or within one of its pieces) and the two
-- |   pieces either side of that corner lie strictly on opposite sides of
-- |   the other line there, the lines cross once. Until 0.7 that was a
-- |   touch and scored nothing, which missed the commonest crossing a
-- |   layered layout has: two neighbours of a tier that change places on
-- |   the way to the next cross exactly at the middle of their two curves,
-- |   and the middle is where a curve is cut. A corner that touches and
-- |   turns back, and two lines that share a stretch, still cross nowhere.
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
-- |
-- | THE MEASURES OF LEGIBILITY (0.7). The measures above score a drawing as
-- | a graph. These score it as something a general reader has to follow,
-- | by four heuristics that are **guesses** (Andrew's word) and are here to
-- | be calibrated; the numbers they are held against are `thresholds`, in
-- | this one place. They need what a `Drawn` has not: how broad each ribbon
-- | is, what colour it is drawn in, and which way the flow runs. That is a
-- | `Banded` (`bandedOf` makes one of a layout and a colour for each edge;
-- | it can be made by hand).
-- |
-- | - `overlaps`: the pairs of ribbons that cross (their middle lines, by
-- |   `crossings`' rule) or overlie one another (their ink has more than
-- |   `thresholds.overlap` square pixels in common), each with that area.
-- |   `overlapsInOneColour` are those drawn in one colour: the ones a
-- |   reader cannot follow through. The area is exact where both ribbons
-- |   are swept bands (flat across the flow: a forward ribbon as the tree's
-- |   `Swept` draws it); a ribbon drawn as a stroke (turned back, or a
-- |   hairline) is taken piece by piece and its corners counted twice, so
-- |   its areas are a little over.
-- | - `proportions`: of each ribbon, its run along the flow, its breadth
-- |   (the data's, not the ink's), and how far it travels across the flow;
-- |   `squat` are those whose run is less than `thresholds.proportion`
-- |   times its breadth, or times its travel.
-- | - `pictureProportion`: the picture's length along the flow over its
-- |   breadth across it.
-- | - `againstTheFlow`: the ribbons that anywhere run backward along the
-- |   flow by more than `thresholds.against` pixels in all, read from
-- |   their lines and not from what the engine says of them.
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
  , Thresholds
  , thresholds
  , Band
  , Banded
  , bandedOf
  , Overlap
  , overlaps
  , overlapsInOneColour
  , Proportion
  , proportions
  , squat
  , pictureProportion
  , againstTheFlow
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (abs, atan2, pi)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Projection)
import Hylograph.Flow.Render.Engine (Box, Course(..), Direction(..), Layout, PlacedEdge)
import Hylograph.Flow.Render.Route (Ink(..), Point, Route(..), cross, distance, flatten, turning)

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

-- | `next`: where the edge's line goes after this piece, where it goes on.
type Piece = { edge :: Int, a :: Point, b :: Point, next :: Maybe Point, left :: Number, right :: Number, top :: Number, bottom :: Number }

piecesOf :: Drawn -> Array Piece
piecesOf drawn = Array.concat $ Array.mapWithIndex
  (\edge drawnEdge -> Array.mapWithIndex (\place (Tuple a b) -> { edge, a, b, next: Array.index drawnEdge.points (place + 2), left: min a.x b.x, right: max a.x b.x, top: min a.y b.y, bottom: max a.y b.y }) (Array.zip drawnEdge.points (Array.drop 1 drawnEdge.points)))
  drawn.edges

-- | Whether one line crosses another at the corner where `piece` ends (0.7):
-- | the corner is on the other line, at the corner where `other` ends or
-- | within `other`, and the pieces either side of it are strictly on
-- | opposite sides of the other line there. Each such crossing is found for
-- | one pair of pieces only: the two that end at it, or the one that ends
-- | at it and the one it is within.
cornerCrossing :: Piece -> Piece -> Boolean
cornerCrossing piece other = case piece.next of
  Nothing -> false
  Just after ->
    if same piece.b other.b then case other.next of
      Just beyond -> sectors other.a beyond piece.b piece.a after
      Nothing -> false
    else if same piece.b other.a then false
    else inside piece.b other && opposite (cross other.a other.b piece.a) (cross other.a other.b after)
  where
  same p q = distance p q <= 1.0e-7
  opposite m n = (m > tiny && n < -tiny) || (m < -tiny && n > tiny)
  tiny = 1.0e-9
  -- strictly within a piece: on its line, and between its ends
  inside point q =
    let
      long = distance q.a q.b
      reach = ((point.x - q.a.x) * (q.b.x - q.a.x) + (point.y - q.a.y) * (q.b.y - q.a.y)) / long
    in
      long > 1.0e-7 && abs (cross q.a q.b point) / long <= 1.0e-7 && reach > 1.0e-7 && reach < long - 1.0e-7
  -- the rays from `at` to `d` and to `e` cut the plane in two; `a` and `c`
  -- are strictly in different halves
  sectors d e at a c =
    let
      turn point = let angle = atan2 (point.y - at.y) (point.x - at.x) - atan2 (d.y - at.y) (d.x - at.x) in if angle < 0.0 then angle + 2.0 * pi else angle
      wedge = turn e
      strictly angle = angle > 1.0e-9 && abs (angle - wedge) > 1.0e-9 && angle < 2.0 * pi - 1.0e-9
      within angle = angle < wedge
    in
      strictly (turn a) && strictly (turn c) && wedge > 1.0e-9 && within (turn a) /= within (turn c)

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
        ( if other.edge /= piece.edge && other.top <= piece.bottom && piece.top <= other.bottom then
            found
              + (if properly piece other && not (meeting piece other) then 1 else 0)
              + (if cornerCrossing piece other then 1 else 0)
              + (if not (distance piece.b other.b <= 1.0e-7) && cornerCrossing other piece then 1 else 0)
          else found
        )

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

-- ---------------------------------------------------------------------------
-- legibility (0.7)
-- ---------------------------------------------------------------------------

-- | What the measures of legibility are held against, and what the engines
-- | and the tree take from them. Guesses, to be calibrated.
-- |
-- | - `proportion`: a ribbon, and a picture, should be at least this many
-- |   times as long along the flow as it is broad across it (and a ribbon
-- |   that many times as long as it travels across).
-- | - `overlap`: two ribbons overlie one another when their ink has more
-- |   than this in common, in square pixels.
-- | - `against`: a ribbon runs against the flow when it runs backward by
-- |   more than this, in pixels.
-- | - `hair`: a ribbon less broad than this, in pixels, cannot be seen; the
-- |   unfolding engine draws it this broad and says it is not to scale.
-- | - `nameReach`: an open group's name should be within this of where its
-- |   closed bar's name was, in pixels.
type Thresholds = { proportion :: Number, overlap :: Number, against :: Number, hair :: Number, nameReach :: Number }

thresholds :: Thresholds
thresholds = { proportion: 2.0, overlap: 4.0, against: 1.0, hair: 1.0, nameReach: 80.0 }

-- | A ribbon as it is drawn. `edge`: its index in the layout. `from`,
-- | `to`: the places of its two nodes among the layout's nodes (-1 where it
-- | has none). `breadth`: the data's. `ink`: how broad it is drawn.
-- | `swept`: drawn flat across the flow (a band going forward), not as a
-- | stroke along its line.
type Band = { edge :: Int, from :: Int, to :: Int, colour :: String, breadth :: Number, ink :: Number, swept :: Boolean, points :: Array Point }

type Banded = { direction :: Direction, width :: Number, height :: Number, bands :: Array Band }

-- | A layout's ribbons as drawn, each in the colour it is given. A band
-- | going forward is taken as swept, as the unfolding pages draw it
-- | (`Tree.Swept`); a route that is no ribbon is a line 1.25 broad
-- | (`Tree.hairline`).
bandedOf :: forall wire. (PlacedEdge wire -> String) -> Layout wire -> Banded
bandedOf colourOf laid =
  { direction: laid.direction
  , width: laid.width
  , height: laid.height
  , bands: Array.zipWith
      ( \placed drawnEdge ->
          let
            found = case placed.route of
              Ribbon ribbon -> case ribbon.ink of
                Band -> { breadth: ribbon.breadth, ink: ribbon.breadth, band: true }
                Hairline stroke -> { breadth: ribbon.breadth, ink: stroke, band: false }
                Least stroke -> { breadth: ribbon.breadth, ink: stroke, band: false }
              _ -> { breadth: 0.0, ink: 1.25, band: false }
          in
            { edge: placed.index, from: drawnEdge.from, to: drawnEdge.to, colour: colourOf placed, breadth: found.breadth, ink: found.ink, swept: found.band && placed.course == Forward, points: drawnEdge.points }
      )
      laid.edges
      (drawnOf laid).edges
  }

along :: Direction -> Point -> Number
along direction point = case direction of
  LeftToRight -> point.x
  TopDown -> point.y

athwart :: Direction -> Point -> Number
athwart direction point = case direction of
  LeftToRight -> point.y
  TopDown -> point.x

-- | A ribbon's ink as four-sided pieces, one to each straight piece of its
-- | line: flat across the flow where it is swept, square to the line where
-- | it is stroked. Each with its corners in one sense of turning.
quadsOf :: Direction -> Band -> Array (Array Point)
quadsOf direction band = Array.mapMaybe identity (Array.zipWith quad band.points (Array.drop 1 band.points))
  where
  half = band.ink / 2.0
  quad p q =
    let
      long = distance p q
      set =
        if band.swept then case direction of
          LeftToRight -> { x: 0.0, y: half }
          TopDown -> { x: half, y: 0.0 }
        else if long > 0.0 then { x: -(q.y - p.y) / long * half, y: (q.x - p.x) / long * half }
        else { x: 0.0, y: 0.0 }
      corners = [ { x: p.x - set.x, y: p.y - set.y }, { x: q.x - set.x, y: q.y - set.y }, { x: q.x + set.x, y: q.y + set.y }, { x: p.x + set.x, y: p.y + set.y } ]
      signed = twice corners
    in
      if abs signed < 1.0e-9 then Nothing else Just (if signed < 0.0 then Array.reverse corners else corners)

-- | Twice the signed area of a polygon.
twice :: Array Point -> Number
twice corners = sum (Array.zipWith (\p q -> p.x * q.y - q.x * p.y) corners (Array.drop 1 corners <> Array.take 1 corners))

-- | What two convex polygons of one sense of turning have in common, in
-- | square pixels (Sutherland and Hodgman's clipping, one side at a time).
common :: Array Point -> Array Point -> Number
common subject clipper = abs (twice (foldl cut subject (Array.zip clipper (Array.drop 1 clipper <> Array.take 1 clipper)))) / 2.0
  where
  cut polygon (Tuple a b) = Array.concat (Array.zipWith (step a b) polygon (Array.drop 1 polygon <> Array.take 1 polygon))
  step a b p q =
    let
      sideP = cross a b p
      sideQ = cross a b q
      meet = let t = sideP / (sideP - sideQ) in { x: p.x + t * (q.x - p.x), y: p.y + t * (q.y - p.y) }
    in
      if sideP >= 0.0 && sideQ >= 0.0 then [ q ]
      else if sideP >= 0.0 then [ meet ]
      else if sideQ >= 0.0 then [ meet, q ]
      else []

-- | Two ribbons that cross or overlie one another. `one`, `other`: their
-- | edges' indices, the lesser first. `area`: the ink they have in common.
-- | `crossings`: how many times their middle lines cross.
type Overlap = { one :: Int, other :: Int, colour :: String, otherColour :: String, area :: Number, crossings :: Int }

overlaps :: Banded -> Array Overlap
overlaps banded = Array.concat (Array.mapWithIndex (\place band -> Array.mapMaybe (pair band) (Array.drop (place + 1) dressed)) dressed)
  where
  dressed = banded.bands <#> \band ->
    let
      quads = quadsOf banded.direction band <#> \corners -> { corners, round: extent { nodes: [], edges: [ { from: -1, to: -1, points: corners } ], outlines: [] } }
    in
      { band, quads, round: extent { nodes: [], edges: [], outlines: map (\quad -> { box: quad.round, members: [] }) quads } }
  pair a b =
    if not (near a.round b.round) then Nothing
    else
      let
        area = sum (a.quads >>= \p -> b.quads <#> \q -> if near p.round q.round then common p.corners q.corners else 0.0)
        crossed = crossings { nodes: [], edges: [ lineOf a.band, lineOf b.band ], outlines: [] }
      in
        if area > thresholds.overlap || crossed > 0 then Just { one: min a.band.edge b.band.edge, other: max a.band.edge b.band.edge, colour: a.band.colour, otherColour: b.band.colour, area, crossings: crossed }
        else Nothing
  -- two ribbons of one node keep their node, so that meeting there is no crossing
  lineOf band = { from: band.from, to: band.to, points: band.points }

-- | The overlaps a reader cannot follow: both ribbons in one colour.
overlapsInOneColour :: Banded -> Array Overlap
overlapsInOneColour = Array.filter (\found -> found.colour == found.otherColour) <<< overlaps

-- | A ribbon's shape. `run`: how far its line reaches along the flow.
-- | `travel`: how far it reaches across it. `byBreadth`, `byTravel`: the
-- | run over each (as great as can be where there is none of it).
type Proportion = { edge :: Int, run :: Number, breadth :: Number, travel :: Number, byBreadth :: Number, byTravel :: Number }

proportions :: Banded -> Array Proportion
proportions banded = banded.bands <#> \band ->
  let
    reach by = case Array.uncons (map by band.points) of
      Just { head, tail } -> let found = foldl (\acc v -> { least: min acc.least v, most: max acc.most v }) { least: head, most: head } tail in found.most - found.least
      Nothing -> 0.0
    run = reach (along banded.direction)
    travel = reach (athwart banded.direction)
    over n = if n > 0.0 then run / n else 1.0 / 0.0
  in
    { edge: band.edge, run, breadth: band.breadth, travel, byBreadth: over band.breadth, byTravel: over travel }

-- | The ribbons too broad, or too steep, for their run: under the
-- | threshold on either count.
squat :: Banded -> Array Proportion
squat = Array.filter (\found -> found.byBreadth < thresholds.proportion || found.byTravel < thresholds.proportion) <<< proportions

-- | The picture's length along the flow over its breadth across it.
pictureProportion :: Banded -> Number
pictureProportion banded =
  let
    size = { x: banded.width, y: banded.height }
    across = athwart banded.direction size
  in
    if across > 0.0 then along banded.direction size / across else 0.0

-- | The ribbons that run against the flow, by their edges' indices.
againstTheFlow :: Banded -> Array Int
againstTheFlow banded = banded.bands # Array.mapMaybe \band ->
  let
    back = sum (Array.zipWith (\p q -> max 0.0 (along banded.direction p - along banded.direction q)) band.points (Array.drop 1 band.points))
  in
    if back > thresholds.against then Just band.edge else Nothing
