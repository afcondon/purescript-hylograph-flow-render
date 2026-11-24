-- | The line an edge is drawn along, as data.
-- |
-- | In 0.2 a placed edge held an SVG path string. A string can be drawn and
-- | nothing else: to measure an edge an earlier layout trial had to
-- | parse the string back, and a second layout engine had to print its
-- | routes as strings only for them to be read again. Here the route is a
-- | value, the string is derived from it (`svgPath`), and so are the points
-- | it is measured along (`flatten`).
-- |
-- | THE THREE ROUTES. `Route` is closed, and has exactly the routes of the
-- | three engines this renderer is built to take. None is there for an
-- | engine nobody has.
-- |
-- | - `Ribbon`, the Sankey layout's: a centre line of straight and cubic
-- |   pieces, and a `breadth`, the room the edge takes on the side of each
-- |   node it touches (its two attachment spans are the centre line's two
-- |   ends, half the breadth either way across the flow). The Sankey
-- |   layout's ribbons are as broad at one end as at the other, so the width
-- |   profile is one number; an engine that tapers one would need a profile
-- |   here, and none of the three does. `ink` says how the ribbon is drawn:
-- |   a `Band` as broad as it is, or a `Hairline` down its middle. It is an
-- |   ADT and not a second number because a stroke beside a breadth is two
-- |   numbers that can disagree, and in a band they are one.
-- | - `Polyline`, `layered-layout`'s: straight legs from point to point,
-- |   and its line jumps (where it hops over an edge it crosses). That the
-- |   bends are right angles is not in the type: that engine's routes
-- |   through nested groups are joined at the group's boundary and may
-- |   slant by a pixel or two there, so right angles are a promise an engine
-- |   makes and a law checks (`RoutesOrthogonal`), not a constructor.
-- | - `Spline`, Graphviz dot's (Stemma): cubic Bézier pieces end to end.
-- |
-- | NO ROUTE WITHOUT A LINE. Each constructor has a first point and at least
-- | one more (a `NonEmptyArray` of pieces, or a `to`), so an edge that is
-- | placed has somewhere to be drawn; an engine that cannot route an edge
-- | leaves the edge out, and the law that every edge is placed says so. The
-- | trial's `unrouted` measure counted polylines of fewer than two points;
-- | there is no such value now.
-- |
-- | MEASURING. `flatten` is the one rule by which every route becomes
-- | straight pieces: a straight piece is its two ends, a cubic piece is
-- | `curveSteps` (12) straight pieces, and a cubic whose four points lie on
-- | one line is one. It is the trial's rule (it read the same pieces back
-- | from the path string), kept so that its figures stand. A polyline's
-- | jumps are ink, not geometry: they are drawn and not measured.
module Hylograph.Flow.Render.Route
  ( Point
  , Curve
  , Piece(..)
  , Ink(..)
  , Route(..)
  , start
  , end
  , flatten
  , flattenPieces
  , curveSteps
  , length
  , turning
  , bends
  , numbers
  , transpose
  , svgPath
  , sweptPath
  , hopRadius
  , distance
  , cross
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs, atan2, pi, sqrt)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))

type Point = { x :: Number, y :: Number }

-- | A cubic Bézier piece from where the line has got to: it leaves towards
-- | `leaving`, arrives from `arriving`, and ends at `to`.
type Curve = { leaving :: Point, arriving :: Point, to :: Point }

-- | One piece of a ribbon's centre line, from where the last piece ended.
data Piece
  = Straight Point
  | Curved Curve

derive instance Eq Piece

instance Show Piece where
  show = case _ of
    Straight to -> "(Straight " <> show to <> ")"
    Curved curve -> "(Curved " <> show curve <> ")"

-- | How a ribbon is drawn: as broad as it is, or as a line of this stroke
-- | down its middle.
data Ink
  = Band
  | Hairline Number

derive instance Eq Ink

instance Show Ink where
  show = case _ of
    Band -> "Band"
    Hairline stroke -> "(Hairline " <> show stroke <> ")"

data Route
  = Ribbon { from :: Point, pieces :: NonEmptyArray Piece, breadth :: Number, ink :: Ink }
  | Polyline { from :: Point, through :: Array Point, to :: Point, jumps :: Array Point }
  | Spline { from :: Point, curves :: NonEmptyArray Curve }

derive instance Eq Route

instance Show Route where
  show = case _ of
    Ribbon ribbon -> "(Ribbon " <> show ribbon <> ")"
    Polyline line -> "(Polyline " <> show line <> ")"
    Spline spline -> "(Spline " <> show spline <> ")"

-- | Where a route leaves its source.
start :: Route -> Point
start = case _ of
  Ribbon ribbon -> ribbon.from
  Polyline line -> line.from
  Spline spline -> spline.from

-- | Where a route enters its target.
end :: Route -> Point
end = case _ of
  Ribbon ribbon -> pieceEnd (NonEmptyArray.last ribbon.pieces)
  Polyline line -> line.to
  Spline spline -> (NonEmptyArray.last spline.curves).to

pieceEnd :: Piece -> Point
pieceEnd = case _ of
  Straight to -> to
  Curved curve -> curve.to

-- | Straight pieces to a cubic piece.
curveSteps :: Int
curveSteps = 12

-- | A route as the points of its straight pieces, first to last; see the
-- | module's header for the rule. A point is not given twice running.
flatten :: Route -> Array Point
flatten = case _ of
  Ribbon ribbon -> flattenPieces ribbon.from (NonEmptyArray.toArray ribbon.pieces)
  Polyline line -> foldl extend [ line.from ] (Array.snoc line.through line.to)
  Spline spline -> flattenPieces spline.from (map Curved (NonEmptyArray.toArray spline.curves))

-- | A line of pieces from a point, as points.
flattenPieces :: Point -> Array Piece -> Array Point
flattenPieces from = foldl step [ from ]
  where
  step points = case _ of
    Straight to -> extend points to
    Curved curve -> case Array.last points of
      Just last -> points <> cubic last curve
      Nothing -> [ curve.to ]

extend :: Array Point -> Point -> Array Point
extend points point = case Array.last points of
  Just last | last.x == point.x && last.y == point.y -> points
  _ -> Array.snoc points point

cubic :: Point -> Curve -> Array Point
cubic p0 curve =
  -- a curve with all four points on one line is one straight piece
  if collinear p0 curve.leaving curve.to && collinear p0 curve.arriving curve.to then [ curve.to ]
  else Array.range 1 curveSteps <#> \index ->
    let
      t = toNumber index / toNumber curveSteps
      u = 1.0 - t
    in
      { x: u * u * u * p0.x + 3.0 * u * u * t * curve.leaving.x + 3.0 * u * t * t * curve.arriving.x + t * t * t * curve.to.x
      , y: u * u * u * p0.y + 3.0 * u * u * t * curve.leaving.y + 3.0 * u * t * t * curve.arriving.y + t * t * t * curve.to.y
      }
  where
  collinear a b c = abs (cross a b c) < 1.0e-9

-- | Twice the signed area of the triangle a, b, c.
cross :: Point -> Point -> Point -> Number
cross a b c = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

distance :: Point -> Point -> Number
distance a b = sqrt ((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y))

-- | A route's length along its straight pieces, pixels.
length :: Route -> Number
length route = let points = flatten route in sum (Array.zipWith distance points (Array.drop 1 points))

-- | How far a line of points turns, in radians: the sum over every interior
-- | point of the angle turned there.
turning :: Array Point -> Number
turning points = sum (Array.zipWith (\(Tuple a b) c -> turned a b c) (Array.zip points (Array.drop 1 points)) (Array.drop 2 points))
  where
  turned a b c =
    if distance a b < 1.0e-9 || distance b c < 1.0e-9 then 0.0
    else abs (atan2 (cross a b c) ((b.x - a.x) * (c.x - b.x) + (b.y - a.y) * (c.y - b.y)))

-- | A route's total turning in right angles: one for each corner of an
-- | orthogonal route; for a curve, by how far it swings.
bends :: Route -> Number
bends route = turning (flatten route) / (pi / 2.0)

-- | Every number a route is made of, for a law that each is finite.
numbers :: Route -> Array Number
numbers = case _ of
  Ribbon ribbon -> [ ribbon.breadth ] <> inkNumbers ribbon.ink <> pointNumbers ribbon.from <> Array.concatMap pieceNumbers (NonEmptyArray.toArray ribbon.pieces)
  Polyline line -> Array.concatMap pointNumbers ([ line.from ] <> line.through <> [ line.to ] <> line.jumps)
  Spline spline -> pointNumbers spline.from <> Array.concatMap curveNumbers (NonEmptyArray.toArray spline.curves)
  where
  pointNumbers point = [ point.x, point.y ]
  curveNumbers curve = pointNumbers curve.leaving <> pointNumbers curve.arriving <> pointNumbers curve.to
  pieceNumbers = case _ of
    Straight to -> pointNumbers to
    Curved curve -> curveNumbers curve
  inkNumbers = case _ of
    Band -> []
    Hairline stroke -> [ stroke ]

-- | A route with x for y at every point: the route of a layout stood on
-- | end (0.5).
transpose :: Route -> Route
transpose = case _ of
  Ribbon ribbon -> Ribbon ribbon { from = turned ribbon.from, pieces = map piece ribbon.pieces }
  Polyline line -> Polyline { from: turned line.from, through: map turned line.through, to: turned line.to, jumps: map turned line.jumps }
  Spline spline -> Spline { from: turned spline.from, curves: map curve spline.curves }
  where
  turned point = { x: point.y, y: point.x }
  curve c = { leaving: turned c.leaving, arriving: turned c.arriving, to: turned c.to }
  piece = case _ of
    Straight to -> Straight (turned to)
    Curved c -> Curved (curve c)

-- | The radius of the hop a polyline makes over an edge it crosses.
hopRadius :: Number
hopRadius = 3.0

-- | A route as the `d` of an SVG path: absolute `M`, `L` and `C`, and for a
-- | polyline's jumps `A`. A jump is drawn as a half circle of `hopRadius`
-- | where it lies on a leg with that much of the leg clear on either side
-- | of it (of the leg's ends and of the jump before); a jump with less room
-- | is passed straight through.
svgPath :: Route -> String
svgPath = case _ of
  Ribbon ribbon -> "M" <> at ribbon.from <> Array.fold (map piece (NonEmptyArray.toArray ribbon.pieces))
  Polyline line -> "M" <> at line.from <> Array.fold (Array.zipWith (leg line.jumps) points (Array.drop 1 points))
    where
    points = foldl extend [ line.from ] (Array.snoc line.through line.to)
  Spline spline -> "M" <> at spline.from <> Array.fold (map curved (NonEmptyArray.toArray spline.curves))
  where
  at point = toString point.x <> "," <> toString point.y
  curved curve = " C" <> at curve.leaving <> " " <> at curve.arriving <> " " <> at curve.to
  piece = case _ of
    Straight to -> " L" <> at to
    Curved curve -> curved curve
  leg jumps a b =
    let
      long = distance a b
      along = { x: (b.x - a.x) / long, y: (b.y - a.y) / long }
      -- how far along the leg each jump on it is
      found = Array.sort $ jumps # Array.mapMaybe \jump ->
        let
          reach = (jump.x - a.x) * along.x + (jump.y - a.y) * along.y
          off = abs ((jump.x - a.x) * along.y - (jump.y - a.y) * along.x)
        in
          if off < 1.0e-6 && reach >= hopRadius && reach <= long - hopRadius then Just reach else Nothing
      clear = (foldl (\state reach -> if reach - state.last >= 2.0 * hopRadius then { last: reach, kept: Array.snoc state.kept reach } else state) { last: -hopRadius, kept: [] } found).kept
      pointAt reach = { x: a.x + along.x * reach, y: a.y + along.y * reach }
      hop reach = " L" <> at (pointAt (reach - hopRadius)) <> " A" <> toString hopRadius <> "," <> toString hopRadius <> " 0 0 1 " <> at (pointAt (reach + hopRadius))
    in
      Array.fold (map hop clear) <> " L" <> at b

-- | A ribbon as the `d` of the area it sweeps (0.5): its span on its
-- | source, carried along its centre line to its span on its target. `across`
-- | is one pixel across the flow ({ x: 0, y: 1 } where the flow runs left
-- | to right, { x: 1, y: 0 } top-down); the outline is the centre line set
-- | half the breadth to one side, the end, and the centre line set half the
-- | breadth to the other side and run backwards, closed. So both ends are
-- | flat on their nodes' sides, and the area is exactly its breadth across
-- | the flow all the way, however much it slants, which a line stroked as
-- | broad as the ribbon is not: a stroke is broad across the line. A route
-- | that is no ribbon has no breadth to sweep, and its path is `svgPath`'s.
sweptPath :: Point -> Route -> String
sweptPath across = case _ of
  Ribbon ribbon ->
    let
      half = ribbon.breadth / 2.0
      set side point = { x: point.x + side * half * across.x, y: point.y + side * half * across.y }
      at point = toString point.x <> "," <> toString point.y
      -- each piece with where it starts
      legs = (foldl (\state piece -> { from: endOf piece, found: Array.snoc state.found { from: state.from, piece } }) { from: ribbon.from, found: [] } (NonEmptyArray.toArray ribbon.pieces)).found
      endOf = case _ of
        Straight to -> to
        Curved curve -> curve.to
      out leg = case leg.piece of
        Straight to -> " L" <> at (set (-1.0) to)
        Curved curve -> " C" <> at (set (-1.0) curve.leaving) <> " " <> at (set (-1.0) curve.arriving) <> " " <> at (set (-1.0) curve.to)
      back leg = case leg.piece of
        Straight _ -> " L" <> at (set 1.0 leg.from)
        Curved curve -> " C" <> at (set 1.0 curve.arriving) <> " " <> at (set 1.0 curve.leaving) <> " " <> at (set 1.0 leg.from)
    in
      "M" <> at (set (-1.0) ribbon.from) <> Array.fold (map out legs) <> " L" <> at (set 1.0 (end (Ribbon ribbon))) <> Array.fold (map back (Array.reverse legs)) <> " Z"
  other -> svgPath other
