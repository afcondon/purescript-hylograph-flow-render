-- | A layout with its boxes moved, its ribbons following; and two layouts
-- | of one projection blended (0.6).
-- |
-- | Both are for showing a layout on the way to being another: a transition
-- | between two cuts (`Hylograph.Flow.Render.Transition`), and a node drawn
-- | quiet (`Hylograph.Flow.Render.Present`). Neither places anything: the
-- | boxes are the caller's, and a route goes with its two ends.
-- |
-- | WHAT IS KEPT. A route's start goes with the side of its source that it
-- | leaves by and its end with the side of its target that it enters by
-- | (the far side and the near side along the flow), each by exactly as
-- | much as that side moved; what is between goes by as much as its place
-- | along the route says, from the one to the other. So a ribbon stands
-- | where it stood on each node's side, as broad as it was: if widths were
-- | carried before (`Laws.Carried`), they are carried after, exactly, and
-- | every route still meets its nodes (`Laws.Routed`). A route keeps its
-- | pieces, so the layout before and the layout after can be blended piece
-- | by piece. What is not kept: boxes may overlap, a ribbon may cross a
-- | node, and an outline is wherever the caller put it. These are pictures
-- | of a layout in motion, not layouts an engine would make.
module Hylograph.Flow.Render.Reshape
  ( Reshaping
  , reshaped
  , blend
  , sameShape
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Hylograph.Flow.Render.Engine (Box, Direction(..), Layout, PlacedNode, PlacedOutline)
import Hylograph.Flow.Render.Route (Curve, Piece(..), Point, Route(..))

-- | Where each node's box is to be, and each outline's; and how large the
-- | picture then is.
type Reshaping = { node :: PlacedNode -> Box, outline :: PlacedOutline -> Box, width :: Number, height :: Number }

-- | A layout with every box where the reshaping puts it and every route
-- | gone with its ends; see the module's header.
reshaped :: forall wire. Reshaping -> Layout wire -> Layout wire
reshaped to laid = laid
  { nodes = laid.nodes <#> \n -> n { box = to.node n }
  , edges = laid.edges <#> \e ->
      let
        leaving = fromMaybe still (Map.lookup e.edge.from moves <#> _.far)
        entering = fromMaybe still (Map.lookup e.edge.to moves <#> _.near)
      in
        if leaving == still && entering == still then e else e { route = carried leaving entering e.route }
  , outlines = laid.outlines <#> \outline -> outline { box = to.outline outline }
  , width = to.width
  , height = to.height
  }
  where
  still = { x: 0.0, y: 0.0 }

  -- how far each node's two sides along the flow have moved: the near one,
  -- that edges enter by, and the far one, that they leave by
  moves = foldl
    ( \m n ->
        if Map.member n.node.key m then m
        else
          let
            now = to.node n
            near = { x: now.x - n.box.x, y: now.y - n.box.y }
            far = case laid.direction of
              LeftToRight -> { x: now.x + now.width - (n.box.x + n.box.width), y: now.y - n.box.y }
              TopDown -> { x: now.x - n.box.x, y: now.y + now.height - (n.box.y + n.box.height) }
          in
            Map.insert n.node.key { near, far } m
    )
    Map.empty
    laid.nodes

-- | A route with its start moved by one step and its end by another, and
-- | what is between by as much as its place along the route says.
carried :: Point -> Point -> Route -> Route
carried leaving entering = case _ of
  Ribbon ribbon ->
    let
      count = NonEmptyArray.length ribbon.pieces
      at index = stepAt (toNumber index / toNumber count)
    in
      Ribbon ribbon
        { from = add ribbon.from leaving
        , pieces = NonEmptyArray.mapWithIndex
            ( \index -> case _ of
                Straight to -> Straight (add to (at (index + 1)))
                Curved curve -> Curved (curved (at index) (at (index + 1)) curve)
            )
            ribbon.pieces
        }
  Polyline line ->
    let
      count = Array.length line.through + 1
    in
      Polyline
        { from: add line.from leaving
        , through: Array.mapWithIndex (\index point -> add point (stepAt (toNumber (index + 1) / toNumber count))) line.through
        , to: add line.to entering
        , jumps: []
        }
  Spline spline ->
    let
      count = NonEmptyArray.length spline.curves
      at index = stepAt (toNumber index / toNumber count)
    in
      Spline { from: add spline.from leaving, curves: NonEmptyArray.mapWithIndex (\index -> curved (at index) (at (index + 1))) spline.curves }
  where
  add point step = { x: point.x + step.x, y: point.y + step.y }
  stepAt along = { x: leaving.x + (entering.x - leaving.x) * along, y: leaving.y + (entering.y - leaving.y) * along }

  curved :: Point -> Point -> Curve -> Curve
  curved before after curve = { leaving: add curve.leaving before, arriving: add curve.arriving after, to: add curve.to after }

-- | Whether two layouts are of one projection, piece for piece: the same
-- | nodes in order, the same edges, and every route of the same pieces. Two
-- | such layouts can be blended.
sameShape :: forall wire. Layout wire -> Layout wire -> Boolean
sameShape one other =
  one.direction == other.direction
    && map _.node.key one.nodes == map _.node.key other.nodes
    && Array.length one.edges == Array.length other.edges
    && all identity (Array.zipWith (\a b -> a.index == b.index && a.edge.from == b.edge.from && a.edge.to == b.edge.to && shapeOf a.route == shapeOf b.route) one.edges other.edges)
  where
  shapeOf = case _ of
    Ribbon ribbon -> [ 0 ] <> map pieceShape (NonEmptyArray.toArray ribbon.pieces)
    Polyline line -> [ 1, Array.length line.through ]
    Spline spline -> [ 2, NonEmptyArray.length spline.curves ]
  pieceShape = case _ of
    Straight _ -> 0
    Curved _ -> 1

-- | A layout part of the way from one to another of the same shape
-- | (`sameShape`): every number of the first gone that part of the way to
-- | the second's (0 is the first, 1 the second). A route whose pieces are
-- | not the other's is the first's until half way and the second's after;
-- | an outline that only one of them has is drawn while that one is nearer.
-- | Where both carry widths and their ribbons stand alike on their nodes,
-- | so does every blend of them: a ribbon's end goes with its node's side.
blend :: forall wire. Layout wire -> Layout wire -> Number -> Layout wire
blend one other part =
  { direction: one.direction
  , nodes: Array.zipWith (\a b -> a { box = box a.box b.box }) one.nodes other.nodes
  , edges: Array.zipWith (\a b -> a { route = route a.route b.route, course = if part < 0.5 then a.course else b.course }) one.edges other.edges
  , outlines:
      Array.mapMaybe (\a -> case Array.find (\b -> b.group == a.group) other.outlines of
        Just b -> Just a { box = box a.box b.box, header = mix a.header b.header }
        Nothing -> if part < 0.5 then Just a else Nothing) one.outlines
        <> Array.filter (\b -> part >= 0.5 && not (Array.any (\a -> a.group == b.group) one.outlines)) other.outlines
  , width: mix one.width other.width
  , height: mix one.height other.height
  }
  where
  mix a b = a + (b - a) * part
  point a b = { x: mix a.x b.x, y: mix a.y b.y }
  box a b = { x: mix a.x b.x, y: mix a.y b.y, width: mix a.width b.width, height: mix a.height b.height }
  curve a b = { leaving: point a.leaving b.leaving, arriving: point a.arriving b.arriving, to: point a.to b.to }
  nearer :: Route -> Route -> Route
  nearer a b = if part < 0.5 then a else b
  route a b = case a, b of
    Ribbon x, Ribbon y | NonEmptyArray.length x.pieces == NonEmptyArray.length y.pieces ->
      fromMaybe (nearer a b) do
        pieces <- NonEmptyArray.fromArray =<< pieced (NonEmptyArray.toArray x.pieces) (NonEmptyArray.toArray y.pieces)
        pure (Ribbon { from: point x.from y.from, pieces, breadth: mix x.breadth y.breadth, ink: if part < 0.5 then x.ink else y.ink })
    Polyline x, Polyline y | Array.length x.through == Array.length y.through ->
      Polyline { from: point x.from y.from, through: Array.zipWith point x.through y.through, to: point x.to y.to, jumps: [] }
    Spline x, Spline y | NonEmptyArray.length x.curves == NonEmptyArray.length y.curves ->
      Spline { from: point x.from y.from, curves: NonEmptyArray.zipWith curve x.curves y.curves }
    _, _ -> nearer a b
  pieced xs ys =
    let
      found = Array.zipWith
        ( \x y -> case x, y of
            Straight p, Straight q -> Just (Straight (point p q))
            Curved p, Curved q -> Just (Curved (curve p q))
            _, _ -> Nothing
        )
        xs
        ys
    in
      if all (_ /= Nothing) found then Just (Array.catMaybes found) else Nothing
