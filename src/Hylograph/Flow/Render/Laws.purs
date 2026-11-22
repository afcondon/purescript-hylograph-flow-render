-- | What must hold of a layout, as functions that return what does not.
-- | Here, and not in a test directory, so that every adapter's tests can
-- | hold the layout of its own real flows to it, and every engine's author
-- | can hold the engine to it.
-- |
-- | THE LAWS ARE SPLIT BY WHAT THEY ARE TRUE OF. In 0.2 there was one list,
-- | and it was the Sankey layout's: an edge as wide as its quantity says, a
-- | node as tall as its wider side. A correct layout by another engine was
-- | reported as dozens of faults. Now:
-- |
-- | **General laws** hold of any layout of any projection under any
-- | picture, whichever engine made it (`generalFaults`):
-- |
-- | - `Placed`: each node of the projection has exactly one place, in the
-- |   projection's order, and each edge one;
-- | - `Finite`: every number is finite (boxes, ports, routes, outlines, the
-- |   picture's size) and no box has a negative side;
-- | - `Within`: every node's box and every outline is inside the picture;
-- | - `Apart`: no two node boxes overlap;
-- | - `Ported`: a placed port is one of its node's own (an input of that
-- |   name, or an output), is placed once, and is within its side's length;
-- | - `Routed`: every route starts on the box of the node its edge is from
-- |   and ends on the box of the node it is to (on the boundary, to within
-- |   `meeting`), and where the edge names a port and the node has that
-- |   port placed, at the port; and a polyline's jumps are on the polyline;
-- | - `Lettered`: where the picture sets words, every node has its words
-- |   set, within the lines allowed; no line is wider than the node's own
-- |   breadth has room for, as the estimate has it (a single character too
-- |   wide for the room aside); and the node is tall enough for them. Where
-- |   it sets none, no node has any;
-- | - `Outlined`: there is one outline for each open group that holds a
-- |   node, and none where the picture tells of no group; its members are
-- |   the nodes the grouping puts in it, however deep; every member is
-- |   inside its outline, under the strip for its name; an outline's parent
-- |   is the nearest open group over it, and the outline is inside its
-- |   parent's, under the parent's strip; the outlines of two groups
-- |   overlap only where one holds the other; and no node that is not a
-- |   member is in an outline, or partly in it.
-- |
-- | **Promised laws** hold of the layouts of an engine that promises them
-- | (`Hylograph.Flow.Render.Engine.Promise`), and are asked of no other
-- | (`promiseFaults`):
-- |
-- | - `FlowForward` (`FlowsForward`): an edge not turned back ends further
-- |   along the flow than it starts (to the right, or below, by the
-- |   layout's direction), and in a later layer where both its nodes have
-- |   one; and where the projection has no cycle no edge is turned back;
-- | - `LayerAlignment` (`LayersAligned`): the nodes of one layer have their
-- |   middles in one line across the flow, and a later layer's line is
-- |   further along it;
-- | - `Orthogonality` (`RoutesOrthogonal`): every route is a polyline, each
-- |   leg of which runs along an axis;
-- | - `Conservation` (`WidthsConserved`): every route is a ribbon, as broad
-- |   as its width says at the promise's scale; the ribbons on one side of
-- |   a node stand inside the node and beside one another; and the node is
-- |   as tall as the broader side, or as its words need where that is more.
-- |   (Checked where every width is positive.)
-- |
-- | `check engine picture projection layout` is the general laws and
-- | exactly the engine's promises. A fault says which law it is of (`Law`,
-- | a closed ADT, so a test can say "this engine breaks `Apart` and nothing
-- | else" without reading words) and what is wrong, in words.
-- |
-- | `faults` and `layoutFaults` are 0.2's, of the Sankey engine with no
-- | words and no groups, and say what they said.
module Hylograph.Flow.Render.Laws
  ( Law(..)
  , Fault
  , generalLaws
  , isGeneral
  , lawOf
  , generalFaults
  , promiseFaults
  , check
  , broken
  , faultsOf
  , layoutFaultsOf
  , faults
  , layoutFaults
  , strangers
  , acyclic
  , varied
  , meeting
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, any, sum)
import Data.Int (toNumber)
import Data.Graph.Algorithms (isDAG, mkSimpleGraph)
import Data.Maybe (Maybe(..))
import Data.Number (abs, isFinite)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Hylograph.Flow (GroupId, Projection)
import Hylograph.Flow.Render.Engine (Box, Conservation, Course(..), Direction(..), Engine, Layout, Picture, PlacedEdge, PlacedNode, PlacedOutline, PortFlow(..), Promise(..), Side(..), openPaths, plainPicture, portPoint, promises, run)
import Hylograph.Flow.Render.Route (Point, Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Sankey (Metrics, Widths(..), sankey)
import Hylograph.Flow.Render.Text (available, estimateWidth, heavier, heightOf)

-- | A law; see the module's header for what each says.
data Law
  = Placed
  | Finite
  | Within
  | Apart
  | Ported
  | Routed
  | Lettered
  | Outlined
  | FlowForward
  | LayerAlignment
  | Orthogonality
  | Conservation

derive instance Eq Law
derive instance Ord Law

instance Show Law where
  show = case _ of
    Placed -> "Placed"
    Finite -> "Finite"
    Within -> "Within"
    Apart -> "Apart"
    Ported -> "Ported"
    Routed -> "Routed"
    Lettered -> "Lettered"
    Outlined -> "Outlined"
    FlowForward -> "FlowForward"
    LayerAlignment -> "LayerAlignment"
    Orthogonality -> "Orthogonality"
    Conservation -> "Conservation"

-- | What is wrong, and which law it is against.
type Fault = { law :: Law, words :: String }

-- | The laws every layout is held to.
generalLaws :: Array Law
generalLaws = [ Placed, Finite, Within, Apart, Ported, Routed, Lettered, Outlined ]

isGeneral :: Law -> Boolean
isGeneral law = Array.elem law generalLaws

-- | The law a promise is a promise of.
lawOf :: forall wire. Promise wire -> Law
lawOf = case _ of
  FlowsForward -> FlowForward
  LayersAligned -> LayerAlignment
  RoutesOrthogonal -> Orthogonality
  WidthsConserved _ -> Conservation

-- | No path of edges returns to where it began (an edge from a node to
-- | itself is such a path).
acyclic :: forall wire. Projection wire -> Boolean
acyclic p = isDAG (mkSimpleGraph (map _.key p.nodes) (p.edges <#> \e -> Tuple e.from e.to))

-- | Widths that differ from edge to edge and mean nothing, for holding the
-- | Sankey engine to its laws on a flow whose wires carry no quantity: the
-- | number of wires an edge stands for, and a half more where its source's
-- | key is below its target's.
varied :: forall wire. Widths wire
varied = Sankey \edge -> toNumber (NonEmptyArray.length edge.wires) + (if edge.from < edge.to then 0.5 else 0.0)

-- | Two boxes with more than a touch in common.
overlapping :: Box -> Box -> Boolean
overlapping a b =
  a.x < b.x + b.width - tolerance && b.x < a.x + a.width - tolerance
    && a.y < b.y + b.height - tolerance
    && b.y < a.y + a.height - tolerance

-- | One box inside another, sides allowed to touch.
insideOf :: Box -> Box -> Boolean
insideOf inner outer =
  inner.x >= outer.x - tolerance && inner.y >= outer.y - tolerance
    && inner.x + inner.width <= outer.x + outer.width + tolerance
    && inner.y + inner.height <= outer.y + outer.height + tolerance

tolerance :: Number
tolerance = 1.0e-6

-- | How near a route's end must be to the box, or the port, it is said to
-- | be on: a hundredth of a pixel. (Every engine held to this so far meets
-- | to within a millionth; the room is for an engine whose ports and
-- | routes are worked out on different grids.)
meeting :: Number
meeting = 0.01

-- | The nodes that are in an outline's box, or partly in it, and are no
-- | member of its group: each with the group. The law is that there are
-- | none; this is the list, for a caller that wants the number.
strangers :: forall wire. Layout wire -> Array { group :: GroupId, node :: PlacedNode }
strangers laid = do
  outline <- laid.outlines
  let members = Set.fromFoldable outline.members
  node <- laid.nodes
  if not (Set.member node.index members) && overlapping node.box outline.box then [ { group: outline.group, node } ] else []

-- | The general laws and exactly the laws the engine promises, of a layout
-- | said to be the engine's of the projection under the picture.
check :: forall wire. Engine wire -> Picture -> Projection wire -> Layout wire -> Array Fault
check engine given projection laid =
  generalFaults given projection laid <> Array.concatMap (\promise -> promiseFaults promise given projection laid) (promises engine)

-- | The laws some fault is against, each once, in `Law`'s order.
broken :: Array Fault -> Array Law
broken = Array.nub <<< Array.sort <<< map _.law

-- | `check`, in words alone.
faultsOf :: forall wire. Engine wire -> Picture -> Projection wire -> Layout wire -> Array String
faultsOf engine given projection laid = map _.words (check engine given projection laid)

-- | The faults of the layout an engine makes of a projection.
layoutFaultsOf :: forall wire. Engine wire -> Picture -> Projection wire -> Array String
layoutFaultsOf engine given projection = faultsOf engine given projection (run engine given projection)

-- | Every fault in a layout of a projection by the Sankey engine, with no
-- | words set and no group outlined (as in 0.2).
faults :: forall wire. Metrics -> Widths wire -> Projection wire -> Layout wire -> Array String
faults metrics widths = faultsOf (sankey metrics widths) plainPicture

-- | The faults of the layout the Sankey engine makes of a projection (as in
-- | 0.2).
layoutFaults :: forall wire. Metrics -> Widths wire -> Projection wire -> Array String
layoutFaults metrics widths = layoutFaultsOf (sankey metrics widths) plainPicture

describe :: PlacedNode -> String
describe n = n.node.label <> " " <> show n.node.key

describeEdge :: forall wire. PlacedEdge wire -> String
describeEdge e = "edge " <> show e.index <> " " <> show e.edge.from <> " -> " <> show e.edge.to

near :: Number -> Number -> Boolean
near a b = abs (a - b) <= tolerance * max 1.0 (max (abs a) (abs b))

-- | Every fault against a general law; none is the law.
generalFaults :: forall wire. Picture -> Projection wire -> Layout wire -> Array Fault
generalFaults given projection laid =
  places <> edgePlaces <> finite <> inside <> overlaps <> portFaults <> routeFaults <> wordFaults <> outlineFaults
  where
  upTo count = if count <= 0 then [] else Array.range 0 (count - 1)
  against law = map \words -> { law, words }

  places = against Placed $
    if map _.index laid.nodes == upTo (Array.length projection.nodes) && all identity (Array.zipWith (\n p -> n.node.key == p.key) laid.nodes projection.nodes) then []
    else [ "the placed nodes are not the projection's nodes, each once, in order" ]

  edgePlaces = against Placed $
    if map _.index laid.edges == upTo (Array.length projection.edges) then []
    else [ "the placed edges are not the projection's edges, each once, in order" ]

  boxNumbers box = [ box.x, box.y, box.width, box.height ]
  everyNumber =
    [ laid.width, laid.height ]
      <> Array.concatMap (\n -> boxNumbers n.box <> map _.along n.ports) laid.nodes
      <> Array.concatMap (\e -> Route.numbers e.route) laid.edges
      <> Array.concatMap (\outline -> boxNumbers outline.box <> [ outline.header ]) laid.outlines
  finite = against Finite $
    (if all isFinite everyNumber then [] else [ "a number is not finite" ])
      <> (if all (\n -> n.box.width >= 0.0 && n.box.height >= 0.0) laid.nodes then [] else [ "a box has a negative side" ])

  wholePicture = { x: 0.0, y: 0.0, width: laid.width, height: laid.height }

  inside = against Within $
    (laid.nodes # Array.mapMaybe \n -> if insideOf n.box wholePicture then Nothing else Just (describe n <> " is outside the picture"))
      <> (laid.outlines # Array.mapMaybe \outline -> if insideOf outline.box wholePicture then Nothing else Just (called outline <> " is outside the picture"))

  overlaps = against Apart do
    a <- laid.nodes
    b <- laid.nodes
    if a.index < b.index && overlapping a.box b.box then [ describe a <> " overlaps " <> describe b ] else []

  -- ---- ports ------------------------------------------------------------------
  sideLength box side = case side of
    LeftSide -> box.height
    RightSide -> box.height
    TopSide -> box.width
    BottomSide -> box.width

  portFaults = against Ported $ laid.nodes # Array.concatMap \n ->
    let
      own port = case port.flow of
        Input -> Array.elem port.name n.node.inputs
        Output -> Array.elem port.name n.node.outputs
      named = n.ports <#> \port -> Tuple port.flow port.name
    in
      (n.ports # Array.concatMap \port ->
        (if own port then [] else [ describe n <> " has a place for " <> show port.name <> ", " <> show port.flow <> ", which is no port of its" ])
          <> (if port.along >= -tolerance && port.along <= sideLength n.box port.side + tolerance then [] else [ describe n <> " has " <> show port.name <> " placed beyond the end of its side" ]))
        <> (if Array.length (Array.nub named) == Array.length named then [] else [ describe n <> " has a port placed twice" ])

  -- ---- routes -----------------------------------------------------------------
  nodeOf key = Array.find (\n -> n.node.key == key) laid.nodes

  onBoundary :: Box -> Point -> Boolean
  onBoundary box point =
    point.x >= box.x - meeting && point.x <= box.x + box.width + meeting && point.y >= box.y - meeting && point.y <= box.y + box.height + meeting
      && not (point.x > box.x + meeting && point.x < box.x + box.width - meeting && point.y > box.y + meeting && point.y < box.y + box.height - meeting)

  at :: Point -> Point -> Boolean
  at a b = abs (a.x - b.x) <= meeting && abs (a.y - b.y) <= meeting

  endFaults e which key named flow point = case nodeOf key of
    Nothing -> [ describeEdge e <> " " <> which <> " a node that has no place" ]
    Just n ->
      (if onBoundary n.box point then [] else [ describeEdge e <> " " <> which <> " " <> describe n <> " and its route does not meet that node's box" ])
        <> case named >>= \name -> Array.find (\port -> port.flow == flow && port.name == name) n.ports of
          Just port | not (at (portPoint n.box port) point) -> [ describeEdge e <> " " <> which <> " the port " <> show port.name <> " of " <> describe n <> " and its route does not meet the port" ]
          _ -> []

  onLeg :: Point -> Point -> Point -> Boolean
  onLeg point a b =
    let
      long = Route.distance a b
    in
      if long < tolerance then at point a
      else
        let
          reach = ((point.x - a.x) * (b.x - a.x) + (point.y - a.y) * (b.y - a.y)) / long
          off = abs (Route.cross a b point) / long
        in
          off <= meeting && reach >= -meeting && reach <= long + meeting

  jumpFaults e = case e.route of
    Polyline line ->
      let
        points = Route.flatten e.route
        legs = Array.zip points (Array.drop 1 points)
      in
        if all (\jump -> any (\(Tuple a b) -> onLeg jump a b) legs) line.jumps then [] else [ describeEdge e <> " has a jump that is not on its route" ]
    _ -> []

  routeFaults = against Routed $ laid.edges # Array.concatMap \e ->
    endFaults e "is from" e.edge.from e.edge.fromPort Output (Route.start e.route)
      <> endFaults e "is to" e.edge.to e.edge.toPort Input (Route.end e.route)
      <> jumpFaults e

  -- ---- words ------------------------------------------------------------------
  tooWide room size factor line = String.length line > 1 && factor * estimateWidth size line > room + tolerance
  wordFaults = against Lettered case given.words of
    Nothing -> Array.mapMaybe (\n -> n.words $> (describe n <> " has words set that the picture did not give")) laid.nodes
    Just _ -> laid.nodes # Array.concatMap \n -> case n.words of
      Nothing -> [ describe n <> " has no words set" ]
      Just set ->
        let
          room = available given.lettering n.box.width
        in
          (if Array.length set.title >= 1 && Array.length set.title <= max 1 given.lettering.titleLines && Array.length set.detail <= max 1 given.lettering.detailLines then [] else [ describe n <> " has more lines than are allowed, or no title" ])
            <> (if Array.any (tooWide room given.lettering.titleSize (given.lettering.caution * heavier)) set.title || Array.any (tooWide room given.lettering.detailSize given.lettering.caution) set.detail then [ describe n <> " has a line wider than its room" ] else [])
            <> (if n.box.height >= heightOf given.lettering set - tolerance then [] else [ describe n <> " is " <> show n.box.height <> " tall and its words need " <> show (heightOf given.lettering set) ])

  -- ---- open groups --------------------------------------------------------------
  called :: PlacedOutline -> String
  called outline = "the outline of " <> show outline.group

  under :: PlacedOutline -> Box
  under outline = outline.box { y = outline.box.y + outline.header, height = outline.box.height - outline.header }

  outlineOf group = Array.find (\outline -> outline.group == group) laid.outlines

  outlineFaults = against Outlined case given.groups of
    Nothing -> if Array.null laid.outlines then [] else [ "there are outlines, and the picture was told of no group" ]
    Just groups ->
      let
        home = openPaths groups
        pathsOf = laid.nodes <#> \n -> { index: n.index, path: home.pathOf n.node.key }
        wanted = Array.nub (Array.concatMap _.path pathsOf)
        membersOf group = Array.sort (map _.index (Array.filter (\n -> Array.elem group n.path) pathsOf))
        parentOf group = Array.head (Array.mapMaybe (\n -> Array.elemIndex group n.path >>= \place -> if place == 0 then Just Nothing else Just (Array.index n.path (place - 1))) pathsOf)
        boxOf index = _.box <$> Array.find (\n -> n.index == index) laid.nodes
        ancestors outline = case outline.parent >>= outlineOf of
          Just parent -> [ parent.group ] <> ancestors parent
          Nothing -> []
      in
        (if Array.sort (map _.group laid.outlines) == Array.sort wanted then [] else [ "the outlines are not the open groups that hold a node, each once" ])
          <> Array.concatMap
            ( \outline ->
                (if outline.members == membersOf outline.group then [] else [ called outline <> " has not the members the grouping gives it" ])
                  <> (if parentOf outline.group == Just outline.parent then [] else [ called outline <> " has not for its parent the nearest open group over it" ])
                  <> Array.mapMaybe (\index -> if map (\box -> insideOf box (under outline)) (boxOf index) == Just true then Nothing else Just (called outline <> " does not hold its member, node " <> show index <> ", under its strip")) outline.members
                  <> case outline.parent of
                    Nothing -> []
                    Just parent -> case outlineOf parent of
                      Just outer -> if insideOf outline.box (under outer) then [] else [ called outline <> " is not inside its parent's, " <> show parent ]
                      Nothing -> [ called outline <> " has a parent with no outline, " <> show parent ]
            )
            laid.outlines
          <> do
            a <- laid.outlines
            b <- laid.outlines
            if a.group < b.group && overlapping a.box b.box && not (Array.elem a.group (ancestors b)) && not (Array.elem b.group (ancestors a)) then [ called a <> " overlaps " <> called b <> ", and neither holds the other" ] else []
          <> (strangers laid <#> \found -> describe found.node <> " is in " <> show found.group <> "'s outline and is no member of it")

-- | Every fault against the law a promise is of; none is the law.
promiseFaults :: forall wire. Promise wire -> Picture -> Projection wire -> Layout wire -> Array Fault
promiseFaults promise given projection laid = map (\words -> { law: lawOf promise, words }) case promise of
  FlowsForward -> forward
  LayersAligned -> aligned
  RoutesOrthogonal -> orthogonal
  WidthsConserved conservation -> conserved conservation
  where
  -- how far along the flow, and across it, a point is
  along :: Point -> Number
  along point = case laid.direction of
    LeftToRight -> point.x
    TopDown -> point.y

  across :: Point -> Number
  across point = case laid.direction of
    LeftToRight -> point.y
    TopDown -> point.x

  middle :: Box -> Point
  middle box = { x: box.x + box.width / 2.0, y: box.y + box.height / 2.0 }

  layerOf key = Array.find (\n -> n.node.key == key) laid.nodes >>= _.layer

  -- ---- with the flow ------------------------------------------------------------
  goesForward e = along (Route.end e.route) > along (Route.start e.route) && case layerOf e.edge.from, layerOf e.edge.to of
    Just from, Just to -> to > from
    _, _ -> true

  forward =
    Array.mapMaybe (\e -> if e.course == Forward && not (goesForward e) then Just (describeEdge e <> " is drawn forward and does not go forward") else Nothing) laid.edges
      <>
        if acyclic projection then Array.mapMaybe (\e -> if e.course == Backward then Just (describeEdge e <> " turns back in a projection with no cycle") else Nothing) laid.edges
        else []

  -- ---- layers in line -------------------------------------------------------------
  layers = Array.sortWith _.layer $ Array.nub (Array.mapMaybe _.layer laid.nodes) <#> \layer ->
    { layer, lines: Array.mapMaybe (\n -> if n.layer == Just layer then Just (along (middle n.box)) else Nothing) laid.nodes }

  aligned =
    (layers # Array.mapMaybe \found -> case Array.head found.lines of
      Just first | not (all (near first) found.lines) -> Just ("the nodes of layer " <> show found.layer <> " are not in one line across the flow")
      _ -> Nothing)
      <> Array.catMaybes
        ( Array.zipWith
            ( \earlier later -> case Array.head earlier.lines, Array.head later.lines of
                Just a, Just b | b <= a -> Just ("layer " <> show later.layer <> " is not further along the flow than layer " <> show earlier.layer)
                _, _ -> Nothing
            )
            layers
            (Array.drop 1 layers)
        )

  -- ---- right angles -----------------------------------------------------------------
  orthogonal = laid.edges # Array.mapMaybe \e -> case e.route of
    Polyline _ ->
      let
        points = Route.flatten e.route
        square a b = abs (a.x - b.x) <= tolerance || abs (a.y - b.y) <= tolerance
      in
        if all identity (Array.zipWith square points (Array.drop 1 points)) then Nothing else Just (describeEdge e <> " has a leg that runs along neither axis")
    _ -> Just (describeEdge e <> " is not a polyline")

  -- ---- widths ---------------------------------------------------------------------------
  conserved :: Conservation wire -> Array String
  conserved conservation =
    notRibbons <>
      if not allPositive then []
      else
        Array.mapMaybe (\e -> if near e.breadth (conservation.pixelsPerUnit * conservation.unitsOf e.placed.edge) then Nothing else Just (describeEdge e.placed <> " is " <> show e.breadth <> " wide, its width says " <> show (conservation.pixelsPerUnit * conservation.unitsOf e.placed.edge))) ribbons
          <> Array.concatMap
            ( \n ->
                let
                  entering = side (\e -> e.placed.edge.to == n.node.key) (across <<< Route.end <<< _.placed.route)
                  leaving = side (\e -> e.placed.edge.from == n.node.key) (across <<< Route.start <<< _.placed.route)
                  wider = max (sum (map _.width entering)) (sum (map _.width leaving))
                in
                  if Array.null entering && Array.null leaving then []
                  else
                    sideFaults n "into" entering <> sideFaults n "out of" leaving
                      <> (if near (extentAcross n.box) (max wider (needOf n)) then [] else [ describe n <> " is " <> show (extentAcross n.box) <> " tall, its wider side is " <> show wider <> " and its words need " <> show (needOf n) ])
            )
            laid.nodes
    where
    ribbons = laid.edges # Array.mapMaybe \placed -> case placed.route of
      Ribbon ribbon -> Just { placed, breadth: ribbon.breadth }
      _ -> Nothing
    notRibbons = laid.edges # Array.mapMaybe \placed -> case placed.route of
      Ribbon _ -> Nothing
      _ -> Just (describeEdge placed <> " is not a ribbon")
    allPositive = all (\e -> conservation.unitsOf e > 0.0) projection.edges

    -- the ribbons standing on one side of a node, in order across the flow
    side touches centre = Array.sortBy (comparing _.centre) (Array.filter touches ribbons <#> \e -> { centre: centre e, width: e.breadth })

    startAcross box = across { x: box.x, y: box.y }
    extentAcross box = case laid.direction of
      LeftToRight -> box.height
      TopDown -> box.width

    sideFaults n name stand =
      let
        top s = s.centre - s.width / 2.0
        bottom s = s.centre + s.width / 2.0
        within = all (\s -> top s >= startAcross n.box - 1.0e-4 && bottom s <= startAcross n.box + extentAcross n.box + 1.0e-4) stand
        apart = all identity (Array.zipWith (\a b -> top b >= bottom a - 1.0e-4) stand (Array.drop 1 stand))
      in
        (if within then [] else [ "an edge " <> name <> " " <> describe n <> " stands outside it" ])
          <> (if apart then [] else [ "two edges " <> name <> " " <> describe n <> " stand on one another" ])

    -- words need room down the page, which is across the flow only where
    -- the flow runs left to right
    needOf n = case laid.direction, n.words of
      LeftToRight, Just set -> heightOf given.lettering set
      _, _ -> 0.0
