-- | The columns engine (0.4): nodes as big as the caller says, in columns
-- | by layer, with ribbons from port to port. Pure, so it runs in node.
-- |
-- | WHY A SECOND ENGINE OF ITS OWN, AND NOT THE SANKEY ENGINE TAUGHT PORTS.
-- | The Sankey layout's whole method is that a node is as tall as what
-- | passes through it, and that a node's edges stack along its side by
-- | where they go. A table drawn with its rows is the opposite case on both
-- | counts: it is as tall as its rows, which only the caller knows, and an
-- | edge belongs beside the row it governs, which only the caller knows. A
-- | table does not conserve width either (240 rows in, one total out). So
-- | the Sankey engine keeps its promises and this engine makes others: it
-- | keeps the sizes and ports the picture fixes (`Picture.fixed`), and
-- | draws every edge as a ribbon at one stated scale, with a least breadth
-- | so that a single unit still shows.
-- |
-- | WHAT IT DOES.
-- |
-- | - **Layers.** The core's `layers` (longest path from a node nothing
-- |   enters), and then a node nothing enters is moved as late as it can go:
-- |   to the layer before the first of the nodes it feeds. A parameter then
-- |   stands beside the table it feeds, not in the first column with every
-- |   other source.
-- | - **Columns.** One column for each layer, as wide as its widest node, the
-- |   columns `layerGap` apart; a node is centred in its column, so the
-- |   nodes of one layer have their middles in one line (`LayersAligned`).
-- | - **Down the page.** Each node is pulled towards the places of the
-- |   nodes at the other ends of its edges, each edge weighing as much as it
-- |   is broad, and measured from where the edge meets the node (its port
-- |   where the port is placed, else the middle of the side): a sweep with
-- |   the flow (by what enters a node) and one against it (by what leaves),
-- |   `passes` times. After each pull a column is put in the order its nodes
-- |   want, pushed apart to `nodeGap`, and moved as a whole by the mean of
-- |   what the push cost, so that it does not creep down the page.
-- | - **Routes.** A ribbon from where an edge leaves its source to where it
-- |   enters its target: at a placed port where the edge names one the node
-- |   has placed, and otherwise stacked on the node's right side (leaving)
-- |   or left (entering), centred on the side's middle, in the order of the
-- |   other ends down the page. One cubic piece, level at both ends. An
-- |   edge into a layer no later than its source's is turned back
-- |   (`Backward`) and loops out and back by a layer's gap.
-- |
-- | WHAT IT PROMISES, AND WHAT IT DOES NOT. `FlowsForward`, `LayersAligned`,
-- | `KeepsFixed` and `WidthsScaled` at the scaling it is made with. Not
-- | `WidthsConserved`: a node is not as tall as what passes through it.
-- | Not `RoutesOrthogonal`: its routes are curves. It draws no outlines:
-- | told of open groups it outlines none, and the general law `Outlined`
-- | says so. Ribbons that meet one port, or that are stacked at a side,
-- | stand beside one another; ribbons that meet different ports stand
-- | where the caller put the ports, and whether those overlap is the
-- | caller's to say.
module Hylograph.Flow.Render.Columns
  ( Spacing
  , defaultSpacing
  , columns
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, maximum, minimum, sum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Edge, ElementKey, Projection, layers)
import Hylograph.Flow.Render.Engine (Box, Course(..), Direction(..), Engine(..), Layout, Picture, PlacedEdge, PlacedNode, PlacedPort, PortFlow(..), Promise(..), Scaling, portPoint, scaledBreadth)
import Hylograph.Flow.Render.Route (Ink(..), Piece(..), Point, Route(..))
import Hylograph.Flow.Render.Text (heightOf, setWords)

-- | `layerGap`: between one column and the next. `nodeGap`: between two
-- | nodes of a column. `margin`: round the picture. `nodeWidth`,
-- | `nodeHeight`: the size of a node the picture does not fix (taller
-- | where its words need it). `passes`: how many times the nodes are pulled
-- | down the page, each time with the flow and against it.
type Spacing =
  { layerGap :: Number
  , nodeGap :: Number
  , margin :: Number
  , nodeWidth :: Number
  , nodeHeight :: Number
  , passes :: Int
  }

defaultSpacing :: Spacing
defaultSpacing = { layerGap: 90.0, nodeGap: 36.0, margin: 24.0, nodeWidth: 112.0, nodeHeight: 36.0, passes: 12 }

-- | The columns engine at a spacing and a scaling of widths.
columns :: forall wire. Spacing -> Scaling wire -> Engine wire
columns spacing scaling = Engine
  { name: "columns"
  , promises: [ FlowsForward, LayersAligned, KeepsFixed, WidthsScaled scaling ]
  , layout: laidOut spacing scaling
  }

-- | An edge, with the indices of its two nodes and how broad it is drawn.
type End wire = { index :: Int, edge :: Edge wire, from :: Int, to :: Int, breadth :: Number }

-- | A node before it has a place down the page.
type Sized =
  { index :: Int
  , key :: ElementKey
  , width :: Number
  , height :: Number
  , ports :: Array PlacedPort
  , layer :: Int
  }

laidOut :: forall wire. Spacing -> Scaling wire -> Picture -> Projection wire -> Layout wire
laidOut spacing scaling given projection =
  { direction: LeftToRight
  , nodes: placedNodes
  , edges: placedEdges
  , outlines: []
  , width: (fromMaybe 0.0 (maximum (placedNodes <#> \n -> n.box.x + n.box.width))) + spacing.margin
  , height: (fromMaybe 0.0 (maximum (placedNodes <#> \n -> n.box.y + n.box.height))) + spacing.margin
  }
  where
  -- ---- sizes, words, ports ---------------------------------------------------------
  wordsOf node width = given.words <#> \say -> setWords given.lettering width (say node)

  longest = layers projection
  layerAtFirst key = fromMaybe 0 (Map.lookup key longest)

  -- a node nothing enters goes to the layer before the first it feeds
  entered key = Array.any (\e -> e.to == key && e.from /= key) projection.edges
  layerOf key =
    if entered key then layerAtFirst key
    else case minimum (Array.mapMaybe (\e -> if e.from == key && e.to /= key then Just (layerAtFirst e.to) else Nothing) projection.edges) of
      Just first | first - 1 > layerAtFirst key -> first - 1
      _ -> layerAtFirst key

  sized :: Array Sized
  sized = Array.mapWithIndex
    ( \index node -> case given.fixed node of
        Just fixed -> { index, key: node.key, width: fixed.width, height: fixed.height, ports: fixed.ports, layer: layerOf node.key }
        Nothing ->
          { index
          , key: node.key
          , width: spacing.nodeWidth
          , height: max spacing.nodeHeight (fromMaybe 0.0 (heightOf given.lettering <$> wordsOf node spacing.nodeWidth))
          , ports: []
          , layer: layerOf node.key
          }
    )
    projection.nodes

  indexOfKey :: Map ElementKey Int
  indexOfKey = Map.fromFoldable (Array.reverse sized <#> \n -> Tuple n.key n.index)

  sizedAt :: Int -> Maybe Sized
  sizedAt = Array.index sized

  -- ---- columns ------------------------------------------------------------------------
  lastLayer = fromMaybe 0 (maximum (map _.layer sized))
  layerIndices = Array.range 0 lastLayer
  columnWidth layer = fromMaybe 0.0 (maximum (map _.width (Array.filter (\n -> n.layer == layer) sized)))
  columnLeft :: Map Int Number
  columnLeft = (foldl (\state layer -> { at: state.at + columnWidth layer + spacing.layerGap, lefts: Map.insert layer state.at state.lefts }) { at: spacing.margin, lefts: Map.empty } layerIndices).lefts
  xOf n = fromMaybe spacing.margin (Map.lookup n.layer columnLeft) + (columnWidth n.layer - n.width) / 2.0

  -- ---- the edges, by index of their ends -------------------------------------------------
  breadthOf :: Edge wire -> Number
  breadthOf edge = scaledBreadth scaling (scaling.unitsOf edge)

  ends :: Array (End wire)
  ends = Array.mapWithIndex
    ( \index edge ->
        { index
        , edge
        , from: fromMaybe (-1) (Map.lookup edge.from indexOfKey)
        , to: fromMaybe (-1) (Map.lookup edge.to indexOfKey)
        , breadth: breadthOf edge
        }
    )
    projection.edges

  -- how far down its node an edge meets it: at the port placed for it, or
  -- the middle of the side
  portNamed n flow named = named >>= \name -> Array.find (\port -> port.flow == flow && port.name == name) n.ports
  downAt n flow named = case portNamed n flow named of
    Just port -> (portPoint { x: 0.0, y: 0.0, width: n.width, height: n.height } port).y
    Nothing -> n.height / 2.0

  forwardEnds = Array.filter (\e -> e.from /= e.to && layerFrom e < layerTo e) ends
  layerFrom e = maybe (-1) _.layer (sizedAt e.from)
  layerTo e = maybe (-1) _.layer (sizedAt e.to)

  -- ---- down the page -------------------------------------------------------------------------
  firstTops :: Map Int Number
  firstTops = foldl
    ( \tops layer ->
        (foldl (\state n -> { at: state.at + n.height + spacing.nodeGap, tops: Map.insert n.index state.at state.tops }) { at: spacing.margin, tops } (Array.filter (\n -> n.layer == layer) sized)).tops
    )
    Map.empty
    layerIndices

  topOf tops index = fromMaybe 0.0 (Map.lookup index tops)

  -- one column pulled towards the other ends of the edges given, pushed
  -- apart, and moved back by the mean of the push
  pull :: (Sized -> Array { other :: Number, own :: Number, weight :: Number }) -> Map Int Number -> Int -> Map Int Number
  pull pulls tops layer =
    let
      members = Array.filter (\n -> n.layer == layer) sized
      wanted = members <#> \n ->
        let
          found = pulls n
          weight = sum (map _.weight found)
        in
          { node: n
          , desired: if weight > 0.0 then sum (found <#> \p -> p.weight * (p.other - p.own)) / weight else topOf tops n.index
          , current: topOf tops n.index
          }
      ordered = Array.sortBy (comparing _.desired <> comparing _.current) wanted
      pushed = (foldl (\state w -> let top = max w.desired state.floor in { floor: top + w.node.height + spacing.nodeGap, placed: Array.snoc state.placed { index: w.node.index, top, desired: w.desired } }) { floor: -1.0e12, placed: [] } ordered).placed
      shift = if Array.null pushed then 0.0 else sum (pushed <#> \p -> p.desired - p.top) / toNumber (Array.length pushed)
    in
      foldl (\acc p -> Map.insert p.index (p.top + shift) acc) tops pushed

  entering tops n = forwardEnds # Array.mapMaybe \e ->
    if e.to /= n.index then Nothing
    else sizedAt e.from <#> \source -> { other: topOf tops source.index + downAt source Output e.edge.fromPort, own: downAt n Input e.edge.toPort, weight: e.breadth }

  leaving tops n = forwardEnds # Array.mapMaybe \e ->
    if e.from /= n.index then Nothing
    else sizedAt e.to <#> \target -> { other: topOf tops target.index + downAt target Input e.edge.toPort, own: downAt n Output e.edge.fromPort, weight: e.breadth }

  sweep tops _ =
    let
      withFlow = foldl (\acc layer -> pull (entering acc) acc layer) tops (Array.drop 1 layerIndices)
    in
      foldl (\acc layer -> pull (leaving acc) acc layer) withFlow (Array.reverse (Array.dropEnd 1 layerIndices))

  settled = foldl sweep firstTops (Array.range 1 spacing.passes)
  highest = fromMaybe 0.0 (minimum (Array.fromFoldable (Map.values settled)))
  finalTop index = topOf settled index - highest + spacing.margin

  boxOfSized :: Sized -> Box
  boxOfSized n = { x: xOf n, y: finalTop n.index, width: n.width, height: n.height }

  placedNodes :: Array PlacedNode
  placedNodes = Array.zipWith
    ( \n node ->
        { index: n.index
        , node
        , box: boxOfSized n
        , layer: Just n.layer
        , words: wordsOf node n.width
        , ports: n.ports
        }
    )
    sized
    projection.nodes

  boxAt index = maybe { x: 0.0, y: 0.0, width: 0.0, height: 0.0 } boxOfSized (sizedAt index)
  middleOf index = let box = boxAt index in box.y + box.height / 2.0

  -- ---- where each edge leaves and enters -----------------------------------------------------
  -- the edges at a side that name no placed port, stacked about its middle
  -- in the order of their other ends down the page
  stacked :: (End wire -> Boolean) -> (End wire -> Int) -> (End wire -> Int) -> Map Int Number
  stacked unported sideOwner otherEnd =
    let
      atSide = Array.filter unported ends
      bySide = Map.fromFoldableWith (<>) (atSide <#> \e -> Tuple (sideOwner e) [ e ])
    in
      foldl
        ( \acc (Tuple owner found) ->
            let
              ordered = Array.sortBy (comparing (\e -> middleOf (otherEnd e)) <> comparing _.index) found
              total = sum (map _.breadth ordered)
              top = middleOf owner - total / 2.0
            in
              (foldl (\state e -> { at: state.at + e.breadth, centres: Map.insert e.index (state.at + e.breadth / 2.0) state.centres }) { at: top, centres: acc } ordered).centres
        )
        Map.empty
        (Map.toUnfoldable bySide :: Array (Tuple Int (Array (End wire))))

  leavingUnported e = case sizedAt e.from of
    Just n -> case portNamed n Output e.edge.fromPort of
      Just _ -> false
      Nothing -> true
    Nothing -> false
  enteringUnported e = case sizedAt e.to of
    Just n -> case portNamed n Input e.edge.toPort of
      Just _ -> false
      Nothing -> true
    Nothing -> false

  leavingCentres = stacked leavingUnported _.from _.to
  enteringCentres = stacked enteringUnported _.to _.from

  startOf e = case sizedAt e.from of
    Just n -> case portNamed n Output e.edge.fromPort of
      Just port -> portPoint (boxAt e.from) port
      Nothing -> let box = boxAt e.from in { x: box.x + box.width, y: fromMaybe (middleOf e.from) (Map.lookup e.index leavingCentres) }
    Nothing -> { x: 0.0, y: 0.0 }

  endOf e = case sizedAt e.to of
    Just n -> case portNamed n Input e.edge.toPort of
      Just port -> portPoint (boxAt e.to) port
      Nothing -> let box = boxAt e.to in { x: box.x, y: fromMaybe (middleOf e.to) (Map.lookup e.index enteringCentres) }
    Nothing -> { x: 0.0, y: 0.0 }

  placedEdges :: Array (PlacedEdge wire)
  placedEdges = ends # Array.filter (\e -> e.from >= 0 && e.to >= 0) <#> \e ->
    let
      forward = e.from /= e.to && layerFrom e < layerTo e
      start = startOf e
      end = endOf e
    in
      { index: e.index
      , edge: e.edge
      , route: ribbon forward start end e.breadth
      , course: if forward then Forward else Backward
      }

  ribbon :: Boolean -> Point -> Point -> Number -> Route
  ribbon forward start end breadth =
    let
      middle = (start.x + end.x) / 2.0
      reach = spacing.layerGap
      curve =
        if forward then { leaving: { x: middle, y: start.y }, arriving: { x: middle, y: end.y }, to: end }
        else { leaving: { x: start.x + reach, y: start.y }, arriving: { x: end.x - reach, y: end.y }, to: end }
    in
      Ribbon { from: start, pieces: NonEmptyArray.singleton (Curved curve), breadth, ink: Band }

