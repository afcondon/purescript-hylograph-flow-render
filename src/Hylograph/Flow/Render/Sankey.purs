-- | The Sankey engine: the layout this package has had since 0.1, as one
-- | engine among those the renderer draws (`Hylograph.Flow.Render.Engine`).
-- | Pure, so it runs in node.
-- |
-- | `Metrics` and `Widths` are this engine's own configuration: `sankey
-- | metrics widths` is the engine, and a `Picture` (words, open groups) is
-- | what it is told as any engine is. It promises `FlowsForward`,
-- | `LayersAligned` and `WidthsConserved` at its own scale; every route it
-- | makes is a `Ribbon`, inked as a `Band` by quantity and as a `Hairline`
-- | in a plain picture. It places no ports: it stacks the edges on a node's
-- | side by where they go, so the edges of one port need not stand
-- | together, and a port would have no one place.
-- |
-- | One layout serves both pictures. Every edge has a width, in units; a
-- | node is as tall as the wider of what enters it and what leaves it, and
-- | its edges stack along its sides. A Sankey takes the widths from the
-- | wires' payloads and draws each edge as wide as it is. A plain DAG gives
-- | every edge one unit and draws each as a hairline down the middle of its
-- | place. The positions come from the same code either way: the Sankey
-- | layout in `hylograph-layout`, with its lanes for edges that span more
-- | than one layer. The flow runs left to right (`sankey`), or the way the
-- | engine is told (`sankeyToward`, 0.5).
-- |
-- | That layout does the placing (`DataViz.Layout.Sankey.Whole`): the scale
-- | is fixed (`unit` pixels to one unit of width, the layers a fixed
-- | distance apart) and the picture is as large as that makes it; a node no
-- | edge touches stands on a shelf under the picture; and an edge that
-- | closes a cycle, an edge from a node to itself among them, leaves its
-- | source below the edges that go forward, runs back under the picture in a
-- | channel of its own, and enters its target likewise. The engine is the
-- | translation and nothing more: from a projection to rows and names, and
-- | from the layout's boxes and routes to placed nodes and centre lines, set
-- | in from the left by a margin with room for an edge to turn.
-- |
-- | ## Words, and open groups
-- |
-- | Two things the Sankey layout cannot be asked for, and what is done here
-- | about each.
-- |
-- | **A node tall enough for its words.** Told what is written in a node
-- | (`words`), the layout wraps it to the node's breadth by an estimate of
-- | each line's width (`Hylograph.Flow.Render.Text`, which says how the
-- | estimate is made and how wrong it can be) and makes the node at least
-- | tall enough for the lines; its edges stand in the middle of its sides.
-- | The breadth is `metrics.nodeBreadth`, one for the picture, so a caller
-- | with long labels may choose narrower and taller nodes.
-- |
-- | **An open group, outlined.** Told the grouping and which groups are
-- | closed (what `collapse` was told), the layout gives every open group
-- | that holds a drawn node an outline: a box round its members, with a
-- | strip for its name above them, the outline of a group inside it nested
-- | in it. Nothing that is not a member is inside an outline, and the
-- | outlines of two groups overlap only where one holds the other.
-- |
-- | How, in three steps. (1) The Sankey layout runs once, to learn where it
-- | would put everything. From that an order is made: at each level of the
-- | grouping, the groups and the loose nodes by the mean height of what is
-- | in them. (2) It runs again held to that order within every column
-- | (`nodeSort`), so in each column a group's members stand together, the
-- | members of a group inside it together among them, and the groups in the
-- | same order in every column. A long edge's waypoints go with the
-- | innermost group that holds both its ends. (3) `Internal.Bands` then
-- | moves boxes down, never up and never past one another, until every
-- | outline has its padding and its strip and no outline has anything else
-- | in any column it spans; the same pass makes the room a taller node
-- | needs. A member of an open group that no edge touches would be on the
-- | shelf, far from its group: it stands in its group's first column
-- | instead.
-- |
-- | What `hylograph-layout` 0.6.0 lacks, exactly, which is why step 3 is
-- | here and not there: a node's height is its value at the scale and
-- | nothing else can be asked (no least height by node); the gap between
-- | two nodes of a column is one number, `nodePadding` (no gap by pair, so
-- | no room for a frame); and a node is in one column (no node that spans
-- | columns, and no notion of a set of nodes to be kept clear of others
-- | across columns). `nodeSort` is the one thing it offers, and it is used.
-- | The cost of doing step 3 afterwards is that the Sankey relaxation does
-- | not know of the moves, so edges into a group that was moved down slant
-- | more than they would if the relaxation had made the room itself.
-- |
-- | ## Top-down (0.5)
-- |
-- | `sankeyToward TopDown` is the same layout stood on end: its layers are
-- | rows, a node is as broad as the broader of what enters and what leaves
-- | it, its edges stack along its top and its bottom, and every edge not
-- | turned back runs down the page. It is made by laying the flow out left
-- | to right and exchanging x and y at the last, so the placing, the order,
-- | the lanes and the channels are the left-to-right layout's, and the laws
-- | it promises are the same three, read by the layout's direction. Four
-- | things do not survive being stood on end, and are done otherwise:
-- |
-- | - **A node's words.** Left to right a node is `nodeBreadth` along the
-- |   flow, its words are wrapped to that, and it is made taller where they
-- |   need it. Top-down the words are still wrapped to `nodeBreadth`, which
-- |   is then the most room a node is given across the flow for its words'
-- |   sake: a node is as broad as its ribbons, or as its widest line needs
-- |   where that is more (`Text.breadthOf`), and its ribbons stand in the
-- |   middle of it. So a thin ribbon under a long name has a node as broad
-- |   as the name, and two such nodes cannot have their names collide.
-- | - **How far a node reaches along the flow.** Left to right,
-- |   `nodeBreadth`. Top-down, as tall as the tallest words of any node in
-- |   the picture (one height, so that the nodes of a layer are one row);
-- |   `restHeight` where the picture sets no words.
-- | - **An outline's strip.** An outline's name is in a strip at the top of
-- |   its box, either way. Left to right that is across the flow, and the
-- |   room is made by moving boxes down a column. Top-down it is along the
-- |   flow, before the outline's first row, and the room is in the gap
-- |   between layers (and above the first): for each outline one in another,
-- |   its padding and its strip before, its padding after. Where
-- |   `layerGap` has not room for what the picture asks, the strip and the
-- |   padding are both less, in proportion, and `PlacedOutline.header` says
-- |   what the strip is.
-- | - **The margins.** Left to right the picture has half a `layerGap`
-- |   before its first layer and after its last, room for an edge that turns
-- |   back, whether or not one does (as since 0.1, kept so that no picture
-- |   moves). Top-down it has only what is used: that room where an edge
-- |   does turn back, an outline's where there is an outline, and otherwise
-- |   none, so a picture of forward edges begins at its first row and ends
-- |   at its last, and a page can stand something directly under it.
-- |
-- | The shelf of nodes no edge touches, under the picture left to right, is
-- | at its right top-down.
module Hylograph.Flow.Render.Sankey
  ( Widths(..)
  , Metrics
  , defaultMetrics
  , widthOf
  , sankey
  , sankeyToward
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Number (isFinite)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Sankey.Lanes (laneOf)
import DataViz.Layout.Sankey.Path (findNode)
import DataViz.Layout.Sankey.Types (Alignment(..), LinkColorMode(..), SankeyConfig, defaultSankeyConfig)
import DataViz.Layout.Sankey.Whole (Sizing(..), WholeLayout, computeWholeLayout)
import Hylograph.Flow (Edge, ElementKey, GroupId(..), Projection)
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Engine(..), Layout, Picture, Promise(..), openPaths)
import Hylograph.Flow.Render.Internal.Bands (Block(..), arrange)
import Hylograph.Flow.Render.Route (Ink(..), Piece(..), Point, Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Text (SetWords, breadthOf, heightOf, setWords)

-- | How wide an edge is. `Plain`: every edge one unit, drawn as a hairline.
-- | `Sankey`: the caller reads the width from the edge, which carries what
-- | its wires carry (`wires`: one for a wire, more where the edge crosses
-- | undrawn elements, and which of them is the quantity is the domain's to
-- | say). A width that is not a positive number is drawn as nothing.
data Widths wire
  = Plain
  | Sankey (Edge wire -> Number)

-- | `unit`: pixels to one unit of width. `hairline`: the stroke of a plain
-- | edge. `nodeBreadth`: the breadth a node's words are wrapped to; left to
-- | right it is also the node's extent along the flow. `layerGap`: between
-- | one layer and the next. `nodeGap`: between two nodes of a layer.
-- | `restHeight`: how far across the flow a node reaches that no edge gives
-- | an extent; top-down, also how tall every node is where no words are set.
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

-- | An edge's width in units under a choice of widths.
widthOf :: forall wire. Widths wire -> Edge wire -> Number
widthOf widths edge = case widths of
  Plain -> 1.0
  Sankey measure ->
    let w = measure edge
    in if isFinite w && w > 0.0 then w else 0.0


-- | The Sankey engine at a scale and a choice of widths, the flow running
-- | left to right.
sankey :: forall wire. Metrics -> Widths wire -> Engine wire
sankey = sankeyToward LeftToRight

-- | The Sankey engine with the flow running the way it is told (0.5): left
-- | to right, its layers columns, or top-down, its layers rows. The promises
-- | are the same three either way, and `Laws` reads each by the layout's
-- | direction. See the module's header for what top-down means for a node's
-- | size, its words, and an open group's outline.
sankeyToward :: forall wire. Direction -> Metrics -> Widths wire -> Engine wire
sankeyToward direction metrics widths = Engine
  { name: case direction of
      LeftToRight -> "sankey"
      TopDown -> "sankey, top-down"
  , promises: [ FlowsForward, LayersAligned, WidthsConserved { pixelsPerUnit: metrics.unit, unitsOf: widthOf widths } ]
  , layout: laidOut direction metrics widths
  }

-- | A box of the Sankey layout to be ordered: a node or a waypoint.
-- | `centre`: where the first pass put its middle; `Nothing` for a node it
-- | did not place. `path`: the open groups it is in.
type Entry = { name :: String, order :: Int, centre :: Maybe Number, path :: Array GroupId }

-- | The order things are to stand in, top to bottom.
data Shape
  = One String
  | Many GroupId (Array Shape)

-- | Everything below is worked out with the flow running left to right: x
-- | along the flow, y across it. A top-down layout is that one stood on end
-- | at the last (`onEnd`), with the four things that are not the same on
-- | end told apart by `direction` where they are made: what a node's words
-- | need across the flow, how far a node reaches along it, where an
-- | outline's strip is, and the margins before the first layer and after
-- | the last.
laidOut :: forall wire. Direction -> Metrics -> Widths wire -> Picture -> Projection wire -> Layout wire
laidOut direction metrics widths given projection = case direction of
  LeftToRight -> flat
  TopDown -> onEnd flat
  where
  flat =
    { direction: LeftToRight
    , nodes: Array.sortWith _.index (layered <> stood <> shelved)
    , edges: Array.sortWith _.index (forward <> backward)
    , outlines
    , width: lead + trail + foldl max (foldl (\acc n -> max acc n.x1) placed.chart.x1 placed.isolated) (map (\n -> n.box.x - lead + n.box.width) stood)
    , height:
        if Array.null chartBoxes then foldl (\acc n -> max acc (n.y1 + sink)) arranged.bottom placed.isolated
        else placed.extent.y1 + sink
    }

  -- the first node with a key stands for the key
  indexOf :: Map ElementKey Int
  indexOf = foldl (\m (Tuple i n) -> if Map.member n.key m then m else Map.insert n.key i m) Map.empty (Array.mapWithIndex Tuple projection.nodes)

  nameOf :: Int -> String
  nameOf i = "node:" <> show i

  names = Array.mapWithIndex (\index _ -> nameOf index) projection.nodes
  numberOf :: Map String Int
  numberOf = Map.fromFoldable (Array.mapWithIndex (\index name -> Tuple name index) names)

  -- the edges both of whose ends are nodes, as the Sankey layout's rows
  routable = Array.catMaybes $ Array.mapWithIndex
    ( \index edge -> ado
        from <- Map.lookup edge.from indexOf
        to <- Map.lookup edge.to indexOf
        in { index, edge, from, to }
    )
    projection.edges

  rows = routable <#> \r -> { s: nameOf r.from, t: nameOf r.to, v: widthOf widths r.edge }
  mentioned = Set.fromFoldable (rows >>= \row -> [ row.s, row.t ])

  -- room for an edge that turns back
  turn = metrics.layerGap / 2.0
  reach = turn * 0.9

  -- how far a node reaches along the flow. Left to right, the breadth the
  -- metrics give. Top-down that is how tall it is, and its words are set
  -- down the page: as tall as the tallest words of any node, so that the
  -- nodes of a layer are one row; `restHeight` where no words are set
  extent = case direction of
    LeftToRight -> metrics.nodeBreadth
    TopDown -> if isJust given.words then foldl (\tallest words -> max tallest (fromMaybe 0.0 (heightOf given.lettering <$> words))) 0.0 set else metrics.restHeight
  pitch = extent + metrics.layerGap

  -- the margins before the first layer and after the last. Left to right,
  -- room for an edge to turn back, as since 0.1, whether or not one does.
  -- Top-down, only what is used: that room where an edge does turn back,
  -- and an outline's padding and strip where there is an outline; so a
  -- picture with neither begins at its first row and ends at its last
  turnsBack = not (Array.null placed.backEdges)
  lead = case direction of
    LeftToRight -> turn
    TopDown -> max (if turnsBack then turn else 0.0) (if Array.null arranged.frames then 0.0 else toNumber deepest * (along.padding + along.header) + 2.0)
  trail = case direction of
    LeftToRight -> turn
    TopDown -> max (if turnsBack then turn else 0.0) (if Array.null arranged.frames then 0.0 else toNumber deepest * along.padding + 2.0)

  -- ---- words ----------------------------------------------------------------
  set :: Array (Maybe SetWords)
  set = projection.nodes <#> \node -> given.words <#> \say -> setWords given.lettering metrics.nodeBreadth (say node)

  needs :: Map String Number
  needs = Map.fromFoldable $ Array.catMaybes $ Array.mapWithIndex (\index words -> words <#> \lines -> Tuple (nameOf index) (roomFor lines)) set

  -- what words need across the flow: left to right, their lines' height;
  -- top-down, their widest line's breadth
  roomFor :: SetWords -> Number
  roomFor lines = case direction of
    LeftToRight -> heightOf given.lettering lines
    TopDown -> breadthOf given.lettering lines

  needOf :: String -> Number
  needOf name = fromMaybe 0.0 (Map.lookup name needs)

  -- ---- open groups ------------------------------------------------------------
  homes = map openPaths given.groups

  paths :: Map String (Array GroupId)
  paths = case homes of
    Nothing -> Map.empty
    Just home -> Map.fromFoldable $ Array.catMaybes $ Array.mapWithIndex
      (\index node -> let path = home.pathOf node.key in if Array.null path then Nothing else Just (Tuple (nameOf index) path))
      projection.nodes

  pathOf :: String -> Array GroupId
  pathOf name = fromMaybe [] (Map.lookup name paths)

  grouped = not (Map.isEmpty paths)

  -- boxes are moved only where something asks: words, or an open group
  active = grouped || isJust given.words

  -- a member of an open group that no edge touches: not for the shelf
  strays = Array.filter (\name -> Map.member name paths && not (Set.member name mentioned)) names
  straySet = Set.fromFoldable strays
  forShelf = Array.filter (\name -> not (Set.member name straySet)) names

  shelfHeight = foldl (\tallest name -> if Set.member name mentioned then tallest else max tallest (needOf name)) metrics.restHeight forShelf

  -- ---- the Sankey layout ----------------------------------------------------------
  -- only the corner of the extent counts: the scale settles the rest
  config :: SankeyConfig
  config = (defaultSankeyConfig 1000.0 1000.0)
    { alignment = Left
    , linkColorMode = StaticColor ""
    , nodeWidth = extent
    , nodePadding = metrics.nodeGap
    , extent = { x0: 0.0, y0: 0.0, x1: 1000.0, y1: 1000.0 }
    }

  whole :: SankeyConfig -> WholeLayout
  whole = computeWholeLayout
    { sizing: AtScale { pixelsPerUnit: metrics.unit, columnPitch: pitch }
    , backEdges: { margin: metrics.nodeGap, gap: metrics.nodeGap / 2.0, reach }
    , shelf: { margin: 2.0 * metrics.nodeGap, minimumColumns: 4, columnPitch: pitch, nodeHeight: shelfHeight, rowGap: metrics.nodeGap }
    }
    (\_ _ -> true)
    forShelf
    rows

  first :: WholeLayout
  first = whole config

  -- a waypoint is with the innermost group that holds both ends of its edge
  commonPath :: Array GroupId -> Array GroupId -> Array GroupId
  commonPath a b = Array.catMaybes (Array.takeWhile isJust (Array.zipWith (\x y -> if x == y then Just x else Nothing) a b))

  waypointPath :: String -> Array GroupId
  waypointPath name = fromMaybe [] do
    lane <- laneOf name
    row <- Array.index rows lane.link
    pure (commonPath (pathOf row.s) (pathOf row.t))

  entries :: Array Entry
  entries = Array.mapWithIndex (\place entry -> { name: entry.name, order: place, centre: entry.centre, path: entry.path }) $
    (first.nodes <#> \n -> { name: n.name, centre: Just ((n.y0 + n.y1) / 2.0), path: pathOf n.name })
      <> (first.waypoints <#> \n -> { name: n.name, centre: Just ((n.y0 + n.y1) / 2.0), path: waypointPath n.name })
      <> (strays <#> \name -> { name, centre: Nothing, path: pathOf name })

  -- at each level: the loose boxes and the groups, by the mean height of
  -- what the first pass placed in each; what it placed nothing of, last
  shapes :: Int -> Array Entry -> Array Shape
  shapes depth within = map _.shape (Array.sortWith _.key (loose <> clusters))
    where
    loose = Array.filter (\entry -> Array.length entry.path == depth) within <#> \entry ->
      { key: Tuple (isNothing entry.centre) (Tuple (fromMaybe 0.0 entry.centre) entry.order), shape: One entry.name }
    deeper = Array.filter (\entry -> Array.length entry.path > depth) within
    clusters = Array.nub (Array.mapMaybe (\entry -> Array.index entry.path depth) deeper) <#> \group ->
      let
        inside = Array.filter (\entry -> Array.index entry.path depth == Just group) deeper
        centres = Array.mapMaybe _.centre inside
        mean = if Array.null centres then 0.0 else sum centres / toNumber (Array.length centres)
      in
        { key: Tuple (Array.null centres) (Tuple mean (foldl (\least entry -> min least entry.order) top inside)), shape: Many group (shapes (depth + 1) inside) }

  order :: Array Shape
  order = shapes 0 entries

  inOrder :: Shape -> Array String
  inOrder = case _ of
    One name -> [ name ]
    Many _ within -> Array.concatMap inOrder within

  position :: Map String Int
  position = Map.fromFoldable (Array.mapWithIndex (\place name -> Tuple name place) (Array.concatMap inOrder order))

  placeOf :: String -> Int
  placeOf name = fromMaybe top (Map.lookup name position)

  -- the second pass, held to the order: only where there are groups
  placed :: WholeLayout
  placed =
    if grouped then whole config { nodeSort = Just (\a b -> compare (placeOf a.name) (placeOf b.name)) }
    else first

  chartBoxes = placed.nodes <> placed.waypoints

  -- ---- room made down the page ------------------------------------------------------
  boxOf :: Map String { layer :: Int, top :: Number, height :: Number }
  boxOf = Map.fromFoldable (chartBoxes <#> \n -> Tuple n.name { layer: n.layer, top: n.y0, height: n.y1 - n.y0 })

  -- a stray stands in the first column its innermost group has anything in
  strayLayer :: String -> Int
  strayLayer name =
    let
      path = pathOf name
      beside = entries # Array.mapMaybe \entry ->
        if Array.take (Array.length path) entry.path == path then _.layer <$> Map.lookup entry.name boxOf else Nothing
    in
      fromMaybe 0 (Array.head (Array.sort beside))

  blockOf :: Shape -> Block
  blockOf = case _ of
    One name -> case Map.lookup name boxOf of
      Just box -> Single { name, layer: box.layer, top: Just box.top, height: max box.height (needOf name) }
      Nothing -> Single { name, layer: strayLayer name, top: Nothing, height: max metrics.restHeight (needOf name) }
    Many group within -> Cluster group (map blockOf within)

  blocks :: Array Block
  blocks = if active then map blockOf order else []

  -- top-down, an outline's strip is before its first layer, along the
  -- flow, and takes no room across it
  arranged = arrange
    { gap: metrics.nodeGap
    , padding: given.outline.padding
    , header: case direction of
        LeftToRight -> given.outline.header
        TopDown -> 0.0
    }
    blocks

  heights :: Map String Number
  heights = Map.fromFoldable (blocks >>= sized)
    where
    sized = case _ of
      Single leaf -> [ Tuple leaf.name leaf.height ]
      Cluster _ within -> within >>= sized

  -- where a box's top is, and how tall it is: the Sankey layout's unless moved
  topOf :: String -> Number -> Number
  topOf name was = fromMaybe was (Map.lookup name arranged.tops)

  heightAt :: String -> Number -> Number
  heightAt name was = fromMaybe was (Map.lookup name heights)

  -- how far what stands on a box's side has moved: with the box, and to
  -- the middle of a side grown taller
  lifts :: Map String Number
  lifts = Map.fromFoldable $ chartBoxes # Array.mapMaybe \n ->
    if active then Just (Tuple n.name (topOf n.name n.y0 - n.y0 + (heightAt n.name (n.y1 - n.y0) - (n.y1 - n.y0)) / 2.0)) else Nothing

  lift :: String -> Number
  lift name = fromMaybe 0.0 (Map.lookup name lifts)

  -- how far what is under the chart (channels, shelf) has moved down
  sink :: Number
  sink =
    if not active then 0.0
    else if Array.null chartBoxes then (if Array.null strays then 0.0 else arranged.bottom + 2.0 * metrics.nodeGap)
    else max 0.0 (arranged.bottom - placed.chart.y1)

  shift :: Point -> Point
  shift p = { x: p.x + lead, y: p.y }

  inLayers = Map.fromFoldable (placed.nodes <#> \n -> Tuple n.name n)
  onShelf = Map.fromFoldable (placed.isolated <#> \n -> Tuple n.name n)

  wordsOf :: Int -> Maybe SetWords
  wordsOf index = join (Array.index set index)

  layered = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) inLayers <#> \n ->
        { index
        , node
        , box: { x: n.x0 + lead, y: topOf n.name n.y0, width: n.x1 - n.x0, height: heightAt n.name (n.y1 - n.y0) }
        , layer: Just n.layer
        , words: wordsOf index
        , ports: []
        }
    )
    projection.nodes

  stood = Array.catMaybes $ Array.mapWithIndex
    ( \index node ->
        let
          name = nameOf index
        in
          if Set.member name straySet then Map.lookup name arranged.tops <#> \y ->
            { index
            , node
            , box: { x: toNumber (strayLayer name) * pitch + lead, y, width: extent, height: heightAt name metrics.restHeight }
            , layer: Just (strayLayer name)
            , words: wordsOf index
            , ports: []
            }
          else Nothing
    )
    projection.nodes

  shelved = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) onShelf <#> \n ->
        { index, node, box: { x: n.x0 + lead, y: n.y0 + sink, width: n.x1 - n.x0, height: n.y1 - n.y0 }, layer: Nothing, words: wordsOf index, ports: [] }
    )
    projection.nodes

  -- ---- outlines -----------------------------------------------------------------------
  -- across the flow an outline has less room than down it: half a layer's
  -- gap, shared by every outline one in another
  deepest = foldl (\most frame -> max most (frame.levels + 1)) 1 arranged.frames
  across = max 0.0 (min given.outline.padding ((turn - 2.0) / toNumber deepest))

  -- top-down, the same room between two layers holds, for each outline one
  -- in another, its padding after its last layer, and its padding and its
  -- strip before its first: all three as the picture asks where there is
  -- room for them, and all three less, in proportion, where there is not
  along :: { padding :: Number, header :: Number }
  along =
    let
      asked = 2.0 * given.outline.padding + given.outline.header
      room = max 0.0 ((metrics.layerGap - 4.0) / toNumber deepest)
      share = if asked <= room || asked <= 0.0 then 1.0 else room / asked
    in
      { padding: given.outline.padding * share, header: given.outline.header * share }

  groupName :: GroupId -> String
  groupName group@(GroupId written) = case homes of
    Just home -> home.nameOf group
    Nothing -> written

  outlines = arranged.frames <#> \frame ->
    let
      within = toNumber (frame.levels + 1)
      -- from the outline to its first layer, and from its last layer to it
      before = case direction of
        LeftToRight -> across * within
        TopDown -> (along.padding + along.header) * within
      after = case direction of
        LeftToRight -> across * within
        TopDown -> along.padding * within
    in
      { group: frame.group
      , name: groupName frame.group
      , box:
          { x: toNumber frame.firstLayer * pitch + lead - before
          , y: frame.top
          , width: toNumber (frame.lastLayer - frame.firstLayer) * pitch + extent + before + after
          , height: frame.bottom - frame.top
          }
      , header: case direction of
          LeftToRight -> given.outline.header
          TopDown -> along.header
      , depth: frame.depth
      , parent: frame.parent
      , members: Array.sort (Array.mapMaybe (\name -> Map.lookup name numberOf) frame.leaves)
      }

  -- ---- edges ----------------------------------------------------------------------------
  -- an edge forward: one curve for each layer it crosses, straight across
  -- each waypoint between (an edge turned back has a route of no segments)
  forward = Array.catMaybes $ Array.zipWith
    ( \r route -> do
        spans <- for route.segments \link -> ado
          source <- findNode chartBoxes link.sourceIndex
          target <- findNode chartBoxes link.targetIndex
          in { from: shift { x: source.x1, y: link.y0 + lift source.name }, to: shift { x: target.x0, y: link.y1 + lift target.name }, width: link.width }
        pieces <- NonEmptyArray.fromArray (interleave (map (\s -> curve s.from s.to) spans) (Array.zipWith (\a b -> curve a.to b.from) spans (Array.drop 1 spans)))
        start <- Array.head spans
        pure
          { index: r.index
          , edge: r.edge
          , route: Ribbon { from: start.from, pieces, breadth: start.width, ink }
          , course: Forward
          }
    )
    routable
    placed.routes

  -- the curve the Sankey layout draws between two points: level at both ends
  curve :: Point -> Point -> Piece
  curve from to = let middle = (from.x + to.x) / 2.0 in Curved { leaving: { x: middle, y: from.y }, arriving: { x: middle, y: to.y }, to }

  ink :: Ink
  ink = case widths of
    Plain -> Hairline metrics.hairline
    Sankey _ -> Band

  interleave :: forall a. Array a -> Array a -> Array a
  interleave xs ys = Array.concat (Array.zipWith (\x y -> [ x, y ]) xs ys) <> Array.drop (Array.length ys) xs

  -- an edge turned back: the layout says which row it is, where it stands
  -- on its two nodes, and which channel is its own. Out of the source's
  -- right side, round and down to the channel, back along it, and round and
  -- up into the target's left side (as `generateBackEdgePath` draws it)
  backward = placed.backEdges # Array.mapMaybe \routed ->
    let
      start = shift { x: routed.backEdge.x0, y: routed.backEdge.y0 + lift routed.backEdge.source }
      end = shift { x: routed.backEdge.x1, y: routed.backEdge.y1 + lift routed.backEdge.target }
      channel = routed.channel + sink
    in
      Array.index routable routed.backEdge.index <#> \r ->
        { index: r.index
        , edge: r.edge
        , route: Ribbon
            { from: start
            , pieces: NonEmptyArray.cons' (Curved { leaving: { x: start.x + reach, y: start.y }, arriving: { x: start.x + reach, y: channel }, to: { x: start.x, y: channel } })
                [ Straight { x: end.x, y: channel }
                , Curved { leaving: { x: end.x - reach, y: channel }, arriving: { x: end.x - reach, y: end.y }, to: end }
                ]
            , breadth: routed.backEdge.width
            , ink
            }
        , course: Backward
        }

-- | A layout made left to right, stood on end: x for y in every box and
-- | every route, so that its columns are rows and it runs top-down. (The
-- | Sankey engine places no ports, so there is none to move to another
-- | side.) An outline's strip, which `laidOut` put before the outline's
-- | first layer, is then at its top, where `PlacedOutline.header` says it is.
onEnd :: forall wire. Layout wire -> Layout wire
onEnd laid =
  { direction: TopDown
  , nodes: laid.nodes <#> \placed -> placed { box = turned placed.box }
  , edges: laid.edges <#> \placed -> placed { route = Route.transpose placed.route }
  , outlines: laid.outlines <#> \outline -> outline { box = turned outline.box }
  , width: laid.height
  , height: laid.width
  }
  where
  turned box = { x: box.y, y: box.x, width: box.height, height: box.width }
