-- | The unfolding engine: one layout for every cut of a flow, so made that
-- | the layouts of two cuts that differ by one group can be laid side by
-- | side and the reader keeps their place (0.6).
-- |
-- | `Hylograph.Flow.Grouping.collapse` gives the coarser flow for a cut (a
-- | set of closed groups). The Sankey engine lays each such flow out afresh:
-- | its relaxation places every layer again, an open group is given room
-- | for its outline, and a closed group is as broad as what crosses its
-- | boundary, which need not be what its parts are abreast. So nothing can
-- | be promised of the pair. This engine is told the *finest* flow once
-- | (`Whole`: the grouping, and the projection with nothing closed) and
-- | derives from it three things that no cut changes; every cut's layout is
-- | then made of them, and is a function of the cut alone (so closing what
-- | was opened gives back, to the number, the layout there was).
-- |
-- | **1. One order across the flow (`Plan.rank`).** Every node of the
-- | finest flow has a rank, and every group's nodes are a run of
-- | consecutive ranks, a group inside it a run within that run. It is read
-- | once from the Sankey layout of the finest flow (at each level of the
-- | grouping, the groups and the loose nodes by the mean of where that
-- | layout put them). In every cut, the nodes of a tier stand in the order
-- | of their ranks, a closed group at its run; the ribbons on a node's side
-- | stand in the order of the ranks of their other ends, whether they run
-- | forward or are turned back; and the lane of a long ribbon stands just
-- | after the block its source is in, at the level of the innermost group
-- | that holds both its ends. Hence **Ordered**, always: no two nodes of a
-- | tier and no two ribbons at a node change order between any two cuts.
-- |
-- | **2. One breadth for each group (`Plan.breadth`).** A closed group is
-- | as broad as *the most of its inside that is ever abreast*: the heaviest
-- | set of its nodes and inner ribbons no two of which lie on one path (a
-- | maximum-weight antichain of the finest flow, within the group), or
-- | what crosses its boundary one way where that is more. For a chain that
-- | is its throughput, and what the Sankey engine gives. For a group that
-- | holds a spring beside a dead end, or a loop, it is more than its
-- | ribbons, and the bar says so: there is more inside than crosses. An
-- | open group is a *band* of exactly that breadth, in which its parts
-- | stand with no gap between them, a group inside it a band within the
-- | band. Parts in one tier are never on one path, so they fit; hence
-- | **Nested**, for breadth: the parts of an opened group stand in a slot
-- | as broad as its closed bar. (The one case where they do not fit is said
-- | under "What is not kept".)
-- |
-- | **3. One rule for gaps.** `nodeGap` stands between two boxes of a tier
-- | that are in different top-level blocks of the grouping (a top-level
-- | group, open or closed, or a node no group holds), and nothing stands
-- | between two boxes of one block, but this: **every group has a strip
-- | for its name before it across the flow, closed or open** (`Whole.strip`;
-- | left to right only, where a name is written across the flow). Closed,
-- | the strip is bare paper over the bar; open, it is the top of the
-- | outline, with the name in it. So a group inside a group has a place
-- | for its name that is its own, and since the strip is there either way
-- | no gap depends on the cut. Two lanes are of one block when their
-- | sources are in one top-level block and their targets in one. Top-down
-- | a name is written along the flow, before the group's first row, in the
-- | gap between tiers, one row of names for each depth, and there is no
-- | strip across it.
-- |
-- | The placing is then a packing with no relaxation in it. The tiers are
-- | the Sankey layout's of the cut's own flow (its layers, its lanes for
-- | long ribbons, its channels for ribbons turned back, its shelf), with
-- | one difference from the Sankey engine: **a node that feeds nothing
-- | stands in the last tier** (that layout's `Justify`), where the Sankey
-- | engine stands it just after what feeds it. The ends of a flow are
-- | commonly one group (its sinks, its fates), and a group whose parts are
-- | in one tier opens Held; left where their feeders put them, the sinks
-- | of a real flow are tiers apart and their group spans the picture. Across
-- | the flow, the top-level blocks are taken in rank order and each is set
-- | as near the start as what stands before it in the tiers it spans
-- | allows. An open group spans every tier from its first part's to its
-- | last's, and nothing else enters its band in any of them.
-- |
-- | So **Held**, where there is room: when a group whose parts are in one
-- | tier opens, the parts take the bar's place exactly and no other box
-- | moves; `Continuity.continuity` says so of the pair, by measuring.
-- |
-- | ## What is not kept
-- |
-- | - **The tiers are the cut's.** Closing a group whose parts are in
-- |   several tiers makes it one node in one tier, so what is downstream
-- |   moves along the flow, and where the group's parts straddled something
-- |   outside it the closed flow has a cycle and a ribbon is turned back.
-- |   Then the pair is not Held, and is Nested only if the band kept its
-- |   place across the flow.
-- | - **A band can be too narrow where a ribbon is turned back.** Two
-- |   parts on one path are never in one tier, unless the path ran through
-- |   a ribbon that this cut turned back. Then a tier of the group can hold
-- |   more than the group's breadth, the band is as broad as it needs, and
-- |   the pair is not Nested. `continuity` reports it; nothing is drawn
-- |   wrong.
-- | - **Words make no room.** A node is as broad as its ribbons (or its
-- |   group's breadth) and nothing else: breadth is data. The engine sets
-- |   no words, so under a picture that gives words it breaks the general
-- |   law `Lettered`; give it none, and let the tree write each label
-- |   beside its node (`Tree.treeLabelled (Beside least)`).
-- | - **A ribbon turned back does not stand below the others** on its
-- |   node, as the Sankey engine has it: it stands where its other end's
-- |   rank puts it, since whether a ribbon is turned back is the cut's.
-- | - **A node no edge touches** is on the shelf, as ever, unless it is a
-- |   member of an open group (then it is in its group's band, with no
-- |   breadth, since nothing passes through it) or a closed group that
-- |   nothing crosses into or out of (then it is in the first tier, as
-- |   broad as the group is).
-- | - **An open group that spans tiers keeps its whole band in each of
-- |   them.** So a part of the group over it that stands in one of those
-- |   tiers stands beside the band, though it may be on one path with what
-- |   is in the band; the group over it can then need more than its
-- |   breadth, and opening it is not Nested. The breadth is exact for a
-- |   group none of whose inner groups, open, spans tiers.
-- |
-- | The engine promises `FlowsForward`, `LayersAligned` and `WidthsCarried`
-- | (a node is *at least* as broad as its broader side), not
-- | `WidthsConserved` (exactly as broad), because of 2.
module Hylograph.Flow.Render.Unfold
  ( Whole
  , Plan
  , plan
  , unfolding
  , unfoldingBy
  , groupBreadth
  , rankOf
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import DataViz.Layout.Sankey.Lanes (laneOf)
import DataViz.Layout.Sankey.Path (findNode)
import DataViz.Layout.Sankey.Types (Alignment(..), LinkColorMode(..), SankeyConfig, defaultSankeyConfig)
import DataViz.Layout.Sankey.Whole (Sizing(..), WholeLayout, computeWholeLayout)
import Hylograph.Flow (Edge, ElementKey(..), GroupId, ItemId(..), Projection, StepId(..))
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Engine(..), Layout, Picture, Promise(..), openPaths)
import Hylograph.Flow.Render.Route (Ink(..), Piece(..), Point, Route(..))
import Hylograph.Flow.Render.Sankey (Metrics, Widths(..), stoodOnEnd, widthOf)

-- | The flow whatever the cut: its grouping, and its projection with no
-- | group closed (`project drawing (collapse grouping Set.empty flow)`).
-- | Every projection the engine is then run on must be of the same flow
-- | under the same grouping and drawing, with some groups closed, and the
-- | picture must say which (`Picture.groups`; a picture that tells of no
-- | groups is taken to have none closed, and gets no outlines).
-- | `strip`: the room for a group's name, across the flow, before every
-- | group (left to right; the picture's `outline.header` is not used).
type Whole wire = { grouping :: Array Grouping, finest :: Projection wire, strip :: Number }

-- | What no cut changes. `rank`: each node of the finest flow, its place in
-- | the one order. `span`: each group that holds a node, its first and last
-- | rank. `block`: each node's and each group's top-level block, as the
-- | first rank of it. `breadth`: each group's breadth, in pixels, its own
-- | strip aside. `strip`: the strip before each group, across the flow.
-- | `levels`: how many levels of group are inside each group. `deepest`:
-- | how many groups deep the grouping goes.
type Plan =
  { rank :: Map ElementKey Int
  , span :: Map GroupId { lo :: Int, hi :: Int }
  , block :: Map ElementKey Int
  , blockOfGroup :: Map GroupId Int
  , breadth :: Map GroupId Number
  , strip :: Number
  , levels :: Map GroupId Int
  , deepest :: Int
  }

-- | How broad a group is, closed or open, in pixels; nothing for a group
-- | that holds no node of the finest flow.
groupBreadth :: Plan -> GroupId -> Maybe Number
groupBreadth planned group = Map.lookup group planned.breadth

-- | A node's place in the one order: its own rank, or for a closed group's
-- | step or summary the first rank of the group.
rankOf :: Plan -> ElementKey -> Int
rankOf planned key = case groupOfKey key of
  Just group -> maybe top _.lo (Map.lookup group planned.span)
  Nothing -> fromMaybe top (Map.lookup key planned.rank)

groupOfKey :: ElementKey -> Maybe GroupId
groupOfKey = case _ of
  StepKey (GroupStep group) -> Just group
  ItemKey (GroupItem group) -> Just group
  _ -> Nothing

-- | The Sankey layout's configuration: the Sankey engine's, but for where
-- | a node that feeds nothing stands (see the header).
configOf :: Number -> Metrics -> SankeyConfig
configOf extent metrics = (defaultSankeyConfig 1000.0 1000.0)
  { alignment = Justify
  , linkColorMode = StaticColor ""
  , nodeWidth = extent
  , nodePadding = metrics.nodeGap
  , extent = { x0: 0.0, y0: 0.0, x1: 1000.0, y1: 1000.0 }
  }

wholeOf :: Number -> Number -> Metrics -> Array String -> Array { s :: String, t :: String, v :: Number } -> WholeLayout
wholeOf extent shelfHeight metrics names rows = computeWholeLayout
  { sizing: AtScale { pixelsPerUnit: metrics.unit, columnPitch: extent + metrics.layerGap }
  , backEdges: { margin: metrics.nodeGap, gap: metrics.nodeGap / 2.0, reach: metrics.layerGap / 2.0 * 0.9 }
  , shelf: { margin: 2.0 * metrics.nodeGap, minimumColumns: 4, columnPitch: extent + metrics.layerGap, nodeHeight: shelfHeight, rowGap: metrics.nodeGap }
  }
  (\_ _ -> true)
  names
  rows
  (configOf extent metrics)

nameOf :: Int -> String
nameOf i = "node:" <> show i

-- | The edges both of whose ends are nodes, each with its ends' places.
routableOf :: forall wire. Projection wire -> Array { index :: Int, edge :: Edge wire, from :: Int, to :: Int }
routableOf projection = Array.catMaybes $ Array.mapWithIndex
  ( \index edge -> ado
      from <- Map.lookup edge.from indexOf
      to <- Map.lookup edge.to indexOf
      in { index, edge, from, to }
  )
  projection.edges
  where
  indexOf = foldl (\m (Tuple i n) -> if Map.member n.key m then m else Map.insert n.key i m) Map.empty (Array.mapWithIndex Tuple projection.nodes)

-- | Every group over each element the grouping names, outermost first, and
-- | every group's own chain (itself last), whatever is open.
chainsOf :: Array Grouping -> { ofKey :: Map ElementKey (Array GroupId), ofGroup :: Map GroupId (Array GroupId), levels :: Map GroupId Int }
chainsOf forest = foldl (walk []) { ofKey: Map.empty, ofGroup: Map.empty, levels: Map.empty } forest
  where
  walk chain known = case _ of
    Member step -> known { ofKey = Map.insert (StepKey step) chain known.ofKey }
    Holds item -> known { ofKey = Map.insert (ItemKey item) chain known.ofKey }
    whole@(Group group _ inner) ->
      foldl (walk (Array.snoc chain group)) (known { ofGroup = Map.insert group (Array.snoc chain group) known.ofGroup, levels = Map.insert group (levelsIn whole - 1) known.levels }) inner
  levelsIn = case _ of
    Group _ _ inner -> 1 + foldl (\most g -> max most (levelsIn g)) 0 inner
    _ -> 0

-- | What no cut changes, read once from the finest flow.
plan :: forall wire. Direction -> Metrics -> Widths wire -> Whole wire -> Plan
plan direction metrics widths whole =
  { rank
  , span
  , block: Map.fromFoldable (Array.mapWithIndex (\index node -> Tuple node.key (blockOf (chainAt index) (rankAt index))) fine.nodes)
  , blockOfGroup: Map.mapMaybeWithKey (\_ chain -> Array.head chain >>= \outer -> _.lo <$> Map.lookup outer span) chains.ofGroup
  , breadth: breadths
  , strip
  , levels: chains.levels
  , deepest: foldl (\most chain -> max most (Array.length chain)) 0 (Map.values chains.ofGroup)
  }
  where
  fine = whole.finest
  chains = chainsOf whole.grouping
  strip = case direction of
    LeftToRight -> max 0.0 whole.strip
    TopDown -> 0.0
  count = Array.length fine.nodes
  routable = routableOf fine

  chainAt :: Int -> Array GroupId
  chainAt index = fromMaybe [] (Array.index fine.nodes index >>= \node -> Map.lookup node.key chains.ofKey)

  -- ---- the one order -----------------------------------------------------------
  rows = routable <#> \r -> { s: nameOf r.from, t: nameOf r.to, v: widthOf widths r.edge }
  free = wholeOf metrics.nodeBreadth metrics.restHeight metrics (Array.mapWithIndex (\index _ -> nameOf index) fine.nodes) rows
  centres :: Map String Number
  centres = Map.fromFoldable (free.nodes <#> \n -> Tuple n.name ((n.y0 + n.y1) / 2.0))

  entries = Array.mapWithIndex (\index _ -> { index, centre: Map.lookup (nameOf index) centres, chain: chainAt index }) fine.nodes

  -- at each level: the loose nodes and the groups, by the mean of where the
  -- free layout put what is in each; what it placed nothing of, last
  ordered :: Int -> Array { index :: Int, centre :: Maybe Number, chain :: Array GroupId } -> Array Int
  ordered depth within = Array.concatMap _.run (Array.sortWith _.key (loose <> clusters))
    where
    loose = Array.filter (\entry -> Array.length entry.chain == depth) within <#> \entry ->
      { key: Tuple (isNothing entry.centre) (Tuple (fromMaybe 0.0 entry.centre) entry.index), run: [ entry.index ] }
    deeper = Array.filter (\entry -> Array.length entry.chain > depth) within
    clusters = Array.nub (Array.mapMaybe (\entry -> Array.index entry.chain depth) deeper) <#> \group ->
      let
        inside = Array.filter (\entry -> Array.index entry.chain depth == Just group) deeper
        found = Array.mapMaybe _.centre inside
        mean = if Array.null found then 0.0 else sum found / toNumber (Array.length found)
      in
        { key: Tuple (Array.null found) (Tuple mean (foldl (\least entry -> min least entry.index) top inside)), run: ordered (depth + 1) inside }

  inOrder = ordered 0 entries
  rankByIndex :: Map Int Int
  rankByIndex = Map.fromFoldable (Array.mapWithIndex (\place index -> Tuple index place) inOrder)
  rankAt index = fromMaybe top (Map.lookup index rankByIndex)

  rank :: Map ElementKey Int
  rank = foldl (\m (Tuple index node) -> if Map.member node.key m then m else Map.insert node.key (rankAt index) m) Map.empty (Array.mapWithIndex Tuple fine.nodes)

  span :: Map GroupId { lo :: Int, hi :: Int }
  span = foldl
    ( \found entry -> foldl
        (\m group -> Map.insertWith (\a b -> { lo: min a.lo b.lo, hi: max a.hi b.hi }) group { lo: rankAt entry.index, hi: rankAt entry.index } m)
        found
        entry.chain
    )
    Map.empty
    entries

  blockOf chain own = fromMaybe own (Array.head chain >>= \outer -> _.lo <$> Map.lookup outer span)

  -- ---- each group's breadth -------------------------------------------------------
  into = foldl (\m r -> Map.insertWith (+) r.to (widthOf widths r.edge) m) Map.empty routable
  outOf = foldl (\m r -> Map.insertWith (+) r.from (widthOf widths r.edge) m) Map.empty routable
  through index = max (fromMaybe 0.0 (Map.lookup index into)) (fromMaybe 0.0 (Map.lookup index outOf))

  -- what each node reaches, itself among them
  next :: Map Int (Array Int)
  next = foldl (\m r -> Map.insertWith (<>) r.from [ r.to ] m) Map.empty routable
  reached :: Map Int (Set Int)
  reached = Map.fromFoldable (Array.range 0 (count - 1) <#> \from -> Tuple from (walk (Set.singleton from) [ from ]))
    where
    walk seen frontier = case Array.uncons frontier of
      Nothing -> seen
      Just { head, tail } ->
        let fresh = Array.filter (\to -> not (Set.member to seen)) (Array.nub (fromMaybe [] (Map.lookup head next)))
        in walk (foldl (flip Set.insert) seen fresh) (tail <> fresh)
  reaches a b = maybe false (Set.member b) (Map.lookup a reached)

  -- deepest groups first, so that a group's breadth is known before the
  -- breadth of the group that holds it
  breadths :: Map GroupId Number
  breadths = foldl (\known group -> maybe known (\found -> Map.insert group found known) (breadthOf known group)) Map.empty
    (map _.group (Array.sortWith (\g -> negate g.depth) ((Map.toUnfoldable chains.ofGroup :: Array (Tuple GroupId (Array GroupId))) <#> \(Tuple group chain) -> { group, depth: Array.length chain })))

  breadthOf :: Map GroupId Number -> GroupId -> Maybe Number
  breadthOf known group =
    let
      inside index = Array.elem group (chainAt index)
      nodes = Array.filter inside (Array.range 0 (count - 1))
      inner = Array.filter (\r -> inside r.from && inside r.to) routable
      entering = sum (Array.filter (\r -> inside r.to && not (inside r.from)) routable <#> \r -> widthOf widths r.edge)
      leaving = sum (Array.filter (\r -> inside r.from && not (inside r.to)) routable <#> \r -> widthOf widths r.edge)
      -- the groups inside this one, each as broad (in units) as it is closed
      within = Array.nub (nodes >>= \index -> Array.drop 1 (Array.dropWhile (_ /= group) (chainAt index))) # Array.mapMaybe \held ->
        Map.lookup held known <#> \pixels -> let its = Array.filter (\index -> Array.elem held (chainAt index)) nodes in { firsts: its, lasts: its, weight: (pixels + strip) / metrics.unit }
      -- a thing abreast: a node, a ribbon between two of the group's nodes,
      -- or a group inside it, closed
      things = (nodes <#> \index -> { firsts: [ index ], lasts: [ index ], weight: through index })
        <> (inner <#> \r -> { firsts: [ r.from ], lasts: [ r.to ], weight: widthOf widths r.edge })
        <> within
      before a b = Array.any (\last -> Array.any (reaches last) b.firsts) a.lasts
      onOnePath a b = before a b || before b a
      clashes = Array.mapWithIndex (\i a -> Set.fromFoldable (Array.catMaybes (Array.mapWithIndex (\j b -> if i /= j && onOnePath a b then Just j else Nothing) things))) things
      weights = map _.weight things
    in
      if Array.null nodes then Nothing
      else Just (metrics.unit * max (heaviest weights clashes (Set.fromFoldable (Array.range 0 (Array.length things - 1)))) (max entering leaving))

-- | The most weight of a set of things no two of which clash: exact, by
-- | taking or leaving the thing that clashes with most. Quick where the
-- | things are nearly all on one path or nearly all abreast, which a
-- | group's are; it is exponential in between, and a group of some dozens
-- | of nodes tangled every way would want the flow-network method instead.
heaviest :: Array Number -> Array (Set Int) -> Set Int -> Number
heaviest weights clashes = go
  where
  weightOf i = fromMaybe 0.0 (Array.index weights i)
  clashesOf i = fromMaybe Set.empty (Array.index clashes i)
  go left =
    let
      degree i = Set.size (Set.intersection (clashesOf i) left)
      worst = foldl (\found i -> let d = degree i in if d > found.degree then { at: i, degree: d } else found) { at: -1, degree: 0 } (Set.toUnfoldable left :: Array Int)
    in
      if worst.degree == 0 then sum (map weightOf (Set.toUnfoldable left :: Array Int))
      else max (weightOf worst.at + go (Set.difference (Set.delete worst.at left) (clashesOf worst.at))) (go (Set.delete worst.at left))

-- | The unfolding engine for a flow, the flow running the way it is told.
-- | The plan is made once, when the engine is first run.
unfolding :: forall wire. Direction -> Metrics -> Widths wire -> Whole wire -> Engine wire
unfolding direction metrics widths whole = unfoldingBy (plan direction metrics widths whole) direction metrics widths whole

-- | The same, by a plan already made (`plan`, of the same metrics, widths
-- | and whole).
unfoldingBy :: forall wire. Plan -> Direction -> Metrics -> Widths wire -> Whole wire -> Engine wire
unfoldingBy planned direction metrics widths whole = Engine
  { name: case direction of
      LeftToRight -> "unfolding"
      TopDown -> "unfolding, top-down"
  , promises: [ FlowsForward, LayersAligned, WidthsCarried { pixelsPerUnit: metrics.unit, unitsOf: widthOf widths } ]
  , layout: laidOut planned direction metrics widths whole
  }

-- | Where a box stands among those of its level: by `primary`, a rank; a
-- | lane (`kind` 1) after the block whose last rank that is.
type Key = { primary :: Int, kind :: Int, secondary :: Int, tertiary :: Int }

keyOrder :: Key -> Tuple Int (Tuple Int (Tuple Int Int))
keyOrder key = Tuple key.primary (Tuple key.kind (Tuple key.secondary key.tertiary))

-- | A box to be packed: a node, or the lane of a long ribbon. `path`: the
-- | open groups it is in. `cluster`: the top-level blocks it is of (see the
-- | header's rule for gaps). `node`: its place in the projection, for a node.
-- | `before`: bare room kept before it across the flow (a closed group's
-- | strip).
type Entry = { name :: String, tier :: Int, breadth :: Number, before :: Number, path :: Array GroupId, key :: Key, cluster :: Tuple Int Int, node :: Maybe Int }

data Block
  = Leaf Entry
  | Cluster GroupId Key (Array Block)

type Frame = { group :: GroupId, parent :: Maybe GroupId, depth :: Int, top :: Number, bottom :: Number, first :: Int, last :: Int, members :: Array Int }

type Sized = { first :: Int, last :: Int, breadth :: Number, cluster :: Tuple Int Int, tops :: Array (Tuple String Number), frames :: Array Frame }

-- | Worked out with the flow running left to right (x along the flow, y
-- | across it), and stood on end at the last for a top-down layout.
laidOut :: forall wire. Plan -> Direction -> Metrics -> Widths wire -> Whole wire -> Picture -> Projection wire -> Layout wire
laidOut planned direction metrics widths whole given projection = case direction of
  LeftToRight -> flat
  TopDown -> stoodOnEnd flat
  where
  flat =
    { direction: LeftToRight
    , nodes: Array.sortWith _.index (layered <> stood <> shelved)
    , edges: Array.sortWith _.index (forward <> backward)
    , outlines
    , width: lead + trail + foldl max (foldl (\acc n -> max acc n.x1) placed.chart.x1 placed.isolated) (map (\n -> n.box.x - lead + n.box.width) stood)
    , height: max (bottom + footroom) (if Array.null boxes then foldl (\acc n -> max acc (n.y1 + sink)) 0.0 placed.isolated else placed.extent.y1 + sink)
    }

  closed = maybe Set.empty _.closed given.groups
  home = openPaths { grouping: whole.grouping, closed }
  outlined = isJust given.groups && not (Array.null whole.grouping)

  names = Array.mapWithIndex (\index _ -> nameOf index) projection.nodes
  numberOf :: Map String Int
  numberOf = Map.fromFoldable (Array.mapWithIndex (\index name -> Tuple name index) names)

  routable = routableOf projection
  unitsAt :: Int -> Number
  unitsAt place = maybe 0.0 (\r -> widthOf widths r.edge) (Array.index routable place)
  rows = Array.mapWithIndex (\place r -> { s: nameOf r.from, t: nameOf r.to, v: unitsAt place }) routable
  mentioned = Set.fromFoldable (rows >>= \row -> [ row.s, row.t ])

  keyAt :: Int -> Maybe ElementKey
  keyAt index = _.key <$> Array.index projection.nodes index

  pathAt :: Int -> Array GroupId
  pathAt index = maybe [] home.pathOf (keyAt index)

  rankAt :: Int -> Int
  rankAt index = maybe top (rankOf planned) (keyAt index)

  -- the last rank of the block a node is: its own, or its group's last
  lastRankOf :: ElementKey -> Int
  lastRankOf key = case groupOfKey key of
    Just group -> maybe top _.hi (Map.lookup group planned.span)
    Nothing -> fromMaybe top (Map.lookup key planned.rank)

  blockAt :: Int -> Int
  blockAt index = fromMaybe (rankAt index) do
    key <- keyAt index
    case groupOfKey key of
      Just group -> Map.lookup group planned.blockOfGroup
      Nothing -> Map.lookup key planned.block

  -- ---- the scale along the flow ----------------------------------------------------
  turn = metrics.layerGap / 2.0
  reach = turn * 0.9
  extent = case direction of
    LeftToRight -> metrics.nodeBreadth
    TopDown -> metrics.restHeight
  pitch = extent + metrics.layerGap

  -- a node no edge touches that is a member of an open group, or is a
  -- closed group (one that nothing crosses into or out of): in the chart,
  -- at its place in the order, not on the shelf
  strays = Array.filter (\index -> not (Set.member (nameOf index) mentioned) && (not (Array.null (pathAt index)) || isJust (keyAt index >>= groupOfKey))) (Array.range 0 (Array.length names - 1))
  straySet = Set.fromFoldable (map nameOf strays)
  forShelf = Array.filter (\name -> not (Set.member name straySet)) names

  placed :: WholeLayout
  placed = wholeOf extent metrics.restHeight metrics forShelf rows

  boxes = placed.nodes <> placed.waypoints
  turnsBack = not (Array.null placed.backEdges)

  -- ---- how broad each node is ---------------------------------------------------------
  into = foldl (\m (Tuple place r) -> Map.insertWith (+) r.to (unitsAt place) m) Map.empty (Array.mapWithIndex Tuple routable)
  outOf = foldl (\m (Tuple place r) -> Map.insertWith (+) r.from (unitsAt place) m) Map.empty (Array.mapWithIndex Tuple routable)

  breadthAt :: Int -> Number
  breadthAt index =
    let
      sides = metrics.unit * max (fromMaybe 0.0 (Map.lookup index into)) (fromMaybe 0.0 (Map.lookup index outOf))
    in
      case keyAt index of
        Just (StepKey (GroupStep group)) -> max sides (fromMaybe 0.0 (groupBreadth planned group))
        _ -> sides

  -- ---- the boxes, in blocks -------------------------------------------------------------
  -- a stray stands in the first tier its innermost group has anything in
  strayTier :: Int -> Int
  strayTier index =
    let
      path = pathAt index
      beside = placed.nodes # Array.mapMaybe \n -> Map.lookup n.name numberOf >>= \other ->
        if Array.take (Array.length path) (pathAt other) == path then Just n.layer else Nothing
    in
      fromMaybe 0 (Array.head (Array.sort beside))

  nodeEntry :: Int -> Int -> Entry
  nodeEntry tier index =
    { name: nameOf index
    , tier
    , breadth: breadthAt index
    , before: if isJust (keyAt index >>= groupOfKey) then planned.strip else 0.0
    , path: pathAt index
    , key: { primary: rankAt index, kind: 0, secondary: 0, tertiary: index }
    , cluster: Tuple (blockAt index) (blockAt index)
    , node: Just index
    }

  commonPath :: Array GroupId -> Array GroupId -> Array GroupId
  commonPath a b = Array.catMaybes (Array.takeWhile isJust (Array.zipWith (\x y -> if x == y then Just x else Nothing) a b))

  laneEntry :: { name :: String, layer :: Int } -> Maybe Entry
  laneEntry box = do
    lane <- laneOf box.name
    r <- Array.index routable lane.link
    source <- keyAt r.from
    let
      path = commonPath (pathAt r.from) (pathAt r.to)
      -- the block its source is in, at the level of that path
      after = case Array.index (pathAt r.from) (Array.length path) of
        Just group -> maybe top _.hi (Map.lookup group planned.span)
        Nothing -> lastRankOf source
    pure
      { name: box.name
      , tier: box.layer
      , breadth: metrics.unit * unitsAt lane.link
      , before: 0.0
      , path
      , key: { primary: after, kind: 1, secondary: rankAt r.to, tertiary: lane.link }
      , cluster: Tuple (blockAt r.from) (blockAt r.to)
      , node: Nothing
      }

  entries :: Array Entry
  entries =
    Array.mapMaybe (\n -> Map.lookup n.name numberOf <#> nodeEntry n.layer) placed.nodes
      <> Array.mapMaybe (\n -> laneEntry { name: n.name, layer: n.layer }) placed.waypoints
      <> map (\index -> nodeEntry (strayTier index) index) strays

  blocksAt :: Int -> Array Entry -> Array Block
  blocksAt depth within = map _.block (Array.sortWith (keyOrder <<< _.key) (loose <> clusters))
    where
    loose = Array.filter (\entry -> Array.length entry.path == depth) within <#> \entry -> { key: entry.key, block: Leaf entry }
    deeper = Array.filter (\entry -> Array.length entry.path > depth) within
    clusters = Array.nub (Array.mapMaybe (\entry -> Array.index entry.path depth) deeper) <#> \group ->
      let key = { primary: maybe top _.lo (Map.lookup group planned.span), kind: 0, secondary: 0, tertiary: 0 }
      in { key, block: Cluster group key (blocksAt (depth + 1) (Array.filter (\entry -> Array.index entry.path depth == Just group) deeper)) }

  -- ---- the packing ----------------------------------------------------------------------
  -- The blocks of one level in order, each as near the start as what is
  -- before it in the tiers it spans allows. Tops are from the level's own
  -- start; a cluster's parts and frames are from the cluster's.
  packed :: Maybe GroupId -> Int -> Array Block -> { tops :: Array (Tuple String Number), frames :: Array Frame, bottom :: Number, first :: Int, last :: Int }
  packed parent depth level =
    let
      atTop = depth == 0
      step state block =
        let
          sized = size parent depth block
          tiers = Array.range sized.first sized.last
          start = foldl
            ( \most tier -> case Map.lookup tier state.skyline of
                Just under -> max most (under.bottom + (if atTop && under.cluster /= sized.cluster then metrics.nodeGap else 0.0))
                Nothing -> most
            )
            0.0
            tiers
          end = start + sized.breadth
        in
          { skyline: foldl (\skyline tier -> Map.insert tier { bottom: end, cluster: sized.cluster } skyline) state.skyline tiers
          , tops: state.tops <> map (\(Tuple name at) -> Tuple name (at + start)) sized.tops
          , frames: state.frames <> map (\frame -> frame { top = frame.top + start, bottom = frame.bottom + start }) sized.frames
          , bottom: max state.bottom end
          , first: min state.first sized.first
          , last: max state.last sized.last
          }
      done = foldl step { skyline: Map.empty, tops: [], frames: [], bottom: 0.0, first: top, last: bottom' } level
    in
      { tops: done.tops, frames: done.frames, bottom: done.bottom, first: done.first, last: done.last }
    where
    bottom' = -1

  size :: Maybe GroupId -> Int -> Block -> Sized
  size parent depth = case _ of
    Leaf entry -> { first: entry.tier, last: entry.tier, breadth: entry.before + entry.breadth, cluster: entry.cluster, tops: [ Tuple entry.name entry.before ], frames: [] }
    Cluster group key within ->
      let
        inner = packed (Just group) (depth + 1) within
        -- its strip, and under it its band
        breadth = planned.strip + max inner.bottom (fromMaybe 0.0 (groupBreadth planned group))
      in
        { first: inner.first
        , last: inner.last
        , breadth
        , cluster: Tuple key.primary key.primary
        , tops: map (\(Tuple name at) -> Tuple name (at + planned.strip)) inner.tops
        , frames:
            [ { group, parent, depth, top: 0.0, bottom: breadth, first: inner.first, last: inner.last, members: Array.sort (Array.concatMap membersOf within) } ]
              <> map (\frame -> frame { top = frame.top + planned.strip, bottom = frame.bottom + planned.strip }) inner.frames
        }

  membersOf :: Block -> Array Int
  membersOf = case _ of
    Leaf entry -> Array.fromFoldable entry.node
    Cluster _ _ within -> Array.concatMap membersOf within

  -- nothing is kept before the first block or after the last: a group's
  -- strip is part of the group
  headroom = 0.0
  footroom = 0.0

  settled = packed Nothing 0 (blocksAt 0 entries)
  bottom = headroom + settled.bottom

  tops :: Map String Number
  tops = Map.fromFoldable (settled.tops <#> \(Tuple name at) -> Tuple name (at + headroom))

  topOf :: String -> Number
  topOf name = fromMaybe 0.0 (Map.lookup name tops)

  -- how far what is under the chart (channels, shelf) has moved
  sink :: Number
  sink =
    if Array.null boxes then (if Array.null strays then 0.0 else bottom + footroom + 2.0 * metrics.nodeGap)
    else bottom + footroom - placed.chart.y1

  -- ---- the margins along the flow -------------------------------------------------------
  deepest = max 1 planned.deepest
  across = max 0.0 (min given.outline.padding ((turn - 2.0) / toNumber deepest))
  along :: { padding :: Number, header :: Number }
  along =
    let
      asked = 2.0 * given.outline.padding + given.outline.header
      room = max 0.0 ((metrics.layerGap - 4.0) / toNumber deepest)
      share = if asked <= room || asked <= 0.0 then 1.0 else room / asked
    in
      { padding: given.outline.padding * share, header: given.outline.header * share }

  lead = case direction of
    LeftToRight -> turn
    TopDown -> max (if turnsBack then turn else 0.0) (if outlined then toNumber deepest * (along.padding + along.header) + 2.0 else 0.0)
  trail = case direction of
    LeftToRight -> turn
    TopDown -> max (if turnsBack then turn else 0.0) (if outlined then toNumber deepest * along.padding + 2.0 else 0.0)

  -- ---- nodes -------------------------------------------------------------------------------
  inLayers = Map.fromFoldable (placed.nodes <#> \n -> Tuple n.name n)
  onShelf = Map.fromFoldable (placed.isolated <#> \n -> Tuple n.name n)

  layered = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) inLayers <#> \n ->
        { index
        , node
        , box: { x: n.x0 + lead, y: topOf n.name, width: n.x1 - n.x0, height: breadthAt index }
        , layer: Just n.layer
        , words: Nothing
        , ports: []
        }
    )
    projection.nodes

  stood = strays # Array.mapMaybe \index -> Array.index projection.nodes index <#> \node ->
    { index
    , node
    , box: { x: toNumber (strayTier index) * pitch + lead, y: topOf (nameOf index), width: extent, height: breadthAt index }
    , layer: Just (strayTier index)
    , words: Nothing
    , ports: []
    }

  shelved = Array.catMaybes $ Array.mapWithIndex
    ( \index node -> Map.lookup (nameOf index) onShelf <#> \n ->
        { index, node, box: { x: n.x0 + lead, y: n.y0 + sink, width: n.x1 - n.x0, height: n.y1 - n.y0 }, layer: Nothing, words: Nothing, ports: [] }
    )
    projection.nodes

  -- ---- outlines ------------------------------------------------------------------------------
  groupName :: GroupId -> String
  groupName = home.nameOf

  outlines = if not outlined then [] else settled.frames <#> \frame ->
    let
      within = toNumber (fromMaybe 0 (Map.lookup frame.group planned.levels) + 1)
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
          { x: toNumber frame.first * pitch + lead - before
          , y: frame.top + headroom
          , width: toNumber (frame.last - frame.first) * pitch + extent + before + after
          , height: frame.bottom - frame.top
          }
      , header: case direction of
          LeftToRight -> planned.strip
          TopDown -> along.header
      , depth: frame.depth
      , parent: frame.parent
      , members: frame.members
      }

  -- ---- edges -----------------------------------------------------------------------------------
  breadthOfEdge :: Int -> Number
  breadthOfEdge place = metrics.unit * unitsAt place

  -- where each ribbon's middle is on a side of each node: the ribbons of
  -- a side from the node's start, in the order of their other ends' ranks
  stacked :: forall r. ({ from :: Int, to :: Int | r } -> Int) -> ({ from :: Int, to :: Int | r } -> Int) -> Array { from :: Int, to :: Int | r } -> Map Int Number
  stacked own other edges = Map.fromFoldable $ Array.concat $
    Array.groupAllBy (comparing _.node) (Array.mapWithIndex (\place r -> { place, node: own r, order: Tuple (rankAt (other r)) place }) edges) <#> \side ->
      let
        inOrder = Array.sortWith _.order (NonEmptyArray.toArray side)
        start = topOf (nameOf (NonEmptyArray.head side).node)
      in
        (foldl (\state e -> { at: state.at + breadthOfEdge e.place, found: Array.snoc state.found (Tuple e.place (start + state.at + breadthOfEdge e.place / 2.0)) }) { at: 0.0, found: [] } inOrder).found

  leavingAt :: Map Int Number
  leavingAt = stacked _.from _.to routable
  enteringAt :: Map Int Number
  enteringAt = stacked _.to _.from routable

  -- the middle of ribbon `place` on a box of its route
  middleOn :: Map Int Number -> Int -> String -> Number
  middleOn side place name = case laneOf name of
    Just _ -> topOf name + breadthOfEdge place / 2.0
    Nothing -> fromMaybe 0.0 (Map.lookup place side)

  shift :: Point -> Point
  shift p = { x: p.x + lead, y: p.y }

  forward = Array.catMaybes $ Array.mapWithIndex
    ( \place (Tuple r route) -> do
        spans <- for route.segments \link -> ado
          source <- findNode boxes link.sourceIndex
          target <- findNode boxes link.targetIndex
          in { from: shift { x: source.x1, y: middleOn leavingAt place source.name }, to: shift { x: target.x0, y: middleOn enteringAt place target.name } }
        pieces <- NonEmptyArray.fromArray (interleave (map (\s -> curve s.from s.to) spans) (Array.zipWith (\a b -> curve a.to b.from) spans (Array.drop 1 spans)))
        start <- Array.head spans
        edge <- Array.index projection.edges r.index
        pure
          { index: r.index
          , edge
          , route: Ribbon { from: start.from, pieces, breadth: breadthOfEdge place, ink }
          , course: Forward
          }
    )
    (Array.zip routable placed.routes)

  curve :: Point -> Point -> Piece
  curve from to = let middle = (from.x + to.x) / 2.0 in Curved { leaving: { x: middle, y: from.y }, arriving: { x: middle, y: to.y }, to }

  ink :: Ink
  ink = case widths of
    Plain -> Hairline metrics.hairline
    Sankey _ -> Band

  interleave :: forall a. Array a -> Array a -> Array a
  interleave xs ys = Array.concat (Array.zipWith (\x y -> [ x, y ]) xs ys) <> Array.drop (Array.length ys) xs

  backward = placed.backEdges # Array.mapMaybe \routed -> do
    r <- Array.index routable routed.backEdge.index
    edge <- Array.index projection.edges r.index
    let
      start = shift { x: routed.backEdge.x0, y: fromMaybe 0.0 (Map.lookup routed.backEdge.index leavingAt) }
      end = shift { x: routed.backEdge.x1, y: fromMaybe 0.0 (Map.lookup routed.backEdge.index enteringAt) }
      channel = routed.channel + sink
    pure
      { index: r.index
      , edge
      , route: Ribbon
          { from: start
          , pieces: NonEmptyArray.cons' (Curved { leaving: { x: start.x + reach, y: start.y }, arriving: { x: start.x + reach, y: channel }, to: { x: start.x, y: channel } })
              [ Straight { x: end.x, y: channel }
              , Curved { leaving: { x: end.x - reach, y: channel }, arriving: { x: end.x - reach, y: end.y }, to: end }
              ]
          , breadth: breadthOfEdge routed.backEdge.index
          , ink
          }
      , course: Backward
      }
