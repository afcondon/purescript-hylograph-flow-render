-- | Engines as values, held to the laws by what each promises.
-- |
-- | The Sankey engine is held to the general laws and to each of its three
-- | promises, one at a time, on every made flow under every drawing.
-- |
-- | A second engine is made here by hand (`byHand`): it places four nodes
-- | where it is told, with a step's ports on its sides and each edge a
-- | straight polyline from port to port, and it has no widths at all. Sound,
-- | it breaks no law, and is not asked the Sankey engine's (asked them
-- | anyway, it fails only `Conservation`: the false faults the layout trial
-- | saw, which an engine that does not promise is now never shown). Spoiled
-- | in one way at a time (`Wrong`), it breaks exactly the law that way is
-- | against, and no other.
module Test.Render.Engines (suite, Wrong(..), byHand, small, grouped, worded) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), Flow, GroupId(..), ItemId(..), PortName(..), Projection, StepId(..), project)
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Engine (Box, Course(..), Direction(..), Engine(..), Layout, Picture, PlacedNode, PortFlow(..), Promise(..), Side(..), openPaths, plainPicture, portPoint, promises, run)
import Hylograph.Flow.Render.Laws (Law(..), broken, check, generalFaults, generalLaws, isGeneral, lawOf, promiseFaults)
import Hylograph.Flow.Render.Measures (scoreOf)
import Hylograph.Flow.Render.Route (Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Sankey (Widths(..), defaultMetrics, sankey)
import Hylograph.Flow.Render.Text (setWords)
import Hylograph.Flow.Render.Tree (plainDecoration)
import Test.Render.Fixtures (drawings, expect, expectNone, flowOf, quantity, sankeyMetrics, single, subjects)
import Test.Render.Realized (attribute)
import Test.Render.Tree (partsOf, pictureOf, structureFaults)

-- | `f` makes `b` from `a`; `lone` is touched by nothing.
small :: Flow Unit Unit Number
small = flowOf [ "lone" ] [ { step: "f", reads: [ single "a" ], writes: [ single "b" ] } ]

-- | The step and what it reads are a group, and it is open.
grouped :: Picture
grouped = plainPicture { groups = Just { grouping: [ Group (GroupId "G") "G, a group" [ Member (StepId "f"), Holds (ItemId "a") ] ], closed: Set.empty } }

worded :: Picture
worded = plainPicture { words = Just \node -> { title: node.label, detail: Nothing } }

-- | One way to be wrong, each against one law.
data Wrong
  = Sound
  | DropsAnEdge
  | NotFinite
  | TooSmall
  | Overlaps
  | StrayPort
  | MissesNode
  | MissesPort
  | NoWords
  | StrangerInOutline
  | GoesBack
  | Misaligned
  | Slanted

-- | An engine made by hand for `small` drawn `Both`: `a`, `f`, `b` in
-- | three columns and `lone` under `a`. Every node is 100 by 40; `f` has
-- | its input on its left side and its output on its right, half way down;
-- | an edge is one straight leg from its source's right side to its
-- | target's left. It promises what such a picture keeps: edges forward,
-- | layers in line, right angles. It conserves no widths and does not say
-- | it does.
byHand :: forall wire. Wrong -> Engine wire
byHand wrong = Engine
  { name: "by hand"
  , promises: [ FlowsForward, LayersAligned, RoutesOrthogonal ]
  , layout: \given projection -> spoil (sound given projection)
  }
  where
  column :: Int -> Number
  column place = 20.0 + 150.0 * toNumber place

  swapped = case wrong of
    GoesBack -> true
    _ -> false

  -- where a node stands, by its label: its column, its row and its layer
  standing :: String -> { column :: Int, row :: Int, layer :: Maybe Int }
  standing label = case label of
    "a" -> { column: 0, row: 0, layer: Just 0 }
    "f" -> if swapped then { column: 2, row: 0, layer: Just 2 } else { column: 1, row: 0, layer: Just 1 }
    "b" -> if swapped then { column: 1, row: 0, layer: Just 1 } else { column: 2, row: 0, layer: Just 2 }
    _ -> { column: 0, row: 1, layer: Nothing }

  boxOf :: String -> Box
  boxOf label = let stand = standing label in { x: column stand.column, y: 40.0 + 100.0 * toNumber stand.row, width: 100.0, height: 40.0 }

  sound :: Picture -> Projection wire -> Layout wire
  sound given projection =
    { direction: LeftToRight
    , nodes
    , edges: Array.mapWithIndex
        ( \index edge ->
            let
              from = fromMaybe { x: 0.0, y: 0.0, width: 0.0, height: 0.0 } (Map.lookup edge.from boxes)
              to = fromMaybe { x: 0.0, y: 0.0, width: 0.0, height: 0.0 } (Map.lookup edge.to boxes)
            in
              { index
              , edge
              , route: Polyline { from: { x: from.x + from.width, y: from.y + 20.0 }, through: [], to: { x: to.x, y: to.y + 20.0 }, jumps: [] }
              , course: Forward
              }
        )
        projection.edges
    , outlines
    , width: 440.0
    , height: 200.0
    }
    where
    nodes :: Array PlacedNode
    nodes = Array.mapWithIndex
      ( \index node ->
          { index
          , node
          , box: boxOf node.label
          , layer: (standing node.label).layer
          , words: given.words <#> \say -> setWords given.lettering 100.0 (say node)
          , ports: map (\name -> { name, flow: Input, side: LeftSide, along: 20.0 }) node.inputs <> map (\name -> { name, flow: Output, side: RightSide, along: 20.0 }) node.outputs
          }
      )
      projection.nodes
    boxes = Map.fromFoldable (nodes <#> \placed -> Tuple placed.node.key placed.box)
    homes = map openPaths given.groups
    pathOf placed = case homes of
      Just home -> home.pathOf placed.node.key
      Nothing -> []
    -- one level of groups is all `grouped` has: an outline is the box round
    -- its members, with the padding and the strip
    outlines = Array.nub (Array.concatMap pathOf nodes) <#> \group ->
      let
        members = Array.filter (\placed -> Array.elem group (pathOf placed)) nodes
        left = Array.foldl (\least placed -> min least placed.box.x) 1.0e9 members
        top = Array.foldl (\least placed -> min least placed.box.y) 1.0e9 members
        right = Array.foldl (\most placed -> max most (placed.box.x + placed.box.width)) (-1.0e9) members
        bottom = Array.foldl (\most placed -> max most (placed.box.y + placed.box.height)) (-1.0e9) members
      in
        { group
        , name: case homes of
            Just home -> home.nameOf group
            Nothing -> ""
        , box: { x: left - given.outline.padding, y: top - given.outline.padding - given.outline.header, width: right - left + 2.0 * given.outline.padding, height: bottom - top + 2.0 * given.outline.padding + given.outline.header }
        , header: given.outline.header
        , depth: 0
        , parent: Nothing
        , members: map _.index members
        }

  labelled label placed = placed.node.label == label

  spoil :: Layout wire -> Layout wire
  spoil laid = case wrong of
    Sound -> laid
    GoesBack -> laid
    DropsAnEdge -> laid { edges = Array.take 1 laid.edges }
    NotFinite -> laid { width = 1.0 / 0.0 }
    TooSmall -> laid { width = 300.0 }
    Overlaps -> laid { nodes = laid.nodes <#> \placed -> if labelled "lone" placed then placed { box = placed.box { x = 60.0, y = 60.0 } } else placed }
    StrayPort -> laid { nodes = laid.nodes <#> \placed -> if labelled "a" placed then placed { ports = [ { name: PortName "nonsense", flow: Input, side: LeftSide, along: 10.0 } ] } else placed }
    MissesNode -> laid { edges = laid.edges <#> \placed -> if placed.index == 0 then placed { route = moved 5.0 0.0 placed.route } else placed }
    MissesPort -> laid { edges = laid.edges <#> \placed -> if placed.index == 0 then placed { route = moved 0.0 10.0 (ending 0.0 10.0 placed.route) } else placed }
    NoWords -> laid { nodes = laid.nodes <#> _ { words = Nothing } }
    StrangerInOutline -> laid { outlines = laid.outlines <#> \outline -> outline { box = outline.box { height = 176.0 } } }
    Misaligned -> laid { nodes = laid.nodes <#> \placed -> if labelled "lone" placed then placed { box = placed.box { x = 40.0 }, layer = Just 0 } else placed }
    Slanted -> laid { edges = laid.edges <#> \placed -> if placed.index == 0 then placed { route = moved 0.0 (-10.0) placed.route } else placed }

  -- a polyline with its first point moved, or its last
  moved dx dy = case _ of
    Polyline line -> Polyline line { from = { x: line.from.x + dx, y: line.from.y + dy } }
    other -> other
  ending dx dy = case _ of
    Polyline line -> Polyline line { to = { x: line.to.x + dx, y: line.to.y + dy } }
    other -> other

-- | The same engine, promising nothing.
unpromising :: forall wire. Wrong -> Engine wire
unpromising wrong = case byHand wrong of
  Engine engine -> Engine engine { name = "by hand, promising nothing", promises = [] }

-- | The same engine, promising what the Sankey engine promises.
overpromising :: forall wire. Engine wire
overpromising = case byHand Sound of
  Engine engine -> Engine engine { name = "by hand, promising widths", promises = engine.promises <> [ WidthsConserved { pixelsPerUnit: 26.0, unitsOf: const 1.0 } ] }

-- | The Sankey engine with its first ribbon made twice as broad.
swollen :: Engine Number
swollen = case sankey sankeyMetrics quantity of
  Engine engine -> Engine engine
    { name = "sankey, one ribbon swollen"
    , layout = \given projection ->
        let
          laid = engine.layout given projection
        in
          laid
            { edges = laid.edges <#> \placed -> case placed.index, placed.route of
                0, Ribbon ribbon -> placed { route = Ribbon ribbon { breadth = 2.0 * ribbon.breadth } }
                _, _ -> placed
            }
    }

suite :: Effect Unit
suite = do
  log "render: the Sankey engine keeps the general laws and each law it promises"
  let engines = [ { name: "plain", engine: sankey defaultMetrics Plain }, { name: "by quantity", engine: sankey sankeyMetrics quantity } ]
  for_ engines \made -> do
    expect ("the Sankey engine, " <> made.name <> ", promises three laws: with the flow, layers in line, widths conserved")
      (map lawOf (promises made.engine) == [ FlowForward, LayerAlignment, Conservation ])
    let
      found = do
        subject <- subjects
        drawing <- drawings
        let projection = project drawing.drawing subject.flow
        let laid = run made.engine plainPicture projection
        pure
          { general: Array.length (generalFaults plainPicture projection laid)
          , promised: promises made.engine <#> \promise -> Array.length (promiseFaults promise plainPicture projection laid)
          }
    expect (made.name <> ": over " <> show (Array.length found) <> " layouts, no fault against a general law") (sum (map _.general found) == 0)
    expect (made.name <> ": and none against a promised law, each asked by itself") (sum (map (sum <<< _.promised) found) == 0)

  log "render: an engine made by hand, with ports and right angles and no widths"
  let both = project Both small
  let handLaid = run (byHand Sound) plainPicture both
  expect "the flow drawn Both is four nodes and two edges, each edge naming the step's port it meets"
    (Array.length both.nodes == 4 && (both.edges <#> \edge -> Tuple edge.fromPort edge.toPort) == [ Tuple Nothing (Just (PortName "a")), Tuple (Just (PortName "b")) Nothing ])
  expectNone "sound, it breaks no general law and none it promises" (map _.words (check (byHand Sound) plainPicture both handLaid))
  expectNone "sound, with words" (map _.words (check (byHand Sound) worded both (run (byHand Sound) worded both)))
  -- nothing is closed, so the flow projected is the flow itself
  let open = both
  expectNone "sound, with an open group" (map _.words (check (byHand Sound) grouped open (run (byHand Sound) grouped open)))
  expect "its step has its two ports placed, and each edge's route ends at the port the edge names"
    ( (handLaid.nodes >>= \placed -> placed.ports <#> \port -> Tuple port.name (portPoint placed.box port))
        == [ Tuple (PortName "a") { x: 170.0, y: 60.0 }, Tuple (PortName "b") { x: 270.0, y: 60.0 } ]
        && map (Route.end <<< _.route) (Array.take 1 handLaid.edges) == [ { x: 170.0, y: 60.0 } ]
        && map (Route.start <<< _.route) (Array.drop 1 handLaid.edges) == [ { x: 270.0, y: 60.0 } ]
    )
  let asSankey = check overpromising plainPicture both (run overpromising plainPicture both)
  expect ("held to the widths it never promised, the same sound layout has " <> show (Array.length asSankey) <> " faults, every one against Conservation: what the trial saw, and what an engine that does not promise is not asked")
    (not (Array.null asSankey) && broken asSankey == [ Conservation ])
  expectNone "the tree draws it: its nodes, its edges along their routes, a square for each port" (structureFaults plainDecoration handLaid)
  case pictureOf plainDecoration handLaid of
    Right drawn -> do
      expect "each edge's path is its route: one straight leg from port to port"
        (map (attribute "d") drawn.edges == [ Just "M120,60 L170,60", Just "M270,60 L320,60" ])
      expect "the step's mark has two port squares, named for the ports; an item's has none"
        ((drawn.nodes <#> \mark -> map (attribute "data-port") (partsOf "flow-port" mark)) == [ [], [], [], [ Just "a", Just "b" ] ])
    Left why -> expect why false
  let scored = scoreOf handLaid
  expect "the measures read it as any layout: two edges, 100 pixels of edge, no bend, no crossing, nothing overlapping"
    (scored.edges == 2 && scored.edgeLength == 100.0 && scored.bends == 0.0 && scored.crossings == 0 && scored.nodeOverlaps == 0 && scored.edgesThroughNodes == 0)

  log "render: a wrong engine fails exactly the law it breaks"
  let
    wrongs :: Array { name :: String, wrong :: Wrong, picture :: Picture, projection :: Projection Number, breaks :: Law }
    wrongs =
      [ { name: "drops an edge", wrong: DropsAnEdge, picture: plainPicture, projection: both, breaks: Placed }
      , { name: "says the picture is infinitely wide", wrong: NotFinite, picture: plainPicture, projection: both, breaks: Finite }
      , { name: "says the picture is narrower than what is in it", wrong: TooSmall, picture: plainPicture, projection: both, breaks: Within }
      , { name: "puts one node on another", wrong: Overlaps, picture: plainPicture, projection: both, breaks: Apart }
      , { name: "places a port its node has not", wrong: StrayPort, picture: plainPicture, projection: both, breaks: Ported }
      , { name: "starts a route five pixels off its node", wrong: MissesNode, picture: plainPicture, projection: both, breaks: Routed }
      , { name: "ends a route on the right node, ten pixels down its side from its port", wrong: MissesPort, picture: plainPicture, projection: both, breaks: Routed }
      , { name: "sets no words where the picture gives them", wrong: NoWords, picture: worded, projection: both, breaks: Lettered }
      , { name: "outlines a group round a stranger", wrong: StrangerInOutline, picture: grouped, projection: open, breaks: Outlined }
      , { name: "runs an edge against the flow", wrong: GoesBack, picture: plainPicture, projection: both, breaks: FlowForward }
      , { name: "puts a node out of its layer's line", wrong: Misaligned, picture: plainPicture, projection: both, breaks: LayerAlignment }
      , { name: "slants a leg", wrong: Slanted, picture: plainPicture, projection: both, breaks: Orthogonality }
      ]
  for_ wrongs \given -> do
    let found = check (byHand given.wrong) given.picture given.projection (run (byHand given.wrong) given.picture given.projection)
    for_ found \fault -> log ("    " <> show fault.law <> ": " <> fault.words)
    expect ("an engine that " <> given.name <> " breaks " <> show given.breaks <> " and no other law") (broken found == [ given.breaks ])
    let unasked = check (unpromising given.wrong) given.picture given.projection (run (unpromising given.wrong) given.picture given.projection)
    expect ("  promising nothing, it " <> (if isGeneral given.breaks then "breaks it all the same: the law is general" else "breaks nothing: the law is asked only of an engine that promises it"))
      (broken unasked == (if isGeneral given.breaks then [ given.breaks ] else []))
  expect "between them the wrong engines break every general law and three of the four promised"
    (Array.nub (Array.sort (map _.breaks wrongs)) == Array.sort (generalLaws <> [ FlowForward, LayerAlignment, Orthogonality ]))
  let weighted = project Both (flowOf [] [ { step: "join", reads: [ { item: "coal", carries: 7.0 }, { item: "gas", carries: 3.0 } ], writes: [ { item: "power", carries: 10.0 } ] } ])
  let swollenFaults = check swollen plainPicture weighted (run swollen plainPicture weighted)
  for_ swollenFaults \fault -> log ("    " <> show fault.law <> ": " <> fault.words)
  expect "the fourth: the Sankey engine with one ribbon made twice as broad breaks Conservation and no other law"
    (broken swollenFaults == [ Conservation ])
