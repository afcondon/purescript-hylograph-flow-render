-- | The columns engine (0.4), held to the general laws and to the four it
-- | promises, on every made flow under every drawing: with nothing fixed,
-- | and with every node's size and ports fixed by the picture. Then its
-- | widths and ports against figures worked out by hand, and two wrong
-- | columns engines, each breaking exactly one of the two laws 0.4 added.
module Test.Render.Columns (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), ElementKey(..), GroupId(..), ItemId(..), Node, PortName(..), Projection, StepId(..), project)
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Columns (columns, defaultSpacing)
import Hylograph.Flow.Render.Engine (Engine(..), FixedNode, Picture, PortFlow(..), Promise(..), Scaling, Side(..), plainPicture, portPoint, promises, run, scaledBreadth)
import Hylograph.Flow.Render.Laws (Law(..), broken, check, generalFaults, lawOf, promiseFaults)
import Hylograph.Flow.Render.Route (Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Tree (plainDecoration)
import Hylograph.HATS (ElementType(Text), elem, staticStr)
import Test.Render.Fixtures (carrying, drawings, expect, expectNone, flowOf, subjects)
import Test.Render.Realized (attribute, children, hasClass)
import Test.Render.Tree (partsOf, pictureOf, structureFaults)

-- | A tenth of a pixel to the unit, and never less than two pixels: one
-- | unit is drawn two pixels broad, a hundred units ten.
scaling :: Scaling Number
scaling = { pixelsPerUnit: 0.1, least: 2.0, unitsOf: \edge -> (NonEmptyArray.last edge.wires).payload }

engine :: Engine Number
engine = columns defaultSpacing scaling

-- | Every node fixed: 80 wide, 30 tall and 20 more for each port on its
-- | busier side; its inputs spread down its left side, its outputs down its
-- | right.
fixedAll :: Node -> Maybe FixedNode
fixedAll node =
  let
    busier = max (Array.length node.inputs) (Array.length node.outputs)
    height = 30.0 + 20.0 * toNumber busier
    spread flow side names = Array.mapWithIndex (\i name -> { name, flow, side, along: height * toNumber (i + 1) / toNumber (Array.length names + 1) }) names
  in
    Just { width: 80.0, height, ports: spread Input LeftSide node.inputs <> spread Output RightSide node.outputs }

fixing :: Picture
fixing = plainPicture { fixed = fixedAll }

-- | `join` reads a hundred units of `coal` and one of `gas` and writes a
-- | hundred and one of `power`.
joined :: Projection Number
joined = project Both (flowOf [] [ { step: "join", reads: [ carrying 100.0 "coal", carrying 1.0 "gas" ], writes: [ carrying 101.0 "power" ] } ])

-- | The columns engine with its first fixed node made two pixels taller.
taller :: Engine Number
taller = case engine of
  Engine made -> Engine made
    { name = "columns, a fixed box made taller"
    , layout = \given projection ->
        let
          laid = made.layout given projection
        in
          laid { nodes = laid.nodes <#> \placed -> if placed.index == 0 then placed { box = placed.box { height = placed.box.height + 2.0 } } else placed, height = laid.height + 2.0 }
    }

-- | The columns engine with its first ribbon drawn half as broad.
thinned :: Engine Number
thinned = case engine of
  Engine made -> Engine made
    { name = "columns, one ribbon thinned"
    , layout = \given projection ->
        let
          laid = made.layout given projection
        in
          laid
            { edges = laid.edges <#> \placed -> case placed.index, placed.route of
                0, Ribbon ribbon -> placed { route = Ribbon ribbon { breadth = ribbon.breadth / 2.0 } }
                _, _ -> placed
            }
    }

breadthOf :: Route -> Number
breadthOf = case _ of
  Ribbon ribbon -> ribbon.breadth
  _ -> 0.0

nearly :: Number -> Number -> Boolean
nearly a b = abs (a - b) < 1.0e-9

suite :: Effect Unit
suite = do
  log "render: the columns engine keeps the general laws and the four it promises"
  expect "it promises edges with the flow, layers in line, the fixed nodes kept, widths at its scale"
    (map lawOf (promises engine) == [ FlowForward, LayerAlignment, FixedKept, Scaled ])
  for_ [ { name: "nothing fixed", picture: plainPicture }, { name: "every node fixed", picture: fixing } ] \setting -> do
    let
      found = do
        subject <- subjects
        drawing <- drawings
        let projection = project drawing.drawing subject.flow
        let laid = run engine setting.picture projection
        pure (map (\fault -> subject.name <> ", " <> drawing.name <> ": " <> show fault.law <> ": " <> fault.words) (check engine setting.picture projection laid))
    expectNone (setting.name <> ": over " <> show (Array.length found) <> " layouts, no fault against a general law or a promised one") (Array.concat found)

  log "render: the columns engine's widths and ports, worked out by hand"
  let laid = run engine fixing joined
  expect "a hundred units are ten pixels broad, a hundred and one 10.1, and one unit the least, two"
    ((laid.edges <#> _.edge.from) == [ ItemKey (ItemId "coal"), ItemKey (ItemId "gas"), StepKey (StepId "join") ]
      && (map (breadthOf <<< _.route) laid.edges # \breadths -> Array.length breadths == 3 && Array.all identity (Array.zipWith nearly breadths [ 10.0, 2.0, 10.1 ])))
  expect "scaledBreadth says the same: a unit at the least, nothing at the least, a hundred at the scale"
    (scaledBreadth scaling 1.0 == 2.0 && scaledBreadth scaling 0.0 == 2.0 && scaledBreadth scaling (-3.0) == 2.0 && nearly (scaledBreadth scaling 100.0) 10.0)
  let join = Array.find (\placed -> placed.node.key == StepKey (StepId "join")) laid.nodes
  expect "the step is 80 by 70, as fixed, with its two inputs a third and two thirds down its left side and its output half way down its right"
    ( map (\placed -> { width: placed.box.width, height: placed.box.height, ports: placed.ports <#> \port -> { side: port.side, along: port.along } }) join
        == Just { width: 80.0, height: 70.0, ports: [ { side: LeftSide, along: 70.0 / 3.0 }, { side: LeftSide, along: 140.0 / 3.0 }, { side: RightSide, along: 35.0 } ] }
    )
  expect "each edge into the step ends at the port it names, and the edge out of it starts at its output"
    ( case join of
        Just placed ->
          let
            at name flow = map (portPoint placed.box) (Array.find (\port -> port.name == PortName name && port.flow == flow) placed.ports)
            ending from = map (Route.end <<< _.route) (Array.find (\e -> e.edge.from == ItemKey (ItemId from)) laid.edges)
            starting = map (Route.start <<< _.route) (Array.find (\e -> e.edge.from == StepKey (StepId "join")) laid.edges)
          in
            ending "coal" == at "coal" Input && ending "gas" == at "gas" Input && starting == at "power" Output && ending "coal" /= Nothing
        Nothing -> false
    )
  let sources = Array.filter (\placed -> placed.node.key == ItemKey (ItemId "coal") || placed.node.key == ItemKey (ItemId "gas")) laid.nodes
  expect "each item fed to the step stands in the column before it, pulled level with its port: their middles 70/3 apart, as the ports are"
    ( case sources of
        [ coal, gas ] -> coal.layer == Just 0 && gas.layer == Just 0 && abs ((gas.box.y + gas.box.height / 2.0) - (coal.box.y + coal.box.height / 2.0)) >= 70.0 / 3.0 - 1.0e-6
        _ -> false
    )

  log "render: a node nothing enters stands in the layer before the first node it feeds"
  let late = project Holders (flowOf [] [ { step: "f", reads: [ carrying 1.0 "a" ], writes: [ carrying 1.0 "b" ] }, { step: "g", reads: [ carrying 1.0 "b" ], writes: [ carrying 1.0 "c" ] }, { step: "h", reads: [ carrying 1.0 "c", carrying 1.0 "p" ], writes: [ carrying 1.0 "d" ] } ])
  let lateLaid = run engine plainPicture late
  let layerOf name = Array.find (\placed -> placed.node.key == ItemKey (ItemId name)) lateLaid.nodes >>= _.layer
  expect "a reaches d through b and c, in layers 0 to 3; p, read only where d is made, is in layer 2 beside c, not in layer 0 beside a"
    (map layerOf [ "a", "b", "c", "d", "p" ] == [ Just 0, Just 1, Just 2, Just 3, Just 2 ])
  expectNone "and the layout is lawful" (map _.words (check engine plainPicture late lateLaid))

  log "render: a wrong columns engine fails exactly the law it breaks"
  for_ [ { name: "makes a fixed box two pixels taller", made: taller, breaks: FixedKept }, { name: "draws a ribbon half as broad as its units say", made: thinned, breaks: Scaled } ] \given -> do
    let found = check given.made fixing joined (run given.made fixing joined)
    for_ found \fault -> log ("    " <> show fault.law <> ": " <> fault.words)
    expect ("a columns engine that " <> given.name <> " breaks " <> show given.breaks <> " and no other law") (broken found == [ given.breaks ])
  let unfixed = promiseFaults KeepsFixed plainPicture joined (run taller plainPicture joined)
  expect "where the picture fixes nothing, KeepsFixed asks nothing, and the taller box is no fault of it" (Array.null unfixed)

  log "render: the tree draws a node's inside where the caller draws it"
  expectNone "the tree draws the columns engine's layout as it draws any: a mark for each node and each edge, each where the layout says" (structureFaults plainDecoration laid)
  let
    written = elem Text [ staticStr "class" "mine", staticStr "textContent" "drawn by the caller" ] []
    drawing = plainDecoration { inside = \node -> if node.key == StepKey (StepId "join") then Just written else Nothing }
  case pictureOf drawing laid, join of
    Right drawn, Just placed -> do
      let mark = Array.find (\m -> attribute "data-key" m == Just "step:join") drawn.nodes
      expect "the step's mark is the caller's inside, in a group moved to the step's box, and its three port squares; no box, no label"
        ( case mark of
            Just found -> hasClass "flow-drawn" found
              && (partsOf "flow-inside" found <#> attribute "transform") == [ Just ("translate(" <> show placed.box.x <> "," <> show placed.box.y <> ")") ]
              && (partsOf "flow-inside" found >>= children <#> attribute "textContent") == [ Just "drawn by the caller" ]
              && Array.null (partsOf "flow-box" found)
              && Array.null (partsOf "flow-label" found)
              && Array.length (partsOf "flow-port" found) == 3
            Nothing -> false
        )
      expect "the other marks are drawn as before: a box each, and no inside"
        (Array.all (\m -> attribute "data-key" m == Just "step:join" || (Array.length (partsOf "flow-box" m) == 1 && Array.null (partsOf "flow-inside" m))) drawn.nodes)
    Left why, _ -> expect why false
    _, Nothing -> expect "the step is placed" false

  log "render: what the columns engine does not do"
  let grouped = plainPicture { groups = Just { grouping: [ Group (GroupId "G") "G" [ Member (StepId "join"), Holds (ItemId "coal") ] ], closed: Set.empty } }
  let outlined = check engine grouped joined (run engine grouped joined)
  expect "told of an open group it outlines nothing, and the general law Outlined says so, and nothing else"
    (broken outlined == [ Outlined ])
  let general = generalFaults plainPicture joined (run engine plainPicture joined)
  expect "with nothing fixed it sizes every node itself, and is lawful" (Array.null general)
  let total = sum (map (breadthOf <<< _.route) laid.edges)
  expect "it conserves nothing: the ribbons into the step are 12 pixels broad and the one out 10.1" (nearly total 22.1)

