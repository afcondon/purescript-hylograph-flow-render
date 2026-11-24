-- | The renderer held to its laws on flows made here for their shapes: the
-- | cases a real subject may not happen to hold, and no adapter needed. Each
-- | adapter's own tests hold the layout to the same laws on that adapter's
-- | real flows.
module Test.Render (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), ElementKey(..), ItemId(..), Projection, StepId(..), project)
import Hylograph.Flow.Laws as Flow.Laws
import Hylograph.Flow.Render.Laws (acyclic, faults, layoutFaults, varied)
import Hylograph.Flow.Render.Layout (Course(..), Metrics, Widths(..), defaultMetrics, layout)
import Hylograph.Flow.Render.Route (Route(..))
import Hylograph.Flow.Render.Tree (strokeOf)
import Test.Render.Fixtures (chain, drawings, expect, expectNone, long, quantity, sankeyMetrics, scan, subjects, unconnected, weighted)
import Test.Render.Columns as Columns
import Test.Render.Down as Down
import Test.Render.Engines as Engines
import Test.Render.Example as Example
import Test.Render.Groups as Groups
import Test.Render.Measures as Measures
import Test.Render.Tree as Tree
import Test.Render.Unfold as Unfold

held :: String -> Metrics -> Widths Number -> Projection Number -> Effect Unit
held name metrics widths p = do
  let laid = layout metrics widths p
  expectNone (name <> ": " <> show (Array.length p.nodes) <> " nodes, " <> show (Array.length p.edges) <> " edges, " <> show laid.width <> " by " <> show laid.height)
    (faults metrics widths p laid)

-- | A ribbon's breadth; nothing for a route that is no ribbon.
breadthOf :: Route -> Number
breadthOf = case _ of
  Ribbon ribbon -> ribbon.breadth
  _ -> 0.0

main :: Effect Unit
main = do
  log "render: the layout's laws on made flows"
  for_ subjects \subject -> for_ drawings \d -> do
    let p = project d.drawing subject.flow
    expectNone (subject.name <> ", " <> d.name <> ": the edges are the paths of wires between drawn elements, each carrying its wires")
      (Flow.Laws.projectionFaults d.drawing subject.flow)
    held (subject.name <> ", " <> d.name <> ", plain") defaultMetrics Plain p
    held (subject.name <> ", " <> d.name <> ", by quantity") sankeyMetrics quantity p
    held (subject.name <> ", " <> d.name <> ", widths varied from edge to edge") sankeyMetrics varied p

  log "render: particular facts"
  expect "the made flows cover both cases: chain has no cycle, scan has one"
    (acyclic (project Both chain) && not (acyclic (project Both scan)) && not (acyclic (project Holders scan)))
  let scanLaid = layout defaultMetrics Plain (project Both scan)
  expect "the scan's cycle is closed by exactly one edge turned back"
    (Array.length (Array.filter (\e -> e.course == Backward) scanLaid.edges) == 1)
  let selfLaid = layout defaultMetrics Plain (project Holders scan)
  expect "an edge from a node to itself is drawn, turned back"
    (Array.any (\e -> e.edge.from == e.edge.to && e.course == Backward) selfLaid.edges)
  let longLaid = layout defaultMetrics Plain (project Holders long)
  let boxOf label = Array.find (\n -> n.node.label == label) longLaid.nodes
  expect "the long edge a -> d does not start and end in neighbouring layers"
    ((boxOf "a" >>= _.layer) == Just 0 && (boxOf "d" >>= _.layer) == Just 3)
  let shelf = layout defaultMetrics Plain (project Both unconnected)
  expect "nodes no edge touches are placed, with no layer"
    (Array.length shelf.nodes == 7 && Array.all (\n -> n.layer == Nothing) shelf.nodes)
  -- widths, stated outright and not through the laws
  let energy = project Both weighted
  let drawn = layout sankeyMetrics quantity energy
  let at label = Array.find (\n -> n.node.label == label) drawn.nodes
  let height label = (_.box.height) <$> at label
  let nearly a b = abs (a - b) < 1.0e-6
  let strokesInto key = sum (map (strokeOf <<< _.route) (Array.filter (\e -> e.edge.to == key) drawn.edges))
  let strokesOutOf key = sum (map (strokeOf <<< _.route) (Array.filter (\e -> e.edge.from == key) drawn.edges))
  expect "by quantity: the join is 10 units tall (7 + 3 in, 10 out), at 7 pixels to the unit"
    (map (nearly (10.0 * 7.0)) (height "join") == Just true)
  expect "by quantity: the strokes drawn into the join sum to 7 + 3 units, out of it to 10"
    (nearly (strokesInto (StepKey (StepId "join"))) 70.0 && nearly (strokesOutOf (StepKey (StepId "join"))) 70.0)
  expect "by quantity: power is 10 units tall and its strokes out sum to 6 + 4 units"
    (map (nearly 70.0) (height "power") == Just true && nearly (strokesOutOf (ItemKey (ItemId "power"))) 70.0)
  let plainDrawn = layout defaultMetrics Plain energy
  expect "plain: the same picture has every edge a hairline, and the join two units tall"
    (Array.all (\e -> strokeOf e.route == defaultMetrics.hairline) plainDrawn.edges
      && map (\n -> nearly n.box.height (2.0 * defaultMetrics.unit)) (Array.find (\n -> n.node.label == "join") plainDrawn.nodes) == Just true)
  expect "a width that is not a positive number is drawn as nothing, and the layout is still lawful"
    (Array.null (faults sankeyMetrics (Sankey \_ -> toNumber 0) energy (layout sankeyMetrics (Sankey \_ -> toNumber 0) energy)))
  expect "layoutFaults is the faults of the layout this package makes"
    (layoutFaults sankeyMetrics quantity energy == faults sankeyMetrics quantity energy drawn)
  expect "the varied widths do vary: the weighted flow under Holders has edges of more than one width"
    (Array.length (Array.nub (map (breadthOf <<< _.route) (layout sankeyMetrics varied (project Holders weighted)).edges)) > 1)
  Tree.suite
  Groups.suite
  Engines.suite
  Columns.suite
  Down.suite
  Measures.suite
  Example.suite
  Unfold.suite
  log "render: all green"
