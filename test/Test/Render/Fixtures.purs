-- | Flows made here for their shapes: with and without a cycle, with an edge
-- | from a node to itself, with nodes no edge touches, with quantities on
-- | the wires. Nothing here comes from any adapter's subject.
module Test.Render.Fixtures
  ( flowOf
  , single
  , carrying
  , chain
  , long
  , scan
  , tangled
  , ouroboros
  , weighted
  , unconnected
  , nothing
  , subjects
  , drawings
  , sankeyMetrics
  , quantity
  , expect
  , expectNone
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId(..), stepLabel)
import Hylograph.Flow.Render.Layout (Metrics, Widths(..), defaultMetrics)

expect :: String -> Boolean -> Effect Unit
expect label ok = if ok then log ("  ok: " <> label) else throw ("FAILED: " <> label)

-- | Holds where there is nothing to report; says what there was otherwise.
expectNone :: String -> Array String -> Effect Unit
expectNone label found = do
  for_ found \fault -> log ("    fault: " <> fault)
  expect label (Array.null found)

-- | A flow from a list of steps, each reading some items and writing some,
-- | every wire carrying a number.
flowOf :: Array String -> Array { step :: String, reads :: Array { item :: String, carries :: Number }, writes :: Array { item :: String, carries :: Number } } -> Flow Unit Unit Number
flowOf lone steps = Flow
  { items: Array.nub (lone <> Array.concatMap (\s -> map _.item s.reads <> map _.item s.writes) steps) <#> \i -> { id: ItemId i, name: Just i, count: 1, payload: unit }
  , steps: steps <#> \s -> { id: StepId s.step, name: Just s.step, inputs: map (PortName <<< _.item) s.reads, outputs: map (PortName <<< _.item) s.writes, payload: unit }
  , wires: steps # Array.concatMap \s ->
      (s.reads <#> \r -> { id: Reads (ItemId r.item) (StepId s.step) (PortName r.item), payload: r.carries })
        <> (s.writes <#> \w -> { id: Writes (StepId s.step) (PortName w.item) (ItemId w.item), payload: w.carries })
  }

single :: String -> { item :: String, carries :: Number }
single item = { item, carries: 1.0 }

carrying :: Number -> String -> { item :: String, carries :: Number }
carrying carries item = { item, carries }

chain :: Flow Unit Unit Number
chain = flowOf []
  [ { step: "f", reads: [ single "a" ], writes: [ single "b" ] }
  , { step: "g", reads: [ single "b" ], writes: [ single "c" ] }
  ]

-- | `c` is made from `a` directly and from `b`, which is made from `a`: an
-- | edge that spans a layer and has to pass `b`.
long :: Flow Unit Unit Number
long = flowOf []
  [ { step: "f", reads: [ single "a" ], writes: [ single "b" ] }
  , { step: "g", reads: [ single "b" ], writes: [ single "c" ] }
  , { step: "h", reads: [ single "a", single "c" ], writes: [ single "d" ] }
  ]

-- | A scan: `total` is read by the step that writes it.
scan :: Flow Unit Unit Number
scan = flowOf []
  [ { step: "add", reads: [ single "amount", single "total" ], writes: [ single "total" ] }
  , { step: "report", reads: [ single "total" ], writes: [ single "page" ] }
  ]

-- | A longer cycle, two cycles sharing a node, and a part with no cycle.
tangled :: Flow Unit Unit Number
tangled = flowOf [ "alone", "apart" ]
  [ { step: "f", reads: [ single "a", single "d" ], writes: [ single "b" ] }
  , { step: "g", reads: [ single "b" ], writes: [ single "c" ] }
  , { step: "h", reads: [ single "c" ], writes: [ single "d" ] }
  , { step: "k", reads: [ single "c", single "e" ], writes: [ single "e" ] }
  , { step: "m", reads: [ single "x" ], writes: [ single "y" ] }
  ]

-- | Nothing but a step that reads what it writes.
ouroboros :: Flow Unit Unit Number
ouroboros = flowOf [] [ { step: "again", reads: [ single "it" ], writes: [ single "it" ] } ]

-- | Quantities: 7 and 3 are joined into 10, which is split 6 and 4.
weighted :: Flow Unit Unit Number
weighted = flowOf []
  [ { step: "join", reads: [ carrying 7.0 "coal", carrying 3.0 "gas" ], writes: [ carrying 10.0 "power" ] }
  , { step: "homes", reads: [ carrying 6.0 "power" ], writes: [ carrying 6.0 "heat" ] }
  , { step: "works", reads: [ carrying 4.0 "power" ], writes: [ carrying 4.0 "goods" ] }
  ]

unconnected :: Flow Unit Unit Number
unconnected = flowOf [ "a", "b", "c", "d", "e", "f", "g" ] []

nothing :: Flow Unit Unit Number
nothing = flowOf [] []

-- | The three fixed drawings and two chosen ones: every item and step whose
-- | id sorts no later than "c", and every item and step but those.
drawings :: Array { name :: String, drawing :: Drawing }
drawings =
  [ { name: "Both", drawing: Both }
  , { name: "Holders", drawing: Holders }
  , { name: "Steps", drawing: Steps }
  , { name: "Chosen a to c", drawing: Chosen early }
  , { name: "Chosen the rest", drawing: Chosen (not <<< early) }
  ]
  where
  early = case _ of
    ItemKey (ItemId name) -> name <= "c"
    StepKey step -> stepLabel step <= "c"
    WireKey _ -> false

subjects :: Array { name :: String, flow :: Flow Unit Unit Number }
subjects =
  [ { name: "chain", flow: chain }
  , { name: "long edge", flow: long }
  , { name: "scan", flow: scan }
  , { name: "tangled", flow: tangled }
  , { name: "ouroboros", flow: ouroboros }
  , { name: "weighted", flow: weighted }
  , { name: "unconnected", flow: unconnected }
  , { name: "nothing", flow: nothing }
  ]

-- | Tall enough to see, small enough that a hundred units fit a screen.
sankeyMetrics :: Metrics
sankeyMetrics = defaultMetrics { unit = 7.0 }

-- | The width of an edge that carries numbers: the last wire's.
quantity :: Widths Number
quantity = Sankey \edge -> (NonEmptyArray.last edge.wires).payload
