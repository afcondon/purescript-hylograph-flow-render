-- | The usage example of the README, compiled and held to what the README
-- | says of it.
module Test.Render.Example
  ( bakery
  , picture
  , draw
  , suite
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId(..), plain, project)
import Hylograph.Flow.Render.Laws (layoutFaults)
import Hylograph.Flow.Render.Layout (Layout, Widths(..), defaultMetrics, layout)
import Hylograph.Flow.Render.Tree (plainDecoration, tree)
import Hylograph.HATS (Tree)
import Hylograph.HATS.InterpreterTick (rerender)
import Test.Render.Fixtures (expect, expectNone)

-- | Flour and water are mixed into dough.
bakery :: Flow Unit Unit Unit
bakery = Flow
  { items: [ item "flour", item "water", item "dough" ]
  , steps: [ { id: StepId "mix", name: Just "mix", inputs: [ PortName "dry", PortName "wet" ], outputs: [ PortName "out" ], payload: unit } ]
  , wires: map plain
      [ Reads (ItemId "flour") (StepId "mix") (PortName "dry")
      , Reads (ItemId "water") (StepId "mix") (PortName "wet")
      , Writes (StepId "mix") (PortName "out") (ItemId "dough")
      ]
  }
  where
  item name = { id: ItemId name, name: Just name, count: 1, payload: unit }

-- | Where everything goes: items and steps both drawn, every edge a hairline.
placed :: Layout Unit
placed = layout defaultMetrics Plain (project Both bakery)

-- | The picture, as a HATS tree: one SVG.
picture :: Tree
picture = tree plainDecoration placed

-- | In a browser: put the picture in the element with the id `chart`.
draw :: Effect Unit
draw = void (rerender "#chart" picture)

suite :: Effect Unit
suite = do
  log "render: the README's example"
  expect "the bakery is placed as four nodes and three edges"
    (Array.length placed.nodes == 4 && Array.length placed.edges == 3)
  expectNone "and its layout is lawful" (layoutFaults defaultMetrics Plain (project Both bakery))
