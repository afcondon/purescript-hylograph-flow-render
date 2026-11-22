-- | The usage example of the README, compiled and held to what the README
-- | says of it.
module Test.Render.Example
  ( bakery
  , picture
  , draw
  , kitchen
  , outlined
  , suite
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), Flow(..), GroupId(..), ItemId(..), PortName(..), StepId(..), WireId(..), plain, project)
import Hylograph.Flow.Grouping (Grouping(..), collapse)
import Hylograph.Flow.Render.Engine (Engine, Layout, Picture, plainPicture, run)
import Hylograph.Flow.Render.Laws (layoutFaults, layoutFaultsOf)
import Hylograph.Flow.Render.Layout (layout)
import Hylograph.Flow.Render.Sankey (Widths(..), defaultMetrics, sankey)
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

-- | The mixing is a group, and it is open: nothing is closed.
kitchen :: Array Grouping
kitchen = [ Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Holds (ItemId "flour"), Holds (ItemId "water") ] ]

-- | What any engine is told beside the projection: the group, and what is
-- | written in each node.
setting :: Picture
setting = plainPicture
  { groups = Just { grouping: kitchen, closed: Set.empty }
  , words = Just \node -> { title: node.label, detail: Just "as weighed this morning" }
  }

-- | An engine is a value. This is the Sankey engine with narrower nodes;
-- | its metrics and widths are its own, not the picture's.
narrow :: Engine Unit
narrow = sankey (defaultMetrics { nodeBreadth = 88.0 }) Plain

-- | The kitchen outlined round its three nodes, each tall enough for a
-- | title and a line under it; a click on the outline's name is told to the
-- | decoration's `notify` as `PointedGroup (GroupId "kitchen")`.
outlined :: Layout Unit
outlined = run narrow setting (project Both (collapse kitchen Set.empty bakery))

suite :: Effect Unit
suite = do
  log "render: the README's example"
  expect "the bakery is placed as four nodes and three edges"
    (Array.length placed.nodes == 4 && Array.length placed.edges == 3)
  expectNone "and its layout is lawful" (layoutFaults defaultMetrics Plain (project Both bakery))
  expect "told of the kitchen, the layout outlines it round its three nodes, each with its words on two or more lines"
    ( (outlined.outlines <#> \outline -> { name: outline.name, members: Array.length outline.members }) == [ { name: "Kitchen", members: 3 } ]
        && Array.all (\node -> (map (\words -> Array.length words.title + Array.length words.detail >= 2) node.words) == Just true) outlined.nodes
    )
  expectNone "and that layout is lawful: the general laws, and the ones the engine promises" (layoutFaultsOf narrow setting (project Both (collapse kitchen Set.empty bakery)))
