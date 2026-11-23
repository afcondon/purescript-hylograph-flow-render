-- | The usage example of the README, compiled and held to what the README
-- | says of it.
module Test.Render.Example
  ( bakery
  , picture
  , draw
  , kitchen
  , outlined
  , mixer
  , fixedLayout
  , drawnInside
  , fixedPicture
  , suite
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), Flow(..), GroupId(..), ItemId(..), Node, PortName(..), StepId(..), WireId(..), plain, project)
import Hylograph.Flow.Grouping (Grouping(..), collapse)
import Hylograph.Flow.Render.Engine (Engine, Layout, Picture, plainPicture, run)
import Hylograph.Flow.Render.Laws (check, layoutFaults, layoutFaultsOf)
import Hylograph.Flow.Render.Layout (layout)
import Hylograph.Flow.Render.Sankey (Widths(..), defaultMetrics, sankey)
import Hylograph.Flow.Render.Columns (columns, defaultSpacing)
import Hylograph.Flow.Render.Engine (FixedNode, PortFlow(..), Side(..))
import Hylograph.Flow.Render.Tree (Decoration, plainDecoration, tree)
import Hylograph.HATS (ElementType(Text), Tree, elem, staticStr)
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

-- The mixing step drawn by the caller: 160 by 60, its two inputs a third
-- and two thirds down its left side, its output half way down its right.
mixer :: Node -> Maybe FixedNode
mixer node = case node.label of
  "mix" -> Just
    { width: 160.0
    , height: 60.0
    , ports:
        [ { name: PortName "dry", flow: Input, side: LeftSide, along: 20.0 }
        , { name: PortName "wet", flow: Input, side: LeftSide, along: 40.0 }
        , { name: PortName "out", flow: Output, side: RightSide, along: 30.0 }
        ]
    }
  _ -> Nothing

-- The columns engine keeps that size and those ports; every edge is a
-- ribbon one pixel to a unit, never less than two pixels.
fixedLayout :: Layout Unit
fixedLayout = run (columns defaultSpacing { pixelsPerUnit: 1.0, least: 2.0, unitsOf: const 1.0 }) (plainPicture { fixed = mixer }) (project Both bakery)

-- What is inside the step is the caller's to draw, in its own coordinates.
drawnInside :: Decoration Unit
drawnInside = plainDecoration
  { inside = \node -> if node.label == "mix" then Just (elem Text [ staticStr "x" "12", staticStr "y" "34", staticStr "textContent" "flour and water, mixed" ] []) else Nothing }

fixedPicture :: Tree
fixedPicture = tree drawnInside fixedLayout

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
  let mix = Array.find (\node -> node.node.label == "mix") fixedLayout.nodes
  expect "the mixing step, fixed, is 160 by 60 with its three ports where they were fixed"
    (map (\node -> { width: node.box.width, height: node.box.height, ports: Array.length node.ports }) mix == Just { width: 160.0, height: 60.0, ports: 3 })
  expectNone "and the columns engine's layout of it is lawful" (map _.words (check (columns defaultSpacing { pixelsPerUnit: 1.0, least: 2.0, unitsOf: const 1.0 }) (plainPicture { fixed = mixer }) (project Both bakery) fixedLayout))
