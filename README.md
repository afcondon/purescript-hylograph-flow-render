# hylograph-flow-render

One drawing for every flow.

[`hylograph-flow`](https://github.com/afcondon/purescript-hylograph-flow)
is a value for pictures of computation: items that hold values, steps that
compute, wires between them, and `project`, which turns a flow into plain
nodes and edges. This package draws that projection: a layered layout, left
to right, and an SVG of it as a
[HATS](https://github.com/afcondon/purescript-hylograph-selection) tree.
It knows nothing about where a flow came from.

![Two small spreadsheets drawn as flows: columns as open boxes, formulas as filled lozenges](docs/spreadsheet.png)

*Two small spreadsheets as flows. Columns are items (open boxes); each
column of formulas is a step (a filled lozenge), coloured and noted by the
caller's decoration; a dashed edge turns back to close a cycle.*

## What it does

- **One layout for two pictures.** Every edge has a width. `Plain` gives
  each edge one unit and draws it as a hairline: a DAG. `Sankey f` reads
  each edge's width from what its wires carry and draws it as wide as it
  is. The positions come from the same code either way, the Sankey layout
  of `hylograph-layout`.
- **Cycles and strays are placed, not refused.** An edge that closes a
  cycle (an edge from a node to itself among them) runs back under the
  picture in a channel of its own. A node no edge touches stands on a shelf
  below.
- **The dress is the caller's.** Labels, notes, classes and colours come
  from a `Decoration`; what an `Overlay` knows of an element changes its
  mark; clicks and the pointer's coming and going are reported by element.
- **Laws as a function.** `Hylograph.Flow.Render.Laws.faults` returns
  everything wrong with a layout, in words, so that you can hold the layout
  of your own flows to it in your own tests.

## Usage

```purescript
import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hylograph.Flow (Drawing(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId(..), plain, project)
import Hylograph.Flow.Render.Layout (Layout, Widths(..), defaultMetrics, layout)
import Hylograph.Flow.Render.Tree (plainDecoration, tree)
import Hylograph.HATS (Tree)
import Hylograph.HATS.InterpreterTick (rerender)

-- Flour and water are mixed into dough.
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

-- Where everything goes: items and steps both drawn, every edge a hairline.
placed :: Layout Unit
placed = layout defaultMetrics Plain (project Both bakery)

-- The picture, as a HATS tree: one SVG.
picture :: Tree
picture = tree plainDecoration placed

-- In a browser: put the picture in the element with the id `chart`.
draw :: Effect Unit
draw = void (rerender "#chart" picture)
```

This example is compiled and checked with the tests
(`test/Test/Render/Example.purs`).

For a Sankey, put a quantity on the wires (`Flow item step Number`, say)
and pass `Sankey` a function from an edge to its width. An edge carries
the wires it stands for, so the width of an edge that is one wire is
`(NonEmptyArray.head edge.wires).payload`.

## The modules

| Module | What it holds |
|---|---|
| `Hylograph.Flow.Render.Layout` | `layout :: Metrics -> Widths wire -> Projection wire -> Layout wire`. Pure, so it runs anywhere, without a DOM. |
| `Hylograph.Flow.Render.Tree` | `tree :: Decoration wire -> Layout wire -> Tree`. Items as open boxes, steps as filled lozenges. `plainDecoration`, `withOverlay`, `keyName`. |
| `Hylograph.Flow.Render.Laws` | `faults`, `layoutFaults`: what must hold of any layout, as a function that returns what does not. |

What is drawn, for a stylesheet or a test to find:

```
svg.flow
  g.flow-edges > path.flow-edge[.flow-backward][data-edge][data-from][data-to]
  g.flow-nodes > g.flow-node.flow-item|.flow-step[data-key]
                   > rect.flow-box, rect.flow-swatch?, text.flow-label, text.flow-note?
```

## Installing

```bash
spago install hylograph-flow-render
```

It depends on `hylograph-flow`, `hylograph-layout` (0.6.0 or later),
`hylograph-graph` (0.3.1 or later) and `hylograph-selection` (0.6.0 or
later), and on `arrays`, `effect`, `foldable-traversable`, `integers`,
`maybe`, `numbers`, `ordered-collections`, `prelude`, `strings` and
`tuples`.

## Running the tests

```bash
spago test
```

The suite runs in Node, with no browser. It holds the layout to its laws on
flows made for their shapes (with and without a cycle, with an edge from a
node to itself, with nodes no edge touches), plain, by quantity, and with
widths varied from edge to edge; and it reads the HATS tree back, checking
one mark for each placed node, one path for each placed edge, and the
caller's decoration, overlay and gestures on the element they were said of.

## Licence

MIT.
