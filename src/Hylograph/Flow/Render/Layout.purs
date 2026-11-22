-- | The door 0.2 came in by: `layout metrics widths projection`, which is
-- | the Sankey engine run on a picture with no words and no groups, and the
-- | names 0.2 imported from here.
-- |
-- | Since 0.3 the layout is pluggable, and this module holds none of it:
-- |
-- | - `Hylograph.Flow.Render.Engine` is what any engine makes (`Layout`,
-- |   placed nodes with their ports, placed edges with their routes,
-- |   outlines, the direction), what every engine is told (`Picture`), and
-- |   an engine as a value (`Engine`, `Promise`, `run`);
-- | - `Hylograph.Flow.Render.Route` is an edge's route as data;
-- | - `Hylograph.Flow.Render.Sankey` is the Sankey engine, with `Metrics`
-- |   and `Widths` as its own configuration.
-- |
-- | Everything here is one of theirs under the name it had, so that code
-- | written for 0.2 that calls `layout` and reads boxes still compiles. For
-- | words, open groups or another engine: `run engine picture projection`.
module Hylograph.Flow.Render.Layout
  ( layout
  , module Exports
  ) where

import Hylograph.Flow (Projection)
import Hylograph.Flow.Render.Engine (Box, Course(..), Direction(..), Engine(..), Groups, Layout, OutlineMetrics, Picture, PlacedEdge, PlacedNode, PlacedOutline, PlacedPort, Point, PortFlow(..), Promise(..), Side(..), defaultOutline, openPaths, plainPicture, portPoint, run) as Exports
import Hylograph.Flow.Render.Engine (Layout, plainPicture, run)
import Hylograph.Flow.Render.Sankey (Metrics, Widths(..), defaultMetrics, sankey, widthOf) as Exports
import Hylograph.Flow.Render.Sankey (Metrics, Widths, sankey)

-- | The Sankey engine's layout of a projection with no words set and no
-- | group outlined: `run (sankey metrics widths) plainPicture`.
layout :: forall wire. Metrics -> Widths wire -> Projection wire -> Layout wire
layout metrics widths = run (sankey metrics widths) plainPicture
