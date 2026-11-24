-- | The measures of legibility (0.7), each held to a drawing small enough
-- | to work out on paper, and what part B draws.
module Test.Render.Legible (suite) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Data.Number (abs)
import Hylograph.Flow.Render.Engine (Direction(..))
import Hylograph.Flow.Render.Measures (Band, Banded, Drawn, againstTheFlow, crossings, overlaps, overlapsInOneColour, pictureProportion, proportions, squat, thresholds)
import Hylograph.Flow (ElementKey(..), GroupId(..), ItemId(..), PortName(..), Projection, StepId(..), WireId(..))
import Hylograph.Flow.Render.Unfold (gathered)
import Hylograph.Flow.Grouping (Grouping(..))
import Hylograph.Flow.Render.Signal (coloured, palette, shade)
import Hylograph.Flow.Render.Tree (plainDecoration)
import Hylograph.Flow.Render.Route (Ink(..), Piece(..), Point, Route(..), flatten)
import Test.Render.Fixtures (expect)

-- | The unfolding engine's ribbon between two tiers: one cubic piece that
-- | leaves and arrives along the flow.
ess :: Point -> Point -> Number -> Route
ess from to breadth =
  let middle = (from.x + to.x) / 2.0
  in Ribbon { from, pieces: NonEmptyArray.singleton (Curved { leaving: { x: middle, y: from.y }, arriving: { x: middle, y: to.y }, to }), breadth, ink: Band }

box :: Number -> Number -> { x :: Number, y :: Number, width :: Number, height :: Number }
box x y = { x, y, width: 10.0, height: 10.0 }

-- two neighbours of one tier that change places on the way to the next:
-- node 0 over node 1 on the left, their targets 2 and 3 the other way up.
-- The two curves are mirror images and cross at the middle of both, which
-- is where both are cut into pieces.
changingPlaces :: Drawn
changingPlaces =
  { nodes: [ box 0.0 0.0, box 0.0 100.0, box 110.0 100.0, box 110.0 0.0 ]
  , edges:
      [ { from: 0, to: 2, points: flatten (ess { x: 10.0, y: 5.0 } { x: 110.0, y: 105.0 } 10.0) }
      , { from: 1, to: 3, points: flatten (ess { x: 10.0, y: 105.0 } { x: 110.0, y: 5.0 } 10.0) }
      ]
  , outlines: []
  }

-- the same two, each going straight on: they never meet
goingStraight :: Drawn
goingStraight = changingPlaces
  { edges =
      [ { from: 0, to: 3, points: flatten (ess { x: 10.0, y: 5.0 } { x: 110.0, y: 5.0 } 10.0) }
      , { from: 1, to: 2, points: flatten (ess { x: 10.0, y: 105.0 } { x: 110.0, y: 105.0 } 10.0) }
      ]
  }

-- two lines that turn on one point and go back the way they came: a touch
turningOnOnePoint :: Drawn
turningOnOnePoint =
  { nodes: []
  , edges:
      [ { from: -1, to: -1, points: [ { x: 0.0, y: 0.0 }, { x: 50.0, y: 50.0 }, { x: 100.0, y: 0.0 } ] }
      , { from: -1, to: -1, points: [ { x: 0.0, y: 100.0 }, { x: 50.0, y: 50.0 }, { x: 100.0, y: 100.0 } ] }
      ]
  , outlines: []
  }

-- a line that passes through the corner of another: once
throughACorner :: Drawn
throughACorner =
  { nodes: []
  , edges:
      [ { from: -1, to: -1, points: [ { x: 0.0, y: 0.0 }, { x: 50.0, y: 50.0 }, { x: 100.0, y: 0.0 } ] }
      , { from: -1, to: -1, points: [ { x: 50.0, y: 0.0 }, { x: 50.0, y: 100.0 } ] }
      ]
  , outlines: []
  }

-- a line that turns on the middle of another's straight piece and goes
-- back: a touch; and one that goes on through it: once
turningOnALine :: Boolean -> Drawn
turningOnALine through =
  { nodes: []
  , edges:
      [ { from: -1, to: -1, points: [ { x: 0.0, y: 50.0 }, { x: 100.0, y: 50.0 } ] }
      , { from: -1, to: -1, points: [ { x: 20.0, y: 0.0 }, { x: 50.0, y: 50.0 }, { x: 80.0, y: if through then 100.0 else 0.0 } ] }
      ]
  , outlines: []
  }

-- ---------------------------------------------------------------------------
-- ribbons made by hand, for the measures of legibility
-- ---------------------------------------------------------------------------

near :: Number -> Number -> Boolean
near a b = abs (a - b) < 1.0e-6

band :: Int -> String -> Number -> Array Point -> Band
band edge colour breadth points = { edge, from: -1, to: -1, colour, breadth, ink: breadth, swept: true, points }

picture :: Array Band -> Banded
picture bands = { direction: LeftToRight, width: 300.0, height: 100.0, bands }

-- a level band 20 broad, from 0 to 100 along the flow at 50 across it: its
-- ink is the rectangle from 40 to 60
level :: String -> Band
level colour = band 0 colour 20.0 [ { x: 0.0, y: 50.0 }, { x: 100.0, y: 50.0 } ]

-- a band 20 broad that falls 100 in a run of 100: swept, its ink is the
-- parallelogram between y = x - 10 and y = x + 10. With the level band it
-- has in common the points where |y - 50| <= 10 and |y - x| <= 10: a
-- parallelogram 20 along the flow and 20 across it, 400 square pixels.
falling :: String -> Band
falling colour = band 1 colour 20.0 [ { x: 0.0, y: 0.0 }, { x: 100.0, y: 100.0 } ]

-- a level band beside the first, touching it all the way: from 60 to 80
beside :: String -> Band
beside colour = band 2 colour 20.0 [ { x: 0.0, y: 70.0 }, { x: 100.0, y: 70.0 } ]

-- a level band half over the first: from 50 to 70, so 10 by 100 in common
halfOver :: String -> Band
halfOver colour = band 3 colour 20.0 [ { x: 0.0, y: 60.0 }, { x: 100.0, y: 60.0 } ]

-- the same falling band drawn as a stroke along its line: square to the
-- line, so across the flow it is 20 times the square root of 2 broad, and
-- with the level band (a strip 20 deep) it has 20 * 20 * sqrt 2 in common
stroked :: String -> Band
stroked colour = (falling colour) { swept = false }

-- a ribbon that goes out 30, comes back 130 under itself, and turns in:
-- the unfolding engine's turned-back ribbon, squared off
turnedBack :: Band
turnedBack = (band 4 "ink" 6.0 [ { x: 200.0, y: 10.0 }, { x: 230.0, y: 10.0 }, { x: 230.0, y: 90.0 }, { x: 100.0, y: 90.0 }, { x: 100.0, y: 10.0 }, { x: 130.0, y: 10.0 } ]) { swept = false }

-- ---------------------------------------------------------------------------
-- parallel ribbons from a closed group
-- ---------------------------------------------------------------------------

-- a closed group, Process, fed by the intake and losing to one sink by
-- three of its members; and two substances from the intake to the group
losing :: Projection Number
losing =
  { nodes: map node [ process, StepKey (StepId "intake"), StepKey (StepId "loss") ]
  , edges:
      [ edge (StepKey (StepId "intake")) process "ore" 6.0
      , edge process (StepKey (StepId "loss")) "crusher's loss" 2.0
      , edge (StepKey (StepId "intake")) process "water" 4.0
      , edge process (StepKey (StepId "loss")) "washer's loss" 2.5
      , edge process (StepKey (StepId "loss")) "dryer's loss" 1.5
      ]
  }
  where
  process = StepKey (GroupStep (GroupId "Process"))
  node key = { key, label: show key, inputs: [], outputs: [] }
  stepOf = case _ of
    StepKey step -> step
    _ -> StepId "?"
  edge from to item carries =
    { from
    , fromPort: Just (PortName item)
    , to
    , toPort: Just (PortName item)
    , via: [ ItemKey (ItemId item) ]
    , wires: NonEmptyArray.cons' { id: Writes (stepOf from) (PortName item) (ItemId item), payload: carries } [ { id: Reads (ItemId item) (stepOf to) (PortName item), payload: carries } ]
    }

-- what an edge's reads carry, added up: a width that adds
readsOf :: forall r. { wires :: NonEmptyArray.NonEmptyArray { id :: WireId, payload :: Number } | r } -> Number
readsOf found = Array.foldl (\total wire -> case wire.id of
  Reads _ _ _ -> total + wire.payload
  _ -> total) 0.0 (NonEmptyArray.toArray found.wires)

-- a flow for colour: two sources in a group, one loose source, one sink
tinted :: Projection Number
tinted =
  { nodes: map node [ "coal", "gas", "wind", "grid" ]
  , edges: [ edge "coal" "grid", edge "gas" "grid", edge "wind" "grid" ]
  }
  where
  node name = { key: StepKey (StepId name), label: name, inputs: [], outputs: [] }
  edge from to =
    { from: StepKey (StepId from), fromPort: Nothing, to: StepKey (StepId to), toPort: Nothing, via: []
    , wires: NonEmptyArray.singleton { id: Writes (StepId from) (PortName "out") (ItemId (from <> to)), payload: 1.0 }
    }

suite :: Effect Unit
suite = do
  log "render: colour by default (0.7)"
  let
    dressed = coloured [ Group (GroupId "Fossil") "Fossil" [ Member (StepId "coal"), Member (StepId "gas") ] ] tinted plainDecoration
    nodeColour name = (dressed.node { key: StepKey (StepId name), label: name, inputs: [], outputs: [] }).colour
    edgeColours = map (\e -> (dressed.edge e).colour) tinted.edges
  expect "the palette is five hues, no two alike" (Array.length palette == 5 && Array.length (Array.nub palette) == 5)
  expect "a shade is the colour mixed with white or with black: half way to white, half way to black, and itself"
    (shade 0.5 "#204060" == "#90a0b0" && shade (-0.5) "#204060" == "#102030" && shade 0.0 "#204060" == "#204060")
  expect "a group's two parts are two shades of the first hue, the darker first; the loose source is the second hue; the sink, which gives nothing, is left in the ink"
    (nodeColour "coal" == (shade (-0.3) <$> Array.index palette 0) && nodeColour "gas" == (shade 0.45 <$> Array.index palette 0) && nodeColour "wind" == Array.index palette 1 && nodeColour "grid" == Nothing)
  expect "a ribbon takes the colour of where it comes from" (edgeColours == map nodeColour [ "coal", "gas", "wind" ])
  expect "the closed group's bar is the group's hue itself"
    ((dressed.node { key: StepKey (GroupStep (GroupId "Fossil")), label: "Fossil", inputs: [], outputs: [] }).colour == Array.index palette 0)

  log "render: parallel ribbons from a closed group are one ribbon (0.7)"
  let found = gathered losing
  expect "the three ribbons from Process to the loss become one, in the place of the first; the two into Process, which come from an open node, stay two"
    (map (\e -> { from: e.from, to: e.to }) found.edges == map (\e -> { from: e.from, to: e.to }) (Array.take 3 losing.edges))
  expect "the one ribbon stands for all six wires and the three items, in order, names no port, and carries the six that the three carried"
    ( (Array.index found.edges 1 <#> \e -> { wires: NonEmptyArray.length e.wires, via: e.via, ports: [ e.fromPort, e.toPort ], carries: readsOf e })
        == Just { wires: 6, via: map (ItemKey <<< ItemId) [ "crusher's loss", "washer's loss", "dryer's loss" ], ports: [ Nothing, Nothing ], carries: 6.0 }
    )
  expect "what the flow carries in all is what it carried" (Array.foldl (\total e -> total + readsOf e) 0.0 found.edges == Array.foldl (\total e -> total + readsOf e) 0.0 losing.edges)
  expect "a projection with nothing to gather is itself, and gathering twice is gathering once"
    (let again = gathered found in map (\e -> NonEmptyArray.length e.wires) again.edges == map (\e -> NonEmptyArray.length e.wires) found.edges && Array.length again.edges == 3)

  log "render: the measures of legibility (0.7), on ribbons made by hand"
  log ("  thresholds: " <> show thresholds)
  let
    crossed = overlaps (picture [ level "red", falling "red" ])
    areaOf found = Array.head found <#> _.area
  expect "a level band and a falling band, each 20 broad, cross once and have 400 square pixels in common"
    (map (\found -> { one: found.one, other: found.other, crossings: found.crossings }) crossed == [ { one: 0, other: 1, crossings: 1 } ] && map (near 400.0) (areaOf crossed) == pure true)
  expect "in one colour, that is one overlap a reader cannot follow" (Array.length (overlapsInOneColour (picture [ level "red", falling "red" ])) == 1)
  expect "in two colours, it is an overlap still, and none in one colour"
    (Array.length (overlaps (picture [ level "red", falling "blue" ])) == 1 && Array.null (overlapsInOneColour (picture [ level "red", falling "blue" ])))
  expect "two bands side by side that touch all the way do not overlap" (Array.null (overlaps (picture [ level "red", beside "red" ])))
  expect "a band half over another, never crossing it, overlies it: 1000 square pixels, no crossing"
    (let found = overlaps (picture [ level "red", halfOver "red" ]) in map (near 1000.0) (areaOf found) == pure true && map _.crossings found == [ 0 ])
  expect "four bands: every pair overlaps but the level one and the one beside it, which touch (the one half over the first is half over the one beside it too)"
    (map (\found -> [ found.one, found.other ]) (overlaps (picture [ level "a", falling "a", beside "a", halfOver "a" ])) == [ [ 0, 1 ], [ 0, 3 ], [ 1, 2 ], [ 1, 3 ], [ 2, 3 ] ])
  expect "a stroked band is square to its line: 20 broad on a slope of one in one it covers 400 times the square root of 2 of the level band"
    (map (near (400.0 * 1.4142135623730951)) (areaOf (overlaps (picture [ level "red", stroked "red" ]))) == pure true)
  let
    shapes = proportions (picture
      [ band 0 "a" 60.0 [ { x: 0.0, y: 50.0 }, { x: 100.0, y: 50.0 } ]
      , band 1 "a" 10.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 90.0 } ]
      , band 2 "a" 10.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 30.0 } ]
      , band 3 "a" 50.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 10.0 } ]
      ])
  expect "a ribbon's run, breadth and travel are read from its line: 100 along, 60 broad, level; 100 along, 10 broad, 80 across; 100, 10, 20; 100, 50, level"
    (map (\found -> [ found.run, found.breadth, found.travel ]) shapes == [ [ 100.0, 60.0, 0.0 ], [ 100.0, 10.0, 80.0 ], [ 100.0, 10.0, 20.0 ], [ 100.0, 50.0, 0.0 ] ])
  expect "the ribbons under 2 to 1: the one too broad for its run (1.67 to 1) and the one too steep (1.25 to 1); not the one at 5 to 1, nor the one at exactly 2 to 1"
    (map _.edge (squat (picture
      [ band 0 "a" 60.0 [ { x: 0.0, y: 50.0 }, { x: 100.0, y: 50.0 } ]
      , band 1 "a" 10.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 90.0 } ]
      , band 2 "a" 10.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 30.0 } ]
      , band 3 "a" 50.0 [ { x: 0.0, y: 10.0 }, { x: 100.0, y: 10.0 } ]
      ])) == [ 0, 1 ])
  expect "a picture 300 along the flow and 100 across it is 3 to 1; stood on end, the same picture is 3 to 1 still"
    (near 3.0 (pictureProportion (picture [])) && near 3.0 (pictureProportion { direction: TopDown, width: 100.0, height: 300.0, bands: [] }))
  expect "of a forward ribbon and one that turns back under itself, only the second runs against the flow"
    (againstTheFlow (picture [ level "a", turnedBack ]) == [ 4 ])
  expect "the turned-back ribbon's run is all its line reaches along the flow (130), its travel all it reaches across (80)"
    (map (\found -> [ found.run, found.travel ]) (proportions (picture [ turnedBack ])) == [ [ 130.0, 80.0 ] ])

  log "render: crossings, mended (0.7): a crossing at a point where both lines are cut into pieces"
  expect "two neighbours that change places between two tiers cross once" (crossings changingPlaces == 1)
  expect "the same two going straight on do not cross" (crossings goingStraight == 0)
  expect "two lines that turn on one point and go back do not cross" (crossings turningOnOnePoint == 0)
  expect "a line through the corner of another crosses it once" (crossings throughACorner == 1)
  expect "a line that turns on another's straight piece and goes back does not cross it" (crossings (turningOnALine false) == 0)
  expect "a line that turns on another's straight piece and goes on through crosses it once" (crossings (turningOnALine true) == 1)
