-- | The measures of legibility (0.7), each held to a drawing small enough
-- | to work out on paper, and what part B draws.
module Test.Render.Legible (suite) where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow.Render.Measures (Drawn, crossings)
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

suite :: Effect Unit
suite = do
  log "render: crossings, mended (0.7): a crossing at a point where both lines are cut into pieces"
  expect "two neighbours that change places between two tiers cross once" (crossings changingPlaces == 1)
  expect "the same two going straight on do not cross" (crossings goingStraight == 0)
  expect "two lines that turn on one point and go back do not cross" (crossings turningOnOnePoint == 0)
  expect "a line through the corner of another crosses it once" (crossings throughACorner == 1)
  expect "a line that turns on another's straight piece and goes back does not cross it" (crossings (turningOnALine false) == 0)
  expect "a line that turns on another's straight piece and goes on through crosses it once" (crossings (turningOnALine true) == 1)
