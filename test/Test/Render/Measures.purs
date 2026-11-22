-- | Routes and measures held to figures worked out by hand: a route's
-- | points, length, bends and SVG path, and the scoreboard's measures on
-- | drawings small enough to count on paper (the layout trial's own, kept).
module Test.Render.Measures (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), project)
import Hylograph.Flow.Render.Engine (Direction(..), Engine(..), Layout, plainPicture, run)
import Hylograph.Flow.Render.Laws (check)
import Hylograph.Flow.Render.Measures (Drawn, bends, crossings, drawnOf, edgeLength, edgesThroughNodes, extent, nodeOverlaps, outlineTrespasses, score, scoreOf, strangersIn, unplaced, unrouted)
import Hylograph.Flow.Render.Route (Ink(..), Piece(..), Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Sankey (Widths(..), defaultMetrics, sankey)
import Hylograph.Flow.Render.Tree (plainDecoration)
import Test.Render.Engines (Wrong(..), byHand, small)
import Test.Render.Fixtures (expect, expectNone, scan)
import Test.Render.Realized (attribute)
import Test.Render.Tree (partsOf, pictureOf, structureFaults)

near :: Number -> Number -> Boolean
near a b = abs (a - b) < 1.0e-6

-- ---------------------------------------------------------------------------
-- routes made by hand
-- ---------------------------------------------------------------------------

-- right along 100, then down 50: one corner
ell :: Route
ell = Polyline { from: { x: 0.0, y: 0.0 }, through: [ { x: 100.0, y: 0.0 } ], to: { x: 100.0, y: 50.0 }, jumps: [] }

-- the same, hopping over an edge it crosses half way along its first leg
hopping :: Route
hopping = Polyline { from: { x: 0.0, y: 0.0 }, through: [ { x: 100.0, y: 0.0 } ], to: { x: 100.0, y: 50.0 }, jumps: [ { x: 50.0, y: 0.0 } ] }

-- an S from one level to another, then straight on: a Sankey ribbon's shape
ribbon :: Route
ribbon = Ribbon
  { from: { x: 0.0, y: 0.0 }
  , pieces: NonEmptyArray.cons' (Curved { leaving: { x: 50.0, y: 0.0 }, arriving: { x: 50.0, y: 100.0 }, to: { x: 100.0, y: 100.0 } }) [ Straight { x: 200.0, y: 100.0 } ]
  , breadth: 14.0
  , ink: Band
  }

-- the Sankey engine's turned-back edge: a curve, a line, a curve
turnedBack :: Route
turnedBack = Ribbon
  { from: { x: 10.0, y: 0.0 }
  , pieces: NonEmptyArray.cons' (Curved { leaving: { x: 40.0, y: 0.0 }, arriving: { x: 40.0, y: 50.0 }, to: { x: 10.0, y: 50.0 } })
      [ Straight { x: -90.0, y: 50.0 }
      , Curved { leaving: { x: -120.0, y: 50.0 }, arriving: { x: -120.0, y: 0.0 }, to: { x: -90.0, y: 0.0 } }
      ]
  , breadth: 26.0
  , ink: Hairline 1.25
  }

-- two cubic pieces end to end, as dot writes an edge
spline :: Route
spline = Spline
  { from: { x: 0.0, y: 0.0 }
  , curves: NonEmptyArray.cons' { leaving: { x: 10.0, y: 0.0 }, arriving: { x: 20.0, y: 10.0 }, to: { x: 30.0, y: 10.0 } } [ { leaving: { x: 40.0, y: 10.0 }, arriving: { x: 50.0, y: 0.0 }, to: { x: 60.0, y: 0.0 } } ]
  }

-- a cubic piece with all four points on one line
level :: Route
level = Spline { from: { x: 0.0, y: 5.0 }, curves: NonEmptyArray.singleton { leaving: { x: 50.0, y: 5.0 }, arriving: { x: 50.0, y: 5.0 }, to: { x: 100.0, y: 5.0 } } }

-- ---------------------------------------------------------------------------
-- drawings made by hand (the layout trial's)
-- ---------------------------------------------------------------------------

box :: Number -> Number -> { x :: Number, y :: Number, width :: Number, height :: Number }
box x y = { x, y, width: 20.0, height: 20.0 }

-- four nodes at the corners of a square; the two diagonals cross once
diagonals :: Drawn
diagonals =
  { nodes: [ box 0.0 0.0, box 100.0 100.0, box 0.0 100.0, box 100.0 0.0 ]
  , edges:
      [ { from: 0, to: 1, points: [ { x: 20.0, y: 20.0 }, { x: 100.0, y: 100.0 } ] }
      , { from: 2, to: 3, points: [ { x: 20.0, y: 100.0 }, { x: 100.0, y: 20.0 } ] }
      ]
  , outlines: []
  }

-- one edge with two right-angled corners, past a node that is not its end
-- and through an outline it has no end in
dogLeg :: Drawn
dogLeg =
  { nodes: [ box 0.0 0.0, box 200.0 100.0, box 90.0 0.0, box 300.0 300.0 ]
  , edges: [ { from: 0, to: 1, points: [ { x: 20.0, y: 10.0 }, { x: 150.0, y: 10.0 }, { x: 150.0, y: 110.0 }, { x: 200.0, y: 110.0 } ] } ]
  , outlines: [ { box: { x: 80.0, y: -10.0, width: 40.0, height: 40.0 }, members: [ 2 ] }, { box: { x: 190.0, y: 90.0, width: 40.0, height: 40.0 }, members: [ 1 ] } ]
  }

-- two edges that leave one point and part: they touch and do not cross;
-- and one that runs along the first for a way: a shared trunk
fan :: Drawn
fan =
  { nodes: [ box 0.0 0.0, box 100.0 0.0, box 100.0 50.0, box 100.0 100.0 ]
  , edges:
      [ { from: 0, to: 1, points: [ { x: 20.0, y: 10.0 }, { x: 100.0, y: 10.0 } ] }
      , { from: 0, to: 2, points: [ { x: 20.0, y: 10.0 }, { x: 60.0, y: 10.0 }, { x: 60.0, y: 60.0 }, { x: 100.0, y: 60.0 } ] }
      , { from: 0, to: 3, points: [ { x: 20.0, y: 10.0 }, { x: 60.0, y: 10.0 }, { x: 60.0, y: 110.0 }, { x: 100.0, y: 110.0 } ] }
      ]
  , outlines: []
  }

suite :: Effect Unit
suite = do
  log "render: a route is data; its points, its length, its bends and its path are read from it"
  expect "an L-shaped route has one bend, is 150 long, and starts and ends where it says"
    (near (Route.bends ell) 1.0 && near (Route.length ell) 150.0 && Route.start ell == { x: 0.0, y: 0.0 } && Route.end ell == { x: 100.0, y: 50.0 })
  expect "its points are its three corners" (Route.flatten ell == [ { x: 0.0, y: 0.0 }, { x: 100.0, y: 0.0 }, { x: 100.0, y: 50.0 } ])
  expect "its path is two straight legs" (Route.svgPath ell == "M0,0 L100,0 L100,50")
  expect "a jump is drawn as a half circle on the leg it is on, and is not measured: the same length, the same one bend"
    (Route.svgPath hopping == "M0,0 L47,0 A3,3 0 0 1 53,0 L100,0 L100,50" && near (Route.length hopping) 150.0 && near (Route.bends hopping) 1.0 && Route.flatten hopping == Route.flatten ell)
  expect "a jump too near a corner to hop is passed straight through"
    (Route.svgPath (Polyline { from: { x: 0.0, y: 0.0 }, through: [], to: { x: 100.0, y: 0.0 }, jumps: [ { x: 1.0, y: 0.0 }, { x: 50.0, y: 20.0 } ] }) == "M0,0 L100,0")
  expect "a ribbon's path is its centre line: a curve, then a line" (Route.svgPath ribbon == "M0,0 C50,0 50,100 100,100 L200,100")
  expect "a curve is twelve straight pieces from its start to its end, and the line after it one more"
    (Array.length (Route.flatten ribbon) == 14 && Array.head (Route.flatten ribbon) == Just { x: 0.0, y: 0.0 } && Array.index (Route.flatten ribbon) 12 == Just { x: 100.0, y: 100.0 } && Array.last (Route.flatten ribbon) == Just { x: 200.0, y: 100.0 })
  expect "an S that leaves level, reaches its steepest and comes back level turns as far one way as the other; the ribbon is longer than the straight line between its ends"
    (Route.bends ribbon > 0.9 && Route.bends ribbon < 2.0 && Route.length ribbon > 241.4 && Route.end ribbon == { x: 200.0, y: 100.0 })
  expect "the Sankey engine's turned-back edge (curve, line, curve) is read whole: 26 points, a full turn and more"
    (Array.length (Route.flatten turnedBack) == 26 && Route.svgPath turnedBack == "M10,0 C40,0 40,50 10,50 L-90,50 C-120,50 -120,0 -90,0" && Route.end turnedBack == { x: -90.0, y: 0.0 })
  expect "a spline's path is its cubic pieces end to end" (Route.svgPath spline == "M0,0 C10,0 20,10 30,10 C40,10 50,0 60,0" && Array.length (Route.flatten spline) == 25 && Route.end spline == { x: 60.0, y: 0.0 })
  expect "a curve with all its points on one line is one straight piece, with no bend" (Route.flatten level == [ { x: 0.0, y: 5.0 }, { x: 100.0, y: 5.0 } ] && Route.bends level == 0.0 && Route.length level == 100.0)

  log "render: the measures, on drawings counted by hand"
  expect "two diagonals cross once" (crossings diagonals == 1)
  expect "a diagonal of a square of 80 is 80 root 2 long, and two are twice that" (near (edgeLength diagonals) (2.0 * 80.0 * 1.4142135623730951))
  expect "a straight edge has no bend" (near (bends diagonals) 0.0)
  expect "an edge with two right-angled corners has two bends" (near (bends dogLeg) 2.0)
  expect "its length is the sum of its three pieces" (near (edgeLength dogLeg) 280.0)
  expect "it passes through the one node on its way that is not its end" (edgesThroughNodes dogLeg == 1)
  expect "it trespasses on the outline it has no end in, and not on the one its end is in" (outlineTrespasses dogLeg == 1)
  expect "one node is in an outline that is not its own: the edge's target is not, the node the edge passes is a member" (strangersIn dogLeg == 0 && strangersIn (dogLeg { outlines = [ { box: { x: 80.0, y: -10.0, width: 40.0, height: 40.0 }, members: [] } ] }) == 1)
  expect "edges that leave one point, and edges that share a trunk, do not cross" (crossings fan == 0)
  expect "no two boxes of the fan overlap" (nodeOverlaps fan == 0)
  expect "two boxes with a corner in common overlap" (nodeOverlaps (fan { nodes = [ box 0.0 0.0, box 10.0 10.0 ] }) == 1)
  expect "two boxes that touch along a side do not" (nodeOverlaps (fan { nodes = [ box 0.0 0.0, box 20.0 0.0 ] }) == 0)
  expect "the extent is the box round nodes and edges" ((extent diagonals) == { x: 0.0, y: 0.0, width: 120.0, height: 120.0 })
  expect "an edge with no points is not routed" (unrouted (diagonals { edges = [ { from: 0, to: 1, points: [] } ] }) == 1)
  expect "score is every measure at once" (let found = score dogLeg in found.nodes == 4 && found.edges == 1 && found.crossings == 0 && near found.bends 2.0 && found.edgesThroughNodes == 1 && found.outlineTrespasses == 1)

  log "render: the measures read any engine's layout, by its routes"
  let both = project Both small
  let handLaid = run (byHand Sound) plainPicture both
  -- the two straight edges of the hand-made layout, crossed: each made to
  -- end where the other did, one of them by way of an L
  let
    crossed :: Layout Number
    crossed = handLaid
      { edges = handLaid.edges <#> \placed -> case placed.index of
          0 -> placed { route = Polyline { from: { x: 120.0, y: 60.0 }, through: [ { x: 300.0, y: 60.0 } ], to: { x: 300.0, y: 180.0 }, jumps: [] } }
          _ -> placed { route = Polyline { from: { x: 270.0, y: 100.0 }, through: [], to: { x: 320.0, y: 100.0 }, jumps: [] } }
      }
  expect "an L of 180 and 120 crossed by a straight edge of 50: one crossing, one bend, 350 pixels of edge"
    (let found = scoreOf crossed in found.crossings == 1 && near found.bends 1.0 && near found.edgeLength 350.0 && found.unrouted == 0)
  expect "a layout with an edge left out has one edge unplaced; a whole one has none"
    (unplaced both (run (byHand DropsAnEdge) plainPicture both) == 1 && unplaced both handLaid == 0)
  let cyclic = project Both scan
  let sankeyLaid = run (sankey defaultMetrics Plain) plainPicture cyclic
  expect "the Sankey engine's layout of a scan is scored from its ribbons: every edge routed, curves that bend, one edge turned back and longer than any other"
    ( let found = scoreOf sankeyLaid
      in found.edges == Array.length cyclic.edges && found.unrouted == 0 && found.bends > 2.0 && found.nodeOverlaps == 0
        && Array.all (\points -> Array.length points >= 2) (map _.points (drawnOf sankeyLaid).edges)
    )

  log "render: the tree draws either direction"
  -- the hand-made layout stood on end: x for y in every box and route, as
  -- an engine that runs top-down would give it
  let
    stood :: Layout Number
    stood = handLaid
      { direction = TopDown
      , width = handLaid.height + 60.0
      , height = handLaid.width
      , nodes = handLaid.nodes <#> \placed -> placed { box = { x: placed.box.y, y: placed.box.x, width: placed.box.height + 60.0, height: 40.0 }, ports = [], layer = Nothing }
      , edges = handLaid.edges <#> \placed -> placed
          { route = case placed.index of
              0 -> Spline { from: { x: 90.0, y: 60.0 }, curves: NonEmptyArray.singleton { leaving: { x: 90.0, y: 100.0 }, arriving: { x: 90.0, y: 130.0 }, to: { x: 90.0, y: 170.0 } } }
              _ -> Spline { from: { x: 90.0, y: 210.0 }, curves: NonEmptyArray.singleton { leaving: { x: 90.0, y: 250.0 }, arriving: { x: 90.0, y: 280.0 }, to: { x: 90.0, y: 320.0 } } }
          }
      }
  expectNone "a top-down layout of splines breaks no general law" (map _.words (check (unpromised (byHand Sound)) plainPicture both stood))
  expectNone "and the tree draws it: its marks are the layout's" (structureFaults plainDecoration stood)
  case pictureOf plainDecoration stood, pictureOf plainDecoration handLaid of
    Right down, Right across -> do
      expect "the root says which way the flow runs" (attribute "data-direction" down.root == Just "top-down" && attribute "data-direction" across.root == Just "left-to-right")
      expect "top-down, a node's label is set about its middle; left to right, from its left side"
        ( Array.all (\mark -> map (attribute "text-anchor") (partsOf "flow-label" mark) == [ Just "middle" ]) down.nodes
            && (Array.zipWith (\mark placed -> map (attribute "x") (partsOf "flow-label" mark) == [ Just (show (placed.box.x + placed.box.width / 2.0)) ]) down.nodes stood.nodes) == [ true, true, true, true ]
            && Array.all (\mark -> map (attribute "text-anchor") (partsOf "flow-label" mark) == [ Just "start" ]) across.nodes
        )
      expect "a spline is drawn along its curve, as a hairline" (map (attribute "d") down.edges == [ Just "M90,60 C90,100 90,130 90,170", Just "M90,210 C90,250 90,280 90,320" ] && map (attribute "stroke-width") down.edges == [ Just "1.25", Just "1.25" ])
    Left why, _ -> expect why false
    _, Left why -> expect why false
  where
  -- an engine with its promises taken away: held to the general laws alone
  unpromised engine = case engine of
    Engine made -> Engine made { promises = [] }
