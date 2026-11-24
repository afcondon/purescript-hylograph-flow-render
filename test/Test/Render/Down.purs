-- | The Sankey engine with the flow running top-down (0.5).
-- |
-- | The same engine, the same three promises, read by the layout's
-- | direction: so the first thing held here is every made flow, under every
-- | drawing and every choice of widths, and every grouping of
-- | `Test.Render.Groups` with and without words, laid out top-down and
-- | held to the general laws and the engine's promises, exactly as the
-- | left-to-right layouts are.
-- |
-- | Then what top-down means, stated outright and not through the laws:
-- | that the layout is the left-to-right one stood on end where nothing
-- | asks for more (no words, no groups); that it runs down the page and
-- | takes no margin it does not use; that a node's words make it broader
-- | and never narrower than its ribbons; that an outline's strip is at its
-- | top; and that a top-down layout spoiled in one way fails the law that
-- | way is against, so the laws are seen to bite in this direction too.
module Test.Render.Down (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (all, for_, maximum, minimum, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Hylograph.Flow (Drawing(..), ElementKey(..), GroupId(..), ItemId(..), StepId(..), project)
import Hylograph.Flow.Grouping (Grouping(..), collapse)
import Hylograph.Flow.Render.Engine (Course(..), Direction(..), Engine, Layout, Picture, defaultOutline, engineName, plainPicture, run)
import Hylograph.Flow.Render.Laws (Law(..), broken, check, faultsOf, strangers, varied)
import Hylograph.Flow.Render.Route (Piece(..), Route(..))
import Hylograph.Flow.Render.Route as Route
import Hylograph.Flow.Render.Sankey (Metrics, Widths(..), defaultMetrics, sankey, sankeyToward)
import Hylograph.Flow.Render.Text (breadthOf, defaultLettering, heightOf)
import Hylograph.Flow.Render.Tree (Bands(..), Pointed(..), plainDecoration)
import Test.Render.Fixtures (chain, drawings, expect, expectNone, quantity, sankeyMetrics, scan, subjects, weighted)
import Test.Render.Groups as Groups
import Test.Render.Realized (attribute, hasClass)
import Test.Render.Tree (pictureOf, structureFaults)

down :: forall wire. Metrics -> Widths wire -> Engine wire
down = sankeyToward TopDown

nearly :: Number -> Number -> Boolean
nearly a b = abs (a - b) < 1.0e-6

breadthOfRoute :: Route -> Number
breadthOfRoute = case _ of
  Ribbon ribbon -> ribbon.breadth
  _ -> 0.0

suite :: Effect Unit
suite = do
  log "render: the Sankey engine top-down keeps the general laws and each law it promises"
  for_ subjects \subject -> for_ drawings \d -> do
    let p = project d.drawing subject.flow
    for_
      [ { name: "plain", engine: down defaultMetrics Plain }
      , { name: "by quantity", engine: down sankeyMetrics quantity }
      , { name: "widths varied from edge to edge", engine: down sankeyMetrics varied }
      ]
      \given -> do
        let laid = run given.engine plainPicture p
        expectNone (subject.name <> ", " <> d.name <> ", " <> given.name <> ", top-down: " <> show (Array.length p.nodes) <> " nodes, " <> show (Array.length p.edges) <> " edges, " <> show laid.width <> " by " <> show laid.height)
          (faultsOf given.engine plainPicture p laid)
        expectNone (subject.name <> ", " <> d.name <> ", " <> given.name <> ", top-down: the tree has the nodes and edges the layout placed") (structureFaults plainDecoration laid)

  log "render: top-down, open groups are outlined and words are set, lawfully"
  totals <- Ref.new { layouts: 0, outlines: 0, strangers: 0 }
  for_ Groups.cases \given -> do
    let coarser = collapse given.grouping given.closed Groups.works
    for_ drawings \d -> do
      let projection = project d.drawing coarser
      let
        told :: Picture
        told = plainPicture { groups = Just { grouping: given.grouping, closed: given.closed } }

        pictures :: Array { name :: String, engine :: Engine (Array Number), picture :: Picture }
        pictures =
          [ { name: "plain", engine: down defaultMetrics Plain, picture: told }
          , { name: "by quantity", engine: down sankeyMetrics Groups.quantity, picture: told }
          , { name: "plain, with words", engine: down (defaultMetrics { nodeBreadth = 96.0 }) Plain, picture: told { words = Just Groups.wordsOf } }
          , { name: "by quantity, with words", engine: down sankeyMetrics Groups.quantity, picture: told { words = Just Groups.wordsOf } }
          , { name: "by quantity, with words, layers too close for a whole strip", engine: down (sankeyMetrics { layerGap = 30.0 }) Groups.quantity, picture: told { words = Just Groups.wordsOf } }
          ]
      for_ pictures \p -> do
        let laid = run p.engine p.picture projection
        Ref.modify_ (\t -> { layouts: t.layouts + 1, outlines: t.outlines + Array.length laid.outlines, strangers: t.strangers + Array.length (strangers laid) }) totals
        expectNone (given.name <> ", " <> d.name <> ", " <> p.name <> ", top-down: " <> show (Array.length laid.nodes) <> " nodes, " <> show (Array.length laid.outlines) <> " outlines, " <> show laid.width <> " by " <> show laid.height)
          (faultsOf p.engine p.picture projection laid)
        expectNone (given.name <> ", " <> d.name <> ", " <> p.name <> ", top-down: the tree has the outlines, nodes and edges the layout placed") (structureFaults plainDecoration laid)
  counted <- Ref.read totals
  expect ("top-down, over " <> show counted.layouts <> " layouts with " <> show counted.outlines <> " outlines, " <> show counted.strangers <> " nodes are in an outline that is not theirs")
    (counted.strangers == 0 && counted.outlines > 0)

  log "render: top-down is left to right stood on end, where nothing asks for more"
  expect "the engine says which way it runs, in its name and in its layouts"
    ( engineName (down defaultMetrics Plain :: Engine Number) == "sankey, top-down" && engineName (sankey defaultMetrics Plain :: Engine Number) == "sankey"
        && (run (down defaultMetrics Plain) plainPicture (project Both chain)).direction == TopDown
        && (run (sankeyToward LeftToRight defaultMetrics Plain) plainPicture (project Both chain)).direction == LeftToRight
    )
  expect "sankeyToward LeftToRight is sankey: the same layout of every made flow, to the number"
    ( all identity do
        subject <- subjects
        d <- drawings
        let p = project d.drawing subject.flow
        let a = run (sankey sankeyMetrics quantity) plainPicture p
        let b = run (sankeyToward LeftToRight sankeyMetrics quantity) plainPicture p
        pure (map _.box a.nodes == map _.box b.nodes && map _.route a.edges == map _.route b.edges && a.width == b.width && a.height == b.height)
    )
  for_ subjects \subject -> for_ drawings \d -> do
    let p = project d.drawing subject.flow
    let flat = run (sankey sankeyMetrics quantity) plainPicture p
    let stood = run (down sankeyMetrics quantity) plainPicture p
    expect (subject.name <> ", " <> d.name <> ": every node is as far across the flow, and as broad across it, top-down as left to right; every ribbon as broad; and the picture as broad as the other is tall")
      ( all identity (Array.zipWith (\a b -> nearly a.box.y b.box.x && nearly a.box.height b.box.width && a.layer == b.layer) flat.nodes stood.nodes)
          && Array.length flat.nodes == Array.length stood.nodes
          && map (breadthOfRoute <<< _.route) flat.edges == map (breadthOfRoute <<< _.route) stood.edges
          && map _.course flat.edges == map _.course stood.edges
          && nearly flat.height stood.width
      )
  expect "a route stood on end twice is the route it was, and its path has x for y"
    ( let
        ribbon = Ribbon { from: { x: 1.0, y: 2.0 }, pieces: NonEmptyArray.cons' (Curved { leaving: { x: 3.0, y: 2.0 }, arriving: { x: 3.0, y: 8.0 }, to: { x: 5.0, y: 8.0 } }) [ Straight { x: 9.0, y: 8.0 } ], breadth: 4.0, ink: Route.Band }
        line = Polyline { from: { x: 0.0, y: 1.0 }, through: [ { x: 4.0, y: 1.0 } ], to: { x: 4.0, y: 7.0 }, jumps: [ { x: 2.0, y: 1.0 } ] }
      in
        Route.transpose (Route.transpose ribbon) == ribbon && Route.transpose (Route.transpose line) == line
          && Route.svgPath (Route.transpose ribbon) == "M2,1 C2,3 8,3 8,5 L8,9"
          && Route.end (Route.transpose line) == { x: 7.0, y: 4.0 }
    )

  log "render: top-down, particular facts"
  let energy = project Both weighted
  let drawn = run (down sankeyMetrics quantity) plainPicture energy
  let at label = Array.find (\n -> n.node.label == label) drawn.nodes
  expect "by quantity: the join is 10 units broad (7 + 3 in, 10 out) at 7 pixels to the unit, and restHeight tall, as every node is"
    ( map (\n -> nearly n.box.width 70.0) (at "join") == Just true
        && all (\n -> nearly n.box.height sankeyMetrics.restHeight) drawn.nodes
    )
  expect "every edge runs down the page: it leaves the bottom of its source and enters the top of its target, lower"
    ( all identity $ drawn.edges <#> \e ->
        let
          from = Array.find (\n -> n.node.key == e.edge.from) drawn.nodes
          to = Array.find (\n -> n.node.key == e.edge.to) drawn.nodes
        in
          case from, to of
            Just a, Just b -> nearly (Route.start e.route).y (a.box.y + a.box.height) && nearly (Route.end e.route).y b.box.y && b.box.y > a.box.y + a.box.height
            _, _ -> false
    )
  expect "the layers are rows: the nodes of a layer have one top, and a later layer's is lower by the row's height and the gap"
    ( let
        topOf layer = Array.nub (Array.mapMaybe (\n -> if n.layer == Just layer then Just n.box.y else Nothing) drawn.nodes)
      in
        topOf 0 == [ 0.0 ] && topOf 1 == [ sankeyMetrics.restHeight + sankeyMetrics.layerGap ] && topOf 2 == [ 2.0 * (sankeyMetrics.restHeight + sankeyMetrics.layerGap) ]
    )
  expect "with no edge turned back and no outline the picture takes no margin: it begins at its first row and ends at its last"
    ( minimum (map _.box.y drawn.nodes) == Just 0.0
        && maximum (map (\n -> n.box.y + n.box.height) drawn.nodes) == Just drawn.height
        && all (\e -> e.course == Forward) drawn.edges
    )
  let cyclic = run (down defaultMetrics Plain) plainPicture (project Both scan)
  expect "with an edge turned back there is room above the first row and below the last for it to turn: half the gap between layers"
    ( Array.length (Array.filter (\e -> e.course == Backward) cyclic.edges) == 1
        && minimum (map _.box.y cyclic.nodes) == Just (defaultMetrics.layerGap / 2.0)
        && map (\bottom -> nearly (cyclic.height - bottom) (defaultMetrics.layerGap / 2.0)) (maximum (map (\n -> n.box.y + n.box.height) cyclic.nodes)) == Just true
    )

  log "render: top-down, words make a node broader, and every node as tall as the tallest words"
  let worded = plainPicture { words = Just Groups.wordsOf }
  let narrow = down (sankeyMetrics { nodeBreadth = 96.0 }) quantity
  let lettered = run narrow worded energy
  expectNone "the weighted flow with words, top-down, is lawful" (faultsOf narrow worded energy lettered)
  expect "a node whose ribbons are narrower than its words is as broad as its widest line needs, and no line is wrapped to more than nodeBreadth"
    ( all identity $ lettered.nodes <#> \n -> case n.words of
        Just set ->
          let
            ribbons side = sum (map (breadthOfRoute <<< _.route) (Array.filter side lettered.edges))
            wider = max (ribbons (\e -> e.edge.to == n.node.key)) (ribbons (\e -> e.edge.from == n.node.key))
          in
            nearly n.box.width (max wider (breadthOf defaultLettering set)) && breadthOf defaultLettering set <= 96.0 + 1.0e-6
        Nothing -> false
    )
  expect "some node is broader for its words and some for its ribbons, so both cases are held"
    ( Array.any (\n -> map (\set -> n.box.width > breadthOf defaultLettering set + 1.0) n.words == Just true) lettered.nodes
        && Array.any (\n -> map (\set -> nearly n.box.width (breadthOf defaultLettering set)) n.words == Just true) lettered.nodes
    )
  expect "every node is as tall as the tallest words of any, so a layer is one row"
    ( let tallest = fromMaybe 0.0 (maximum (Array.mapMaybe (\n -> heightOf defaultLettering <$> n.words) lettered.nodes))
      in tallest > 0.0 && all (\n -> nearly n.box.height tallest) lettered.nodes
    )
  expect "a ribbon stands in the middle of a node made broader for its words"
    ( all identity $ lettered.edges <#> \e -> case Array.find (\n -> n.node.key == e.edge.from) lettered.nodes of
        Just n | Array.length (Array.filter (\other -> other.edge.from == n.node.key) lettered.edges) == 1 -> nearly (Route.start e.route).x (n.box.x + n.box.width / 2.0)
        _ -> true
    )
  case pictureOf plainDecoration lettered of
    Right picture -> expect "the tree says top-down, and sets every line about its node's middle"
      (attribute "data-direction" picture.root == Just "top-down")
    Left why -> expect why false

  log "render: top-down, an outline's strip is at its top"
  let item = Holds <<< ItemId
  let step = Member <<< StepId
  let nested = { grouping: [ Group (GroupId "Outer") "Outer, a group" [ Group (GroupId "Inner") "Inner, a group" [ step "g", item "c" ], step "f", item "b" ] ], closed: Set.empty }
  let nestedProjection = project Both (collapse nested.grouping nested.closed Groups.works)
  let nestedPicture = plainPicture { groups = Just nested }
  let nestedLaid = run (down defaultMetrics Plain) nestedPicture nestedProjection
  expectNone "a group in a group, top-down, is lawful" (faultsOf (down defaultMetrics Plain) nestedPicture nestedProjection nestedLaid)
  expect "each outline has the whole strip the picture asks for, above its first row, and its members begin under it"
    ( Array.length nestedLaid.outlines == 2
        && all
          ( \outline -> outline.header == defaultOutline.header
              && all (\index -> map (\n -> n.box.y >= outline.box.y + outline.header + defaultOutline.padding - 1.0e-6) (Array.index nestedLaid.nodes index) == Just true) outline.members
          )
          nestedLaid.outlines
    )
  expect "the inner outline is under the outer's strip, with the outer's padding between"
    ( case nestedLaid.outlines of
        [ outer, inner ] -> inner.box.y >= outer.box.y + outer.header + defaultOutline.padding - 1.0e-6 && inner.box.y + inner.box.height <= outer.box.y + outer.box.height - defaultOutline.padding + 1.0e-6
        _ -> false
    )
  -- `works` has a cycle, so its picture has the room an edge turns in; a
  -- chain has none, and its picture only what its outline takes
  let one = { grouping: [ Group (GroupId "G") "G, a group" [ step "f", item "b" ] ], closed: Set.empty }
  let oneLaid = run (down defaultMetrics Plain) (plainPicture { groups = Just one }) (project Both chain)
  expectNone "a chain with one group, top-down, is lawful" (faultsOf (down defaultMetrics Plain) (plainPicture { groups = Just one }) (project Both chain) oneLaid)
  expect "with no edge turned back the picture has room above its first row for the outline and no more: the first row is the padding, the strip and two pixels down, and the last row the padding and two pixels from the bottom"
    ( minimum (map _.box.y oneLaid.nodes) == Just (defaultOutline.padding + defaultOutline.header + 2.0)
        && map (\bottom -> nearly (oneLaid.height - bottom) (defaultOutline.padding + 2.0)) (maximum (map (\n -> n.box.y + n.box.height) oneLaid.nodes)) == Just true
    )
  let tight = run (down (defaultMetrics { layerGap = 30.0 }) Plain) nestedPicture nestedProjection
  expect "where the layers are too close for it, the strip and the padding are less, in proportion, and the layout says how much strip there is"
    (all (\outline -> outline.header < defaultOutline.header && outline.header > 0.0) tight.outlines && Array.null (faultsOf (down (defaultMetrics { layerGap = 30.0 }) Plain) nestedPicture nestedProjection tight))

  log "render: a top-down layout spoiled fails the law it breaks"
  let engine = down sankeyMetrics quantity
  let brokenBy change = broken (check engine plainPicture energy (change drawn))
  let onNode label change (laid :: Layout Number) = laid { nodes = laid.nodes <#> \n -> if n.node.label == label then change n else n }
  expect "sound, it breaks none" (brokenBy identity == [])
  expect "a node made narrower than its ribbons breaks Conservation and nothing else"
    (brokenBy (onNode "works" \n -> n { box = n.box { width = n.box.width - 9.0 } }) == [ Conservation ])
  expect "a ribbon drawn narrower than its quantity breaks Conservation and nothing else"
    ( brokenBy
        ( \laid -> laid
            { edges = laid.edges <#> \e -> if e.index == 0 then e { route = case e.route of
                Ribbon ribbon -> Ribbon ribbon { breadth = ribbon.breadth / 2.0 }
                other -> other } else e
            }
        ) == [ Conservation ]
    )
  expect "a node moved down out of its row breaks LayerAlignment (and, its edges no longer meeting it, Routed)"
    (brokenBy (onNode "works" \n -> n { box = n.box { y = n.box.y + 11.0 } }) == [ Routed, LayerAlignment ])
  expect "the same layout said to run left to right breaks FlowForward and LayerAlignment: the laws do read the direction"
    (let found = brokenBy (_ { direction = LeftToRight }) in Array.elem FlowForward found && Array.elem LayerAlignment found)

  log "render: buttons for the keyboard"
  let
    pressable = plainDecoration
      { button = case _ of
          PointedGroup (GroupId name) -> Just ("Close " <> name)
          PointedElement (StepKey (StepId name)) -> Just ("Open " <> name)
          _ -> Nothing
      }
  let said name marks = map (attribute name) marks
  case pictureOf pressable oneLaid, pictureOf plainDecoration oneLaid of
    Right with, Right without -> do
      expect "a mark the decoration calls a button can be focused, is a button, and is named as the decoration names it; the others are as they were"
        ( said "aria-label" with.outlines == [ Just "Close G" ]
            && said "tabindex" with.outlines == [ Just "0" ] && said "role" with.outlines == [ Just "button" ]
            && Array.sort (Array.catMaybes (said "aria-label" with.nodes)) == [ "Open f", "Open g" ]
            && Array.length (Array.filter (_ == Just "0") (said "tabindex" with.nodes)) == 2
            && all (_ == Nothing) (said "tabindex" with.edges)
        )
      expect "a plain decoration makes no mark a button"
        (all (_ == Nothing) (said "tabindex" (without.outlines <> without.nodes <> without.edges)) && all (_ == Nothing) (said "role" (without.outlines <> without.nodes <> without.edges)))
    Left why, _ -> expect why false
    _, Left why -> expect why false
  expectNone "and the tree with buttons still has the marks the layout placed" (structureFaults pressable oneLaid)

  log "render: top-down, the layers are straightened after room is made"
  -- how far, in all, the edges into each layer are from straight, each
  -- weighed by its breadth: along the flow's cross axis, by direction
  let
    slantInto :: (Route.Point -> Number) -> Layout Number -> Int -> Number
    slantInto crossOf laid layer = sum $ laid.edges <#> \e ->
      case Array.find (\n -> n.node.key == e.edge.to) laid.nodes >>= _.layer of
        Just found | found == layer -> breadthOfRoute e.route * (crossOf (Route.start e.route) - crossOf (Route.end e.route))
        _ -> 0.0
  let layersOf laid = Array.filter (_ > 0) (Array.nub (Array.mapMaybe _.layer laid.nodes))
  expect "with words, top-down: into every layer after the first, the edges' slants, weighed by breadth, add to nothing"
    (Array.length (layersOf lettered) == 4 && all (\layer -> abs (slantInto _.x lettered layer) < 1.0e-6) (layersOf lettered))
  let flatLettered = run (sankey (sankeyMetrics { nodeBreadth = 96.0 }) quantity) worded energy
  expect "left to right the same picture is as it was before 0.5, its layers not straightened: some layer's edges all slant one way"
    (Array.any (\layer -> abs (slantInto _.y flatLettered layer) > 1.0) (layersOf flatLettered))
  expect "and with no words and no groups nothing was moved to make room, so nothing is straightened (the layout is the other stood on end, as held above)"
    (map _.box.x drawn.nodes == map _.box.y (run (sankey sankeyMetrics quantity) plainPicture energy).nodes)
  expect "the layers an outline spans move as one: a group across three layers, with words, is lawful and its members keep their places to one another"
    ( let
        spanning = { grouping: [ Group (GroupId "Span") "Span, a group" [ item "a", item "d", step "h" ] ], closed: Set.empty }
        projection = project Both (collapse spanning.grouping spanning.closed Groups.works)
        picture = plainPicture { groups = Just spanning, words = Just Groups.wordsOf }
        engine = down sankeyMetrics Groups.quantity
      in
        Array.null (faultsOf engine picture projection (run engine picture projection))
    )

  log "render: a band swept across the flow"
  expect "the area a ribbon sweeps is its centre line set half its breadth either side, across the flow: flat at both ends"
    ( let
        ribbon = Ribbon { from: { x: 10.0, y: 0.0 }, pieces: NonEmptyArray.singleton (Curved { leaving: { x: 10.0, y: 20.0 }, arriving: { x: 30.0, y: 20.0 }, to: { x: 30.0, y: 40.0 } }), breadth: 8.0, ink: Route.Band }
      in
        Route.sweptPath { x: 1.0, y: 0.0 } ribbon == "M6,0 C6,20 26,20 26,40 L34,40 C34,20 14,20 14,0 Z"
          && Route.sweptPath { x: 0.0, y: 1.0 } (Route.transpose ribbon) == "M0,6 C20,6 20,26 40,26 L40,34 C20,34 20,14 0,14 Z"
          && Route.sweptPath { x: 1.0, y: 0.0 } (Polyline { from: { x: 0.0, y: 0.0 }, through: [], to: { x: 0.0, y: 9.0 }, jumps: [] }) == "M0,0 L0,9"
    )
  let sweeping = plainDecoration { bands = Swept }
  case pictureOf sweeping drawn, pictureOf plainDecoration drawn of
    Right with, Right without -> do
      expect "asked for, every band is drawn as the area it sweeps: filled, not stroked, marked flow-swept, along the path the route gives"
        ( all identity (Array.zipWith (\mark placed -> hasClass "flow-swept" mark && attribute "stroke" mark == Just "none" && attribute "fill" mark /= Just "none" && attribute "d" mark == Just (Route.sweptPath { x: 1.0, y: 0.0 } placed.route)) with.edges drawn.edges)
            && Array.length with.edges == Array.length drawn.edges
        )
      expect "not asked for, a band is a stroked line, as it was" (all (\mark -> not (hasClass "flow-swept" mark) && attribute "fill" mark == Just "none") without.edges)
    Left why, _ -> expect why false
    _, Left why -> expect why false
  let thin = run (down defaultMetrics Plain) plainPicture energy
  let turned = run (down sankeyMetrics quantity) plainPicture (project Both scan)
  case pictureOf sweeping thin, pictureOf sweeping turned of
    Right hairlines, Right cyclic -> do
      expect "a hairline is a line whatever is asked" (all (\mark -> not (hasClass "flow-swept" mark)) hairlines.edges)
      expect "a band turned back is stroked, and the bands that run forward beside it are swept"
        ( all identity (Array.zipWith (\mark placed -> hasClass "flow-swept" mark == (placed.course == Forward)) cyclic.edges turned.edges)
            && Array.any (\placed -> placed.course == Backward) turned.edges
        )
    Left why, _ -> expect why false
    _, Left why -> expect why false
