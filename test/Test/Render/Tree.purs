-- | The HATS tree, read in node: one mark for each placed node and one path
-- | for each placed edge, and what the caller's decoration says, on the
-- | element it was said of. (The page's browser check sees the same tree as
-- | a DOM; this needs neither a browser nor an adapter.)
module Test.Render.Tree (suite) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sequence_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow, GroupId(..), ItemId(..), PortName(..), StepId(..), WireId(..), project)
import Hylograph.Flow.Overlay (overlay)
import Hylograph.Flow.Render.Layout (Course(..), Layout, Widths(..), defaultMetrics, layout)
import Hylograph.Flow.Render.Tree (Decoration, Gesture, Pointed(..), keyName, plainDecoration, pointedBy, tree, withOverlay)
import Hylograph.Flow.Render.Tree as Gesture
import Hylograph.HATS (ElementType(..))
import Test.Render.Fixtures (drawings, expect, expectNone, flowOf, quantity, sankeyMetrics, single, subjects, weighted)
import Test.Render.Realized (Realized(..), attribute, children, classes, clicks, enters, everything, hasClass, leaves, realize)

elementOf :: Realized -> ElementType
elementOf (Realized r) = r.element

-- | The parts of a realized picture: the root, the edge marks and the node
-- | marks; or why it has not that shape.
type Picture = { root :: Realized, edges :: Array Realized, nodes :: Array Realized }

pictureOf :: forall wire. Decoration wire -> Layout wire -> Either String Picture
pictureOf decoration laid = realize (tree decoration laid) >>= case _ of
  [ root ] -> case children root of
    [ edgeGroup, nodeGroup ]
      | hasClass "flow-edges" edgeGroup && hasClass "flow-nodes" nodeGroup -> Right { root, edges: children edgeGroup, nodes: children nodeGroup }
    _ -> Left "the root has not exactly the group of edges and then the group of nodes"
  roots -> Left ("the tree is " <> show (Array.length roots) <> " elements at the top, not one")

-- | The children of a mark that are of a class.
partsOf :: String -> Realized -> Array Realized
partsOf className mark = Array.filter (hasClass className) (children mark)

-- | Every fault in the tree of a layout, whatever the decoration: the marks
-- | are the placed nodes and edges, each once, in order, each where and what
-- | the layout says.
structureFaults :: forall wire. Decoration wire -> Layout wire -> Array String
structureFaults decoration laid = case pictureOf decoration laid of
  Left why -> [ why ]
  Right picture ->
    let
      all = everything picture.root
      count what = Array.length (Array.filter what all)
      said name mark = attribute name mark
      edgeFaults = Array.concat $ Array.zipWith
        ( \placed mark ->
            let
              name = "the mark of edge " <> show placed.index
              must what ok = if ok then [] else [ name <> " " <> what ]
            in
              must "is not a path of class flow-edge" (elementOf mark == Path && hasClass "flow-edge" mark)
                <> must "has not the edge's index" (said "data-edge" mark == Just (show placed.index))
                <> must "has not the edge's ends" (said "data-from" mark == Just (keyName placed.edge.from) && said "data-to" mark == Just (keyName placed.edge.to))
                <> must "has not the layout's path" (said "d" mark == Just placed.path)
                <> must "is not stroked as wide as the layout says" (said "stroke-width" mark == Just (show placed.stroke))
                <> must "is filled" (said "fill" mark == Just "none")
                <> must "is not marked backward exactly where the layout turns it back" (hasClass "flow-backward" mark == (placed.course == Backward))
                <> must "has not three gestures, a click, an entering and a leaving" (Array.length (clicks mark) == 1 && Array.length (enters mark) == 1 && Array.length (leaves mark) == 1)
        )
        laid.edges
        picture.edges
      nodeFaults = Array.concat $ Array.zipWith
        ( \placed mark ->
            let
              name = "the mark of node " <> show placed.index
              must what ok = if ok then [] else [ name <> " " <> what ]
              isStep = case placed.node.key of
                StepKey _ -> true
                _ -> false
            in
              must "is not a group of class flow-node" (elementOf mark == Group && hasClass "flow-node" mark)
                <> must "has not the node's key" (said "data-key" mark == Just (keyName placed.node.key))
                <> must "is not an item's mark for an item and a step's for a step" (hasClass "flow-step" mark == isStep && hasClass "flow-item" mark == not isStep)
                <> must "has not three gestures, a click, an entering and a leaving" (Array.length (clicks mark) == 1 && Array.length (enters mark) == 1 && Array.length (leaves mark) == 1)
                <> case partsOf "flow-box" mark of
                  [ box ] ->
                    must "has a box that is not the layout's"
                      ( elementOf box == Rect
                          && said "x" box == Just (show placed.box.x)
                          && said "y" box == Just (show placed.box.y)
                          && said "width" box == Just (show placed.box.width)
                          && said "height" box == Just (show placed.box.height)
                      )
                  _ -> [ name <> " has not exactly one box" ]
                <> must "has not exactly one label" (Array.length (partsOf "flow-label" mark) == 1)
        )
        laid.nodes
        picture.nodes
    in
      (if elementOf picture.root == SVG && hasClass "flow" picture.root then [] else [ "the root is not an svg of class flow" ])
        <> (if said "width" picture.root == Just (show (laid.width + 4.0)) && said "height" picture.root == Just (show (laid.height + 4.0)) then [] else [ "the root is not the layout's size and a margin" ])
        <> (if Array.length picture.nodes == Array.length laid.nodes then [] else [ show (Array.length picture.nodes) <> " node marks for " <> show (Array.length laid.nodes) <> " placed nodes" ])
        <> (if Array.length picture.edges == Array.length laid.edges then [] else [ show (Array.length picture.edges) <> " edge marks for " <> show (Array.length laid.edges) <> " placed edges" ])
        <> (if count (hasClass "flow-node") == Array.length laid.nodes then [] else [ "there is a node mark outside the group of nodes" ])
        <> (if count (\mark -> elementOf mark == Path) == Array.length laid.edges then [] else [ "there is a path that is no edge's mark" ])
        <> edgeFaults
        <> nodeFaults

-- | The mark with a key, among node marks.
keyed :: ElementKey -> Array Realized -> Array Realized
keyed key = Array.filter \mark -> attribute "data-key" mark == Just (keyName key)

-- | The keys of the node marks of a class; the indices of such edge marks.
keysWith :: String -> Array Realized -> Array (Maybe String)
keysWith className marks = map (attribute "data-key") (Array.filter (hasClass className) marks)

edgesWith :: String -> Array Realized -> Array (Maybe String)
edgesWith className marks = map (attribute "data-edge") (Array.filter (hasClass className) marks)

textOf :: String -> Realized -> Array (Maybe String)
textOf className mark = map (attribute "textContent") (partsOf className mark)

-- | One step that reads the total it writes, and nothing else.
tally :: Flow Unit Unit Number
tally = flowOf [] [ { step: "add", reads: [ single "amount", single "total" ], writes: [ single "total" ] } ]

amountItem :: ElementKey
amountItem = ItemKey (ItemId "amount")

totalItem :: ElementKey
totalItem = ItemKey (ItemId "total")

addStep :: ElementKey
addStep = StepKey (StepId "add")

-- | Something said of everything, differently of each: a label in brackets,
-- | a note on steps, a class on `total`, a colour on `amount` and on `add`;
-- | a class on edges into `add`, a colour on edges out of it.
dressed :: forall wire. (Gesture -> Pointed -> Effect Unit) -> Decoration wire
dressed notify =
  { node: \node ->
      { label: "[" <> node.label <> "]"
      , note: case node.key of
          StepKey _ -> Just "sum"
          _ -> Nothing
      , className: if node.key == totalItem then "hot" else ""
      , colour: if node.key == amountItem then Just "#cc0000" else if node.key == addStep then Just "#0000cc" else Nothing
      }
  , edge: \edge ->
      { className: if edge.to == addStep then "inward" else ""
      , colour: if edge.from == addStep then Just "#00aa00" else Nothing
      }
  , notify
  }

quiet :: Gesture -> Pointed -> Effect Unit
quiet _ _ = pure unit

suite :: Effect Unit
suite = do
  log "render: the tree has one mark for each placed node and one path for each placed edge"
  for_ subjects \subject -> for_ drawings \d -> do
    let projection = project d.drawing subject.flow
    let plainly = layout defaultMetrics Plain projection
    let byQuantity = layout sankeyMetrics quantity projection
    expectNone (subject.name <> ", " <> d.name <> ", plain: " <> show (Array.length plainly.nodes) <> " node marks, " <> show (Array.length plainly.edges) <> " paths")
      (structureFaults plainDecoration plainly)
    expectNone (subject.name <> ", " <> d.name <> ", by quantity, dressed: the same") (structureFaults (dressed quiet) byQuantity)

  log "render: the caller's decoration is on the element it was said of"
  -- the tally under Both: amount and total (items), add (a step); three
  -- wires, the read of total turned back
  let tallyLaid = layout defaultMetrics Plain (project Both tally)
  case pictureOf (dressed quiet) tallyLaid, pictureOf plainDecoration tallyLaid of
    Right picture, Right bare -> do
      expect "each node's label is what the decoration says of that node, and without one it is the node's own"
        ( (map (textOf "flow-label") (keyed amountItem picture.nodes <> keyed totalItem picture.nodes <> keyed addStep picture.nodes)) == [ [ Just "[amount]" ], [ Just "[total]" ], [ Just "[add]" ] ]
            && (map (textOf "flow-label") (keyed amountItem bare.nodes <> keyed totalItem bare.nodes <> keyed addStep bare.nodes)) == [ [ Just "amount" ], [ Just "total" ], [ Just "add" ] ]
        )
      expect "the note is on the step and on no item; undecorated, on nothing"
        ( map (textOf "flow-note") (keyed addStep picture.nodes) == [ [ Just "sum" ] ]
            && map (textOf "flow-note") (keyed amountItem picture.nodes <> keyed totalItem picture.nodes) == [ [], [] ]
            && Array.all (\mark -> textOf "flow-note" mark == []) bare.nodes
        )
      expect "the class is on total's mark and no other"
        (keysWith "hot" picture.nodes == [ Just (keyName totalItem) ] && keysWith "hot" bare.nodes == [])
      expect "an item's colour is a swatch beside an uncoloured box; an item with none has no swatch"
        ( map (map (attribute "fill") <<< partsOf "flow-swatch") (keyed amountItem picture.nodes) == [ [ Just "#cc0000" ] ]
            && map (map (attribute "fill") <<< partsOf "flow-box") (keyed amountItem picture.nodes) == [ [ Just "#ffffff" ] ]
            && map (partsOf "flow-swatch" >>> Array.length) (keyed totalItem picture.nodes) == [ 0 ]
        )
      expect "a step's colour fills and strokes its box, which is rounded, and it has no swatch; with none it is the ink"
        ( map (map (attribute "fill") <<< partsOf "flow-box") (keyed addStep picture.nodes) == [ [ Just "#0000cc" ] ]
            && map (map (attribute "stroke") <<< partsOf "flow-box") (keyed addStep picture.nodes) == [ [ Just "#0000cc" ] ]
            && map (partsOf "flow-swatch" >>> Array.length) (keyed addStep picture.nodes) == [ 0 ]
            && map (map (attribute "fill") <<< partsOf "flow-box") (keyed addStep bare.nodes) == [ [ Just "#161616" ] ]
            && map (map (attribute "rx") <<< partsOf "flow-box") (keyed addStep bare.nodes) /= [ [ Nothing ] ]
            && map (map (attribute "rx") <<< partsOf "flow-box") (keyed amountItem bare.nodes) == [ [ Nothing ] ]
        )
      let into = Array.filter (\e -> e.edge.to == addStep) tallyLaid.edges
      let outOf = Array.filter (\e -> e.edge.from == addStep) tallyLaid.edges
      expect "an edge's class is on the paths of the edges into add (two) and no other"
        (Array.length into == 2 && edgesWith "inward" picture.edges == map (Just <<< show <<< _.index) into && edgesWith "inward" bare.edges == [])
      expect "an edge's colour is the stroke of the edge out of add (one); every other edge is the ink"
        ( Array.length outOf == 1
            && (picture.edges <#> \mark -> attribute "stroke" mark == Just "#00aa00") == (tallyLaid.edges <#> \e -> e.edge.from == addStep)
            && (picture.edges <#> \mark -> attribute "stroke" mark == Just "#161616") == (tallyLaid.edges <#> \e -> e.edge.from /= addStep)
            && Array.all (\mark -> attribute "stroke" mark == Just "#161616") bare.edges
        )
      let turnedBack = map _.index (Array.filter (\e -> e.course == Backward) tallyLaid.edges)
      expect "the one edge turned back is a dashed hairline; the edges forward are not dashed"
        ( Array.length turnedBack == 1
            && edgesWith "flow-backward" picture.edges == map (Just <<< show) turnedBack
            && (picture.edges <#> \mark -> attribute "stroke-dasharray" mark) == (tallyLaid.edges <#> \e -> if e.course == Backward then Just "5 4" else Nothing)
            && Array.all (hasClass "flow-hairline") picture.edges
        )
    Left why, _ -> expect why false
    _, Left why -> expect why false

  -- by quantity: bands
  let bandsLaid = layout sankeyMetrics quantity (project Both weighted)
  case pictureOf plainDecoration bandsLaid of
    Right picture ->
      expect "by quantity every edge wider than three pixels is a band, stroked as wide as it is and let through"
        ( Array.all (\e -> e.stroke > 3.0) bandsLaid.edges
            && Array.all (\mark -> hasClass "flow-band" mark && not (hasClass "flow-hairline" mark) && attribute "stroke-opacity" mark == Just "0.22") picture.edges
            && map (attribute "stroke-width") picture.edges == map (Just <<< show <<< _.width) bandsLaid.edges
        )
    Left why -> expect why false

  log "render: gestures are told by element"
  heard <- Ref.new []
  let listening = dressed \gesture pointed -> Ref.modify_ (flip Array.snoc (Tuple gesture pointed)) heard
  case pictureOf listening tallyLaid of
    Right picture -> do
      for_ picture.nodes \mark -> sequence_ (clicks mark)
      afterNodes <- Ref.read heard
      expect "a click on each node's mark tells that node's element, and nothing before it is clicked"
        (afterNodes == (tallyLaid.nodes <#> \n -> Tuple Gesture.Clicked (PointedElement n.node.key)))
      Ref.write [] heard
      for_ picture.edges \mark -> sequence_ (enters mark <> leaves mark)
      afterEdges <- Ref.read heard
      expect "under Both the pointer's coming to and going from each edge tells that edge's wire"
        ( afterEdges == Array.concatMap (\e -> [ Tuple Gesture.Entered (pointedBy e.edge), Tuple Gesture.Left (pointedBy e.edge) ]) tallyLaid.edges
            && map (pointedBy <<< _.edge) tallyLaid.edges ==
              [ PointedElement (WireKey (Reads (ItemId "amount") (StepId "add") (PortName "amount")))
              , PointedElement (WireKey (Reads (ItemId "total") (StepId "add") (PortName "total")))
              , PointedElement (WireKey (Writes (StepId "add") (PortName "total") (ItemId "total")))
              ]
        )
    Left why -> expect why false
  Ref.write [] heard
  let contracted = layout defaultMetrics Plain (project Holders tally)
  case pictureOf listening contracted of
    Right picture -> do
      for_ picture.edges \mark -> sequence_ (clicks mark)
      afterPaths <- Ref.read heard
      expect "under Holders a click on an edge tells the path it stands for, with the step it passes through"
        ( afterPaths ==
            [ Tuple Gesture.Clicked (PointedPath { from: amountItem, to: totalItem, via: [ addStep ] })
            , Tuple Gesture.Clicked (PointedPath { from: totalItem, to: totalItem, via: [ addStep ] })
            ]
        )
    Left why -> expect why false

  log "render: an overlay's facts are on the elements they are known of"
  let written = WireKey (Writes (StepId "add") (PortName "total") (ItemId "total"))
  let known = overlay [ Tuple totalItem "stale", Tuple addStep "running", Tuple written "broken" ]
  let
    overlaid :: forall wire. Decoration wire
    overlaid = withOverlay known (\fact mark -> mark { className = fact, note = Just fact }) (\fact line -> line { className = fact, colour = Just "#cc0000" }) plainDecoration
  case pictureOf overlaid tallyLaid, pictureOf overlaid contracted of
    Right picture, Right paths -> do
      expect "under Both: stale is on total's mark alone, running on add's alone, broken on the one wire's path alone"
        ( keysWith "stale" picture.nodes == [ Just (keyName totalItem) ]
            && keysWith "running" picture.nodes == [ Just (keyName addStep) ]
            && edgesWith "broken" picture.edges == [ Just "2" ]
            && (picture.edges <#> \mark -> attribute "stroke" mark == Just "#cc0000") == [ false, false, true ]
            && Array.all (\mark -> classes mark == [ "flow-node", "flow-item" ]) (keyed amountItem picture.nodes)
        )
      expect "what the overlay changed beside the class is there too: the note on the two nodes it knows"
        (map (textOf "flow-note") picture.nodes == [ [], [ Just "stale" ], [ Just "running" ] ])
      expect "under Holders: the node is still found; no edge is a single wire, so none is marked"
        (keysWith "stale" paths.nodes == [ Just (keyName totalItem) ] && edgesWith "broken" paths.edges == [] && Array.length paths.edges == 2)
    Left why, _ -> expect why false
    _, Left why -> expect why false

  log "render: an element's key as one string"
  expect "keyName tells an item, a step, a read and a write apart"
    ( map keyName [ amountItem, addStep, WireKey (Reads (ItemId "total") (StepId "add") (PortName "in")), written ]
        == [ "item:amount", "step:add", "reads:total>add.in", "writes:add.total>total" ]
    )
  expect "keyName tells a closed group's step from a step named as one was, and writes a member's port as it reads"
    ( map keyName
        [ StepKey (StepId "group:G")
        , StepKey (GroupStep (GroupId "G"))
        , WireKey (Writes (GroupStep (GroupId "G")) (MemberPort (StepId "add") (PortName "total")) (ItemId "total"))
        ]
        == [ "step:group:G", "group:G", "writes:group:G.add.total>total" ]
    )
