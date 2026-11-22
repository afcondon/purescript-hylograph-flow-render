-- | Open groups outlined, and words set: the layout held to its laws on
-- | groupings made here for their shapes (two sibling groups, a group in a
-- | group, a group whose members are layers apart with strangers between, a
-- | group of one member, a group with a member no edge touches, an open
-- | group over a closed one), each under every drawing, plain and by
-- | quantity, with and without words; and the tree's outline marks.
module Test.Render.Groups (suite) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sequence_, sum)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow, GroupId(..), ItemId(..), Node, StepId(..), mapPayloads, project)
import Hylograph.Flow.Grouping (Grouping(..), collapse, groupingProblems)
import Hylograph.Flow.Render.Engine (Engine, Picture, plainPicture, run)
import Hylograph.Flow.Render.Laws (faults, faultsOf, strangers)
import Hylograph.Flow.Render.Layout (Widths(..), defaultMetrics, layout)
import Hylograph.Flow.Render.Sankey (sankey)
import Hylograph.Flow.Render.Text (Words, defaultLettering, estimateWidth, heightOf, setWords, wrap)
import Hylograph.Flow.Render.Tree (Pointed(..), plainDecoration)
import Hylograph.Flow.Render.Tree as Gesture
import Test.Render.Fixtures (drawings, expect, expectNone, flowOf, sankeyMetrics, single)
import Test.Render.Realized (clicks)
import Test.Render.Tree (keyed, partsOf, pictureOf, structureFaults, textOf)

-- | `d` is made from `a` directly and through `b` and `c` (an edge that
-- | spans layers); `e` is made from `c`; `y` from `x`, apart from the rest;
-- | `tally` reads what it writes; `alone` and `apart` are touched by nothing.
-- | A wire carries its numbers as a list, for `collapse` to merge.
works :: Flow Unit Unit (Array Number)
works = mapPayloads identity identity (\carried -> [ carried ]) $ flowOf [ "alone", "apart" ]
  [ { step: "f", reads: [ single "a" ], writes: [ single "b" ] }
  , { step: "g", reads: [ single "b" ], writes: [ single "c" ] }
  , { step: "h", reads: [ single "a", single "c" ], writes: [ single "d" ] }
  , { step: "k", reads: [ single "c" ], writes: [ single "e" ] }
  , { step: "m", reads: [ single "x" ], writes: [ single "y" ] }
  , { step: "tally", reads: [ single "e", single "sum" ], writes: [ single "sum" ] }
  ]

item :: String -> Grouping
item = Holds <<< ItemId

step :: String -> Grouping
step = Member <<< StepId

group :: String -> Array Grouping -> Grouping
group name = Group (GroupId name) (name <> ", a group")

type Case = { name :: String, grouping :: Array Grouping, closed :: Set GroupId, outlined :: Array String }

cases :: Array Case
cases =
  [ { name: "two sibling groups"
    , grouping: [ group "Left" [ step "f", item "a", item "b" ], group "Right" [ step "k", item "e", step "tally", item "sum" ] ]
    , closed: Set.empty
    , outlined: [ "Left", "Right" ]
    }
  , { name: "a group in a group"
    , grouping: [ group "Outer" [ group "Inner" [ step "g", item "c" ], step "f", item "b" ] ]
    , closed: Set.empty
    , outlined: [ "Inner", "Outer" ]
    }
  , { name: "a group whose members are layers apart"
    , grouping: [ group "Span" [ item "a", item "d", step "h" ] ]
    , closed: Set.empty
    , outlined: [ "Span" ]
    }
  , { name: "a group of one member"
    , grouping: [ group "One" [ step "h" ], group "Uno" [ item "c" ] ]
    , closed: Set.empty
    , outlined: [ "One", "Uno" ]
    }
  , { name: "a group with members no edge touches"
    , grouping: [ group "Lone" [ item "alone", item "apart" ], group "Mixed" [ step "m", item "x", item "y" ] ]
    , closed: Set.empty
    , outlined: [ "Lone", "Mixed" ]
    }
  , { name: "three deep, with a sibling"
    , grouping:
        [ group "World"
            [ group "Land" [ group "Town" [ step "f", item "b" ], step "g", item "c" ]
            , group "Sea" [ step "k", item "e" ]
            , item "a"
            ]
        , group "Elsewhere" [ step "m", item "x", item "y", item "alone" ]
        ]
    , closed: Set.empty
    , outlined: [ "Elsewhere", "Land", "Sea", "Town", "World" ]
    }
  , { name: "an open group over a closed one"
    , grouping: [ group "Outer" [ group "Inner" [ step "g", item "c", step "f", item "b" ], step "k", item "e" ] ]
    , closed: Set.singleton (GroupId "Inner")
    , outlined: [ "Outer" ]
    }
  , { name: "every group closed"
    , grouping: [ group "Left" [ step "f", item "a", item "b" ], group "Right" [ step "k", item "e" ] ]
    , closed: Set.fromFoldable [ GroupId "Left", GroupId "Right" ]
    , outlined: []
    }
  ]

-- | Words for every node: a title, and for steps a long line of detail.
wordsOf :: Node -> Words
wordsOf node =
  { title: node.label
  , detail: case node.key of
      StepKey _ -> Just ("a step that computes " <> node.label <> " from whatever it reads, described at some length")
      _ -> Nothing
  }

-- | The Sankey engine at its defaults, every edge a hairline.
plainly :: Engine (Array Number)
plainly = sankey defaultMetrics Plain

quantity :: Widths (Array Number)
quantity = Sankey \edge -> sum (map (sum <<< _.payload) (Array.fromFoldable edge.wires))

suite :: Effect Unit
suite = do
  log "render: open groups are outlined, and nothing else is in an outline"
  totals <- Ref.new { layouts: 0, outlines: 0, strangers: 0 }
  for_ cases \given -> do
    expect (given.name <> ": the grouping is a partition") (Array.null (groupingProblems given.grouping))
    let coarser = collapse given.grouping given.closed works
    for_ drawings \d -> do
      let projection = project d.drawing coarser
      let
        told :: Picture
        told = plainPicture { groups = Just { grouping: given.grouping, closed: given.closed } }

        pictures :: Array { name :: String, engine :: Engine (Array Number), picture :: Picture }
        pictures =
          [ { name: "plain", engine: sankey defaultMetrics Plain, picture: told }
          , { name: "by quantity", engine: sankey sankeyMetrics quantity, picture: told }
          , { name: "plain, with words", engine: sankey (defaultMetrics { nodeBreadth = 96.0 }) Plain, picture: told { words = Just wordsOf } }
          , { name: "by quantity, with words", engine: sankey sankeyMetrics quantity, picture: told { words = Just wordsOf } }
          ]
      for_ pictures \p -> do
        let laid = run p.engine p.picture projection
        Ref.modify_ (\t -> { layouts: t.layouts + 1, outlines: t.outlines + Array.length laid.outlines, strangers: t.strangers + Array.length (strangers laid) }) totals
        expectNone (given.name <> ", " <> d.name <> ", " <> p.name <> ": " <> show (Array.length laid.nodes) <> " nodes, " <> show (Array.length laid.outlines) <> " outlines, " <> show laid.width <> " by " <> show laid.height)
          (faultsOf p.engine p.picture projection laid)
        expectNone (given.name <> ", " <> d.name <> ", " <> p.name <> ": the tree has the outlines, nodes and edges the layout placed") (structureFaults plainDecoration laid)
    -- under Both every element is drawn, so every group named has its outline
    let both = run plainly (plainPicture { groups = Just { grouping: given.grouping, closed: given.closed } }) (project Both coarser)
    expect (given.name <> ", Both: the outlines are of " <> show given.outlined)
      (Array.sort (map (\outline -> case outline.group of GroupId name -> name) both.outlines) == given.outlined)
  counted <- Ref.read totals
  expect ("over " <> show counted.layouts <> " layouts with " <> show counted.outlines <> " outlines, " <> show counted.strangers <> " nodes are in an outline that is not theirs")
    (counted.strangers == 0 && counted.outlines > 0)

  log "render: particular facts of outlines"
  let siblings = { grouping: [ group "Left" [ step "f", item "a", item "b" ], group "Right" [ step "k", item "e", step "tally", item "sum" ] ], closed: Set.empty }
  let bothOf groups = project Both (collapse groups.grouping groups.closed works)
  let ungrouped = layout defaultMetrics Plain (bothOf siblings)
  let outlined = run plainly (plainPicture { groups = Just siblings }) (bothOf siblings)
  expect "told of no group, the layout has no outline and is the layout it was"
    (Array.null ungrouped.outlines && Array.null (faults defaultMetrics Plain (bothOf siblings) ungrouped))
  expect "told of a grouping in which nothing drawn is, the layout is the same to the pixel"
    ( let other = run plainly (plainPicture { groups = Just { grouping: [ group "Nowhere" [ step "absent" ] ], closed: Set.empty } }) (bothOf siblings)
      in map _.box other.nodes == map _.box ungrouped.nodes && map _.route other.edges == map _.route ungrouped.edges && Array.null other.outlines && other.height == ungrouped.height
    )
  expect "an outline is named as its group is in the grouping, and holds the group's nodes"
    ( Array.sort (outlined.outlines <#> \outline -> Tuple outline.name (Array.sort (outline.members # Array.mapMaybe \index -> _.node.label <$> Array.index outlined.nodes index)))
        == [ Tuple "Left, a group" [ "a", "b", "f" ], Tuple "Right, a group" [ "e", "k", "sum", "tally" ] ]
    )
  let lone = { grouping: [ group "Lone" [ item "alone", item "apart" ] ], closed: Set.empty }
  let loneLaid = run plainly (plainPicture { groups = Just lone }) (bothOf lone)
  let labelled label = Array.find (\n -> n.node.label == label) loneLaid.nodes
  expect "members of an open group that no edge touches are not on the shelf: they stand in a layer, in their outline"
    ((labelled "alone" >>= _.layer) == Just 0 && (labelled "apart" >>= _.layer) == Just 0 && Array.length loneLaid.outlines == 1)
  let nested = { grouping: [ group "Outer" [ group "Inner" [ step "g", item "c" ], step "f", item "b" ] ], closed: Set.empty }
  let nestedLaid = run plainly (plainPicture { groups = Just nested }) (bothOf nested)
  expect "a group in a group: the outer outline first, at depth 0, the inner at depth 1 with the outer for its parent"
    ((nestedLaid.outlines <#> \outline -> Tuple outline.depth outline.parent) == [ Tuple 0 Nothing, Tuple 1 (Just (GroupId "Outer")) ])

  log "render: a click on an outline's name tells its group"
  heard <- Ref.new []
  let listening = plainDecoration { notify = \gesture pointed -> Ref.modify_ (flip Array.snoc (Tuple gesture pointed)) heard }
  case pictureOf listening nestedLaid of
    Right drawn -> do
      for_ drawn.outlines \mark -> sequence_ (clicks mark)
      told <- Ref.read heard
      expect "each outline's mark tells its own group, outermost first"
        (told == [ Tuple Gesture.Clicked (PointedGroup (GroupId "Outer")), Tuple Gesture.Clicked (PointedGroup (GroupId "Inner")) ])
      expect "the outline's label is the group's name"
        (map (textOf "flow-outline-label") drawn.outlines == [ [ Just "Outer, a group" ], [ Just "Inner, a group" ] ])
    Left why -> expect why false

  log "render: words are wrapped to the node's breadth and the node is made tall enough"
  let narrow = sankey (defaultMetrics { nodeBreadth = 96.0 }) Plain
  let worded = plainPicture { words = Just wordsOf }
  let chainOf = project Both (collapse [] Set.empty works)
  let lettered = run narrow worded chainOf
  expectNone "with words and no groups the layout is lawful" (faultsOf narrow worded chainOf lettered)
  let stepF = Array.find (\n -> n.node.key == StepKey (StepId "f")) lettered.nodes
  let bare = Array.find (\n -> n.node.key == StepKey (StepId "f")) (layout (defaultMetrics { nodeBreadth = 96.0 }) Plain chainOf).nodes
  expect "a step with a long line of detail has it on three lines, the last cut short with an ellipsis, and is taller than its one edge makes it"
    ( case stepF, bare of
        Just with, Just without -> (map (Array.length <<< _.detail) with.words) == Just 3
          && (with.words >>= \set -> Array.last set.detail <#> \line -> String.contains (String.Pattern "…") line) == Just true
          && with.box.height > without.box.height
          && (map (heightOf defaultLettering) with.words) == Just with.box.height
        _, _ -> false
    )
  expect "an item with a short title has one line and no detail"
    ((Array.find (\n -> n.node.key == ItemKey (ItemId "a")) lettered.nodes >>= _.words) == Just { title: [ "a" ], detail: [] })
  case pictureOf plainDecoration lettered of
    Right drawn ->
      expect "the tree writes the lines the layout set: one label, its continuation lines, and the detail's lines"
        ( (keyed (StepKey (StepId "f")) drawn.nodes <#> \mark -> Tuple (Array.length (partsOf "flow-label" mark)) (Array.length (partsOf "flow-detail" mark))) == [ Tuple 1 3 ] )
    Left why -> expect why false
  expect "wrap breaks at spaces, and a word wider than the line where it must"
    ( wrap { width: 60.0, size: 10.0, factor: 1.0, most: 5 } "one two three four five six" == [ "one two", "three four", "five six" ]
        && Array.length (wrap { width: 30.0, size: 10.0, factor: 1.0, most: 9 } "incomprehensibilities") > 1
        && String.joinWith "" (wrap { width: 30.0, size: 10.0, factor: 1.0, most: 9 } "incomprehensibilities") == "incomprehensibilities"
    )
  expect "wrap never gives no line for words, and never more than it may"
    (wrap { width: 5.0, size: 10.0, factor: 1.0, most: 2 } "a long run of words that cannot fit" # \lines -> Array.length lines == 2)
  expect "the estimate: ten figures at ten pixels are 56 wide, and narrow letters are narrower than wide ones"
    (estimateWidth 10.0 "0123456789" > 55.9 && estimateWidth 10.0 "0123456789" < 56.1 && estimateWidth 10.0 "illi" < estimateWidth 10.0 "mwmw")
  expect "setWords keeps a title that fits on one line"
    ((setWords defaultLettering 200.0 { title: "Revenue", detail: Nothing }) == { title: [ "Revenue" ], detail: [] })
