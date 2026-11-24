-- | A watched change between two cuts: the stages of it, as layouts to be
-- | blended, and a transition of them by the ecosystem's own machinery
-- | (0.6).
-- |
-- | When the layouts of two cuts differ, the reader keeps their place by
-- | seeing what moves move. A change is shown in **stages**, so that two
-- | kinds of motion are not on the page at once. Opening a group:
-- |
-- | 1. **make room**: the closed picture, with every node the change does
-- |    not touch going to where the open picture has it, and the group's
-- |    bar going, whole, to where its parts will stand across the flow
-- |    (nowhere, where they will stand within the span it has);
-- | 2. **unfold**: the open picture, with the group's parts starting inside
-- |    the bar (each where it will be across the flow, all where the bar is
-- |    along it) and going out along the flow to their tiers.
-- |
-- | Closing is the same two, backwards: fold, then close up. A stage in
-- | which nothing moves is left out. So a pair that is `Held` has no stage
-- | that makes room; and where the group's parts are in one tier it has no
-- | stages at all: the parts take the bar's place at once, and there is
-- | nothing to watch.
-- |
-- | Every stage is two layouts of one projection and of one shape
-- | (`Reshape.sameShape`), made with `Reshape.reshaped`, so every picture
-- | part of the way through is `Reshape.blend` of the two, and **conserves
-- | what the two conserve**: a ribbon is as broad as it was and stands
-- | where it stood on its node's side all the way. What a picture part of
-- | the way through does not keep is apartness: in the second stage the
-- | parts start on top of one another, inside the bar.
-- |
-- | Between the two stages the picture changes from the closed projection
-- | to the open one. Every untouched node is then in one place in both;
-- | the bar is replaced by its parts, within the span it had; and the
-- | ribbons that met the bar meet the parts, which is the one thing that
-- | is seen to jump (by less than the bar's breadth).
-- |
-- | A change that opens some groups and closes others has no stages here:
-- | take it one group at a time. A change with no group opened or closed,
-- | between two layouts of one shape, is one stage.
-- |
-- | THE MACHINERY is `hylograph-transitions`: `watched` gives a
-- | `TransitionSpec` of layouts (its engine's, with its easings), whose
-- | tick is pure; `watch` runs one on its `Coordinator`, drawing each
-- | value. Nothing here keeps time of its own.
module Hylograph.Flow.Render.Transition
  ( Stage
  , stages
  , frame
  , watched
  , watch
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Hylograph.Flow.Render.Continuity (opened)
import Hylograph.Flow.Render.Engine (Box, Direction(..), Layout)
import Hylograph.Flow.Render.Reshape (blend, reshaped, sameShape)
import Hylograph.Transition.Coordinator (ConsumerId, Coordinator, TickResult(..), register)
import Hylograph.Transition.Easing (EasingType(..), toFunction)
import Hylograph.Transition.Engine (TransitionConfig, TransitionSpec)
import Hylograph.Transition.Engine as Engine

-- | One stage of a change: two layouts of one shape, and what the stage is
-- | called ("make room", "unfold", "fold", "close up", "move").
type Stage wire = { name :: String, from :: Layout wire, to :: Layout wire }

-- | The stages of the change from one layout to another; none where there
-- | is nothing to watch. See the module's header.
stages :: forall wire. Layout wire -> Layout wire -> Array (Stage wire)
stages before after = Array.filter moves case Array.null opening, Array.null closing of
  true, true -> if sameShape before after then [ { name: "move", from: before, to: after } ] else []
  false, true -> unfolding before after
  true, false -> Array.reverse (unfolding after before) <#> \stage ->
    { name: if stage.name == "unfold" then "fold" else "close up", from: stage.to, to: stage.from }
  false, false -> []
  where
  opening = opened before after
  closing = opened after before

  -- by more than a millionth of a pixel: two sums of the same breadths
  -- taken in different orders are not one number
  moves stage = not (all identity (Array.zipWith (\a b -> near a.box.x b.box.x && near a.box.y b.box.y && near a.box.width b.box.width && near a.box.height b.box.height) stage.from.nodes stage.to.nodes))
  near a b = abs (a - b) <= 1.0e-6

-- | The two stages of opening: from the layout with the groups shut to the
-- | layout with them open.
unfolding :: forall wire. Layout wire -> Layout wire -> Array (Stage wire)
unfolding shut open =
  [ { name: "make room", from: shut, to: roomMade }
  , { name: "unfold", from: folded, to: open }
  ]
  where
  bars = opened shut open

  start :: Box -> Number
  start box = case shut.direction of
    LeftToRight -> box.y
    TopDown -> box.x

  -- a box moved across the flow to a start, and no other way
  across :: Number -> Box -> Box
  across to box = case shut.direction of
    LeftToRight -> box { y = to }
    TopDown -> box { x = to }

  -- a box where another is along the flow, and where it is itself across it
  alongOf :: Box -> Box -> Box
  alongOf bar box = case shut.direction of
    LeftToRight -> box { x = bar.x }
    TopDown -> box { y = bar.y }

  inOpen = foldl (\m n -> if Map.member n.node.key m then m else Map.insert n.node.key n.box m) Map.empty open.nodes

  -- each bar where it will be when room is made: where its parts begin
  -- across the flow, and where it was along it
  -- (a bar whose parts will be within the span it has stays where it is)
  barBoxes = Map.fromFoldable $ bars <#> \found -> Tuple found.bar.node.key
    (if found.measured.inPlace then found.bar.box else across (foldl (\least part -> min least (start part.box)) (1.0 / 0.0) found.parts) found.bar.box)

  -- each part, and each outline of an opened group or of a group in one,
  -- with the bar it comes out of
  barOfPart = Map.fromFoldable (bars >>= \found -> Map.lookup found.bar.node.key barBoxes # Array.fromFoldable >>= \box -> found.parts <#> \part -> Tuple part.node.key box)
  barOfOutline = Map.fromFoldable $ bars >>= \found -> Map.lookup found.bar.node.key barBoxes # Array.fromFoldable >>= \box ->
    Array.filter (\outline -> outline.group == found.group || all (\member -> Array.any (\part -> part.index == member) found.parts) outline.members) open.outlines <#> \outline -> Tuple outline.group box

  roomMade = reshaped
    { node: \n -> fromMaybe n.box (if Map.member n.node.key barBoxes then Map.lookup n.node.key barBoxes else Map.lookup n.node.key inOpen)
    , outline: \outline -> fromMaybe outline.box (_.box <$> Array.find (\other -> other.group == outline.group) open.outlines)
    , width: open.width
    , height: open.height
    }
    shut

  folded = reshaped
    { node: \n -> case Map.lookup n.node.key barOfPart of
        Just bar -> alongOf bar n.box
        Nothing -> n.box
    , outline: \outline -> fromMaybe outline.box (Map.lookup outline.group barOfOutline)
    , width: open.width
    , height: open.height
    }
    open

-- | The picture a part of the way through the stages, from 0 (the first
-- | stage's start) to 1 (the last stage's end), each stage taking an equal
-- | share and eased within itself. Nothing where there are no stages.
frame :: forall wire. EasingType -> Array (Stage wire) -> Number -> Maybe (Layout wire)
frame easing found part =
  let
    count = Array.length found
    scaled = max 0.0 (min 1.0 part) * toNumber count
    index = min (count - 1) (floor scaled)
  in
    Array.index found index <#> \stage -> blend stage.from stage.to (toFunction easing (max 0.0 (min 1.0 (scaled - toNumber index))))

-- | The stages as one transition for `hylograph-transitions`' engine: its
-- | `duration` and `easing` are each stage's, its `delay` the whole's.
-- | Nothing where there are no stages (the change is then made at once).
watched :: forall wire. TransitionConfig -> Array (Stage wire) -> Maybe (TransitionSpec (Layout wire))
watched config found = do
  first <- Array.head found
  last <- Array.last found
  pure $ Engine.transitionWith (\_ to part -> fromMaybe to (frame config.easing found part)) { from: first.from, to: last.to }
    { duration: config.duration * toNumber (Array.length found), easing: Linear, delay: config.delay }

-- | Run a transition on a coordinator: `draw` is given each value as it
-- | comes, the last of them the transition's end exactly, and then `done`
-- | is run. (The coordinator must be started; it goes on asking for frames
-- | when it has no consumer, so stop it when nothing is left to watch.)
watch :: forall a. Coordinator -> TransitionSpec a -> (a -> Effect Unit) -> Effect Unit -> Effect ConsumerId
watch coordinator spec draw done = do
  state <- Ref.new (Engine.start spec)
  register coordinator
    { tick: \delta -> do
        now <- Ref.modify (Engine.tick delta) state
        if Engine.isComplete now then draw spec.to $> Completed
        else draw (Engine.currentValue now) $> StillRunning
    , onComplete: done
    }
