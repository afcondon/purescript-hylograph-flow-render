-- | Words in a node, set without a DOM.
-- |
-- | The layout is pure and runs in node, so it cannot ask a browser how
-- | wide a string is. It estimates. `estimateWidth` adds up an advance for
-- | each character from a table of five classes, in ems, taken from the
-- | metrics of Helvetica and Arial (the same numbers to within a hundredth
-- | of an em): narrow letters and marks a quarter of an em, `f t r` and
-- | brackets a third, `m w M W` near nine tenths, capitals seven tenths,
-- | and everything else (lower case, figures) 0.56. Semibold is taken as
-- | six per cent wider.
-- |
-- | HOW WRONG IT CAN BE. For Helvetica, Arial and Liberation Sans the
-- | classes are within about 0.06 em of each glyph's true advance, and over
-- | a label of ordinary words and figures the errors mostly cancel. Measured
-- | once, in Chrome, on 276 lines of cell ranges and short phrases set in
-- | Helvetica Neue: what the browser drew was between 0.87 and 1.01 of the
-- | estimate, 0.97 at the median. So the estimate ran as much as a seventh
-- | too wide (a line wrapped that would have fitted) and a hundredth too
-- | narrow. A face of other proportions is another matter: a condensed face
-- | runs a fifth narrower, a wide one (Verdana) a fifth wider, and the
-- | estimate knows nothing of kerning, letter-spacing or a `text-transform`
-- | set by a stylesheet (upper-casing a label makes it a quarter wider than
-- | estimated). `caution` (1.08 by default) is the factor the estimate is
-- | multiplied by before a line is judged to fit, so that an estimate a
-- | little low still wraps in time; raise it for a wider face. A page can
-- | measure what was drawn (`getComputedTextLength`) against the estimate.
-- |
-- | `wrap` breaks at spaces, and inside a word only where the word alone
-- | is wider than the line. Text that would need more than the lines
-- | allowed ends in an ellipsis on the last line.
module Hylograph.Flow.Render.Text
  ( Words
  , SetWords
  , Lettering
  , defaultLettering
  , advance
  , estimateWidth
  , heavier
  , wrap
  , setWords
  , heightOf
  , breadthOf
  , lineHeight
  , available
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)

-- | What is written in a node: a title, and under it some detail.
type Words = { title :: String, detail :: Maybe String }

-- | Words as set: the title's lines and the detail's, top to bottom.
type SetWords = { title :: Array String, detail :: Array String }

-- | `titleSize`, `detailSize`: font sizes, in pixels. `leading`: a line's
-- | height over its font size. `margin`: above the first line and below the
-- | last. `inset`, `outset`: from the box's left side to the text, and from
-- | the text to its right side. `titleLines`, `detailLines`: the most lines
-- | each may take. `caution`: see the module's header.
type Lettering =
  { titleSize :: Number
  , detailSize :: Number
  , leading :: Number
  , margin :: Number
  , inset :: Number
  , outset :: Number
  , titleLines :: Int
  , detailLines :: Int
  , caution :: Number
  }

defaultLettering :: Lettering
defaultLettering =
  { titleSize: 12.0
  , detailSize: 10.0
  , leading: 1.25
  , margin: 5.0
  , inset: 13.0
  , outset: 10.0
  , titleLines: 2
  , detailLines: 3
  , caution: 1.08
  }

-- | A character's advance, in ems, at regular weight.
advance :: Char -> Number
advance character
  | Array.elem character narrow = 0.25
  | Array.elem character slim = 0.31
  | Array.elem character wide = 0.86
  | character >= 'A' && character <= 'Z' = 0.7
  | otherwise = 0.56

narrow :: Array Char
narrow = toCharArray "ijl.,:;'|!I "

slim :: Array Char
slim = toCharArray "ftr()[]-/\"`"

wide :: Array Char
wide = toCharArray "mwMW@%"

-- | How much wider semibold is taken to be than regular.
heavier :: Number
heavier = 1.06

-- | The width of a string at a font size, in pixels, at regular weight: an
-- | estimate (see the module's header).
estimateWidth :: Number -> String -> Number
estimateWidth size words = size * sum (map advance (toCharArray words))

-- | A string as lines no wider than `width` (as estimated, `factor` times
-- | over), at most `most` of them (and at least one).
wrap :: { width :: Number, size :: Number, factor :: Number, most :: Int } -> String -> Array String
wrap given words = limited
  where
  measure text = given.factor * estimateWidth given.size text
  fits text = measure text <= given.width

  -- a word wider than a line, cut into pieces that fit (a piece is never
  -- empty: one character is taken however narrow the line)
  pieces :: String -> Array String
  pieces word = (foldl take { done: [], current: "" } (toCharArray word)) # \state -> if state.current == "" then state.done else Array.snoc state.done state.current
    where
    take state character =
      let
        longer = state.current <> fromCharArray [ character ]
      in
        if state.current == "" || fits longer then state { current = longer }
        else { done: Array.snoc state.done state.current, current: fromCharArray [ character ] }

  tokens = Array.filter (_ /= "") (split (Pattern " ") words) # Array.concatMap \word -> if fits word then [ word ] else pieces word

  place state token =
    if state.current == "" then state { current = token }
    else if fits (state.current <> " " <> token) then state { current = state.current <> " " <> token }
    else { done: Array.snoc state.done state.current, current: token }

  every = foldl place { done: [], current: "" } tokens # \state -> if state.current == "" then state.done else Array.snoc state.done state.current

  most = max 1 given.most

  limited =
    if Array.length every <= most then every
    else case Array.index every (most - 1) of
      Just last -> Array.take (most - 1) every <> [ shortened (last <> " …") ]
      Nothing -> every

  -- the last line allowed, cut back until it fits with its ellipsis
  shortened :: String -> String
  shortened line =
    if fits line then line
    else
      let
        base = String.take (String.length line - 2) line
        cut = String.trim (String.take (String.length base - 1) base)
      in
        if cut == "" then "…" else shortened (cut <> " …")

-- | The room for text in a box of a breadth.
available :: Lettering -> Number -> Number
available lettering breadth = max 1.0 (breadth - lettering.inset - lettering.outset)

-- | Words set in a box of a breadth: the title semibold, the detail
-- | regular, each wrapped to the room there is.
setWords :: Lettering -> Number -> Words -> SetWords
setWords lettering breadth words =
  { title: wrap { width: room, size: lettering.titleSize, factor: lettering.caution * heavier, most: lettering.titleLines } words.title
  , detail: case words.detail of
      Just detail -> wrap { width: room, size: lettering.detailSize, factor: lettering.caution, most: lettering.detailLines } detail
      Nothing -> []
  }
  where
  room = available lettering breadth

-- | A line's height at a font size.
lineHeight :: Lettering -> Number -> Number
lineHeight lettering size = size * lettering.leading

-- | How tall a box must be to hold words as set.
heightOf :: Lettering -> SetWords -> Number
heightOf lettering set =
  2.0 * lettering.margin
    + toNumber (Array.length set.title) * lineHeight lettering lettering.titleSize
    + toNumber (Array.length set.detail) * lineHeight lettering lettering.detailSize

-- | How broad a box must be to hold words as set (0.5): the widest line,
-- | as estimated and judged (`caution`, and `heavier` for the title), with
-- | the inset and the outset. `available` of it is that widest line, so the
-- | words set in a box this broad are within their room.
breadthOf :: Lettering -> SetWords -> Number
breadthOf lettering set = lettering.inset + lettering.outset + foldl max 0.0
  ( map (\line -> lettering.caution * heavier * estimateWidth lettering.titleSize line) set.title
      <> map (\line -> lettering.caution * estimateWidth lettering.detailSize line) set.detail
  )
