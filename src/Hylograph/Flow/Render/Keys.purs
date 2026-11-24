-- | The keyboard, for a drawn flow (0.5).
-- |
-- | The tree tells a caller of clicks and of the pointer (`Decoration.notify`)
-- | through HATS, which has no behaviour for a key. A mark the decoration
-- | calls a button (`Decoration.button`) can be focused; `pressByKeys` is
-- | the other half: one listener on a container, under which Enter or Space
-- | on a focused button of a drawn flow is a click on that button, told as
-- | any click is. It is attached once, to an element that outlives
-- | redrawing (the container the tree is rendered into), and not to the
-- | marks, which come and go.
-- |
-- | This is the package's one foreign module and it needs a document, so
-- | nothing here runs in node; the pure half (which marks are buttons, and
-- | what each says) is in `Tree` and is tested there.
module Hylograph.Flow.Render.Keys (pressByKeys) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import pressByKeysImpl :: EffectFn1 String (Effect Unit)

-- | Under every element the selector finds, make Enter and Space on a
-- | focused button of a drawn flow a click on it. Returns how to stop.
pressByKeys :: String -> Effect (Effect Unit)
pressByKeys = runEffectFn1 pressByKeysImpl
