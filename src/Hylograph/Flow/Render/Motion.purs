-- | Whether the reader has asked for less motion (0.6): the browser's
-- | `prefers-reduced-motion`. A page that watches a change of cut
-- | (`Hylograph.Flow.Render.Transition`) asks this first, and where the
-- | answer is yes makes the change at once and signals it another way
-- | (`Hylograph.Flow.Render.Signal`). Needs a window, so it does not run
-- | in node; with none it answers no.
module Hylograph.Flow.Render.Motion (prefersReducedMotion) where

import Effect (Effect)

foreign import prefersReducedMotionImpl :: Effect Boolean

prefersReducedMotion :: Effect Boolean
prefersReducedMotion = prefersReducedMotionImpl
