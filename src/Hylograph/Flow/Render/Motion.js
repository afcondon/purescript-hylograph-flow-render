// The browser's prefers-reduced-motion, asked each time (it can change).
export const prefersReducedMotionImpl = () =>
  typeof window !== "undefined" && typeof window.matchMedia === "function"
    && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
