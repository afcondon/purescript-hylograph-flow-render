// Enter or Space on a focused button of a drawn flow is a click on it.
// The listener is on the container, so it holds for marks drawn later.
export const pressByKeysImpl = (selector) => {
  const pressed = (event) => {
    if (event.key !== "Enter" && event.key !== " ") return;
    const mark = event.target instanceof Element ? event.target.closest("svg.flow [role=button]") : null;
    if (!mark) return;
    event.preventDefault();
    mark.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true }));
  };
  const containers = [...document.querySelectorAll(selector)];
  for (const container of containers) container.addEventListener("keydown", pressed);
  return () => {
    for (const container of containers) container.removeEventListener("keydown", pressed);
  };
};
