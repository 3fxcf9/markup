function renderMath() {
  document
    .querySelectorAll("code.katex-inline, code.katex-display")
    .forEach((element) => {
      let math = element.textContent;
      // Create a new element for rendering
      const renderElement = document.createElement(
        element.classList.contains("katex-display") ? "div" : "span",
      );
      // Replace the code element with the new element
      element.parentNode.replaceChild(renderElement, element);
      try {
        katex.render(math, renderElement, {
          displayMode: element.classList.contains("katex-display"),
          throwOnError: false,
        });
      } catch (e) {
        console.error("KaTeX rendering error:", e);
      }
    });
}
window.addEventListener("load", renderMath);
