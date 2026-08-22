const menuElement = document.querySelector(".left");
const menuToggleElement = document.querySelector(".menu-toggle");

function toggleMenuVisibility(force) {
  const shouldOpen =
    typeof force === "boolean"
      ? force
      : !menuElement.classList.contains("opened");

  menuElement.classList.toggle("opened", shouldOpen);
  menuToggleElement.classList.toggle("opened", shouldOpen);
}

menuToggleElement.addEventListener("click", toggleMenuVisibility);

document.querySelectorAll("nav a").forEach((link) => {
  link.addEventListener("click", () => toggleMenuVisibility(false));
});

// Close details before printing
window.addEventListener("beforeprint", (event) => {
  for (const detailEl of document.querySelectorAll("details")) {
    if (detailEl.getAttribute("open") == null) {
      detailEl.setAttribute("data-was-closed", "true");
    }
    detailEl.setAttribute("open", "");
  }
});

window.addEventListener("afterprint", (event) => {
  for (const detailEl of document.querySelectorAll("details")) {
    if (detailEl.getAttribute("data-was-closed") != null) {
      detailEl.removeAttribute("data-was-closed");
      detailEl.removeAttribute("open");
    }
  }
});
