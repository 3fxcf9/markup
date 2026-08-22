function highlightCurrentSection() {
  const toc_links = document.querySelectorAll("nav a");

  const observer = new IntersectionObserver(
    (entries) => {
      entries.forEach((entry) => {
        const id = entry.target.getAttribute("id");
        const toc_item = document.querySelector(`nav a[href="#${id}"]`);
        if (entry.isIntersecting) {
          toc_links.forEach((link) => link.classList.remove("active"));
          if (toc_item) toc_item.classList.add("active");
        }
      });
    },
    {
      rootMargin: "0px 0px -50% 0px",
      threshold: 0,
    },
  );

  const headings = document.querySelectorAll("h2, h3, h4, h5, h6");
  headings.forEach((section) => {
    observer.observe(section);
  });
}

window.addEventListener("load", highlightCurrentSection);
