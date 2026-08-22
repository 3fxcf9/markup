const input = document.querySelector("input[name=search_query]");

const list_items = Array.from(document.querySelectorAll(".chapter")).map(
  (item) => {
    return {
      title: item.querySelector(".chapter-title").innerText.toLowerCase(),
      desc: item.dataset.description || "",
      keywords: item.dataset.keywords || "",
      elem: item,
    };
  },
);

function matches(item, query) {
  return (
    item.title.includes(query) ||
    item.keywords.includes(query) ||
    item.desc.includes(query)
  );
}

input.addEventListener("input", () => {
  let query = input.value.toLowerCase();

  list_items.forEach((c) => c.elem.classList.remove("hidden"));
  list_items
    .filter((c) => !matches(c, query))
    .forEach((c) => c.elem.classList.add("hidden"));
});
