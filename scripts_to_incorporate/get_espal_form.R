library(rvest)
html <- read_html("https://www.bcbl.eu/databases/espal/index.php")
forms <- html_nodes(html, "form")
for(i in seq_along(forms)) {
  cat("\n--- Form", i, "---\n")
  cat("Action:", html_attr(forms[[i]], "action"), "\n")
  cat("Method:", html_attr(forms[[i]], "method"), "\n")
  inputs <- html_nodes(forms[[i]], "input, select, textarea")
  for(inp in inputs) {
    cat(html_name(inp), "name:", html_attr(inp, "name"), "type:", html_attr(inp, "type"), "value:", html_attr(inp, "value"), "\n")
  }
}
