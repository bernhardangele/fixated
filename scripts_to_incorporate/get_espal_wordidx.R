library(rvest)
html <- read_html("https://www.bcbl.eu/databases/espal/wordidx.php")
form <- html_node(html, "form[action='wordidx.php']")

cat("Method:", html_attr(form, "method"), "\n")
inputs <- html_nodes(form, "input, select, textarea")
for(inp in inputs) {
  name <- html_attr(inp, "name")
  type <- html_attr(inp, "type")
  val <- html_attr(inp, "value")
  if(is.na(val)) val <- ""
  
  if(type == "checkbox" || type == "radio") {
      checked <- html_attr(inp, "checked")
      if(!is.na(checked)) {
          cat(type, " name:", name, " value:", val, "(CHECKED)\n")
      } else {
          cat(type, " name:", name, " value:", val, "\n")
      }
  } else {
      cat(html_name(inp), " name:", name, " type:", type, " value:", val, "\n")
  }
}
