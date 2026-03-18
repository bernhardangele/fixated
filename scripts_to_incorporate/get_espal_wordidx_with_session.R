library(httr)
library(rvest)

# Start session and set database to 'subtitles' and phonology to 'es'
url_index <- "https://www.bcbl.eu/databases/espal/index.php"
res <- POST(url_index, body = list(database = "subtitles", phonology = "es"))

# Now fetch wordidx.php
url_wordidx <- "https://www.bcbl.eu/databases/espal/wordidx.php"
res2 <- GET(url_wordidx, set_cookies(unlist(cookies(res)$value)))

html <- read_html(content(res2, "text"))
form <- html_node(html, "form")

cat("Form Action:", html_attr(form, "action"), "\n")
inputs <- html_nodes(form, "input, select, textarea")
for(inp in inputs) {
  name <- html_attr(inp, "name")
  type <- html_attr(inp, "type")
  val <- html_attr(inp, "value")
  if(type == "checkbox") {
    cat("CHECKBOX Name:", name, "Value:", val, "\n")
  } else if (type == "hidden") {
    cat("HIDDEN Name:", name, "Value:", val, "\n")
  } else if (html_name(inp) == "textarea") {
    cat("TEXTAREA Name:", name, "\n")
  } else {
      cat(html_name(inp), "Name:", name, "Value:", val, "\n")
  }
}
