library(pagedown)

fmtdoc <- function(extra_css = NULL, ...) {
  html_paged(
    css = c("style.scss", extra_css),
    template = "template.html",
    ...
  )
}

chrome_print(rmarkdown::render("page2.Rmd", 
                               # params = list(project = "Potato")
                               ), 
             format = "pdf")
