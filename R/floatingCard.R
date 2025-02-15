floatingCard <- function(..., title = "Title", subtitle = "Subtitle") {
  shiny::div(class = "content",
             shiny::div(class = "floating-container-title header", title),
             shiny::div(class = "meta", subtitle),
             shiny::div(class = "description", ...)) %>%
    shiny.semantic::card() |>
    shiny::div(class = "floating-container")
}
