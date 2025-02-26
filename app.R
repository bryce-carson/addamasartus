library(bslib)
library(shiny)
library(visNetwork)

mainUI <- tagList(h1("Addamasartus"))

ui <- page_navbar(
  id = "nav",
  title = div(),
  sidebar = sidebar(
    conditionalPanel("input.nav === 'Addamasartus'"),
    conditionalPanel("input.nav === 'Assalkushalit'")
  ),
  nav_panel("Assalkushalit", "Mod Mgmt."),
  nav_panel(card(
    card_header(
      class = "bg-dark",
      "Addamasartus"
    ),
    markdown("An **experimental** *TES III: Morrowind* **file manager**")
  ),
  value = "Addamasartus",
  visNetworkOutput("minimalVisNetworkExample")
  )
)

server <- function(input, output, session) {
  output$minimalVisNetworkExample <- renderVisNetwork({
    ## minimal example
    nodes <- data.frame(id = 1:3)
    edges <- data.frame(from = c(1,2), to = c(1,3))

    visNetwork(nodes, edges)
  })
}

shinyApp(ui, function(input, output) {})
