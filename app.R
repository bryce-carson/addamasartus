setClass("Person",
         slots = list(name = "character",
                      age = "numeric",
                      details = "list"))

                                        # Create sample data
sample_data <- list(
  person1 = new("Person", name = "John", age = 30,
                details = list(city = "New York", role = "Developer")),
  person2 = new("Person", name = "Alice", age = 25,
                details = list(city = "London", role = "Designer")),
  person3 = new("Person", name = "Bob", age = 35,
                details = list(city = "Paris", role = "Manager"))
)

stopifnot(all(c(require(shiny),
                require(shiny.semantic),
                require(visNetwork),
                require(htmltools),
                require(jsTreeR),
                require(methods),
                require(shinyjs))))

ui <- semanticPage(
  title = "Addamasartus",
  htmltools::includeCSS("www/floating-cards.css"),
  htmltools::includeScript("www/floating-cards.js"),
  div(id = "overlay",
      floatingCard(
        toggle("main_view_toggle", "View as Graph?", is_marked = TRUE),
        multiple_checkbox("float_toggles",
                          "Show Floating Containers:",
                          choices = c("Info" = "info",
                                      "Controls" = "controls",
                                      "Details" = "details")),
        title = "Toggle Container",
        subtitle = "Toggle other floating containers"
      ),
      uiOutput("info_container"),
      uiOutput("controls_container"),
      uiOutput("details_container")),
  uiOutput("main_view")
)

server <- function(input, output, session) {
  ## Floating containers
  output$info_container <- renderUI({
    req("info" %in% input$float_toggles)
    tagList(floatingCard(p(paste(letters, collapse = " ")), "Letters", "The English Alphabet (miniscule)"))
  })

  output$controls_container <- renderUI({
    req("controls" %in% input$float_toggles)
    tagList(floatingCard(p(paste(0:9, collapse = " ")), "Digits", "The Arabic Numerals used in English"))
  })

  output$details_container <- renderUI({
    req("details" %in% input$float_toggles)
    tagList(floatingCard(p(paste(LETTERS, collapse = " ")), "Letters", "The English Alphabet (majiscule)"))
  })

  ## Main view output
  output$main_view <- renderUI({
    if (input$main_view_toggle) {
      ## Render graph
      output$graph_view <- renderVisNetwork({
        graph_data <- s4_to_graph(sample_data)

        visNetwork(graph_data$nodes, graph_data$edges) %>%
          visGroups(groupname = "main", color = "#ff9999") %>%
          visGroups(groupname = "slot", color = "#99ff99") %>%
          visGroups(groupname = "detail", color = "#9999ff") %>%
          visLayout(hierarchical = TRUE) %>%
          visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE))
      })

      ## Graph view
      visNetworkOutput("graph_view", height = "100vh")
    } else {
      ## Render tree
      output$tree_view <- renderJstree({
        jstree(s4_to_tree(sample_data), checkboxes = FALSE)
      })

      ## Tree view
      jstreeOutput("tree_view", height = "100vh")
    }
  })
}

shinyApp(ui, server)
