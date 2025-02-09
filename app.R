library(shiny)
library(bslib)
library(visNetwork)
library(jsTreeR)
library(methods)

floating_card <- function(Id, ..., .title = toupper(Id)) {
  stopifnot(!missing(Id)) # Id is required!
  
  return(
    card(
      h4(class = "card-title", .title),
      ...,
      class = "floating-container",
      id = Id
    )
  )
}

# Example S4 class and objects for demonstration
setClass("Person",
         slots = list(
           name = "character",
           age = "numeric",
           details = "list"
         )
)

# Create sample data
sample_data <- list(
  person1 = new("Person", name = "John", age = 30,
                details = list(city = "New York", role = "Developer")),
  person2 = new("Person", name = "Alice", age = 25,
                details = list(city = "London", role = "Designer")),
  person3 = new("Person", name = "Bob", age = 35,
                details = list(city = "Paris", role = "Manager"))
)

# Helper functions remain the same
s4_to_tree <- function(obj_list) {
  nodes <- lapply(names(obj_list), function(name) {
    obj <- obj_list[[name]]
    slots <- slotNames(obj)
    children <- lapply(slots, function(slot) {
      slot_value <- slot(obj, slot)
      if (is.list(slot_value)) {
        list(
          text = slot,
          children = lapply(names(slot_value), function(k) {
            list(text = paste(k, ":", slot_value[[k]]))
          })
        )
      } else {
        list(text = paste(slot, ":", slot_value))
      }
    })
    
    list(
      text = name,
      children = children
    )
  })
  
  nodes
}

s4_to_graph <- function(obj_list) {
  nodes <- data.frame()
  edges <- data.frame()
  node_id <- 1
  
  for (name in names(obj_list)) {
    obj <- obj_list[[name]]
    class_name <- class(obj)
    
    main_node <- data.frame(
      id = node_id,
      label = paste(name, "\n(", class_name, ")"),
      group = "main"
    )
    nodes <- rbind(nodes, main_node)
    
    parent_id <- node_id
    node_id <- node_id + 1
    
    slots <- slotNames(obj)
    for (slot in slots) {
      slot_value <- slot(obj, slot)
      if (is.list(slot_value)) {
        for (k in names(slot_value)) {
          nodes <- rbind(nodes, data.frame(
            id = node_id,
            label = paste(k, ":", slot_value[[k]]),
            group = "detail"
          ))
          edges <- rbind(edges, data.frame(
            from = parent_id,
            to = node_id
          ))
          node_id <- node_id + 1
        }
      } else {
        nodes <- rbind(nodes, data.frame(
          id = node_id,
          label = paste(slot, ":", slot_value),
          group = "slot"
        ))
        edges <- rbind(edges, data.frame(
          from = parent_id,
          to = node_id
        ))
        node_id <- node_id + 1
      }
    }
  }
  
  list(nodes = nodes, edges = edges)
}

ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      .floating-container {
        position: fixed;
        z-index: 900;
        background: white;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        min-width: 200px;
        min-height: 100px;
        touch-action: none;
      }
      .floating-container .card-title {
        cursor: move;
        padding: 5px;
        margin: -5px;
        background: #f8f9fa;
        border-radius: 5px 5px 0 0;
      }
      .floating-container.dragging {
        opacity: 0.8;
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        let isDragging = false;
        let currentX;
        let currentY;
        let initialX;
        let initialY;
        let xOffset = 0;
        let yOffset = 0;
        let dragItem = null;

        function dragStart(e) {
          if (e.target.closest('.card-title')) {
            dragItem = e.target.closest('.floating-container');
            
            if (e.type === 'touchstart') {
              initialX = e.touches[0].clientX - xOffset;
              initialY = e.touches[0].clientY - yOffset;
            } else {
              initialX = e.clientX - xOffset;
              initialY = e.clientY - yOffset;
            }

            if (dragItem) {
              isDragging = true;
              dragItem.style.cursor = 'grabbing';
            }
          }
        }

        function drag(e) {
          if (isDragging) {
            e.preventDefault();

            if (e.type === 'touchmove') {
              currentX = e.touches[0].clientX - initialX;
              currentY = e.touches[0].clientY - initialY;
            } else {
              currentX = e.clientX - initialX;
              currentY = e.clientY - initialY;
            }

            xOffset = currentX;
            yOffset = currentY;

            setTranslate(currentX, currentY, dragItem);
          }
        }

        function setTranslate(xPos, yPos, el) {
          el.style.transform = `translate3d(${xPos}px, ${yPos}px, 0)`;
        }

        function dragEnd(e) {
          if (dragItem) {
            initialX = currentX;
            initialY = currentY;
            dragItem.style.cursor = '';
            isDragging = false;
            dragItem = null;
          }
        }

        document.addEventListener('touchstart', dragStart, false);
        document.addEventListener('touchmove', drag, false);
        document.addEventListener('touchend', dragEnd, false);
        document.addEventListener('mousedown', dragStart, false);
        document.addEventListener('mousemove', drag, false);
        document.addEventListener('mouseup', dragEnd, false);
      });
    "))
  ),
  
  # Main content area
  div(
    style = "position: relative; height: 100vh;",
    
    floating_card("toggle_container",
                  input_switch("main_view_toggle", "View as Graph?", value = TRUE),
                  tags$hr(),
                  checkboxGroupInput("float_toggles", "Show Floating Containers:",
                                   choices = c("Info" = "info",
                                             "Controls" = "controls",
                                             "Details" = "details"))),
    
    # Floating containers
    uiOutput("floating_containers"),
    
    # Main view area
    uiOutput("main_view")
  )
)

server <- function(input, output, session) {
  ## Floating containers
  output$floating_containers <- renderUI({
    containers <- list()
    
    if ("info" %in% input$float_toggles) {
      containers[[1]] <- floating_card("info_container", "An informational container.")
    }
    
    if ("controls" %in% input$float_toggles) {
      containers[[2]] <- floating_card("controls_container", "A container for controls.")
    }
    
    if ("details" %in% input$float_toggles) {
      containers[[3]] <- floating_card("details_container", "A container for details.")
    }
    
    div(
      id = "floating_containers_wrapper",
      do.call(tagList, containers)
    )
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
