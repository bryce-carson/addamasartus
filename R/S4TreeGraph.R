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
