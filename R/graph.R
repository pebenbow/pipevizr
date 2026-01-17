#' Build a graph representation from a pipeline with verb metadata
#'
#' @param expr Pipeline expression
#' @return list(nodes=..., edges=...)
#' @export
pipe_graph <- function(expr) {
  parts <- pipe_split(expr, detect_ggplot = TRUE)
  
  # Analyze each step
  step_info <- purrr::map(parts$steps, analyze_verb)
  
  # Build nodes
  nodes <- tibble::tibble(
    id    = seq_len(1L + length(parts$steps)),
    label = c(
      as_label_1line(parts$input),
      purrr::map_chr(parts$steps, as_label_1line)
    ),
    type  = c("input", purrr::map_chr(step_info, "type")),
    icon  = c("📅", purrr::map_chr(step_info, "icon")),  # Table icon for input
    color = c("#E8F0FE", purrr::map_chr(step_info, "color")),
    detail = c(NA_character_, purrr::map_chr(step_info, ~.x$detail %||% NA_character_)),
    details = c(list(NULL), purrr::map(step_info, "details")),
    join_df = c(NA_character_, purrr::map_chr(step_info, ~.x$join_df %||% NA_character_))
  )
  
  # Add ggplot node if pipeline ends with ggplot
  if (parts$has_ggplot) {
    ggplot_node <- tibble::tibble(
      id = nrow(nodes) + 1L,
      label = "ggplot",
      type = "ggplot",
      icon = "📊",
      color = "#FFF3E0",
      detail = NA_character_,
      details = list(NULL),
      join_df = NA_character_
    )
    nodes <- dplyr::bind_rows(nodes, ggplot_node)
  }
  
  # Add output node if there's an assignment
  if (!is.null(parts$output)) {
    output_node <- tibble::tibble(
      id = nrow(nodes) + 1L,
      label = parts$output,
      type = "output",
      icon = "📅",  # Table icon for output
      color = "#E8F0FE",
      detail = NA_character_,
      details = list(NULL),
      join_df = NA_character_
    )
    nodes <- dplyr::bind_rows(nodes, output_node)
  }
  
  # Build edges
  edges <- if (nrow(nodes) >= 2) {
    tibble::tibble(
      from = nodes$id[-nrow(nodes)],
      to   = nodes$id[-1]
    )
  } else {
    tibble::tibble(from = integer(), to = integer())
  }
  
  list(nodes = nodes, edges = edges)
}

#' Build multiple graphs and combine with shared inputs and join dependencies
#'
#' @param pipelines List of pipeline expressions
#' @param combine If TRUE, combine into single graph with branching
#' @return List of graphs or single combined graph
#' @noRd
build_multi_graph <- function(pipelines, combine = TRUE) {
  if (length(pipelines) == 0) {
    return(list())
  }
  
  # Build individual graphs
  graphs <- purrr::map(pipelines, pipe_graph)
  
  if (!combine) {
    return(graphs)
  }
  
  # Extract all inputs and outputs
  input_labels <- purrr::map_chr(graphs, function(g) {
    input_node <- g$nodes[g$nodes$type == "input", ]
    if (nrow(input_node) > 0) input_node$label[1] else NA_character_
  })
  
  output_labels <- purrr::map(graphs, function(g) {
    output_nodes <- g$nodes[g$nodes$type == "output", ]
    if (nrow(output_nodes) > 0) output_nodes$label else character(0)
  })
  
  # Flatten output labels
  all_outputs <- unique(unlist(output_labels))
  
  # Find shared inputs (appear in multiple pipelines)
  unique_inputs <- unique(input_labels[!is.na(input_labels)])
  shared_inputs <- unique_inputs[table(input_labels)[unique_inputs] > 1]
  
  # Create node ID mapping
  node_label_to_id <- list()
  next_id <- 1L
  
  # Assign IDs to shared inputs first
  for (inp in shared_inputs) {
    node_label_to_id[[inp]] <- next_id
    next_id <- next_id + 1L
  }
  
  # Track which nodes we've added
  added_nodes <- character(0)
  
  # Rebuild graphs
  all_nodes <- list()
  all_edges <- list()
  
  for (i in seq_along(graphs)) {
    g <- graphs[[i]]
    input_label <- input_labels[i]
    
    # Determine if input is shared or reused from output
    input_is_shared <- !is.na(input_label) && input_label %in% shared_inputs
    input_is_output <- !is.na(input_label) && input_label %in% all_outputs
    
    if (input_is_shared) {
      # Shared input across multiple pipelines
      shared_input_id <- node_label_to_id[[input_label]]
      
      if (!input_label %in% added_nodes) {
        input_node <- g$nodes[g$nodes$type == "input", ]
        input_node$id <- shared_input_id
        input_node$pipeline_num <- i
        all_nodes[[length(all_nodes) + 1]] <- input_node
        added_nodes <- c(added_nodes, input_label)
      }
      
      # Process rest of pipeline
      non_input_nodes <- g$nodes[g$nodes$type != "input", ]
      if (nrow(non_input_nodes) > 0) {
        non_input_nodes$id <- non_input_nodes$id - 1L + next_id
        non_input_nodes$pipeline_num <- i
        all_nodes[[length(all_nodes) + 1]] <- non_input_nodes
        
        # Update edges
        if (nrow(g$edges) > 0) {
          updated_edges <- g$edges
          updated_edges$from[1] <- shared_input_id
          updated_edges$to[1] <- non_input_nodes$id[1]
          
          if (nrow(updated_edges) > 1) {
            for (j in 2:nrow(updated_edges)) {
              updated_edges$from[j] <- updated_edges$from[j] - 1L + next_id
              updated_edges$to[j] <- updated_edges$to[j] - 1L + next_id
            }
          }
          
          all_edges[[length(all_edges) + 1]] <- updated_edges
        }
        
        next_id <- max(non_input_nodes$id) + 1L
      }
      
    } else if (input_is_output) {
      # Input is actually an output from another pipeline - reuse that node
      reused_node_id <- node_label_to_id[[input_label]]
      
      # Don't create new input node, start from first transformation
      non_input_nodes <- g$nodes[g$nodes$type != "input", ]
      if (nrow(non_input_nodes) > 0) {
        non_input_nodes$id <- non_input_nodes$id - 1L + next_id
        non_input_nodes$pipeline_num <- i
        all_nodes[[length(all_nodes) + 1]] <- non_input_nodes
        
        # Create edge from reused output node to first step
        first_step_id <- non_input_nodes$id[1]
        new_edge <- tibble::tibble(from = reused_node_id, to = first_step_id)
        all_edges[[length(all_edges) + 1]] <- new_edge
        
        # Update remaining edges
        if (nrow(g$edges) > 1) {
          remaining_edges <- g$edges[-1, ]
          remaining_edges$from <- remaining_edges$from - 1L + next_id
          remaining_edges$to <- remaining_edges$to - 1L + next_id
          all_edges[[length(all_edges) + 1]] <- remaining_edges
        }
        
        next_id <- max(non_input_nodes$id) + 1L
      }
      
    } else {
      # Unique input - process normally
      g$nodes$id <- g$nodes$id + next_id - 1L
      g$nodes$pipeline_num <- i
      
      # Store output node IDs for later join edge creation
      output_nodes <- g$nodes[g$nodes$type == "output", ]
      for (j in seq_len(nrow(output_nodes))) {
        node_label_to_id[[output_nodes$label[j]]] <- output_nodes$id[j]
      }
      
      if (nrow(g$edges) > 0) {
        g$edges$from <- g$edges$from + next_id - 1L
        g$edges$to <- g$edges$to + next_id - 1L
        all_edges[[length(all_edges) + 1]] <- g$edges
      }
      
      all_nodes[[length(all_nodes) + 1]] <- g$nodes
      next_id <- max(g$nodes$id) + 1L
    }
    
    # Store output node IDs for join edge creation
    output_nodes <- g$nodes[g$nodes$type == "output", ]
    if (nrow(output_nodes) > 0) {
      for (j in seq_len(nrow(output_nodes))) {
        # Get the actual ID after offsetting
        actual_nodes <- all_nodes[[length(all_nodes)]]
        matching_output <- actual_nodes[actual_nodes$type == "output" & 
                                          actual_nodes$label == output_nodes$label[j], ]
        if (nrow(matching_output) > 0) {
          node_label_to_id[[matching_output$label[1]]] <- matching_output$id[1]
        }
      }
    }
  }
  
  combined_nodes <- dplyr::bind_rows(all_nodes)
  combined_edges <- dplyr::bind_rows(all_edges)
  
  # Add edges for joins - connect the joined data frame to the join node
  join_nodes <- combined_nodes[combined_nodes$type == "join" & !is.na(combined_nodes$join_df), ]
  
  for (i in seq_len(nrow(join_nodes))) {
    join_df_name <- join_nodes$join_df[i]
    join_node_id <- join_nodes$id[i]
    
    # Find the node with this output name
    if (join_df_name %in% names(node_label_to_id)) {
      source_node_id <- node_label_to_id[[join_df_name]]
      
      # Add edge from source to join node
      join_edge <- tibble::tibble(from = source_node_id, to = join_node_id)
      combined_edges <- dplyr::bind_rows(combined_edges, join_edge)
    }
  }
  
  list(
    list(
      nodes = combined_nodes,
      edges = combined_edges,
      is_combined = TRUE,
      num_pipelines = length(pipelines),
      has_branches = length(shared_inputs) > 0 || nrow(join_nodes) > 0
    )
  )
}
