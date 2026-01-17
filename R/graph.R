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
    icon  = c("", purrr::map_chr(step_info, "icon")),
    color = c("#E8F0FE", purrr::map_chr(step_info, "color")),
    detail = c(NA_character_, purrr::map_chr(step_info, ~.x$detail %||% NA_character_)),
    details = c(list(NULL), purrr::map(step_info, "details"))
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
      details = list(NULL)
    )
    nodes <- dplyr::bind_rows(nodes, ggplot_node)
  }
  
  # Add output node if there's an assignment
  if (!is.null(parts$output)) {
    output_node <- tibble::tibble(
      id = nrow(nodes) + 1L,
      label = parts$output,
      type = "output",
      icon = "",
      color = "#E8F0FE",
      detail = NA_character_,
      details = list(NULL)
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

#' Build multiple graphs and combine with shared inputs
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
  
  # Identify shared inputs
  input_labels <- purrr::map_chr(graphs, function(g) {
    input_node <- g$nodes[g$nodes$type == "input", ]
    if (nrow(input_node) > 0) input_node$label[1] else NA_character_
  })
  
  # Create mapping of shared inputs
  unique_inputs <- unique(input_labels[!is.na(input_labels)])
  input_id_map <- list()
  shared_inputs <- unique_inputs[table(input_labels)[unique_inputs] > 1]
  
  # Assign IDs to shared inputs
  next_id <- 1L
  for (inp in shared_inputs) {
    input_id_map[[inp]] <- next_id
    next_id <- next_id + 1L
  }
  
  # Track which shared inputs we've added
  added_shared_inputs <- character(0)
  
  # Rebuild graphs with shared input IDs
  all_nodes <- list()
  all_edges <- list()
  
  for (i in seq_along(graphs)) {
    g <- graphs[[i]]
    input_label <- input_labels[i]
    
    # Check if this is a shared input
    if (!is.na(input_label) && input_label %in% names(input_id_map)) {
      # This pipeline has a shared input
      shared_input_id <- input_id_map[[input_label]]
      
      # Add input node only once
      if (!input_label %in% added_shared_inputs) {
        input_node <- g$nodes[g$nodes$type == "input", ]
        input_node$id <- shared_input_id
        input_node$pipeline_num <- i
        all_nodes[[length(all_nodes) + 1]] <- input_node
        added_shared_inputs <- c(added_shared_inputs, input_label)
      }
      
      # Process non-input nodes with offset
      non_input_nodes <- g$nodes[g$nodes$type != "input", ]
      if (nrow(non_input_nodes) > 0) {
        non_input_nodes$id <- non_input_nodes$id - 1L + next_id
        non_input_nodes$pipeline_num <- i
        all_nodes[[length(all_nodes) + 1]] <- non_input_nodes
        
        # Update edges
        if (nrow(g$edges) > 0) {
          updated_edges <- g$edges
          # Edge from input to first step
          updated_edges$from[1] <- shared_input_id
          updated_edges$to[1] <- non_input_nodes$id[1]
          
          # Remaining edges
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
      
    } else {
      # This pipeline has a unique input - keep it separate
      # Offset all IDs
      g$nodes$id <- g$nodes$id + next_id - 1L
      g$nodes$pipeline_num <- i
      
      # Offset edges
      if (nrow(g$edges) > 0) {
        g$edges$from <- g$edges$from + next_id - 1L
        g$edges$to <- g$edges$to + next_id - 1L
        all_edges[[length(all_edges) + 1]] <- g$edges
      }
      
      all_nodes[[length(all_nodes) + 1]] <- g$nodes
      
      next_id <- max(g$nodes$id) + 1L
    }
  }
  
  combined_nodes <- dplyr::bind_rows(all_nodes)
  combined_edges <- dplyr::bind_rows(all_edges)
  
  list(
    list(
      nodes = combined_nodes,
      edges = combined_edges,
      is_combined = TRUE,
      num_pipelines = length(pipelines),
      has_branches = length(shared_inputs) > 0
    )
  )
}
