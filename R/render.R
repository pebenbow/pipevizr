#' Calculate the display width of a node's content
#'
#' @keywords internal
calculate_node_width <- function(label, details, type, show_expressions) {
  # Start with the function name width
  fn_name <- sub("\\(.*$", "", label)
  max_chars <- nchar(fn_name)
  
  # Check detail widths if present
  if (!is.null(details) && length(details) > 0) {
    for (d in details) {
      if (type %in% c("mutate", "summarize", "transmute")) {
        if (show_expressions && !is.null(d$expr) && d$expr != "") {
          # "name = expr" format
          line_chars <- nchar(d$name) + 3 + nchar(d$expr)  # +3 for " = "
        } else {
          line_chars <- nchar(d$name)
        }
      } else if (type == "filter") {
        line_chars <- nchar(d$name)
      } else if (type == "arrange") {
        line_chars <- 2 + nchar(d$name)  # arrow + space + name
      } else if (type == "select" || type == "rename") {
        if (!is.null(d$expr) && d$expr != "") {
          line_chars <- nchar(d$name) + 3 + nchar(d$expr)  # " ← " format
        } else {
          line_chars <- nchar(d$name)
        }
      } else {
        line_chars <- nchar(d$name)
      }
      
      max_chars <- max(max_chars, line_chars)
    }
  } else {
    # For nodes without details, use the full label
    max_chars <- max(max_chars, nchar(label))
  }
  
  # Convert characters to approximate width in inches
  # Rough estimate: ~12 characters per inch at font size 10
  # Add some padding for margins, icon space, etc.
  width_inches <- (max_chars / 12) + 0.8  # +0.8 for icon and padding
  
  width_inches
}

#' Create a formatted label with icon and details for Graphviz HTML-like labels
#'
#' @noRd
create_node_label <- function(icon, label, is_input = FALSE, is_output = FALSE, 
                              is_ggplot = FALSE, details = NULL, type = "other", 
                              show_expressions = TRUE) {
  esc_html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  
  label <- esc_html(label)
  
  # Input/Output/ggplot nodes: always show icon + label
  if (is_input || is_output || is_ggplot) {
    return(sprintf(
      '<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="4">
        <TR>
          <TD ALIGN="LEFT"><FONT POINT-SIZE="14">%s</FONT></TD>
          <TD ALIGN="LEFT"><FONT POINT-SIZE="12"><B>%s</B></FONT></TD>
        </TR>
      </TABLE>>',
      icon, label
    ))
  }
  
  # Extract just the function name for the header
  fn_name <- sub("\\(.*$", "", label)
  fn_name <- esc_html(fn_name)
  
  # For join nodes, use "join" as the header
  if (type == "join") {
    fn_name <- "join"
  }
  
  # Build the table
  if (!is.null(details) && length(details) > 0) {
    # Header row: icon and function name, both left-aligned
    rows <- sprintf(
      '<TR>
         <TD ALIGN="LEFT" VALIGN="TOP"><FONT POINT-SIZE="14">%s</FONT></TD>
         <TD ALIGN="LEFT" VALIGN="TOP"><FONT POINT-SIZE="10"><B>%s</B></FONT></TD>
       </TR>',
      icon, fn_name
    )
    
    # Separator row
    rows <- c(rows, '<TR><TD COLSPAN="2" HEIGHT="1"></TD></TR>')
    
    # Add detail rows based on type
    detail_rows <- purrr::map_chr(details, function(d) {
      d_name <- esc_html(d$name)
      d_expr <- if (!is.null(d$expr)) esc_html(d$expr) else ""
      
      if (type %in% c("mutate", "summarize", "transmute")) {
        if (show_expressions && d_expr != "") {
          sprintf(
            '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT> <FONT POINT-SIZE="8" COLOR="#000000">= %s</FONT></TD></TR>',
            d_name, d_expr
          )
        } else {
          sprintf(
            '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT></TD></TR>',
            d_name
          )
        }
      } else if (type == "filter") {
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT></TD></TR>',
          d_name
        )
      } else if (type == "arrange") {
        arrow <- if (d_expr == "desc") "↓" else "↑"
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s %s</FONT></TD></TR>',
          arrow, d_name
        )
      } else if (type == "select") {
        # For select, d_name already contains comma-delimited string
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT></TD></TR>',
          d_name
        )
      } else if (type == "rename") {
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT> <FONT POINT-SIZE="8" COLOR="#000000">← %s</FONT></TD></TR>',
          d_name, d_expr
        )
      } else if (type == "join") {
        # Join details: show join type and by clause
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT></TD></TR>',
          d_name
        )
      } else {
        sprintf(
          '<TR><TD COLSPAN="2" ALIGN="LEFT"><FONT POINT-SIZE="9" COLOR="#000000">%s</FONT></TD></TR>',
          d_name
        )
      }
    })
    
    rows <- c(rows, detail_rows)
    
    sprintf(
      '<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="2">
        %s
      </TABLE>>',
      paste(rows, collapse = "\n        ")
    )
  } else {
    # No details to show - use simple format with icon and label side by side
    max_len <- 50
    if (nchar(label) > max_len) {
      label <- paste0(substr(label, 1, max_len - 3), "...")
    }
    
    sprintf(
      '<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="3">
        <TR>
          <TD ALIGN="LEFT" VALIGN="TOP"><FONT POINT-SIZE="14">%s</FONT></TD>
          <TD ALIGN="LEFT" VALIGN="MIDDLE"><FONT POINT-SIZE="10">%s</FONT></TD>
        </TR>
      </TABLE>>',
      icon, label
    )
  }
}

#' Render a pipeline graph with enhanced visual styling
#'
#' @param graph Output of pipe_graph() or build_multi_graph()
#' @param direction Graph direction
#' @param theme Visual theme
#' @param show_expressions Show RHS of assignments
#' @param fixed_width Make all nodes same width
#' @export
pipe_render <- function(graph, 
                        direction = c("LR", "TB", "RL", "BT"),
                        theme = c("default", "minimal", "colorful"),
                        show_expressions = TRUE,
                        fixed_width = TRUE) {
  
  direction <- match.arg(direction)
  theme <- match.arg(theme)
  
  # Check if this is a combined graph
  is_combined <- !is.null(graph$is_combined) && graph$is_combined
  has_branches <- !is.null(graph$has_branches) && graph$has_branches
  
  nodes <- graph$nodes
  edges <- graph$edges
  
  # Theme-based adjustments
  edge_style <- switch(theme,
                       "minimal" = 'color="#DADCE0", penwidth=1.5',
                       "colorful" = 'color="#9AA0A6", penwidth=2, arrowsize=0.8',
                       'color="#9AA0A6", penwidth=1.5'
  )
  
  node_border <- switch(theme,
                        "minimal" = 'penwidth=1',
                        "colorful" = 'penwidth=2',
                        'penwidth=1.5'
  )
  
  # Calculate width
  width_attr <- ""
  if (fixed_width) {
    node_widths <- purrr::pmap_dbl(
      list(nodes$label, nodes$details, nodes$type),
      function(label, details, type) {
        if (type %in% c("input", "output", "ggplot")) {
          return(nchar(label) / 12 + 0.5)
        }
        calculate_node_width(label, details, type, show_expressions)
      }
    )
    
    max_width <- max(node_widths, na.rm = TRUE)
    max_width <- max(2.0, min(8.0, max_width))
    width_attr <- sprintf(', width=%0.2f', max_width)
  }
  
  # Create node definitions
  node_lines <- purrr::pmap_chr(
    list(nodes$id, nodes$icon, nodes$label, nodes$type, nodes$color, nodes$details),
    function(id, icon, label, type, color, details) {
      is_input <- identical(type, "input")
      is_output <- identical(type, "output")
      is_ggplot <- identical(type, "ggplot")
      html_label <- create_node_label(icon, label, is_input, is_output, is_ggplot, 
                                      details, type, show_expressions)
      
      sprintf(
        '%s [label=%s, shape="box", style="rounded,filled", fillcolor="%s", %s%s];',
        id, html_label, color, node_border, width_attr
      )
    }
  )
  
  # Build DOT
  dot_parts <- c(
    "digraph pipe {",
    sprintf("  rankdir=%s;", direction)
  )
  
  # If we have branches, adjust graph settings for better layout
  if (has_branches) {
    dot_parts <- c(dot_parts,
                   "  graph [bgcolor=white, pad=0.2, splines=ortho, nodesep=0.5, ranksep=0.8];"
    )
  } else {
    dot_parts <- c(dot_parts,
                   "  graph [bgcolor=white, pad=0.2];"
    )
  }
  
  dot_parts <- c(dot_parts,
                 "  node  [fontname=\"Helvetica\"];",
                 "  edge  [arrowsize=0.7];",
                 "  "
  )
  
  # Add all nodes
  dot_parts <- c(dot_parts, paste0("  ", node_lines))
  
  # Add edges
  if (nrow(edges) > 0) {
    dot_parts <- c(dot_parts, "  ")
    
    edge_lines <- purrr::pmap_chr(
      list(edges$from, edges$to),
      function(from, to) sprintf("%s -> %s [%s];", from, to, edge_style)
    )
    
    dot_parts <- c(dot_parts, paste0("  ", edge_lines))
  }
  
  dot_parts <- c(dot_parts, "}")
  dot <- paste(dot_parts, collapse = "\n")
  
  DiagrammeR::grViz(dot)
}
