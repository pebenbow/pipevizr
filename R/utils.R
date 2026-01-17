#' Display a legend of available verb icons
#'
#' Shows a table mapping dplyr verbs to their visualization icons and colors.
#'
#' @return A tibble (printed to console, returned invisibly)
#' @export
#' @examples
#' \dontrun{
#' pipe_legend()
#' }
pipe_legend <- function() {
  legend_data <- tibble::tribble(
    ~Icon, ~Verb, ~Description, ~Color,
    "🔍", "filter/slice", "Filter rows", "#FFF4E6",
    "◫", "select", "Select columns", "#E8F5E9",
    "✎", "rename", "Rename columns", "#E8F5E9",
    "✚", "mutate", "Create/modify columns", "#E3F2FD",
    "Σ", "summarize", "Aggregate data", "#F3E5F5",
    "▦", "group_by", "Group data", "#FFF9C4",
    "▢", "ungroup", "Remove grouping", "#FFF9C4",
    "⇅", "arrange", "Sort rows", "#E0F2F1",
    "⋈", "*_join", "Join tables", "#FCE4EC",
    "⇄", "pivot_*", "Reshape data", "#F1F8E9",
    "✂", "separate/unite", "Split/combine columns", "#FFF3E0",
    "◈", "distinct", "Remove duplicates", "#E8EAF6",
    "→", "pull", "Extract vector", "#EFEBE9",
    "⚙", "other", "Other operations", "#F8F9FA"
  )
  
  print(legend_data)
  invisible(legend_data)
}

#' Get the color scheme used for different verb types
#'
#' Returns a named list of colors used in pipeline visualizations.
#'
#' @return A named list of hex color codes
#' @export
#' @examples
#' \dontrun{
#' colors <- pipe_colors()
#' colors$filter  # "#FFF4E6"
#' }
pipe_colors <- function() {
  list(
    input = "#E8F0FE",
    filter = "#FFF4E6",
    select = "#E8F5E9",
    rename = "#E8F5E9",
    mutate = "#E3F2FD",
    summarize = "#F3E5F5",
    group = "#FFF9C4",
    ungroup = "#FFF9C4",
    arrange = "#E0F2F1",
    join = "#FCE4EC",
    pivot = "#F1F8E9",
    reshape = "#FFF3E0",
    distinct = "#E8EAF6",
    extract = "#EFEBE9",
    other = "#F8F9FA"
  )
}

#' Print a text summary of a pipeline
#'
#' Displays a human-readable summary of the pipeline structure without
#' rendering a visualization.
#'
#' @param expr A pipeline expression
#' @return Pipeline parts (invisibly)
#' @export
#' @examples
#' \dontrun{
#' pipe_summary(quote(
#'   mtcars %>%
#'     filter(cyl > 4) %>%
#'     mutate(efficiency = mpg / hp)
#' ))
#' }
pipe_summary <- function(expr) {
  parts <- pipe_split(expr)
  
  cat("Pipeline Summary\n")
  cat("================\n")
  cat("Input:", as_label_1line(parts$input), "\n")
  cat("Steps:", length(parts$steps), "\n\n")
  
  for (i in seq_along(parts$steps)) {
    step_info <- analyze_verb(parts$steps[[i]])
    cat(sprintf("  %d. %s %s\n", i, step_info$icon, 
                sub("\\(.*$", "", as_label_1line(parts$steps[[i]]))))
    
    if (!is.null(step_info$details) && length(step_info$details) > 0) {
      for (d in step_info$details[1:min(3, length(step_info$details))]) {
        cat(sprintf("      %s\n", d$name))
      }
      if (length(step_info$details) > 3) {
        cat(sprintf("      ... +%d more\n", length(step_info$details) - 3))
      }
    }
  }
  
  invisible(parts)
}
