#' Visualize dplyr pipelines from a file
#'
#' Extract and visualize all dplyr pipelines from an R script, R Markdown, 
#' or Quarto file.
#'
#' @param file Path to .R, .Rmd, or .Qmd file
#' @param combine If TRUE, show all pipelines in one diagram. If FALSE, 
#'   create separate diagrams for each pipeline.
#' @param direction Graph layout direction
#' @param theme Visual theme
#' @param show_expressions Show RHS of assignments
#' @param fixed_width Make all nodes same width
#' @return A visualization or list of visualizations
#' 
#' @examples
#' \dontrun{
#' # Visualize all pipelines in a script
#' pipe_vizr_file("my_analysis.R")
#' 
#' # Separate diagram for each pipeline
#' pipe_vizr_file("my_analysis.R", combine = FALSE)
#' 
#' # From R Markdown
#' pipe_vizr_file("report.Rmd")
#' }
#' 
#' @export
pipe_vizr_file <- function(file,
                           combine = TRUE,
                           direction = "LR",
                           theme = "default",
                           show_expressions = TRUE,
                           fixed_width = TRUE) {
  
  # Extract code
  code <- extract_r_code(file)
  
  # Find pipelines
  pipelines <- find_pipelines(code)
  
  if (length(pipelines) == 0) {
    message("No dplyr pipelines found in ", file)
    return(invisible(NULL))
  }
  
  message(sprintf("Found %d pipeline%s in %s", 
                  length(pipelines), 
                  if (length(pipelines) == 1) "" else "s",
                  basename(file)))
  
  # Build graphs
  graphs <- build_multi_graph(pipelines, combine = combine)
  
  # Render
  if (combine) {
    pipe_render(graphs[[1]], direction, theme, show_expressions, fixed_width)
  } else {
    purrr::map(graphs, ~pipe_render(.x, direction, theme, show_expressions, fixed_width))
  }
}
