#' Visualize a dplyr-style pipeline with icons
#'
#' @param ... A single pipeline expression
#' @param direction Graph layout direction: "LR" (left-right), "TB" (top-bottom), 
#'   "RL" (right-left), "BT" (bottom-top)
#' @param theme Visual theme: "default", "minimal", or "colorful"
#' @param show_expressions Show the RHS of assignments in mutate/summarize nodes
#' @param fixed_width Make all nodes the same width (TRUE by default)
#' @param return_graph If TRUE, return the graph object instead of rendering
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' # Basic usage with fixed width
#' pipe_vizr(
#'   mtcars %>%
#'     filter(cyl > 4, mpg > 20) %>%
#'     arrange(desc(mpg)) %>%
#'     select(mpg, cyl, hp) %>%
#'     mutate(
#'       efficiency = mpg / hp,
#'       power_ratio = hp / cyl
#'     ) %>%
#'     summarize(
#'       avg_eff = mean(efficiency),
#'       max_eff = max(efficiency)
#'     )
#' )
#' 
#' # Without fixed width (variable node widths)
#' pipe_vizr(
#'   mtcars %>%
#'     filter(cyl > 4) %>%
#'     select(mpg),
#'   fixed_width = FALSE
#' )
#' }
#' 
#' @export
pipe_vizr <- function(..., 
                      direction = "LR", 
                      theme = "default",
                      show_expressions = TRUE,
                      fixed_width = TRUE,
                      return_graph = FALSE) {
  dots <- as.list(substitute(list(...)))[-1]
  
  if (length(dots) != 1) {
    rlang::abort("pipe_vizr() expects exactly one pipeline expression.")
  }
  
  expr <- dots[[1]]
  graph <- pipe_graph(expr)
  
  if (return_graph) {
    return(graph)
  }
  
  pipe_render(graph, 
              direction = direction, 
              theme = theme, 
              show_expressions = show_expressions,
              fixed_width = fixed_width)
}
