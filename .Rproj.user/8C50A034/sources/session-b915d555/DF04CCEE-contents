#' Strip ggplot portion from end of pipeline
#'
#' Given a pipeline that ends with ggplot, return just the dplyr part
#' @noRd
strip_ggplot_from_pipe <- function(expr) {
  if (!rlang::is_call(expr)) return(expr)
  
  # Check if the top level is a + operator (ggplot layers)
  if (rlang::call_name(expr) == "+") {
    # The LHS should contain the pipeline
    lhs <- expr[[2]]
    # Strip ggplot from LHS
    return(strip_ggplot_from_pipe(lhs))
  }
  
  # If this is a pipe call
  if (is_pipe_call(expr)) {
    lhs <- expr[[2]]
    rhs <- expr[[3]]
    
    # If RHS contains ggplot, return only LHS
    if (contains_ggplot(rhs)) {
      return(lhs)
    }
    
    # Otherwise, recurse on LHS and reconstruct
    stripped_lhs <- strip_ggplot_from_pipe(lhs)
    
    # If the LHS changed, reconstruct the pipe
    if (!identical(stripped_lhs, lhs)) {
      expr[[2]] <- stripped_lhs
    }
    
    return(expr)
  }
  
  # Not a pipe or +, return as-is
  return(expr)
}

#' Split a magrittr/native pipe expression into a list of step calls
#'
#' @param expr An expression/quosure like `df %>% filter(...) %>% mutate(...)`
#' @param detect_ggplot If TRUE, detect and separate ggplot calls
#' @return A list with elements: input, steps, output, has_ggplot
#' @noRd
pipe_split <- function(expr, detect_ggplot = TRUE) {
  if (rlang::is_quosure(expr)) expr <- rlang::get_expr(expr)
  
  if (!rlang::is_call(expr) && !rlang::is_symbol(expr)) {
    rlang::abort(
      "Expected a call or symbol expression",
      class = "pipe_split_invalid_input"
    )
  }
  
  # Check if this is an assignment
  output <- NULL
  pipeline_expr <- expr
  
  if (is_assignment(expr)) {
    if (rlang::is_call(expr, "->")) {
      pipeline_expr <- expr[[2]]
      output <- as.character(expr[[3]])
    } else {
      output <- as.character(expr[[2]])
      pipeline_expr <- expr[[3]]
    }
  }
  
  # Remove ggplot portion if present
  has_ggplot <- FALSE
  if (detect_ggplot && contains_ggplot(pipeline_expr)) {
    has_ggplot <- TRUE
    pipeline_expr <- strip_ggplot_from_pipe(pipeline_expr)
  }
  
  steps <- list()
  input <- NULL
  
  walk <- function(e) {
    if (is_pipe_call(e)) {
      lhs <- e[[2]]
      rhs <- e[[3]]
      walk(lhs)
      steps[[length(steps) + 1]] <<- rhs
    } else {
      input <<- e
    }
  }
  
  walk(pipeline_expr)
  
  if (is.null(input)) {
    rlang::abort("Could not identify pipeline input")
  }
  
  list(input = input, steps = steps, output = output, has_ggplot = has_ggplot)
}

#' Check if expression is a pipe call
#'
#' @param x An expression to check
#' @return TRUE if x is a pipe call (%>% or |>), FALSE otherwise
#' @noRd
is_pipe_call <- function(x) {
  if (!rlang::is_call(x)) return(FALSE)
  fn <- rlang::call_name(x)
  fn %in% c("%>%", "%<>%")
}

#' Check if expression is an assignment
#'
#' @param x An expression to check
#' @return TRUE if x is an assignment (<-, =, ->), FALSE otherwise
#' @noRd
is_assignment <- function(x) {
  rlang::is_call(x, "<-") || 
    rlang::is_call(x, "=") ||
    rlang::is_call(x, "->")
}

#' Convert expression to single-line label
#'
#' @param expr An expression to convert
#' @param max_width Maximum width before line breaking
#' @return A single-line string representation
#' @noRd
as_label_1line <- function(expr, max_width = 60) {
  txt <- paste(deparse(expr, width.cutoff = max_width), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  
  if (nchar(txt) > max_width * 1.5) {
    txt <- paste0(substr(txt, 1, max_width * 1.5), "...")
  }
  txt
}

#' Helper function to check if expression contains ggplot anywhere in the tree
#'
#' @noRd
contains_ggplot <- function(expr) {
  if (!rlang::is_call(expr)) return(FALSE)
  
  fn_name <- rlang::call_name(expr) %||% ""
  
  if (grepl("^(ggplot|geom_|scale_|theme|labs|ggtitle|xlab|ylab|coord_|facet_|aes)", fn_name)) {
    return(TRUE)
  }
  
  if (fn_name == "+") {
    return(contains_ggplot(expr[[2]]) || contains_ggplot(expr[[3]]))
  }
  
  for (i in seq_along(expr)) {
    if (rlang::is_call(expr[[i]])) {
      if (contains_ggplot(expr[[i]])) return(TRUE)
    }
  }
  
  FALSE
}

#' Extract output variable names from mutate/summarize calls
#'
#' @param call A call object
#' @param max_show Maximum number of outputs to show
#' @return A list of output information or NULL
#' @noRd
extract_outputs <- function(call, max_show = 5) {
  if (!rlang::is_call(call)) return(NULL)
  
  fn_name <- rlang::call_name(call) %||% "unknown"
  
  if (!fn_name %in% c("mutate", "transmute", "summarise", "summarize", "rename")) {
    return(NULL)
  }
  
  args <- rlang::call_args(call)
  arg_names <- names(args)
  if (is.null(arg_names)) return(NULL)
  
  named_args <- arg_names[arg_names != "" & !startsWith(arg_names, ".")]
  if (length(named_args) == 0) return(NULL)
  
  outputs <- purrr::map(named_args, function(nm) {
    expr <- args[[nm]]
    rhs <- deparse(expr, width.cutoff = 500)[1]
    
    max_len <- 40
    if (nchar(rhs) > max_len) {
      rhs <- paste0(substr(rhs, 1, max_len), "...")
    }
    
    list(name = nm, expr = rhs)
  })
  
  if (length(outputs) > max_show) {
    outputs <- c(
      outputs[1:max_show],
      list(list(name = sprintf("... +%d more", length(outputs) - max_show), expr = ""))
    )
  }
  
  outputs
}
