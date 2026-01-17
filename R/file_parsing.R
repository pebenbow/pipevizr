#' Extract R code from a file
#'
#' @param file Path to R script, Rmd, or Qmd file
#' @return Character vector of R code lines
#' @noRd
extract_r_code <- function(file) {
  if (!file.exists(file)) {
    rlang::abort(sprintf("File not found: %s", file))
  }
  
  ext <- tolower(tools::file_ext(file))
  
  if (ext %in% c("rmd", "qmd")) {
    temp_r <- tempfile(fileext = ".R")
    on.exit(unlink(temp_r), add = TRUE)
    
    knitr::purl(file, output = temp_r, quiet = TRUE, documentation = 0)
    code <- readLines(temp_r, warn = FALSE)
  } else if (ext == "r") {
    code <- readLines(file, warn = FALSE)
  } else {
    rlang::abort(sprintf("Unsupported file type: %s. Use .R, .Rmd, or .Qmd files.", ext))
  }
  
  code
}

#' Detect base R pipe in text before parsing
#'
#' @param text Character vector of code lines
#' @return TRUE if contains |>
#' @noRd
has_base_pipe <- function(text) {
  any(grepl("\\|>", text))
}

#' Convert base R pipes to magrittr pipes in text
#'
#' @param text Character string of code
#' @return Modified text with %>% instead of |>
#' @noRd
convert_base_to_magrittr <- function(text) {
  gsub("\\|>", "%>%", text)
}

#' Check if expression contains ggplot anywhere
#'
#' @param expr An expression
#' @return TRUE if contains ggplot-related calls
#' @noRd
contains_ggplot <- function(expr) {
  if (!rlang::is_call(expr)) return(FALSE)
  
  fn_name <- rlang::call_name(expr) %||% ""
  
  if (grepl("^(ggplot|geom_|scale_|theme|labs|ggtitle|xlab|ylab|coord_|facet_|aes)", fn_name)) {
    return(TRUE)
  }
  
  # Check if it's a + operator (for ggplot layers)
  if (fn_name == "+") {
    return(contains_ggplot(expr[[2]]) || contains_ggplot(expr[[3]]))
  }
  
  # Recurse into all parts
  for (i in seq_along(expr)) {
    if (rlang::is_call(expr[[i]])) {
      if (contains_ggplot(expr[[i]])) return(TRUE)
    }
  }
  
  FALSE
}

#' Check if expression is or contains a dplyr pipeline
#'
#' @param expr An expression
#' @return TRUE if contains dplyr pipeline operations
#' @noRd
is_dplyr_pipeline <- function(expr) {
  if (!rlang::is_call(expr)) return(FALSE)
  
  # If it's an assignment, check the RHS
  if (is_assignment(expr)) {
    if (rlang::is_call(expr, "->")) {
      expr <- expr[[2]]
    } else {
      expr <- expr[[3]]
    }
  }
  
  has_pipe <- FALSE
  has_dplyr <- FALSE
  
  dplyr_verbs <- c(
    "filter", "select", "mutate", "summarize", "summarise",
    "arrange", "group_by", "ungroup", "rename", "relocate",
    "slice", "slice_head", "slice_tail", "slice_sample",
    "slice_min", "slice_max",
    "left_join", "right_join", "inner_join", "full_join",
    "semi_join", "anti_join", "distinct", "count", "tally",
    "pull", "transmute", "rowwise", "across", "add_count",
    "add_tally"
  )
  
  walk_expr <- function(e) {
    if (!rlang::is_call(e)) return()
    
    fn_name <- rlang::call_name(e) %||% ""
    
    if (fn_name %in% c("%>%", "%<>%")) {
      has_pipe <<- TRUE
    }
    
    if (fn_name %in% dplyr_verbs) {
      has_dplyr <<- TRUE
    }
    
    for (i in seq_along(e)) {
      if (rlang::is_call(e[[i]])) {
        walk_expr(e[[i]])
      }
    }
  }
  
  walk_expr(expr)
  has_pipe && has_dplyr
}

#' Find all pipeline expressions in code
#'
#' @param code Character vector of R code
#' @return List of pipeline expressions
#' @noRd
find_pipelines <- function(code) {
  code_text <- paste(code, collapse = "\n")
  
  # IMPORTANT: Convert base R pipes to magrittr pipes BEFORE parsing
  # This prevents R from desugaring them
  code_text <- convert_base_to_magrittr(code_text)
  
  exprs <- tryCatch({
    parse(text = code_text)
  }, error = function(e) {
    rlang::abort(sprintf("Failed to parse code: %s", e$message))
  })
  
  pipelines <- list()
  
  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    
    if (is_dplyr_pipeline(expr)) {
      pipelines[[length(pipelines) + 1]] <- expr
    }
  }
  
  if (length(pipelines) == 0) {
    rlang::warn("No dplyr pipelines found in the file.")
  }
  
  pipelines
}
