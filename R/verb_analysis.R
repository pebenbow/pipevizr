#' Extract details from various dplyr verbs
#'
#' @keywords internal
extract_verb_details <- function(call, max_show = 5) {
  if (!rlang::is_call(call)) return(NULL)
  
  fn_name <- rlang::call_name(call) %||% "unknown"
  args <- rlang::call_args(call)
  
  # Filter out special arguments (those starting with .)
  arg_names <- names(args)
  if (!is.null(arg_names)) {
    keep_idx <- arg_names == "" | !startsWith(arg_names, ".")
    args <- args[keep_idx]
    arg_names <- arg_names[keep_idx]
  }
  
  if (length(args) == 0) return(NULL)
  
  details <- switch(fn_name,
                    # mutate/summarize: show name = expr
                    "mutate" = ,
                    "transmute" = ,
                    "summarise" = ,
                    "summarize" = extract_assignments(args, arg_names),
                    
                    # filter: show each condition
                    "filter" = extract_conditions(args),
                    
                    # arrange: show each sort variable
                    "arrange" = extract_sort_vars(args),
                    
                    # select: show selected columns
                    "select" = extract_selections(args, arg_names),
                    
                    # rename: show old -> new
                    "rename" = extract_renames(args, arg_names),
                    
                    NULL
  )
  
  # Truncate if too many
  if (!is.null(details) && length(details) > max_show) {
    details <- c(
      details[1:max_show],
      list(list(name = sprintf("... +%d more", length(details) - max_show), expr = ""))
    )
  }
  
  details
}

#' Extract assignments (for mutate/summarize)
#' @keywords internal
extract_assignments <- function(args, arg_names) {
  if (is.null(arg_names)) return(NULL)
  
  named_idx <- arg_names != ""
  if (!any(named_idx)) return(NULL)
  
  purrr::map2(arg_names[named_idx], args[named_idx], function(nm, expr) {
    # Get a readable string representation of the RHS
    rhs <- deparse(expr, width.cutoff = 500)[1]  # Get full expression on one line
    
    # Truncate if too long
    max_len <- 40
    if (nchar(rhs) > max_len) {
      rhs <- paste0(substr(rhs, 1, max_len), "...")
    }
    
    list(name = nm, expr = rhs)
  })
}

#' Extract filter conditions
#' @keywords internal
extract_conditions <- function(args) {
  purrr::map(args, function(expr) {
    text <- deparse(expr, width.cutoff = 40)[1]
    if (nchar(text) > 40) text <- paste0(substr(text, 1, 40), "...")
    list(name = text, expr = "")
  })
}

#' Extract arrange variables
#' @keywords internal
extract_sort_vars <- function(args) {
  purrr::map(args, function(expr) {
    if (rlang::is_call(expr, "desc")) {
      var <- deparse(expr[[2]], width.cutoff = 30)[1]
      list(name = var, expr = "desc")
    } else {
      var <- deparse(expr, width.cutoff = 30)[1]
      list(name = var, expr = "asc")
    }
  })
}

#' Extract select columns
#' @keywords internal
extract_selections <- function(args, arg_names) {
  purrr::map2(args, arg_names %||% rep("", length(args)), function(expr, nm) {
    if (nm != "") {
      # Renamed column
      old <- deparse(expr, width.cutoff = 20)[1]
      list(name = nm, expr = old)
    } else {
      text <- deparse(expr, width.cutoff = 30)[1]
      list(name = text, expr = "")
    }
  })
}

#' Extract rename mappings
#' @keywords internal
extract_renames <- function(args, arg_names) {
  if (is.null(arg_names)) return(NULL)
  
  purrr::map2(arg_names, args, function(new_name, old_expr) {
    if (new_name == "") return(NULL)
    old <- deparse(old_expr, width.cutoff = 20)[1]
    list(name = new_name, expr = old)
  }) %>% purrr::compact()
}

#' Analyze verb and extract details
#'
#' @noRd
analyze_verb <- function(call) {
  if (!rlang::is_call(call)) {
    return(list(
      type = "other", 
      icon = "⚙", 
      color = "#F8F9FA", 
      detail = NULL,
      details = NULL,
      join_df = NULL
    ))
  }
  
  fn_name <- rlang::call_name(call) %||% "unknown"
  
  # Extract arguments for context-aware icons
  detail <- NULL
  join_df <- NULL
  
  # Check for desc() in arrange
  if (fn_name == "arrange") {
    args <- rlang::call_args(call)
    has_desc <- any(purrr::map_lgl(args, ~rlang::is_call(.x, "desc")))
    detail <- if (has_desc) "desc" else "asc"
  }
  
  # Detect join type and extract joining data frame
  if (grepl("_join$", fn_name)) {
    detail <- sub("_join$", "", fn_name)
    args <- rlang::call_args(call)
    arg_names <- names(args)
    
    # Extract the data frame being joined
    if (length(args) >= 1) {
      join_expr <- args[[1]]
      join_df <- deparse(join_expr, width.cutoff = 100)[1]
      # Clean up the name
      join_df <- trimws(join_df)
    }
  }
  
  # Extract details for relevant verbs
  details <- extract_verb_details(call)
  
  # Map to type, icon, and color
  verb_info <- dplyr::case_when(
    fn_name %in% c("filter", "slice", "slice_head", "slice_tail", "slice_sample", "slice_min", "slice_max") ~ 
      list(type = "filter", icon = "🔍", color = "#FFF4E6"),
    
    fn_name %in% c("select", "relocate") ~ 
      list(type = "select", icon = "◫", color = "#E8F5E9"),
    
    fn_name == "rename" ~ 
      list(type = "rename", icon = "✎", color = "#E8F5E9"),
    
    fn_name %in% c("mutate", "transmute") ~ 
      list(type = "mutate", icon = "✚", color = "#E3F2FD"),
    
    fn_name %in% c("summarise", "summarize", "count", "tally", "add_count", "add_tally") ~ 
      list(type = "summarize", icon = "Σ", color = "#F3E5F5"),
    
    fn_name %in% c("group_by", "rowwise") ~ 
      list(type = "group", icon = "▦", color = "#FFF9C4"),
    
    fn_name == "ungroup" ~ 
      list(type = "ungroup", icon = "▢", color = "#FFF9C4"),
    
    fn_name == "arrange" ~ 
      list(type = "arrange", 
           icon = if (identical(detail, "desc")) "↓" else "↑", 
           color = "#E0F2F1"),
    
    fn_name %in% c("left_join", "inner_join", "right_join", "full_join", 
                   "semi_join", "anti_join") ~ 
      list(type = "join", icon = "⋈", color = "#FCE4EC"),
    
    fn_name %in% c("pivot_longer", "pivot_wider") ~ 
      list(type = "pivot", icon = "⇄", color = "#F1F8E9"),
    
    fn_name %in% c("separate", "separate_rows", "unite", "extract") ~ 
      list(type = "reshape", icon = "✂", color = "#FFF3E0"),
    
    fn_name %in% c("distinct", "unique") ~ 
      list(type = "distinct", icon = "◈", color = "#E8EAF6"),
    
    fn_name %in% c("pull", "pluck") ~ 
      list(type = "extract", icon = "→", color = "#EFEBE9"),
    
    TRUE ~ list(type = "other", icon = "⚙", color = "#F8F9FA")
  )
  
  c(verb_info, list(detail = detail, details = details, join_df = join_df))
}

#' Extract join arguments
#' @noRd
extract_join_args <- function(args, arg_names) {
  if (is.null(arg_names) || length(args) == 0) return(NULL)
  
  details <- list()
  join_df <- NULL
  
  # First unnamed arg or first arg is the data to join
  if (length(args) >= 1) {
    join_expr <- args[[1]]
    join_df <- deparse(join_expr, width.cutoff = 100)[1]
    if (nchar(join_df) > 20) {
      join_df <- substr(join_df, 1, 20)
    }
    details[[1]] <- list(name = paste("with:", join_df), expr = "", type = "join_data")
  }
  
  # Look for 'by' argument
  by_idx <- which(arg_names == "by")
  if (length(by_idx) > 0) {
    by_expr <- args[[by_idx[1]]]
    by_text <- deparse(by_expr, width.cutoff = 30)[1]
    if (nchar(by_text) > 30) {
      by_text <- paste0(substr(by_text, 1, 30), "...")
    }
    details[[length(details) + 1]] <- list(name = paste("by:", by_text), expr = "", type = "join_by")
  }
  
  # Return both details and the joining data frame name
  attr(details, "join_df") <- join_df
  details
}
