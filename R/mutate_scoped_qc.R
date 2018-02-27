#' Scoped versions of mutate_qc
#' 
#' \code{mutate_all_qc}, \code{mutate_at_qc}, \code{mutate_if_qc}, and their 
#' \code{transmute} equivalents return identical objects as the scoped versions
#' of \code{dplyr::mutate} and \code{dplyr::transmute}.The only difference is
#' that the \code{_qc} versions print a message indicating the number of NA or
#' INFinite values created in the new or edited variable(s) after calling 
#' \code{mutate}. 
#'   
#' @section Grouping:
#'   All functions work with grouped data.
#'
#' @inheritParams dplyr::summarise_all
#'
#' @param .group_check a logical value, that when TRUE, will print a table with
#' each group variable, and columns called "var_name" and  "n_missing" that 
#' together indicate, for each group, how many values are missing of newly
#' created variables. Only variables that contain at least 1 missing value are 
#' reported. This has no effect on the returned object, and only prints 
#' information. Default is FALSE, to avoid excess printing. If data is not 
#' grouped and .group_check = T, then an error is thrown.
#' 
#' @return An object of the same class as \code{.data}. This object will be
#'   identical to that which is returned when running \code{mutate_all_qc},
#'   \code{mutate_at_qc}, \code{mutate_if_qc}, and their \code{transmute} 
#'   equivalents.
#' 
#' @seealso \code{\link[dplyr]{mutate_all}}, \code{\link[dplyr]{mutate_at}},
#'   \code{\link[dplyr]{mutate_if}}
#' 
#' @examples 
#' practice_data <- 
#'   data.frame(
#'   A = c(1:4, NA), 
#'   B = c(NA, 7:10), 
#'   C = 21:25,
#'   G = c("X", "X", "X", "Y", "Y")
#' )
#' 
#' # Use the _qc versions just like normal dplyr scoped mutate functions.
#' mutate_at_qc(
#'   practice_data, 
#'   vars(A, C), 
#'   funs(m = mean(., na.rm = T), s = sum)
#' )
#' 
#' mutate_all_qc(practice_data, funs(as.character))
#' 
#' # Pipes work, just as they always do in dplyr
#' practice_data %>% mutate_if_qc(is.integer, mean)
#' 
#' # Functions work on grouped data, too
#' grouped_data <- group_by(practice_data, G)
#' grouped_data %>% 
#'   mutate_at_qc(vars(A, C), funs(m = mean(., na.rm = T), s = sum))
#' 
#' # Setting .group_check = T will also print a table indicating which groups
#' # have a missing value, on what variable, and how many values are missing.
#' mutate_at_qc(grouped_data, vars(A, B), funs(mean), .group_check = T)
#' 
#' @name mutate_all_qc
NULL


# HELPER FUNCTIONS -------------------------------------------------------------

# A function to count and print number of missing entries in final columns
na_counter_mutate_scoped <- function(.args = NULL) {

  # Creating a new variable representing the function which calls na_counter
  fn_name <- gsub("\\(.*", "", deparse(sys.call(-1)[[1]]))

  fn_name}
  
#   if (fn_name %in% c("mutate_all_qc", "transmute_all_qc")) {
#     tm_data <- suppressWarnings(do.call(dplyr::transmute_all, args = .args))
#   }
#   
#   if (fn_name %in% c("mutate_at_qc", "transmute_at_qc")) {
#     tm_data <- suppressWarnings(do.call(dplyr::transmute_at, args = .args))
#   }
#   
#   if (fn_name %in% c("mutate_if_qc", "transmute_if_qc")) {
#     tm_data <- suppressWarnings(do.call(dplyr::transmute_if, args = .args))
#   }
#   
#   # Remove group variables, if any
#   group_var <- attr(tm_data, "vars")
#   keep_vars <- names(tm_data)[!names(tm_data) %in% group_var]
#   modified_vars <- dplyr::ungroup(tm_data)
#   modified_vars <- dplyr::select_at(modified_vars, keep_vars)
#   
#   # Count number of NAs in each modified variable
#   num_na <- 
#     dplyr::summarize_all(
#       modified_vars, 
#       dplyr::funs(sum(is.na(.) | is.infinite(.)))
#     )
#       
#   mapply(
#     FUN = function(x, y) message(x, " NAs or INFs produced in ", y), 
#     x = num_na, y = names(num_na)
#   )
# 
# }
# 
# # A function to count and print number of missing entries per group
# na_counter_grp_mutate_scoped <- function(.args = NULL) {
#   
#   # Creating a new variable representing the function which calls na_counter
#   fn_name <- gsub("\\(.*", "", deparse(sys.call(-1)[[1]]))
#   
#   if (fn_name %in% c("mutate_all_qc", "transmute_all_qc")) {
#     tm_data <- suppressMessages(do.call(dplyr::transmute_all, args = .args))
#   }
#   
#   if (fn_name %in% c("mutate_at_qc", "transmute_at_qc")) {
#     tm_data <- suppressMessages(do.call(dplyr::transmute_at, args = .args))
#   }
#   
#   if (fn_name %in% c("mutate_if_qc", "transmute_if_qc")) {
#     tm_data <- suppressMessages(do.call(dplyr::transmute_if, args = .args))
#   }
#   
#   # Obtain newly modified variables
#   group_vars <- attr(tm_data, "vars")
#   new_vars <- names(tm_data)[!names(tm_data) %in% group_vars] 
#   
#   # Counting number of NAs in each newly created variable by group
#   num_na <- 
#     dplyr::summarize_all(
#       tm_data, 
#       dplyr::funs(sum(is.na(.) | is.infinite(.)))
#     )
#   
#   num_na_long <- tidyr::gather(num_na, key = var_name, value = n_mising, new_vars)
#   num_na_long <- dplyr::filter(num_na_long, n_mising >= 1)
#   
#   if (dplyr::tally(num_na_long) > 0) {
#     message("\n", "NUMBER OF VALUES MISSING BY GROUP AND VARIABLE:")
#     print.data.frame(num_na_long)
#   } else {
#     message("\n", "No missing values in any group in newly mutated variables")
#   }
#   
# }
# 


# EXPORTED FUNCTIONS ----------------------------------------------------------- 

#' @rdname mutate_all_qc
#' @export
mutate_all_qc <- function(.tbl, .funs, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(list(".tbl" = .tbl, ".funs" = .funs), add_args)
   
  # Performing mutate
  out <- do.call(dplyr::mutate_all, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(na_counter_mutate_scoped)
  
}

#' @rdname mutate_all_qc
#' @export
transmute_all_qc <- function(.tbl, .funs, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(list(".tbl" = .tbl, ".funs" = .funs), add_args)
  
  # Performing mutate
  out <- do.call(dplyr::transmute_all, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(out)
  
}

#' @rdname mutate_all_qc
#' @export
mutate_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".vars" = .vars, ".funs" = .funs),
    add_args,
    list(".cols" = .cols)
  )

  # Performing mutate
  out <- do.call(dplyr::mutate_at, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(out)
  
}

#' @rdname mutate_all_qc
#' @export
transmute_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".vars" = .vars, ".funs" = .funs),
    add_args,
    list(".cols" = .cols)
  )
  
  # Performing mutate
  out <- do.call(dplyr::transmute_at, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(out)
  
}

#' @rdname mutate_all_qc
#' @export
mutate_if_qc <- function(.tbl, .predicate, .funs, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".predicate" = .predicate, ".funs" = .funs), 
    add_args
  )
  
  # Performing mutate
  out <- do.call(dplyr::mutate_if, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(out)
  
}

#' @rdname mutate_all_qc
#' @export
transmute_if_qc <- function(.tbl, .predicate, .funs, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  add_args <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".predicate" = .predicate, ".funs" = .funs), 
    add_args
  )
  
  # Performing mutate
  out <- do.call(dplyr::transmute_if, .args)
  
  # Print NAs and return outcome
  na_counter_mutate_scoped(.args = .args)
  
  if (.group_check == T) {
    na_counter_grp_mutate_scoped(.args = .args)
  }
  
  return(out)
  
}