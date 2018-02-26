#' Report number of NAs created when performing dplyr mutate
#' 
#' \code{mutate_qc} and \code{transmute_qc} return identical objects as 
#' \code{dplyr::mutate} and \code{dplyr::transmute}. Like dplyr, \code{mutate_qc}
#' adds new variables and preserves existing variables, and \code{transmute_qc} 
#' drops existing variables.
#' 
#' \code{mutate_qc} and \code{transmute_qc} are used exactly the same as 
#' \code{mutate} and \code{transmute} and require all of the same arguments
#' and return identical objects. The only difference is that the \code{_qc}
#' versions print a message indicating the number of NA or INFinite values 
#' created in the new or edited variable(s). 
#' 
#' @section Scoped variants:
#'   There are \code{_qc} versions of the scoped mutate functions. See 
#'   \code{\link{mutate_at_qc}}, \code{\link{mutate_all_qc}}, or
#'   \code{\link{mutate_if_qc}}. Or \code{\link{transmute_at_qc}}, 
#'   \code{\link{transmute_all_qc}}, or \code{\link{transmute_if_qc}}.
#'   
#' @section Grouping:
#'   All functions work with grouped data.
#'
#' @inheritParams dplyr::mutate
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
#'   identical to that which is returned when running  \code{dplyr::mutate} or 
#'   \code{dplyr::transmute} functions.
#' 
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{transmute}}
#' 
#' @examples 
#' practice_data <- 
#'   data.frame(
#'   A = c(1:4, NA), 
#'   B = c(NA, 7:10), 
#'   C = 21:25,
#'   G = c("X", "X", "X", "Y", "Y"),
#'   stringsAsFactors = F
#' )
#' 
#' # Use the _qc versions just like normal dplyr mutate functions
#' mutate_qc(practice_data, new_var_1 = A + B, new_var_2 = A - C)
#' 
#' # mutate_qc will only report the number of NAs and INFs in the final copy of
#' # the variable, so if you mutate the same variable more thna once in the 
#' # call, it's only the final outcome that gets tracked
#' mutate_qc(practice_data, new_var = A + B, new_var = C + 1)
#' 
#' # Functions worked on grouped data, too
#' grouped_data <- dplyr::group_by(practice_data, G)
#' mutate_qc(grouped_data, new_var_1 = A + mean(B), mean_b = mean(B))
#' 
#' # Setting .group_check = T will also print a table indicating which groups
#' # have a missing value, on what variable, and how many values are missing.
#' mutate_qc(grouped_data, new_var_1 = A + mean(B), mean_b = mean(B), .group_check = T)
#' 
#' @name mutate_qc
NULL


# HELPER FUNCTIONS -------------------------------------------------------------

# A function to count and print number of missing entries in final columns
na_counter_mutate <- function(.orig_data = NULL, .processed_data = NULL, ...) {

  # Obtaining names of all new variables created
  new_vars_list <- rlang::quos(...)
  new_vars <- names(new_vars_list)
  
  # Retain only newly mutated variables
  new_vars_data <- dplyr::ungroup(.processed_data)
  new_vars_data <- dplyr::select_at(new_vars_data, new_vars)

  # Counting number of NAs in each newly created variable
  num_na <- 
    dplyr::summarize_all(
      new_vars_data, 
      dplyr::funs(sum(is.na(.) | is.infinite(.)))
    )
  
  mapply(
    FUN = function(x, y) message(x, " NAs or INFs produced in ", y), 
    x = num_na, y = names(num_na)
  )
  
}

# A function to count and print number of missing entries per group
na_counter_grp_mutate <- function(.orig_data = NULL, .processed_data = NULL, ...) {
  
  # Obtaining names of all new variables created
  new_vars_list <- rlang::quos(...)
  new_vars <- names(new_vars_list)
  
  # Retain only newly mutated variables plus grouped variables
  group_vars <- attr(.orig_data, "vars")
  all_vars <- c(group_vars, new_vars)
  new_vars_grp_data <- dplyr::select_at(.processed_data, all_vars)
  
  # Counting number of NAs in each newly created variable by group
  num_na <- 
    dplyr::summarize_all(
      new_vars_grp_data, 
      dplyr::funs(sum(is.na(.) | is.infinite(.)))
    )
  
  num_na_long <- tidyr::gather(num_na, key = var_name, value = n_mising, new_vars)
  num_na_long <- dplyr::filter(num_na_long, n_mising >= 1)
  
  if (dplyr::tally(num_na_long) > 0) {
    message("\n", "NUMBER OF VALUES MISSING BY GROUP AND VARIABLE:")
    print.data.frame(num_na_long)
  } else {
    message("\n", "No missing values in any group in newly mutated variables")
  }
  
}


# EXPORTED FUNCTIONS ----------------------------------------------------------- 

#' @rdname mutate_qc
#' @export
mutate_qc <- function(.data, ..., .group_check = F){
    
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.data, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  .args <- c(list(".data" = .data), nv_pairs)
  
  # Performing mutate
  out <- do.call(dplyr::mutate, .args)

  # Print NAs and return outcome
  na_counter_mutate(.orig_data = .data, .processed_data = out, ... = ...)
  
  if (.group_check == T) {
    na_counter_grp_mutate(.orig_data = .data, .processed_data = out, ... = ...)
  }
  
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_qc <- function(.data, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.data, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  .args <- c(list(".data" = .data), nv_pairs)
  
  # Performing mutate
  out <- do.call(dplyr::transmute, .args)
  
  # Print NAs and return outcome
  na_counter_mutate(.orig_data = .data, .processed_data = out, ... = ...)
  
  if (.group_check == T) {
    na_counter_grp_mutate(.orig_data = .data, .processed_data = out, ... = ...)
  }
  
  return(out)
  
}