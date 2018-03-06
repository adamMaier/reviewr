#' Scoped versions of summarize_qc
#' 
#' \code{summarize_all_qc}, \code{summarize_at_qc}, and \code{summarize_if_qc}
#' are used exactly the same as \code{dplyr::summarize_all},
#' \code{dplyr::summarize_at}, and \code{dplyr::summarize_if}, and require 
#' all of the same arguments and return identical objects. The only difference
#' is that the \code{_qc} versions print a message indicating the number of NA
#' or INFinite values created in the new summary variable(s). This is most
#' useful when using on a grouped data frame.
#' 
#' @section Grouping:
#'   All functions work with grouped data.
#'
#' @section summarize vs. summarise:
#'   There are \code{_qc} versions of \code{summarize} and \code{summarise}.
#'   But this is America, use a z!
#'     
#' @inheritParams dplyr::summarise_all
#' 
#' @param .group_check a logical value, that when TRUE, will print a table with
#' each group variable and a column called "missing_vars" that lists which 
#' variables are missing from the summarized data for each group. Only groups 
#' with at least one missing variable are listed. This has no effect on the 
#' returned object, and only prints information. Default is FALSE, to avoid
#' excess printing. If data is not grouped and .group_check = T, then an error 
#' is thrown.
#'   
#' @return An object of the same class as \code{.data}. This object will be
#'   identical to that which is returned when running the scoped variants of
#'   \code{dplyr::summarize}.
#' 
#' @seealso \code{\link[dplyr]{summarise_all}}, 
#'   \code{\link[dplyr]{summarise_at}}, \code{\link[dplyr]{summarise_if}}
#' 
#' @examples 
#' practice_data <- 
#'   data.frame(
#'   A = c(1:4, NA), 
#'   B = c(NA, 7:10), 
#'   C = 21:25,
#'   G = c(1, 1, 1, 2, 2)
#' )
#' 
#' # Use the _qc versions just like normal dplyr scoped summarize functions.
#' summarize_at_qc(
#'   practice_data, 
#'   vars(A, C), 
#'   funs(m = mean(., na.rm = T), s = sum)
#' )
#' 
#' summarize_all_qc(practice_data, funs(mean))
#' 
#' # Pipes work, just as they always do in dplyr
#' practice_data %>% summarize_if_qc(is.integer, mean)
#' 
#' # Functions work on grouped data, too
#' grouped_data <- group_by(practice_data, G)
#' grouped_data %>% 
#'   summarize_at_qc(vars(A, C), funs(m = mean(., na.rm = T), s = sum))
#' 
#' # Setting .group_check = T will print, for each group with a missing value,
#' # which new variables are missing. 
#' summarize_all_qc(grouped_data, mean, .group_check = T)
#' 
#' @name summarize_all_qc
NULL


# HELPER FUNCTIONS ------------------------------------------------------------- 

# A function to count and print number of missing entries in final columns
na_counter_sum <- function(.orig_data = NULL, .processed_data = NULL, .group_check = NULL) {

  # Obtaining names of original group variables, if any.
  group_vars <- attr(.orig_data, "vars")
  
  # Isolating new, summarized variables
  new_vars <- names(.processed_data)[!names(.processed_data) %in% group_vars]
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
  
  # Identifiying groups with a missing variable, if any
  if (.group_check == T) {
    
    # Keep just rows with missing variables on new vars and
    g_with_missing <- 
      dplyr::filter(.processed_data, !complete.cases(.processed_data[, new_vars]))
    
    # Reshape data so just one variable for each row, which lists missing 
    # variables
    g_with_missing <- 
      tidyr::gather(
        g_with_missing,
        key = key,
        value = value,
        new_vars
      )
    
    g_with_missing <- dplyr::filter(g_with_missing, is.na(value) | is.infinite(value))
    g_with_missing <- dplyr::group_by_at(g_with_missing, group_vars)
    g_with_missing <- dplyr::summarize(g_with_missing, missing_vars = paste0(key, collapse = ", "))
    g_with_missing <- dplyr::ungroup(g_with_missing)
    
    if (dplyr::tally(g_with_missing) > 0) {
      message("\n", "GROUPS WITH MISSING VALUES:")
      print.data.frame(g_with_missing)
      message("\n")
    } else {
      message("\n", "No missing values in any group in newly summarized variables")
      message("\n")
    }
    
  }

}



# EXPORTED FUNCTIONS ----------------------------------------------------------- 

#' @rdname summarize_all_qc
#' @export
summarize_all_qc <- function(.tbl, .funs, ..., .group_check = F) {
    
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  .args <- c(list(".tbl" = .tbl, ".funs" = .funs), nv_pairs)
  
  # Performing summarize
  out <- do.call(dplyr::summarize_all, .args)
  
  # Print NAs and return outcome
  na_counter_sum(.orig_data = .tbl, .processed_data = out, .group_check = .group_check)
  return(out)
  
}

#' @rdname summarize_all_qc
#' @export
summarise_all_qc <- function(.tbl, .funs, ..., .group_check = F) {

  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  args <- c(list(".tbl" = .tbl, ".funs" = .funs, ".group_check" = .group_check), nv_pairs)
  
  # Calling function
  do.call(summarize_all_qc, args)
  
}

#' @rdname summarize_all_qc
#' @export
summarize_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F) {
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".vars" = .vars, ".funs" = .funs), 
    nv_pairs,
    list(".cols" = .cols)
  )
  
  # Performing summarize
  out <- do.call(dplyr::summarize_at, .args)
  
  # Print NAs and return outcome
  na_counter_sum(.orig_data = .tbl, .processed_data = out, .group_check = .group_check)
  return(out)
  
}

#' @rdname summarize_all_qc
#' @export
summarise_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F) {

  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  args <- c(
    list(".tbl" = .tbl, ".vars" = .vars, ".funs" = .funs), 
    nv_pairs,
    list(".cols" = .cols, ".group_check" = .group_check)
  )
  
  # Calling function
  do.call(summarize_at_qc, args)
  
}


#' @rdname summarize_all_qc
#' @export
summarize_if_qc <- function(.tbl, .predicate, .funs, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  .args <- c(
    list(".tbl" = .tbl, ".predicate" = .predicate, ".funs" = .funs), 
    nv_pairs
  )
  
  # Performing summarize
  out <- do.call(dplyr::summarize_if, .args)
  
  # Print NAs and return outcome
  na_counter_sum(.orig_data = .tbl, .processed_data = out, .group_check = .group_check)
  return(out)
  
}

#' @rdname summarize_all_qc
#' @export
summarise_if_qc <- function(.tbl, .predicate, .funs, ..., .group_check = F){
  
  # Preparing arguments to pass to functions
  nv_pairs <- rlang::quos(...) 
  args <- c(
    list(".tbl" = .tbl, ".predicate" = .predicate, ".funs" = .funs, ".group_check" = .group_check), 
    nv_pairs
  )
  
  # Calling function
  do.call(summarize_at_qc, args)
  
}