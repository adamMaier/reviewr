#' Report number of NAs created when performing dplyr summarize
#' 
#' \code{summarize_qc} is used exactly the same as \code{dplyr::summarize} and 
#' requires all of the same arguments and returns an identical object. The only 
#' difference is that \code{summarize_qc} prints a message indicating the number
#' of NA or INFinite values created in the new summary variable(s). This is most
#' useful when using on a grouped data frame.
#' 
#' @section Scoped variants:
#'   There are \code{_qc} versions of the scoped summarize functions. See 
#'   \code{\link{summarize_at_qc}}, \code{\link{summarize_all_qc}}, or
#'   \code{\link{summarize_if_qc}}.
#'
#' @section Grouping:
#'   All functions work with grouped data.
#'
#' @section summarize vs. summarise:
#'   There are \code{_qc} versions of \code{summarize} and \code{summarise}.
#'   But this is America, use a z!
#'     
#' @inheritParams dplyr::summarise
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
#'   identical to that which is returned when running \code{dplyr::summarise}.
#' 
#' @seealso \code{\link[dplyr]{summarise}}
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
#' summarize_qc(practice_data, new_var_1 = mean(C), sum(A))
#' summarize_qc(practice_data, new_var_1 = mean(C), sum(A, na.rm = T))
#' 
#' # Pipes work
#' practice_data %>% 
#'   summarize_qc(practice_data, new_var_1 = mean(C), sum(A, na.rm = T))
#' 
#' # Functions worked on grouped data, too
#' grouped_data <- dplyr::group_by(practice_data, G)
#' summarize_qc(grouped_data, new_var_1 = mean(A), mean_b = mean(B), sum(C))
#' 
#' # Setting .group_check = T will print, for each group with a missing value,
#' which new variables are missing. 
#' summarize_qc(
#'   grouped_data, 
#'   .group_check = T,
#'   new_var_1 = mean(A),
#'   mean_b = mean(B),
#'   sum(C)
#' )
#' 
#' @name summarize_qc
NULL


#' @rdname summarize_qc
#' @export
summarize_qc <- function(.data = NULL, ..., .group_check = F) {

  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.data, "vars"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to functions
  name_value_pairs <- rlang::quos(...) 
  .args <- c(list(".data" = .data), name_value_pairs)
  
  # Performing summarize
  out <- do.call(dplyr::summarize, .args)

  # Dropping group variables from being printed, if any.
  group_vars <- attr(.data, "vars")
  keep_vars <- names(out)[!names(out) %in% group_vars]
  
  if (!is.null(group_vars)) {
    num_na <- dplyr::ungroup(out)
    num_na <- dplyr::select_at(num_na, keep_vars)
  } else {
    num_na <- out
  }

  # Counting number of NAs in each summarize call by making each new variable 
  # them counting NAs in each and storing as list element with appropriate var
  # name.
  num_na <- 
    dplyr::summarize_all(
      num_na, 
      dplyr::funs(sum(is.na(.) | is.infinite(.)))
    )
      
  mapply(
    FUN = function(x, y) message(x, " NAs or INFs produced in ", y), 
    x = num_na, y = names(num_na)
  )

  # Printing groups with missing values by summary variable if group_check
  if (.group_check == T) {

    # Keep just rows with missing variables on new vars
    g_with_missing <- dplyr::mutate_all(out, dplyr::funs(ifelse(is.infinite(.), NA, .)))
    g_with_missing <- dplyr::filter(g_with_missing, !complete.cases(g_with_missing[, keep_vars]))
    
    # Reshape data so just one variable for each row, which lists missing 
    # variables
    g_with_missing <- 
      tidyr::gather(
        g_with_missing,
        key = key,
        value = value,
        keep_vars
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
  
  return(out)
  
}

#' @rdname summarize_qc
#' @export
summarise_qc <- function(.data = NULL, ..., .group_check = F) {
  
  # Preparing arguments to pass to functions
  name_value_pairs <- rlang::quos(...) 
  args <- c(list(".data" = .data, ".group_check" = .group_check), name_value_pairs)
  
  # Calling function
  do.call(summarize_qc, args)
  
}