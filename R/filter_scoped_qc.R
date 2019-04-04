#' Scoped versions of filter_qc
#' 
#' \code{filter_all_qc}, \code{filter_at_qc}, and \code{filter_if_qc} return
#'   identical objects as \code{filter_all}, \code{filter_at}, and 
#'   \code{filter_if} respectively, except that they automatically print the
#'   number of cases (i.e., rows) that do not meet the filter conditions and 
#'   that were thus dropped.
#' 
#' @inheritParams dplyr::filter_all
#'
#' @param .group_check a logical value, that when TRUE, will print a table with
#' each group variable and columns called "n_rows_dropped" and "percent_dropped" 
#' that together indicate, for each group, how many row were dropped when 
#' performing filter. Default is FALSE, to avoid excess printing. If data is not
#' grouped and .group_check = T, then an error is thrown.
#' 
#' @return An object of the same class as \code{.data}. This object will be
#'   identical to that which is returned when running the respective scoped 
#'   version of \code{dplyr::filter}.
#' 
#' @seealso \code{\link[dplyr]{filter_all}}
#' 
#' @examples 
#' practice_data <- 
#'   data.frame(
#'     A = 1:12, 
#'     B = 6:17, 
#'     C = 8:19, 
#'     G = c(rep(c(1, 2), each = 6))
#'   )
#' 
#' 
#' # Scoped filtering
#' filter_all_qc(practice_data, all_vars(. > 9))
#' filter_at_qc(practice_data, vars(B, C), any_vars(. > 9))
#' filter_if_qc(practice_data, is.integer, all_vars(. > 3))
#' 
#' # With grouped data and setting .group_check = T, you can see how many rows
#' # were dropped per group. Note that this will print a large table if you have 
#' # a lot of groups.
#' grouped_data <- group_by(practice_data, G)
#' filter_all_qc(grouped_data, dplyr::all_vars(. > 9), .group_check = T)
#' 
#' @name filter_all_qc
NULL



# HELPER FUNCTIONS -------------------------------------------------------------

# Function to print number of dropped rows per group (for grouped data only)
dropped_grp_fn <- function(init_data, final_data) {
  
  # Number of final rows per group
  final_grp <- dplyr::tally(final_data, wt = NULL)
  names(final_grp)[length(names(final_grp))] <- ".final."
  
  # Number of intial rows per group
  init_grp <- dplyr::tally(init_data, wt = NULL)
  names(init_grp)[length(names(init_grp))] <- ".initial."
  
  # Merging togther and calculating rows dropped
  merged_grp <- suppressMessages(dplyr::left_join(init_grp, final_grp))
  merged_grp <- dplyr::mutate(
    merged_grp, 
    .final. = ifelse(is.na(.final.), 0, .final.),
    .diff. = .initial. - .final.,
    .prop. = paste0(round(100 * .diff. / .initial., 1), "%")
  )
  merged_grp <- dplyr::select(merged_grp, -.final., -.initial.)
  names(merged_grp)[(length(names(merged_grp)) - 1):length(names(merged_grp))] <- 
    c("n_rows_dropped", "percent_dropped")
  
  # Printing grouped filter diagnostics and returning filtered data
  message("\n", "NUMBER OF ROWS DROPPED BY GROUP:")
  print.data.frame(merged_grp)
  
}
  
  

# EXPORTED FUNCTIONS -----------------------------------------------------------

#' @rdname filter_all_qc
#' @export
filter_all_qc <- function(.tbl, .vars_predicate, .group_check = F) {
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "groups"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to function
  .args <- list(".tbl" = .tbl, ".vars_predicate" = .vars_predicate)
  
  # Performing filter and counting remaining rows
  out <- do.call(dplyr::filter_all, .args)
  out_nogrp <- dplyr::ungroup(out)
  final_rows <- dplyr::tally(out_nogrp, wt = NULL)
  
  # Counting initial rows
  init_nogrp <- dplyr::ungroup(.tbl)
  init_rows <- dplyr::tally(init_nogrp, wt = NULL)
  
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows, 
    " Rows dropped (",
    round(100 * (init_rows - final_rows) / init_rows, 1),
    "%), after filtering on: ",
    deparse(substitute(.vars_predicate))
  )
  
  if (.group_check == T) dropped_grp_fn(init_data = .tbl, final_data = out)

  return(out)

}

#' @rdname filter_all_qc
#' @export
filter_at_qc <- function(.tbl, .vars, .vars_predicate, .group_check = F) {
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "groups"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to function
  .args <- list(".tbl" = .tbl, ".vars" = .vars, ".vars_predicate" = .vars_predicate)
  
  # Performing filter and counting remaining rows
  out <- do.call(dplyr::filter_at, .args)
  out_nogrp <- dplyr::ungroup(out)
  final_rows <- dplyr::tally(out_nogrp, wt = NULL)
  
  # Counting initial rows
  init_nogrp <- dplyr::ungroup(.tbl)
  init_rows <- dplyr::tally(init_nogrp, wt = NULL)
  
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows, 
    " Rows dropped (",
    round(100 * (init_rows - final_rows) / init_rows, 1),
    "%), after filtering on: ",
    deparse(substitute(.vars_predicate))
  )
  
  if (.group_check == T) dropped_grp_fn(init_data = .tbl, final_data = out)
  
  return(out)
  
}

#' @rdname filter_all_qc
#' @export
filter_if_qc <- function(.tbl, .predicate, .vars_predicate, .group_check = F) {
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.tbl, "groups"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to function
  .args <- list(".tbl" = .tbl, ".predicate" = .predicate, ".vars_predicate" = .vars_predicate)
  
  # Performing filter and counting remaining rows
  out <- do.call(dplyr::filter_if, .args)
  out_nogrp <- dplyr::ungroup(out)
  final_rows <- dplyr::tally(out_nogrp, wt = NULL)
  
  # Counting initial rows
  init_nogrp <- dplyr::ungroup(.tbl)
  init_rows <- dplyr::tally(init_nogrp, wt = NULL)
  
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows, 
    " Rows dropped (",
    round(100 * (init_rows - final_rows) / init_rows, 1),
    "%), after filtering on: ",
    deparse(substitute(.vars_predicate))
  )
  
  if (.group_check == T) dropped_grp_fn(init_data = .tbl, final_data = out)
  
  return(out)
  
}