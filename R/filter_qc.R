#' Report number of dropped cases when performing dplyr filter.
#' 
#' \code{filter_qc} returns an identical object as \code{dplyr::filter}, except
#'   that it automatically prints the number of cases (i.e., rows) that do not 
#'   meet the filter conditions and that were thus dropped.
#' 
#' @section Scoped variants:
#'   There are \code{_qc} versions of the scoped filter functions. See 
#'   \code{\link{filter_at_qc}}, \code{\link{filter_all_qc}}, or
#'   \code{\link{filter_if_qc}}.
#'   
#'   
#' @inheritParams dplyr::filter
#' 
#' @param .group_check a logical value, that when TRUE, will print a table with
#' each group variable and columns called "n_rows_dropped" and "percent_dropped" 
#' that together indicate, for each group, how many row were dropped when 
#' performing filter. Default is FALSE, to avoid excess printing. If data is not
#' grouped and .group_check = T, then an error is thrown.
#'   
#' @return An object of the same class as \code{.data}. This object will be
#'   identical to that which is returned when running the standard 
#'   \code{dplyr::filter} function.
#' 
#' @seealso \code{\link[dplyr]{filter}}
#' 
#' @examples 
#' practice_data <- 
#'   data.frame(
#'     A = 1:12, 
#'     B = 6:17, 
#'     C = 8:19, 
#'     G = c(rep(c("A", "B"), each = 6)),
#'     stringsAsFactors = F
#'   )
#' 
#' # Basic filtering
#' filter_qc(practice_data, A > 5)
#' filter_qc(practice_data, A > 5 & B > 8)
#' 
#' # With grouped data and setting .group_check = T, you can see how many rows
#' # were dropped per group. Note that this will print a large table if you have 
#' # a lot of groups.
#' grouped_data <- group_by(practice_data, G)
#' filter_qc(grouped_data, A > 3, .group_check = T)
#'
#'@name filter_qc
NULL

#' @rdname filter_qc
#' @export
filter_qc <- function(.data, ..., .group_check = F){
  
  # Check to make sure data is grouped if .group_check = T
  if (.group_check == T & is.null(attr(.data, "groups"))) {
    stop("Data is not grouped, so you cannot have .group_check = T")
  }
  
  # Preparing arguments to pass to function
  conditions <- rlang::quos(...) 
  .args <- c(list(".data" = .data), conditions)
  
  # Performing filter and counting remaining rows
  out <- do.call(dplyr::filter, .args)
  out_nogrp <- dplyr::ungroup(out)
  final_rows <- dplyr::tally(out_nogrp, wt = NULL)
  
  # Counting initial rows
  init_nogrp <- dplyr::ungroup(.data)
  init_rows <- dplyr::tally(init_nogrp, wt = NULL)
  
  # Preparing filter conditions for message
  args_dots <- .args
  args_dots$.data <- NULL
  names(args_dots) <- NULL
  
  args_string <- gsub("list\\(|", "", deparse(substitute(args_dots)))
  args_string <- gsub("~", "", args_string)
  args_string <- substr(args_string, 1, nchar(args_string) - 1)
  args_string
  
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows, 
    " Rows dropped (",
    round(100 * (init_rows - final_rows) / init_rows, 1),
    "%), after filtering on: ",
    args_string
  )
  
  # Printing number of rows dropped per group if .group_check = T
  if (.group_check == T) {
    
    # Number of final rows per group
    final_grp <- dplyr::tally(out, wt = NULL)
    names(final_grp)[length(names(final_grp))] <- ".final."
    
    # Number of intial rows per group
    init_grp <- dplyr::tally(.data, wt = NULL)
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
  
  return(out)
    
}