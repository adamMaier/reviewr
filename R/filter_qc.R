#' Report number of dropped cases when performing dplyr filter.
#' 
#' \code{filter_qc} returns an identical object as \code{dplyr::filter} 
#' 
#' \code{filter_qc} is identical to \code{\link{dplyr::filter}} except that it 
#' automatically prints the number of cases (i.e., rows) that do not meet the 
#' filter conditions and are thus dropped.
#' 
#' @seealso \code{\link{dplyr::filter}}, and \code{\link{dplyr::filter_all}}


# Basic filter
filter_qc <- function(.data, ...){
    
  # Counting initial rows
  init_rows <- dplyr::tally(.data)
    
  # Doing filter and counting remaining rows
  out <- dplyr::filter(.data, ...)
  final_rows <- dplyr::tally(out)
    
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows, 
    " Rows dropped (",
    round(100* (init_rows - final_rows) / init_rows, digits = 2),
    "%), after filtering on: ",
    deparse(substitute(...))
  )
    
  return(out)
    
}

# Filter all
filter_all_qc <- function(.tbl, .vars_predicate){
  
  # Adding dplyr prefix to predicate
  .vars_predicate_evaled <- eval(
    parse(text = paste0(
      "dplyr::",
      deparse(substitute(.vars_predicate)),
      collapse = ""
      )
    )
  )

  # Counting initial rows
  init_rows <- dplyr::tally(.tbl)

  # Doing filter and counting remaining rows
  out <- dplyr::filter_all(.tbl, .vars_predicate = .vars_predicate_evaled)
  final_rows <- dplyr::tally(out)

  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows,
    " Rows dropped (",
    round(100* (init_rows - final_rows) / init_rows, digits = 2),
    "%), after filtering on: ",
    paste(deparse(substitute(.vars_predicate)))
  )

  return(out)

}

# Filter at
filter_at_qc <- function(.tbl, .vars, .vars_predicate){
  
  # Adding dplyr prefix to predicate and vars
  .vars_predicate_evaled <- eval(
    parse(text = paste0(
      "dplyr::",
      deparse(substitute(.vars_predicate)),
      collapse = ""
      )
    )
  )
  .vars_evaled <- eval(
    parse(text = paste0(
      "dplyr::",
      deparse(substitute(.vars)),
      collapse = ""
      )
    )
  )
  
  # Counting initial rows
  init_rows <- dplyr::tally(.tbl)
  
  # Doing filter and counting remaining rows
  out <- dplyr::filter_at(
    .tbl, 
    .vars = .vars_evaled, 
    .vars_predicate = .vars_predicate_evaled
  )
  final_rows <- dplyr::tally(out)
  
  # Printing filter diagnostics and returning filtered data
  message(
    init_rows - final_rows,
    " Rows dropped (",
    round(100* (init_rows - final_rows) / init_rows, digits = 2),
    "%), after filtering on variables: ",
    paste(deparse(substitute(.vars))),
    " with condition: ",
    paste(deparse(substitute(.vars_predicate)))
  )
  
  return(out)
  
}