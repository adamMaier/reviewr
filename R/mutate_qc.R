#' Report number of NAs created when performing dplyr mutate
#' 
#' \code{mutate_qc} and \code{transmute_qc} return identical objects as 
#' \code{mutate} and \code{transmute}. Like dplyr, \code{mutate_qc} adds new 
#' variables and preserves existing variables, and \code{transmute_qc} drops 
#' existing variables.
#' 
#' \code{mutate_qc} and \code{transmute_qc} are used exactly the same as 
#' \code{mutate} and \code{transmute} and require all of the same arguments
#' and return identical objects. The only difference is that the \code{_qc}
#' versions print a message indicating the number of NA values created in the
#' new or edited variable(s) after calling \code{mutate}. 
#' 
#' @section Scoped mutation and transmutation:
#'   Currently there are not _qc versions of \code{mutate_at}, 
#'   \code{mutate_all}, or \code{mutate_if}, or the \code{transmute} 
#'   equivalents.
#'
#' @export
#'    
#' @param .data A tbl.
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop a 
#'   variable.
#'   
#' @return An object of the same class as \code{.data}. This object will be
#'   identical to that which is returned when running the standard 
#'   \code{dplyr::mutate} or \code{dplyr::transmute} functions.
#' 
#' @seealso \code{\link[dplyr]{mutate}}
#' 
#' @examples 
#' practice_data <- data.frame(A = c(1:4, NA), B = c(NA, 7:10), C = 21:25)
#' 
#' mutate_qc(practice_data, new_var = A + B)
#' 
#' transmute_qc(
#'   practice_data,
#'   new_var_1 = A + B,
#'   new_var_2 = A - C
#' )

mutate_qc <- function(.data, ...){
    
  # Counting number of NAs in each mutate call by making each new variable then 
  # counting NAs in each and storing as list element
  new_vars <- dplyr::transmute(.data, ...)
  num_na <- dplyr::summarize_all(new_vars, dplyr::funs(sum(is.na(.))))
  mapply(
    FUN = function(x, y) message(x, " NAs produced in ", y), 
    x = num_na, y = names(num_na)
  )
    
  # Performing mutate
  dplyr::mutate(.data, ...)
  
}

#' @rdname mutate_qc
#' @export
transmute_qc <- function(.data, ...){
  
  # Performing transmute
  out <- dplyr::transmute(.data, ...)
  
  # Counting number of NAs in each transmutate call by making each new variable
  # a list element, then summing number of NAs in each element
  num_na <- dplyr::summarize_all(out, dplyr::funs(sum(is.na(.))))
  mapply(
      FUN = function(x, y) message(x, " NAs produced in ", y), 
      x = num_na, y = names(num_na)
  )
  
  # Returning data
  return(out)
  
}
    