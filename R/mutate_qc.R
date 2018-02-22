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

# Helper Function
na_counter <- function(.data, ...){

  # Counting number of NAs in each mutate call by making each new variable then 
  # counting NAs in each and storing as list element with appropriate var name.
  # Separate processes if data is grouped
    
  # Ungrouped
  if (is.null(attr(.data, "vars"))) {
        
    new_vars <- dplyr::transmute(.data, ...)
    num_na <- 
      dplyr::summarize_all(
        new_vars, 
        dplyr::funs(sum(is.na(.) | is.infinite(.)))
      )
        
    mapply(
      FUN = function(x, y) message(x, " NAs or INFs produced in ", y), 
      x = num_na, y = names(num_na)
    )
  }
    
  # Grouped
  if (!is.null(attr(.data, "vars"))) {
        
    group_var <- attr(.data, "vars")
        
      new_vars <- suppressMessages(dplyr::transmute(.data, ...))
      new_vars <- dplyr::ungroup(new_vars)
        
      keep_vars <- names(new_vars)[!names(new_vars) %in% group_var]
      new_vars <- dplyr::select_at(new_vars, keep_vars)
        
      num_na <- 
        dplyr::summarize_all(
          new_vars, 
          dplyr::funs(sum(is.na(.) | is.infinite(.)))
        )
        
      mapply(
        FUN = function(x, y) message(x, " NAs or INFs produced in ", y), 
        x = num_na, y = names(num_na)
      )
  }
    
}

#' @rdname mutate_qc
#' @export
mutate_qc <- function(.data, ...){
    
  # Performing mutate
  out <- dplyr::mutate(.data, ...)
   
  # Print NAs
  na_counter(.data, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_qc <- function(.data, ...){
  
  # Performing transmute
  out <- dplyr::transmute(.data, ...)
  
  # Print NAs
  na_counter(.data, ...)
  
  # Returning data
  return(out)
  
}
    