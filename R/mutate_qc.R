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
#' versions print a message indicating the number of NA or INFinite values 
#' created in the new or edited variable(s) after calling \code{mutate}. 
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
#' mutate_at_qc(practice_data, vars(A, C), funs(m = mean(., na.rm = T), s = sum))
#' 
#' # This will throw a warning because G is a character, but will still work.
#' mutate_all_qc(practice_data, funs(mean))
#' 
#' # Functions worked on grouped data, too
#' grouped_data <- dplyr::group_by(practice_data, G)
#' mutate_qc(grouped_data, new_var_1 = A + mean(B), mean_b = mean(B))
#' mutate_at_qc(grouped_data, vars(A, C), funs(m = mean(., na.rm = T), s = sum))



# EXPORTED FUNCTIONS ----------------------------------------------------------- 

na_counter <- function(.data, .vars, .funs, .predicate, ...){

  # Counting number of NAs in each mutate call by making each new variable then 
  # counting NAs in each and storing as list element with appropriate var name.
  # Separate processes if data is grouped
  
  # Creating new variables representing the function which calls na_counter
  fn_name <- gsub("\\(.*", "", deparse(sys.call(-1)[[1]]))

  if (fn_name %in% c("mutate_qc", "transmute_qc")) {
    new_vars <- suppressMessages(dplyr::transmute(.data, ...))
  }
  
  if (fn_name %in% c("mutate_all_qc", "transmute_all_qc")) {
    new_vars <- suppressMessages(dplyr::transmute_all(.data, .funs, ...))
  }
  
  if (fn_name %in% c("mutate_at_qc", "transmute_at_qc")) {
    new_vars <- suppressMessages(dplyr::transmute_at(.data, .vars, .funs, ..., .cols = NULL))
  }
  
  if (fn_name %in% c("mutate_if_qc", "transmute_if_qc")) {
    new_vars <- suppressMessages(dplyr::transmute_if(.data, .predicate, .funs, ...))
  }
    
  # Ungrouped
  if (is.null(attr(.data, "vars"))) {
        
    #new_vars <- dplyr::transmute(.data, ...)
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
        
    #new_vars <- suppressMessages(dplyr::transmute(.data, ...))
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



# EXPORTED FUNCTIONS ----------------------------------------------------------- 

#' @rdname mutate_qc
#' @export
mutate_qc <- function(.data, ...){
    
  # Performing mutate
  out <- dplyr::mutate(.data, ...)
   
  # Print NAs
  na_counter(.data = .data, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_qc <- function(.data, ...){
  
  # Performing transmute
  out <- dplyr::transmute(.data, ...)
  
  # Print NAs
  na_counter(.data = .data, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
mutate_all_qc <- function(.tbl, .funs, ...){
    
  # Performing mutate
  out <- dplyr::mutate_all(.tbl, .funs, ...)
  
  # Print NAs
  na_counter(.data = .tbl, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_all_qc <- function(.tbl, .funs, ...){
  
  # Performing mutate
  out <- dplyr::transmute_all(.tbl, .funs, ...)
  
  # Print NAs
  na_counter(.data = .tbl, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
mutate_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL){
  
  # Performing mutate
  out <- dplyr::mutate_at(.tbl, .vars, .funs, ..., .cols = NULL)
  
  # Print NAs
  na_counter(.data = .tbl, .vars = .vars, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_at_qc <- function(.tbl, .vars, .funs, ..., .cols = NULL){
  
  # Performing mutate
  out <- dplyr::transmute_at(.tbl, .vars, .funs, ..., .cols = NULL)
  
  # Print NAs
  na_counter(.data = .tbl, .vars = .vars, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
mutate_if_qc <- function(.tbl, .predicate, .funs, ...){
  
  # Performing mutate
  out <- dplyr::mutate_if(.tbl, .predicate, .funs, ...)
  
  # Print NAs
  na_counter(.data = .tbl, .predicate = .predicate, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}

#' @rdname mutate_qc
#' @export
transmute_if_qc <- function(.tbl, .predicate, .funs, ...){
  
  # Performing mutate
  out <- dplyr::transmute_if(.tbl, .predicate, .funs, ...)
  
  # Print NAs
  na_counter(.data = .tbl, .predicate = .predicate, .funs = .funs, ...)
  
  # Returning data
  return(out)
  
}
