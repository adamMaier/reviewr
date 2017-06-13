# Function that replicates dplyr's mutate, but includes message of how many NAs were made
mutate_qc <- function(.data, ...){
    
    # Counting number of NAs in each mutate call by making each new variable a list element, then
    # summing number of NAs in each element
    num_na <- eval(substitute(list(...)), envir = .data)
    num_na <- lapply(num_na, FUN = function(x) sum(is.na(x)))
    mapply(
        FUN = function(x, y) message(x, " NAs produced in ", y), 
        x = num_na, y =  as.list(names(num_na))
    )
      
    # Performing mutate
    dplyr::mutate(.data, ...)
    
}
    