


# Function that replicates dplyr's mutate, but includes message of how many NAs were made
mutate_qc <- function(.data, ...){
    
    # Counting number of NAs in each mutate call by making each new variable a list element, then
    # summing number of NAs in each element
    eval(substitute(list(...)), envir = .data) %>%
    lapply(., FUN = function(x){ sum(is.na(x)) }) %>%
    mapply(
        FUN = function(x, y){ message(x, " NAs produced in ", y) }, 
        x = ., y =  as.list(names(.))
    )
      
    # Performing mutate
    dplyr::mutate(.data, ...)
    
}
    