# Function to report how many and what proportion of rows are dropped when performing a filter.
filter_qc <- function(data, ...){
    
    # Counting initial rows
    init_rows <- dplyr::tally(data)
    
    # Doing filter and counting remaining rows
    out <- dplyr::filter(data, ...)
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