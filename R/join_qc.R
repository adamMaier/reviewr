# Each function below works identically as the dplyr join functions, but also prints some merge 
# diagnoses and provide an option to create a new .merge variable indicating the merge result for 
# each row.

# Full Join
full_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), .merge = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if(".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if(".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
  
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)

    # Doing full join
    joined <- full_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, !is.na(.x_tracker) & is.na(.y_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker) & !is.na(.y_tracker))
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(suppressMessages(anti_join(x, y, by = by, suffix = suffix,  ...)))
    anti_n_y <- dplyr::tally(suppressMessages(anti_join(y, x, by = by, suffix = suffix,  ...)))
    extra_rows_x <- matched - (dplyr::tally(x) - anti_n_x)
    extra_rows_y <- matched - (dplyr::tally(y) - anti_n_y)
    
    # Print merge diagnoses
    message(
        matched, " Rows are matches", "\n",
        unmatched_x, " Rows are from left only", "\n",
        unmatched_y, " Rows are from right only", "\n",
        extra_rows_x, " Additional rows with a matched ID than original left", "\n",
        extra_rows_y, " Additional rows with a matched ID than original right"
    )
    
    # Create .merge variable if specified
    if(.merge){
        joined <- dplyr::mutate(
            joined,
            .merge = dplyr::case_when(
                !is.na(.$.x_tracker) & is.na(.$.y_tracker) ~ "left_only",
                is.na(.$.x_tracker) & !is.na(.$.y_tracker) ~ "right_only",
                TRUE ~ "matched"
                )
            )
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
  
}


# Inner Join
inner_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...){

    # Doing joins
    joined <- dplyr::inner_join(x, y, by = by, suffix = suffix,  ...)
    joined_left <- suppressMessages(semi_join(x, y, by = by, suffix = suffix,  ...))
    joined_right <- suppressMessages(semi_join(y, x, by = by, suffix = suffix,  ...))
    
    # Calculating merge diagnoses
    matched_x <- dplyr::tally(joined_left)
    unmatched_x <- dplyr::tally(x) - dplyr::tally(joined_left)
    matched_y <- dplyr::tally(joined_right)
    unmatched_y <- dplyr::tally(y) - dplyr::tally(joined_right)
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(suppressMessages(anti_join(x, y, by = by, suffix = suffix,  ...)))
    anti_n_y <- dplyr::tally(suppressMessages(anti_join(y, x, by = by, suffix = suffix,  ...)))
    extra_rows_x <- dplyr::tally(joined) - (dplyr::tally(x) - anti_n_x)
    extra_rows_y <- dplyr::tally(joined) - (dplyr::tally(y) - anti_n_y)

    # Print merge diagnoses
    message(
        matched_x, " Rows from left matched", "\n",
        unmatched_x, " Rows dropped from left", "\n",
        matched_y, " Rows from right matched", "\n",
        unmatched_y, " Rows dropped from right", "\n",
        extra_rows_x, " Additional rows with a matched ID than original left", "\n",
        extra_rows_y, " Additional rows with a matched ID than original right"
    )
    
    # Returning data frame
    return(joined)
    
}


# Left Join
left_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), .merge = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if(".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if(".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
    
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)

    # Doing join
    joined <- dplyr::left_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, is.na(.y_tracker))
    unmatched_y <- dplyr::tally(
        suppressMessages(dplyr::anti_join(y, x, by = by, suffix = suffix,  ...))
    )

    # Counting extra rows created
    anti_n_x <- dplyr::tally(suppressMessages(anti_join(x, y, by = by, suffix = suffix,  ...)))
    anti_n_y <- dplyr::tally(suppressMessages(anti_join(y, x, by = by, suffix = suffix,  ...)))
    extra_rows_x <- matched - (dplyr::tally(x) - anti_n_x)
    extra_rows_y <- matched - (dplyr::tally(y) - anti_n_y)
    
    # Print merge diagnoses
    message(
        matched, " Rows are matches", "\n",
        unmatched_x, " Rows are from left only", "\n",
        unmatched_y, " Rows were dropped from right", "\n",
        extra_rows_x, " Additional rows with a matched ID than original left", "\n",
        extra_rows_y, " Additional rows with a matched ID than original right"
    )
    
    # Create .merge variable if specified
    if(.merge){
        joined <- dplyr::mutate(
            joined,
            .merge = ifelse(!is.na(.x_tracker) & is.na(.y_tracker), "left_only", "matched")
        )
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
    
}


# Right Join
right_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), .merge = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if(".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if(".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
    
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)
    
    # Doing join
    joined <- dplyr::right_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.x_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker))
    unmatched_x <- dplyr::tally(
        suppressMessages(dplyr::anti_join(x, y, by = by, suffix = suffix,  ...))
    )
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(suppressMessages(anti_join(x, y, by = by, suffix = suffix,  ...)))
    anti_n_y <- dplyr::tally(suppressMessages(anti_join(y, x, by = by, suffix = suffix,  ...)))
    extra_rows_x <- matched - (dplyr::tally(x) - anti_n_x)
    extra_rows_y <- matched - (dplyr::tally(y) - anti_n_y)
    
    # Print merge diagnoses
    message(
        matched, " Rows are matches", "\n",
        unmatched_y, " Rows are from right only", "\n",
        unmatched_x, " Rows were dropped from left", "\n",
        extra_rows_y, " Additional rows with a matched ID than original right"
    )
    
    # Create .merge variable if specified
    if(.merge){
        joined <- dplyr::mutate(
            joined,
            .merge = ifelse(is.na(.x_tracker) & !is.na(.y_tracker), "right_only", "matched")
        )
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
    
}


# Anti Join
anti_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...){
    
    # Doing join
    anti_joined <- dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)

    # Calculating merge diagnoses 
    matched_x <- dplyr::tally(x) - dplyr::tally(anti_joined)

    # Print merge diagnoses
    message(matched_x, " Rows were dropped from left")
    
    # Returning data frame
    return(anti_joined)
    
}


# Semi Join
semi_join_qc <- function(x, y, by = NULL, suffix = c(".x", ".y"), ...){
    
    # Doing join
    joined <- dplyr::semi_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    unmatched_x <- dplyr::tally(x) - dplyr::tally(joined)
    
    # Print merge diagnoses
    message(unmatched_x, " Rows were dropped from left")
    
    # Rreturning data frame
    return(joined)
    
}
