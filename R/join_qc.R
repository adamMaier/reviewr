#' Report number of matched and/or extra cases when performing a dplyr join
#' 
#' \code{full_join_qc}, \code{inner_join}, \code{left_join}, \code{right_join}, 
#'   \code{anti_join}, and \code{semi_join} return by default near identical 
#'   objects as their dplyr equivalents.
#' 
#' Each _qc version of the join functions is identical to its dplyr equivalent
#' except that it automatically prints the number of rows that were matched, the
#' number of rows that were not matched, and the number of additional rows 
#' compared to the initial data frame(s) when doing a \code{left} or 
#' \code{right} join, for example when there is more than one match on the
#' \code{by} identifier(s). There are also options to create new variables 
#' identifying and classifying rows based on how/if they matched.
#' 
#' @section Optional New Variables:
#'   For \code{full_join_qc}, \code{left_join_qc}, and \code{right_join_qc}, 
#'   there is an added option of creating a new variable called \code{.merge} 
#'   that indicates whether the row in the joined data was from the 
#'   \code{"left_only"}, \code{"right_only"} or \code{"matched"}. This variable 
#'   can be helpful when diagnosing why the join did or did not match as 
#'   desired. 
#' 
#'   \code{left_join_qc}, and \code{right_join_qc} also have the option of 
#'   creating a new variable called \code{.extra} which indicates whether the 
#'   row in the  joined data is an additional row with the given combination of
#'   \code{by}. For example, if there were only 2 rows with an ID equal to "A" 
#'   in the original left data set but 3 rows with this ID in the right data 
#'   set, then the left joined data will have more rows with this ID than the 
#'   original left. \code{.extra} is a logical that when \code{.extra = TRUE}
#'   flags a row on \code{by} that has additional rows than the original left 
#'   or right data frame depending on whether \code{left_join_qc} or
#'    \code{right_join_qc} was called.
#' 
#' @section Grouping:
#'   Groups in the data frames are ignored for the purpose of joining, and the
#'   result does not preserve the grouping of \code{x}. This is the only
#'   difference with the dplyr joins, which do preserve the grouping of \code{x}.
#' 
#' @param x,y tbls to join
#' 
#' @param by a character vector of variables to join by. If \code{NULL}, the 
#'   default, \code{*_join_qc} will do a natural join, using all variables with 
#'   common names across the two tables. A message lists the variables so that 
#'   you can check they're right (to suppress the message, simply explicitly 
#'   list the variables that you want to join).
#'
#'   To join by different variables on x and y use a named vector. For example, 
#'   \code{by = c("a" = "b")} will match \code{x.a} to \code{y.b}.
#'   
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
#'   same src as \code{x}. This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#'   
#' @param ... other parameters passed onto methods.
#' 
#' @param .merge a character value used to name a new character variable, which
#'   tracks the source of each row of the new, joined data. If \code{NULL}, the
#'   default, no new merge-tracking variable will be created. An error will
#'   occur if a variable is already named the value specified in\code{.merge}, 
#'   so make sure to choose different names for different joins.
#'   
#' @param .extra a character value used to name a new logical variablee, which 
#'   will be \code{TRUE} for any row of the new joined data that represents a 
#'   combination of the \code{by} identifiers that has more rows than the 
#'   original left and/or right data frames. If \code{NULL}, the default, no new
#'   extra row tracking variable will be created. An error will occur if a 
#'   variable is already named the value specified in\code{.extra}, so make sure
#'   to choose different names for different joins. This is only an option for
#'   \code{left_join_qc}, and \code{right_join_qc}.
#' 
#' @seealso \code{\link[dplyr]{join}}
#' 
#' @examples
#' data_A <- data.frame(id = 1:10, A = 11:20)
#' data_B <- data.frame(id = c(5, 5, 5, 5, 7, 7, 9, 10, 11, 12), B = 21:30)
#' 
#' # Full join with new .merge variable
#' full_join_qc(data_A, data_B, .merge = "merge_ab")
#' 
#' # Left join with new .extra variable
#' left_join_qc(data_A, data_B, .extra = "extra_ab")
#' 
#' # Right join with both new variables
#' right_join_qc(data_A, data_B, .merge = "merge_ab", .extra = "extra_ab")
#' 
#' @name join_qc
NULL

#' @rdname join_qc
#' @export
full_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
                         .merge = FALSE, .extra = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if (".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if (".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if (".x_count" %in% names(x)){
        message("Warning: variable .x_count in left data was dropped")
    }
    if (".y_count" %in% names(y)){
        message("Warning: variable .y_count in right data was dropped")
    }
    if (.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
    if (.extra & (".extra" %in% names(x) | ".extra" %in% names(y))){
      stop("Variable .extra already exists; change name before proceeding")
    }
    
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)
    
    # Extracting matched variables from left and from right
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(attr(by, which = "names"))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases where by matches
    # on non-equivalent names
    if (!is.null(attr(by, which = "names"))) {
        by_reverse <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        attr(by_reverse, which = "names") <- c(
            unname(by)[grepl(".", attr(by, which = "names"))],
            unname(by)[grepl("^$", attr(by, which = "names"))]
        )
    } else {
        by_reverse <- by
    }
    
    # Doing full join
    joined <- dplyr::full_join(x, y, by = by, copy = copy, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, !is.na(.x_tracker) & is.na(.y_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker) & !is.na(.y_tracker))
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)
        )
    )
    anti_n_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix,  ...)
        )
    )
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
    if (.merge) {
        joined <- dplyr::mutate(
            joined,
            .merge = dplyr::case_when(
                !is.na(joined$.x_tracker) & is.na(joined$.y_tracker) ~ "left_only",
                is.na(joined$.x_tracker) & !is.na(joined$.y_tracker) ~ "right_only",
                TRUE ~ "matched"
                )
            )
    }
    
    # Create .extra variable if specified.
    # In a full join, extra rows happen when not a 1-1 merge
    if (.extra) {
        x_count <- dplyr::select_at(x, matched_vars_left)
        x_count <- dplyr::group_by_at(x_count, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_count = n())
        x_count <- dplyr::filter(x_count, .x_count  > 1)
        x_count <- dplyr::mutate(x_count, .x_count = T)
        y_count <- dplyr::select(y, matched_vars_right)
        names(y_count) <- matched_vars_left
        y_count <- dplyr::group_by_at(y_count, matched_vars_left)
        y_count <- dplyr::summarize(y_count, .y_count = n())
        y_count <- dplyr::filter(y_count, .y_count  > 1)
        y_count <- dplyr::mutate(y_count, .y_count = T)
        joined <- dplyr::left_join(joined, x_count, by = matched_vars_left)
        joined <- dplyr::left_join(joined, y_count, by = matched_vars_left)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                joined$.x_count & joined$.y_count ~ "extra_both",
                joined$.x_count & is.na(joined$.y_count) ~ "extra_right",
                is.na(joined$.x_count) & joined$.y_count ~ "extra_left",
                TRUE ~ "not_extra"
            )
        )
        joined <- dplyr::select(joined, -.x_count, -.y_count)
    }

    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
  
}

#' @rdname join_qc
#' @export
inner_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),  
                          .extra = FALSE, ...){

    # Checking to make sure used variable names are not already in use
    if (".x_count" %in% names(x)){
        message("Warning: variable .x_count in left data was dropped")
    }
    if (".y_count" %in% names(y)){
        message("Warning: variable .y_count in right data was dropped")
    }
    if (.extra & (".extra" %in% names(x) | ".extra" %in% names(y))){
        stop("Variable .extra already exists; change name before proceeding")
    }
    
    # Extracting matched variables from left and from right
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(attr(by, which = "names"))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases where by matches
    # on non-equivalent names
    if (!is.null(attr(by, which = "names"))){
        by_reverse <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        attr(by_reverse, which = "names") <- c(
            unname(by)[grepl(".", attr(by, which = "names"))],
            unname(by)[grepl("^$", attr(by, which = "names"))]
        )
    } else {
        by_reverse <- by
    }
    
    # Doing joins
    joined <- dplyr::inner_join(x, y, by = by, suffix = suffix,  ...)
    joined_left <- suppressMessages(dplyr::semi_join(x, y, by = by, suffix = suffix,  ...))
    joined_right <- suppressMessages(dplyr::semi_join(y, x, by = by_reverse, suffix = suffix,  ...))
    
    # Calculating merge diagnoses
    matched_x <- dplyr::tally(joined_left)
    unmatched_x <- dplyr::tally(x) - dplyr::tally(joined_left)
    matched_y <- dplyr::tally(joined_right)
    unmatched_y <- dplyr::tally(y) - dplyr::tally(joined_right)
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)
        )
    )
    anti_n_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix,  ...)
        )
    )
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
    
    # Create .extra variable if specified.
    # In an inner join, extra rows happen when not a 1-1 merge
    if (.extra) {
        x_count <- dplyr::select_at(x, matched_vars_left)
        x_count <- dplyr::group_by_at(x_count, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_count = n())
        x_count <- dplyr::filter(x_count, .x_count  > 1)
        x_count <- dplyr::mutate(x_count, .x_count = T)
        y_count <- dplyr::select(y, matched_vars_right)
        names(y_count) <- matched_vars_left
        y_count <- dplyr::group_by_at(y_count, matched_vars_left)
        y_count <- dplyr::summarize(y_count, .y_count = n())
        y_count <- dplyr::filter(y_count, .y_count  > 1)
        y_count <- dplyr::mutate(y_count, .y_count = T)
        joined <- dplyr::left_join(joined, x_count, by = matched_vars_left)
        joined <- dplyr::left_join(joined, y_count, by = matched_vars_left)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                joined$.x_count & joined$.y_count ~ "extra_both",
                joined$.x_count & is.na(joined$.y_count) ~ "extra_right",
                is.na(joined$.x_count) & joined$.y_count ~ "extra_left",
                TRUE ~ "not_extra"
            )
        )
        joined <- dplyr::select(joined, -.x_count, -.y_count)
    }
    
    # Returning data frame
    return(joined)
    
}

#' @rdname join_qc
#' @export
left_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),  
                         .merge = FALSE, .extra = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if (".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if (".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if (".x_count" %in% names(x)){
        message("Warning: variable .x_count in left data was dropped")
    }
    if (.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
    if (.extra & (".extra" %in% names(x) | ".extra" %in% names(y))){
        stop("Variable .extra already exists; change name before proceeding")
    }
  
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)
    
    # Extracting matched variables from left and from right
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
    } else if (is.null(attr(by, which = "names"))) {
        matched_vars_left <- by
    } else {
        matched_vars_left <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases where by matches
    # on non-equivalent names
    if (!is.null(attr(by, which = "names"))) {
        by_reverse <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        attr(by_reverse, which = "names") <- c(
            unname(by)[grepl(".", attr(by, which = "names"))],
            unname(by)[grepl("^$", attr(by, which = "names"))]
        )
    } else {
        by_reverse <- by
    }
    
    # Doing join
    joined <- dplyr::left_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, is.na(.y_tracker))
    unmatched_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix,  ...)
        )
    )

    # Counting extra rows created
    anti_n_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)
        )
    )
    extra_rows_x <- matched - (dplyr::tally(x) - anti_n_x)

    # Print merge diagnoses
    message(
        matched, " Rows are matches", "\n",
        unmatched_x, " Rows are from left only", "\n",
        unmatched_y, " Rows were dropped from right", "\n",
        extra_rows_x, " Additional rows with a matched ID than original left"
    )
    
    # Create .merge variable if specified
    if (.merge) {
        joined <- dplyr::mutate(
            joined,
            .merge = ifelse(!is.na(.x_tracker) & is.na(.y_tracker), "left_only", "matched")
        )
    }
    
    # Create .extra variable if specified.
    # In a left join, extra rows happen when not a more than 1 distinct right combo
    if (.extra) {
        y_count <- dplyr::select(y, matched_vars_right)
        names(y_count) <- matched_vars_left
        y_count <- dplyr::group_by_at(y_count, matched_vars_left)
        y_count <- dplyr::summarize(y_count, .y_count = n())
        y_count <- dplyr::filter(y_count, .y_count  > 1)
        y_count <- dplyr::mutate(y_count, .y_count = T)
        joined <- dplyr::left_join(joined, y_count, by = matched_vars_left)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                joined$.y_count ~ "extra_left",
                TRUE ~ "not_extra"
            )
        )
        joined <- dplyr::select(joined, -.y_count)
    }
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
    
}

#' @rdname join_qc
#' @export
right_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                          .merge = FALSE, .extra = FALSE, ...){
    
    # Checking to make sure used variable names are not already in use
    if (".x_tracker" %in% names(x)){
        message("Warning: variable .x_tracker in left data was dropped")
    }
    if (".y_tracker" %in% names(y)){
        message("Warning: variable .y_tracker in right data was dropped")
    }
    if (".y_count" %in% names(y)){
        message("Warning: variable .y_count in right data was dropped")
    }
    if (.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
        stop("Variable .merge already exists; change name before proceeding")
    }
    if (.extra & (".extra" %in% names(x) | ".extra" %in% names(y))){
        stop("Variable .extra already exists; change name before proceeding")
    }
  
    # Adding simple merge tracker variables to data frames
    x <- dplyr::mutate(x, .x_tracker = 1)
    y <- dplyr::mutate(y, .y_tracker = 1)
    
    # Extracting matched variables from left and from right
    if (is.null(by)) {
        matched_vars_right <- names(y)[names(y) %in% names(x)]
        matched_vars_left <- matched_vars_right
    } else if (is.null(attr(by, which = "names"))) {
        matched_vars_right <- by
        matched_vars_left <- by
    } else {
        matched_vars_right <- unname(by)
        matched_vars_left <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases where by matches
    # on non-equivalent names
    if (!is.null(attr(by, which = "names"))) {
        by_reverse <- c(
            attr(by, which = "names")[grepl(".", attr(by, which = "names"))],
            by[grepl("^$", attr(by, which = "names"))]
        )
        attr(by_reverse, which = "names") <- c(
            unname(by)[grepl(".", attr(by, which = "names"))],
            unname(by)[grepl("^$", attr(by, which = "names"))]
        )
    } else {
        by_reverse <- by
    }
    
        # Doing join
    joined <- dplyr::right_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.x_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker))
    unmatched_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)
        )
    )
    
    # Counting extra rows created
    anti_n_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix,  ...)
        )
    )
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
    
    # Create .extra variable if specified.
    # In a right join, extra rows happen when multiple combos in left
    if (.extra) {
        x_count <- dplyr::select_at(x, matched_vars_left)
        x_count <- dplyr::group_by_at(x_count, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_count = n())
        x_count <- dplyr::filter(x_count, .x_count  > 1)
        x_count <- dplyr::mutate(x_count, .x_count = T)
        joined <- dplyr::left_join(joined, x_count, by = matched_vars_left)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                joined$.x_count ~ "extra_right",
                TRUE ~ "not_extra"
            )
        )
        joined <- dplyr::select(joined, -.x_count)
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    return(joined)
    
}

#' @rdname join_qc
#' @export
anti_join_qc <- function(x, y, by = NULL, copy = FALSE, ...){
    
    # Doing join
    anti_joined <- dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)

    # Calculating merge diagnoses 
    matched_x <- dplyr::tally(x) - dplyr::tally(anti_joined)

    # Print merge diagnoses
    message(matched_x, " Rows were dropped from left")
    
    # Returning data frame
    return(anti_joined)
    
}

#' @rdname join_qc
#' @export
semi_join_qc <- function(x, y, by = NULL, copy = FALSE, ...){
    
    # Doing join
    joined <- dplyr::semi_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    unmatched_x <- dplyr::tally(x) - dplyr::tally(joined)
    
    # Print merge diagnoses
    message(unmatched_x, " Rows were dropped from left")
    
    # Rreturning data frame
    return(joined)
    
}