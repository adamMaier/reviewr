#' Report number of matched and/or extra cases when performing a dplyr join
#' 
#' \code{full_join_qc}, \code{inner_join}, \code{left_join}, \code{right_join}, 
#'   \code{anti_join}, and \code{semi_join} return by default near identical 
#'   objects as their dplyr equivalents.
#' 
#' Each _qc version of the join functions is identical to its dplyr equivalent
#' except that it automatically prints the number of rows that were matched, the
#' number of rows that were not matched, and the number of additional rows 
#' compared to the initial data frame(s), for example when there is more than 
#' one match on the \code{by} identifier(s). For \code{full_join_qc}, 
#' \code{left_join}, and \code{right_join}, there is an added option of creating 
#' a new variable called \code{.merge} that indicates whether the row in the 
#' joined data was from the \code{"left_only"}, \code{"right_only"} or 
#' \code{"matched"}. This variable can be helpful when diagnosing why the join 
#' did or did not match as desired. All join functions except anti and semi also 
#' have the option of creating a new variable called \code{.extra} which is
#'  \code{TRUE} if the row in the joined data represents a combination of 
#'  \code{by} identifiers that has more rows than the original left and/or right
#'   data frames.
#' 
#' @section Grouping:
#'   Groups are ignored for the purpose of joining, and the result does not 
#'   preserve the grouping of \code{x}. This is the only difference with the 
#'   dplyr joins, which do preserve the grouping of \code{x}.
#' 
#' @param x,y tbls to join
#' @param by a character vector of variables to join by. If \code{NULL}, the default,
#'   \code{*_join} will do a natural join, using all variables with common names
#'   across the two tables. A message lists the variables so that you can check
#'   they're right (to suppress the message, simply explicitly list the
#'    variables that you want to join).
#'
#'   To join by different variables on x and y use a named vector.For example, 
#'   \code{by = c("a" = "b")} will match \code{x.a} to \code{y.b}.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it
#' @param ... other parameters passed onto methods
#' @param .merge A logical value indicating whether a new character variable, 
#'   \code{.merge}, should be created, which tracks the source of each row of
#'   the new, joined data. The default is \code{FALSE}. An error will occur if a 
#'   variable is already named \code{.merge}, so make sure to rename or drop 
#'   this variable after each join when doing successive joins.
#' @param .extra A logical value indicating whether a new logical variable, 
#'   \code{.extra}, should be created, which will be \code{TRUE} for any row of
#'   the new joined data that represents a combination of the \code{by}
#'   identifiers that has more rows than the original left and/or right data
#'   frames. The default is \code{FALSE}. An error will occur if a variable is 
#'   already named \code{.extra}, so make sure to rename or drop this variable 
#'   after each join when doing successive joins.
#' 
#' @seealso \code{\link[dplyr]{join}}
#' 
#' @examples
#' data_A <- data.frame(id = 1:10, A = 11:20)
#' data_B <- data.frame(id = c(5, 5, 5, 5, 7, 7, 9, 10, 11, 12), B = 21:30)
#' 
#' # Full join with new .merge variable
#' full_join_qc(data_A, data_B, .merge = T)
#' 
#' # Left join with new .extra variable
#' left_join_qc(data_A, data_B, .extra = T)
#' 
#' # Right join with both new variables
#' right_join_qc(data_A, data_B, .merge = T, .extra = T)
#' 
#' @name join_qc
NULL

#' @rdname join_qc
#' @export
full_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...,
                         .merge = FALSE, .extra = FALSE){
    
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
        matched_vars_right <- matched_vars_left
    } else {
        matched_vars_left <- attr(by, which = "names")
        matched_vars_right <- unname(by)
    }
    
    # Adding count of rows by matched variables
    x <- dplyr::group_by_at(x, matched_vars_left)
    x <- dplyr::mutate(x, .x_count = n())
    x <- dplyr::ungroup(x)
    y <- dplyr::group_by_at(y, matched_vars_right)
    y <- dplyr::mutate(y, .y_count = n())
    y <- dplyr::ungroup(y)
    
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
            dplyr::anti_join(y, x, by = by, suffix = suffix,  ...)
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
    
    # Create .extra variable if specified
    if (.extra) {
        joined <- dplyr::group_by_at(joined, matched_vars_left)
        joined <- dplyr::mutate(joined, .extra = .x_count != n())
        joined <- dplyr::ungroup(joined)
        joined <- dplyr::group_by_at(joined, matched_vars_right)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                is.na(.y_count) ~ .extra,
                .y_count != n() ~ T,
                .y_count == n() & is.na(.extra) ~ F,
                TRUE ~ .extra
            )
        )
        joined <- dplyr::ungroup(joined)
    }

    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker, -.x_count, -.y_count)
    return(joined)
  
}

#' @rdname join_qc
#' @export
inner_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., 
                          .extra = FALSE){

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
        matched_vars_right <- matched_vars_left
    } else {
        matched_vars_left <- attr(by, which = "names")
        matched_vars_right <- unname(by)
    }
    
    # Adding count of rows by matched variables
    x <- dplyr::group_by_at(x, matched_vars_left)
    x <- dplyr::mutate(x, .x_count = n())
    x <- dplyr::ungroup(x)
    y <- dplyr::group_by_at(y, matched_vars_right)
    y <- dplyr::mutate(y, .y_count = n())
    y <- dplyr::ungroup(y)

    # Doing joins
    joined <- dplyr::inner_join(x, y, by = by, suffix = suffix,  ...)
    joined_left <- suppressMessages(dplyr::semi_join(x, y, by = by, suffix = suffix,  ...))
    joined_right <- suppressMessages(dplyr::semi_join(y, x, by = by, suffix = suffix,  ...))
    
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
            dplyr::anti_join(y, x, by = by, suffix = suffix,  ...)
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
    
    # Create .extra variable if specified
    if (.extra) {
        joined <- dplyr::group_by_at(joined, matched_vars_left)
        joined <- dplyr::mutate(joined, .extra = .x_count != n())
        joined <- dplyr::ungroup(joined)
        joined <- dplyr::group_by_at(joined, matched_vars_right)
        joined <- dplyr::mutate(
            joined, 
            .extra = dplyr::case_when(
                is.na(.y_count) ~ .extra,
                .y_count != n() ~ T,
                .y_count == n() & is.na(.extra) ~ F,
                TRUE ~ .extra
            )
        )
        joined <- dplyr::ungroup(joined)
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_count, -.y_count)
    return(joined)
    
}

#' @rdname join_qc
#' @export
left_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., 
                         .merge = FALSE, .extra = FALSE){
    
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
        matched_vars_left <- attr(by, which = "names")
    }
    
    # Adding count of rows by matched variables
    x <- dplyr::group_by_at(x, matched_vars_left)
    x <- dplyr::mutate(x, .x_count = n())
    x <- dplyr::ungroup(x)

    # Doing join
    joined <- dplyr::left_join(x, y, by = by, suffix = suffix,  ...)
    
    # Calculating merge diagnoses 
    matched <- dplyr::tally(joined, !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, is.na(.y_tracker))
    unmatched_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by, suffix = suffix,  ...)
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
    
    # Create .extra variable if specified
    if (.extra) {
        joined <- dplyr::group_by_at(joined, matched_vars_left)
        joined <- dplyr::mutate(joined, .extra = .x_count != n())
        joined <- dplyr::ungroup(joined)
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker, -.x_count)
    return(joined)
    
}

#' @rdname join_qc
#' @export
right_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...,
                          .merge = FALSE, .extra = F){
    
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
        matched_vars_right <- names(x)[names(x) %in% names(y)]
    } else if (is.null(attr(by, which = "names"))) {
        matched_vars_right <- by
    } else {
        matched_vars_right <- unname(by)
    }
    
    # Adding count of rows by matched variables
    y <- dplyr::group_by_at(y, matched_vars_right)
    y <- dplyr::mutate(y, .y_count = n())
    y <- dplyr::ungroup(y)
    
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
            dplyr::anti_join(y, x, by = by, suffix = suffix,  ...)
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
    
    # Create .extra variable if specified
    if (.extra) {
        joined <- dplyr::group_by_at(joined, matched_vars_right)
        joined <- dplyr::mutate(joined, .extra = .y_count != n())
        joined <- dplyr::ungroup(joined)
    }
    
    # Dropping tracker variables and returning data frame
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker, -.y_count)
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