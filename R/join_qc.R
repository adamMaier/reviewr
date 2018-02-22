#' Report number of matched and/or extra cases when performing a dplyr join
#' 
#' \code{full_join_qc}, \code{inner_join}, \code{left_join}, \code{right_join}, 
#'   \code{anti_join}, and \code{semi_join} return by default near identical 
#'   objects as their dplyr equivalents.
#' 
#' Each _qc version of the join functions is identical to its dplyr equivalent
#' except that it automatically prints the number of rows that were matched, the
#' number of rows that were not matched, and the number of additional rows 
#' compared to the initial data frame(s) - for example when there is more than
#' one match on the \code{by} identifier(s). There are also options to create
#' new variables identifying and classifying rows based on how/if they matched.
#'
#' @section Join Descriptions: 
#'   All joins except \code{anti_join} and \code{semi_join} are classified as
#'   one to one, one to many, many to one, or many to many. These definitions
#'   describe the extent to which there are duplicated rows of unique 
#'   combinations of the \code{by} variable(s). In one to one merges, there is
#'   only one unique row of identifiers in each data set. Extra rows are never 
#'   created in one to one joins one to many and many to one joins occur when
#'   one of the data sets has a duplicated id row. One to many implies the right 
#'   data has the duplicated id; many to one implies the left data has the 
#'   duplicated id. Additional rows may be created in these types of joins. Many
#'   to many joins imply that both data sets have duplicated rows on the id
#'   variable(s). Additional rows may be created in this types of join. The
#'   join description is printed when performing any join except 
#'   \code{anti_join} and \code{semi_join}.
#'   
#' @section Optional New Variables:
#'   For \code{full_join_qc}, \code{left_join_qc}, and \code{right_join_qc}, 
#'   there is an added option of creating a new variable that indicates whether 
#'   the row in the joined data was from the \code{"left_only"}, 
#'   \code{"right_only"} or \code{"matched"}. This variable can be helpful when
#'   diagnosing why the join did or did not match as desired. Whatever character 
#'   value that is supplied to \code{.merge} becomes the name of this new
#'   variable.
#' 
#'   \code{left_join_qc}, and \code{right_join_qc} also have the option of 
#'   creating a new variable indicating whether the row in the  joined data is 
#'   an additional row with the given combination of \code{by}. For example, if
#'   there were only 2 rows with an ID equal to "A" in the original left data 
#'   but 3 rows with this ID in the right data set, then the left joined data 
#'   will have more rows with this ID than the original left. \code{.extra} is 
#'   a chracter value that when supplied becomes the name of this new variable
#'   flagging a row  that has additional rows than the original left 
#'   or right data frame.
#' 
#' @section Grouping:
#'   Groups in the data frames are ignored for the purpose of joining, but the 
#'   result preserves the grouping of \code{x}.
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
#' @param suffix If there are non-joined duplicate variables in x and y, these
#'   suffixes will be added to the output to diambiguate them. Should be a 
#'   character vector of length 2.
#' 
#' @param .merge a character value used to name a new character variable, which
#'   tracks the source of each row of the new, joined data. If \code{NULL}, the
#'   default, no new merge-tracking variable will be created. An error will
#'   occur if a variable is already named the value specified in\code{.merge}, 
#'   so make sure to choose different names for different joins.
#'   
#' @param .extra a character value used to name a new character variable, which 
#'   identifies any row of the new joined data that represents a combination of
#'   the \code{by} identifiers that has more rows than the original left and/or 
#'   right data frames. If \code{NULL}, the default, no new extra row tracking 
#'   variable will be created. An error will occur if a variable is already 
#'   named the value specified in\code{.extra}, so make sure to choose different
#'    names for different joins.
#' 
#' @param ... other parameters passed onto methods.
#' 
#' @seealso \code{\link[dplyr]{join}}
#' 
#' @examples
#' data_A <- 
#'   data.frame(
#'     id = 1:10, 
#'     var_A = 11:20
#'    )
#' data_B <- 
#'   data.frame(
#'     id = c(5, 5, 5, 5, 6, 7, 7, 9, 10, 11), 
#'     id_A = c(1:10), 
#'     var_B = 21:30
#'   )
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
                         .merge = NULL, .extra = NULL, ...){
    
    # Removing all group attributes from tables, but keeping group of x to 
    # re-apply after merge
    x_group <- attr(x, "vars")
    x <- dplyr::ungroup(x)
    y <- dplyr::ungroup(y)
    
    # Checking to make sure variable names used in function are not already in
    # use, and supply warning if so.
    if (".x_tracker" %in% names(x)) {
        message("Warning: variable .x_tracker in left table was dropped")
    }
    if (".y_tracker" %in% names(y)) {
        message("Warning: variable .y_tracker in right table was dropped")
    }

    if (!is.null(.merge)) {
        if (.merge %in% names(x) | .merge %in% names(y)) {
            stop(
                paste0(
                    "Column '", 
                    .merge, 
                    "' already exists; change .merge name before proceeding"
                )
            )
        }
    }

    # Adding simple merge tracker variables to tables
    x <- dplyr::mutate(x, .x_tracker = T)
    y <- dplyr::mutate(y, .y_tracker = T)
    
    # Extracting matched variables from left and from right. First is if no
    # by is specified, then if by is a character vector of matched variables,
    # then if by is a named vector.
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(names(by))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- ifelse(grepl(".", names(by)), names(by), unname(by))
        matched_vars_right <- unname(by)
    }

    # Creating "reverse" named vector of by (where left becomes right) in cases 
    # where by matches on non-equivalent names
    if (all.equal(matched_vars_left, matched_vars_right) != T) {
        by_reverse <- matched_vars_left
        names(by_reverse) <- matched_vars_right
    } else {
        by_reverse <- by
    }
    
    # Identifying type of merge that occured
    left_type <- max(duplicated(x[, matched_vars_left]))
    right_type <- max(duplicated(y[, matched_vars_right]))
    
    if (left_type == 0) left_type <- "ONE" else left_type <- "MANY"
    if (right_type == 0) right_type <- "ONE" else right_type <- "MANY"
    
    # Doing full join
    joined <- dplyr::full_join(x, y, by = by, copy = copy, suffix = suffix, ...)
    
    # Calculating distribution of matches in newly joined data
    matched <- dplyr::tally(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, !is.na(.x_tracker) & is.na(.y_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker) & !is.na(.y_tracker))
    total <- dplyr::tally(joined)
    
    # Calculating percent of rows from each data set that matched
    matched_ids <- dplyr::filter(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    matched_ids <- dplyr::select_at(matched_ids, matched_vars_left)
    matched_ids_x <- dplyr::distinct(matched_ids)
    matched_ids_y <- matched_ids_x
    names(matched_ids_y) <- matched_vars_right
    
    matched_percent_x <- suppressMessages(dplyr::inner_join(x, matched_ids_x))
    matched_percent_x <- dplyr::tally(matched_percent_x) / dplyr::tally(x) 
    matched_percent_x <- 100 * round(matched_percent_x, 3)
    matched_percent_y <- suppressMessages(dplyr::inner_join(y, matched_ids_y))
    matched_percent_y <- dplyr::tally(matched_percent_y) / dplyr::tally(x) 
    matched_percent_y <- 100 * round(matched_percent_y, 3)
    
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
        "\n",
        "This was a ", left_type, " TO ", right_type, " join", "\n",
        "\n",
        "MATCH DISTRIBUTION IN JOINED DATA\n",
        matched, " (", 100 * round(matched / total, 3), "%) Rows are matches", "\n",
        unmatched_x, " (", 100 * round(unmatched_x / total, 3), "%) Rows are from left only", "\n",
        unmatched_y, " (", 100 * round(unmatched_y / total, 3), "%) Rows are from right only", "\n",
        "\n",
        "MATCH RATES BASED ON ORIGINAL DATA\n",
        matched_percent_x, "% Percent of rows from left matched" ,"\n",
        matched_percent_y, "% Percent of rows from right matched", "\n",
        "\n",
        "ADDITIONAL ROWS\n",
        extra_rows_x, " More rows with a matched ID than original left", "\n",
        extra_rows_y, " More rows with a matched ID than original right"
    )
    
    # Create .merge variable if specified. This is the only part of the code
    # that requires rlang. Could not figure out how to implement := otherwise
    if (!is.null(.merge)) {
        merge_quo <- rlang::quo_name(.merge)
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(merge_quo) :=
                dplyr::case_when(
                    !is.na(joined$.x_tracker) & is.na(joined$.y_tracker) ~ "left_only",
                    is.na(joined$.x_tracker) & !is.na(joined$.y_tracker) ~ "right_only",
                    TRUE ~ "matched"
                )
        )
    }
    
    # Dropping tracker variables
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
  
    # Create .extra variable if specified.
    # In a full join, extra rows can happen when not a 1-1 merge
    if (!is.null(.extra)) {
        
        extra_quo <- rlang::quo_name(.extra)
        
        # Isolate combinations of ID varaibles in left table with more than one
        # row
        x_count <- dplyr::group_by_at(x, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_tracker = n())
        x_count <- dplyr::ungroup(x_count)
        x_count <- dplyr::filter(x_count, .x_tracker > 1)
        x_count <- dplyr::select(x_count, -.x_tracker)
        names(x_count) <- matched_vars_right
        y_extra <- suppressMessages(dplyr::semi_join(x_count, y))
        y_extra <- dplyr::mutate(y_extra, .y_tracker = T)

        y_count <- dplyr::group_by_at(y, matched_vars_right)
        y_count <- dplyr::summarize(y_count, .y_tracker = n())
        y_count <- dplyr::ungroup(y_count)
        y_count <- dplyr::filter(y_count, .y_tracker > 1)
        y_count <- dplyr::select(y_count, -.y_tracker)
        names(y_count) <- matched_vars_left
        x_extra <- suppressMessages(dplyr::semi_join(y_count, x))
        x_extra <- dplyr::mutate(x_extra, .x_tracker = T)
        
        if(dplyr::tally(x_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, x_extra, by = matched_vars_left)
            )
        } else {
            joined <- dplyr::mutate(joined, .x_tracker = NA)
        }
        
        if(dplyr::tally(y_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, y_extra, by = matched_vars_right)
            )
        } else {
            joined <- dplyr::mutate(joined, .y_tracker = NA)
        }

        joined <- dplyr::mutate(
            joined,
            rlang::UQ(extra_quo) :=
                dplyr::case_when(
                    joined$.x_tracker & joined$.y_tracker ~ "extra_on_both",
                    joined$.x_tracker & is.na(joined$.y_tracker) ~ "extra_on_left",
                    is.na(joined$.x_tracker) & joined$.y_tracker ~ "extra_on_right",
                    TRUE ~ "not_extra"
                )
        )
        joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    }
    
    # Regroup data and return
    if (!is.null(x_group)) joined <- dplyr::group_by_at(joined, x_group)
    return(joined)
  
}

#' @rdname join_qc
#' @export
inner_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
                         .extra = NULL, ...){
    
    # Removing all group attributes from tables, but keeping group of x to 
    # re-apply after merge
    x_group <- attr(x, "vars")
    x <- dplyr::ungroup(x)
    y <- dplyr::ungroup(y)
    
    # Extracting matched variables from left and from right. First is if no
    # by is specified, then if by is a character vector of matched variables,
    # then if by is a named vector.
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(names(by))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- ifelse(grepl(".", names(by)), names(by), unname(by))
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases 
    # where by matches on non-equivalent names
    if (all.equal(matched_vars_left, matched_vars_right) != T) {
        by_reverse <- matched_vars_left
        names(by_reverse) <- matched_vars_right
    } else {
        by_reverse <- by
    }
    
    # Identifying type of merge that occured
    left_type <- max(duplicated(x[, matched_vars_left]))
    right_type <- max(duplicated(y[, matched_vars_right]))
    
    if (left_type == 0) left_type <- "ONE" else left_type <- "MANY"
    if (right_type == 0) right_type <- "ONE" else right_type <- "MANY"
    
    # Doing inner join
    joined <- dplyr::inner_join(x, y, by = by, copy = copy, suffix = suffix, ...)
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix, ...)
        )
    )
    anti_n_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix, ...)
        )
    )
    extra_rows_x <- dplyr::tally(joined) - (dplyr::tally(x) - anti_n_x)
    extra_rows_y <- dplyr::tally(joined) - (dplyr::tally(y) - anti_n_y)
    
    # Print merge diagnoses
    message(
        "\n",
        "This was a ", left_type, " TO ", right_type, " join", "\n",
        "\n",
        "Inner joins only kepp matching cases, so no match diagnosis", "\n",
        "\n",
        "ADDITIONAL ROWS\n",
        extra_rows_x, " More rows with a matched ID than original left", "\n",
        extra_rows_y, " More rows with a matched ID than original right"
    )

    # Create .extra variable if specified.
    # Extra rows can happen when not a 1-1 merge
    if (!is.null(.extra)) {
        
        extra_quo <- rlang::quo_name(.extra)
        
        # Isolate combinations of ID varaibles in left table with more than one
        # row
        x_count <- dplyr::group_by_at(x, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_tracker = n())
        x_count <- dplyr::ungroup(x_count)
        x_count <- dplyr::filter(x_count, .x_tracker > 1)
        x_count <- dplyr::select(x_count, -.x_tracker)
        names(x_count) <- matched_vars_right
        y_extra <- suppressMessages(dplyr::semi_join(x_count, y))
        y_extra <- dplyr::mutate(y_extra, .y_tracker = T)
        
        y_count <- dplyr::group_by_at(y, matched_vars_right)
        y_count <- dplyr::summarize(y_count, .y_tracker = n())
        y_count <- dplyr::ungroup(y_count)
        y_count <- dplyr::filter(y_count, .y_tracker > 1)
        y_count <- dplyr::select(y_count, -.y_tracker)
        names(y_count) <- matched_vars_left
        x_extra <- suppressMessages(dplyr::semi_join(y_count, x))
        x_extra <- dplyr::mutate(x_extra, .x_tracker = T)
        
        if(dplyr::tally(x_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, x_extra, by = matched_vars_left)
            )
        } else {
            joined <- dplyr::mutate(joined, .x_tracker = NA)
        }
        
        if(dplyr::tally(y_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, y_extra, by = matched_vars_right)
            )
        } else {
            joined <- dplyr::mutate(joined, .y_tracker = NA)
        }
        
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(extra_quo) :=
                dplyr::case_when(
                    joined$.x_tracker & joined$.y_tracker ~ "extra_on_both",
                    joined$.x_tracker & is.na(joined$.y_tracker) ~ "extra_on_left",
                    is.na(joined$.x_tracker) & joined$.y_tracker ~ "extra_on_right",
                    TRUE ~ "not_extra"
                )
        )
        joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    }
    
    # Regroup data and return
    if (!is.null(x_group)) joined <- dplyr::group_by_at(joined, x_group)
    return(joined)
    
}

#' @rdname join_qc
#' @export
left_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
                         .merge = NULL, .extra = NULL, ...){
    
    # Removing all group attributes from tables, but keeping group of x to 
    # re-apply after merge
    x_group <- attr(x, "vars")
    x <- dplyr::ungroup(x)
    y <- dplyr::ungroup(y)
    
    # Checking to make sure variable names used in function are not already in
    # use, and supply warning if so.
    if (".x_tracker" %in% names(x)) {
        message("Warning: variable .x_tracker in left table was dropped")
    }
    if (".y_tracker" %in% names(y)) {
        message("Warning: variable .y_tracker in right table was dropped")
    }
    
    if (!is.null(.merge)) {
        if (.merge %in% names(x) | .merge %in% names(y)) {
            stop(
                paste0(
                    "Column '", 
                    .merge, 
                    "' already exists; change .merge name before proceeding"
                )
            )
        }
    }
    
    # Adding simple merge tracker variables to tables
    x <- dplyr::mutate(x, .x_tracker = T)
    y <- dplyr::mutate(y, .y_tracker = T)
    
    # Extracting matched variables from left and from right. First is if no
    # by is specified, then if by is a character vector of matched variables,
    # then if by is a named vector.
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(names(by))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- ifelse(grepl(".", names(by)), names(by), unname(by))
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases 
    # where by matches on non-equivalent names
    if (all.equal(matched_vars_left, matched_vars_right) != T) {
        by_reverse <- matched_vars_left
        names(by_reverse) <- matched_vars_right
    } else {
        by_reverse <- by
    }
    
    # Identifying type of merge that occured
    left_type <- max(duplicated(x[, matched_vars_left]))
    right_type <- max(duplicated(y[, matched_vars_right]))
    
    if (left_type == 0) left_type <- "ONE" else left_type <- "MANY"
    if (right_type == 0) right_type <- "ONE" else right_type <- "MANY"
    
    # Doing left join
    joined <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix, ...)
    
    # Calculating distribution of matches in newly joined data
    matched <- dplyr::tally(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    unmatched_x <- dplyr::tally(joined, !is.na(.x_tracker) & is.na(.y_tracker))
    total <- dplyr::tally(joined)
    
    # Calculating percent of rows from each data set that matched
    matched_ids <- dplyr::filter(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    matched_ids <- dplyr::select_at(matched_ids, matched_vars_left)
    matched_ids_x <- dplyr::distinct(matched_ids)
    matched_ids_y <- matched_ids_x
    names(matched_ids_y) <- matched_vars_right
    
    matched_percent_x <- suppressMessages(dplyr::inner_join(x, matched_ids_x))
    matched_percent_x <- dplyr::tally(matched_percent_x) / dplyr::tally(x) 
    matched_percent_x <- 100 * round(matched_percent_x, 3)
    matched_percent_y <- suppressMessages(dplyr::inner_join(y, matched_ids_y))
    matched_percent_y <- dplyr::tally(matched_percent_y) / dplyr::tally(x) 
    matched_percent_y <- 100 * round(matched_percent_y, 3)
    
    # Counting extra rows created
    anti_n_x <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)
        )
    )
    extra_rows_x <- matched - (dplyr::tally(x) - anti_n_x)

    # Print merge diagnoses
    message(
        "\n",
        "This was a ", left_type, " TO ", right_type, " join", "\n",
        "\n",
        "MATCH DISTRIBUTION IN JOINED DATA\n",
        matched, " (", 100 * round(matched / total, 3), "%) Rows are matches", "\n",
        unmatched_x, " (", 100 * round(unmatched_x / total, 3), "%) Rows are from left only", "\n",
        "\n",
        "MATCH RATES BASED ON ORIGINAL DATA\n",
        matched_percent_x, "% Percent of rows from left matched" ,"\n",
        matched_percent_y, "% Percent of rows from right matched", "\n",
        "\n",
        "ADDITIONAL ROWS\n",
        extra_rows_x, " More rows with a matched ID than original left"
    )
    
    # Create .merge variable if specified. This is the only part of the code
    # that requires rlang. Could not figure out how to implement := otherwise
    if (!is.null(.merge)) {
        merge_quo <- rlang::quo_name(.merge)
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(merge_quo) :=
                dplyr::case_when(
                    !is.na(joined$.x_tracker) & is.na(joined$.y_tracker) ~ "left_only",
                    is.na(joined$.x_tracker) & !is.na(joined$.y_tracker) ~ "right_only",
                    TRUE ~ "matched"
                )
        )
    }
    
    # Dropping tracker variables
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    
    # Create .extra variable if specified.
    # In a full join, extra rows can happen when not a 1-1 merge
    if (!is.null(.extra)) {
        
        extra_quo <- rlang::quo_name(.extra)
        
        # Isolate combinations of ID varaibles in left table with more than one
        # row
        x_count <- dplyr::group_by_at(x, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_tracker = n())
        x_count <- dplyr::ungroup(x_count)
        x_count <- dplyr::filter(x_count, .x_tracker > 1)
        x_count <- dplyr::select(x_count, -.x_tracker)
        names(x_count) <- matched_vars_right
        y_extra <- suppressMessages(dplyr::semi_join(x_count, y))
        y_extra <- dplyr::mutate(y_extra, .y_tracker = T)
        
        y_count <- dplyr::group_by_at(y, matched_vars_right)
        y_count <- dplyr::summarize(y_count, .y_tracker = n())
        y_count <- dplyr::ungroup(y_count)
        y_count <- dplyr::filter(y_count, .y_tracker > 1)
        y_count <- dplyr::select(y_count, -.y_tracker)
        names(y_count) <- matched_vars_left
        x_extra <- suppressMessages(dplyr::semi_join(y_count, x))
        x_extra <- dplyr::mutate(x_extra, .x_tracker = T)
        
        if(dplyr::tally(x_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, x_extra, by = matched_vars_left)
            )
        } else {
            joined <- dplyr::mutate(joined, .x_tracker = NA)
        }
        
        if(dplyr::tally(y_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, y_extra, by = matched_vars_right)
            )
        } else {
            joined <- dplyr::mutate(joined, .y_tracker = NA)
        }
        
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(extra_quo) :=
                dplyr::case_when(
                    joined$.x_tracker & joined$.y_tracker ~ "extra_on_both",
                    joined$.x_tracker & is.na(joined$.y_tracker) ~ "extra_on_left",
                    is.na(joined$.x_tracker) & joined$.y_tracker ~ "extra_on_right",
                    TRUE ~ "not_extra"
                )
        )
        joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    }
    
    # Regroup data and return
    if (!is.null(x_group)) joined <- dplyr::group_by_at(joined, x_group)
    return(joined)
    
}

#' @rdname join_qc
#' @export
right_join_qc <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
                         .merge = NULL, .extra = NULL, ...){
    
    # Removing all group attributes from tables, but keeping group of x to 
    # re-apply after merge
    x_group <- attr(x, "vars")
    x <- dplyr::ungroup(x)
    y <- dplyr::ungroup(y)
    
    # Checking to make sure variable names used in function are not already in
    # use, and supply warning if so.
    if (".x_tracker" %in% names(x)) {
        message("Warning: variable .x_tracker in left table was dropped")
    }
    if (".y_tracker" %in% names(y)) {
        message("Warning: variable .y_tracker in right table was dropped")
    }
    
    if (!is.null(.merge)) {
        if (.merge %in% names(x) | .merge %in% names(y)) {
            stop(
                paste0(
                    "Column '", 
                    .merge, 
                    "' already exists; change .merge name before proceeding"
                )
            )
        }
    }
    
    # Adding simple merge tracker variables to tables
    x <- dplyr::mutate(x, .x_tracker = T)
    y <- dplyr::mutate(y, .y_tracker = T)
    
    # Extracting matched variables from left and from right. First is if no
    # by is specified, then if by is a character vector of matched variables,
    # then if by is a named vector.
    if (is.null(by)) {
        matched_vars_left <- names(x)[names(x) %in% names(y)]
        matched_vars_right <- matched_vars_left
    } else if (is.null(names(by))) {
        matched_vars_left <- by
        matched_vars_right <- by
    } else {
        matched_vars_left <- ifelse(grepl(".", names(by)), names(by), unname(by))
        matched_vars_right <- unname(by)
    }
    
    # Creating "reverse" named vector of by (where left becomes right) in cases 
    # where by matches on non-equivalent names
    if (all.equal(matched_vars_left, matched_vars_right) != T) {
        by_reverse <- matched_vars_left
        names(by_reverse) <- matched_vars_right
    } else {
        by_reverse <- by
    }
    
    # Identifying type of merge that occured
    left_type <- max(duplicated(x[, matched_vars_left]))
    right_type <- max(duplicated(y[, matched_vars_right]))
    
    if (left_type == 0) left_type <- "ONE" else left_type <- "MANY"
    if (right_type == 0) right_type <- "ONE" else right_type <- "MANY"
    
    # Doing right join
    joined <- dplyr::right_join(x, y, by = by, copy = copy, suffix = suffix, ...)
    
    # Calculating distribution of matches in newly joined data
    matched <- dplyr::tally(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    unmatched_y <- dplyr::tally(joined, is.na(.x_tracker) & !is.na(.y_tracker))
    total <- dplyr::tally(joined)
    
    # Calculating percent of rows from each data set that matched
    matched_ids <- dplyr::filter(joined, !is.na(.x_tracker) & !is.na(.y_tracker))
    matched_ids <- dplyr::select_at(matched_ids, matched_vars_left)
    matched_ids_x <- dplyr::distinct(matched_ids)
    matched_ids_y <- matched_ids_x
    names(matched_ids_y) <- matched_vars_right
    
    matched_percent_x <- suppressMessages(dplyr::inner_join(x, matched_ids_x))
    matched_percent_x <- dplyr::tally(matched_percent_x) / dplyr::tally(x) 
    matched_percent_x <- 100 * round(matched_percent_x, 3)
    matched_percent_y <- suppressMessages(dplyr::inner_join(y, matched_ids_y))
    matched_percent_y <- dplyr::tally(matched_percent_y) / dplyr::tally(x) 
    matched_percent_y <- 100 * round(matched_percent_y, 3)
    
    # Counting extra rows created
    anti_n_y <- dplyr::tally(
        suppressMessages(
            dplyr::anti_join(y, x, by = by_reverse, suffix = suffix,  ...)
        )
    )
    extra_rows_y <- matched - (dplyr::tally(y) - anti_n_y)
    
    # Print merge diagnoses
    message(
        "\n",
        "This was a ", left_type, " TO ", right_type, " join", "\n",
        "\n",
        "MATCH DISTRIBUTION IN JOINED DATA\n",
        matched, " (", 100 * round(matched / total, 3), "%) Rows are matches", "\n",
        unmatched_y, " (", 100 * round(unmatched_y / total, 3), "%) Rows are from right only", "\n",
        "\n",
        "MATCH RATES BASED ON ORIGINAL DATA\n",
        matched_percent_x, "% Percent of rows from left matched" ,"\n",
        matched_percent_y, "% Percent of rows from right matched", "\n",
        "\n",
        "ADDITIONAL ROWS\n",
        extra_rows_y, " More rows with a matched ID than original right"
    )
    
    # Create .merge variable if specified. This is the only part of the code
    # that requires rlang. Could not figure out how to implement := otherwise
    if (!is.null(.merge)) {
        merge_quo <- rlang::quo_name(.merge)
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(merge_quo) :=
                dplyr::case_when(
                    !is.na(joined$.x_tracker) & is.na(joined$.y_tracker) ~ "left_only",
                    is.na(joined$.x_tracker) & !is.na(joined$.y_tracker) ~ "right_only",
                    TRUE ~ "matched"
                )
        )
    }
    
    # Dropping tracker variables
    joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    
    # Create .extra variable if specified.
    # In a full join, extra rows can happen when not a 1-1 merge
    if (!is.null(.extra)) {
        
        extra_quo <- rlang::quo_name(.extra)
        
        # Isolate combinations of ID varaibles in left table with more than one
        # row
        x_count <- dplyr::group_by_at(x, matched_vars_left)
        x_count <- dplyr::summarize(x_count, .x_tracker = n())
        x_count <- dplyr::ungroup(x_count)
        x_count <- dplyr::filter(x_count, .x_tracker > 1)
        x_count <- dplyr::select(x_count, -.x_tracker)
        names(x_count) <- matched_vars_right
        y_extra <- suppressMessages(dplyr::semi_join(x_count, y))
        y_extra <- dplyr::mutate(y_extra, .y_tracker = T)
        
        y_count <- dplyr::group_by_at(y, matched_vars_right)
        y_count <- dplyr::summarize(y_count, .y_tracker = n())
        y_count <- dplyr::ungroup(y_count)
        y_count <- dplyr::filter(y_count, .y_tracker > 1)
        y_count <- dplyr::select(y_count, -.y_tracker)
        names(y_count) <- matched_vars_left
        x_extra <- suppressMessages(dplyr::semi_join(y_count, x))
        x_extra <- dplyr::mutate(x_extra, .x_tracker = T)
        
        if(dplyr::tally(x_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, x_extra, by = matched_vars_left)
            )
        } else {
            joined <- dplyr::mutate(joined, .x_tracker = NA)
        }
        
        if(dplyr::tally(y_extra) > 0) {
            joined <- suppressMessages(
                dplyr::left_join(joined, y_extra, by = matched_vars_right)
            )
        } else {
            joined <- dplyr::mutate(joined, .y_tracker = NA)
        }
        
        joined <- dplyr::mutate(
            joined,
            rlang::UQ(extra_quo) :=
                dplyr::case_when(
                    joined$.x_tracker & joined$.y_tracker ~ "extra_on_both",
                    joined$.x_tracker & is.na(joined$.y_tracker) ~ "extra_on_left",
                    is.na(joined$.x_tracker) & joined$.y_tracker ~ "extra_on_right",
                    TRUE ~ "not_extra"
                )
        )
        joined <- dplyr::select(joined, -.x_tracker, -.y_tracker)
    }
    
    # Regroup data and return
    if (!is.null(x_group)) joined <- dplyr::group_by_at(joined, x_group)
    return(joined)
    
}

#' @rdname join_qc
#' @export
anti_join_qc <- function(x, y, by = NULL, copy = FALSE, ...){
    
    # Doing join
    anti_joined <- dplyr::anti_join(x, y, by = by, suffix = suffix,  ...)

    # Calculating merge diagnoses 
    unmatched_x <- dplyr::tally(x) - dplyr::tally(anti_joined)

    # Print merge diagnoses
    message(
        "\n",
        "Anti joins only keep matching cases, so no match diagnosis", "\n",
        "Anti joins never create extra rows, so now additional row diagnosis", "\n",
        "\n",
        "DROPPED ROWS", "\n",
        unmatched_x, " Rows were dropped from left"
    )
    
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
    message(
        "\n",
        "Semi joins only keep matching cases, so no match diagnosis", "\n",
        "Semi joins never create extra rows, so now additional row diagnosis", "\n",
        "\n",
        "DROPPED ROWS", "\n",
        unmatched_x, " Rows were dropped from left"
    )
    
    # Rreturning data frame
    return(joined)
    
}