% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_scoped_qc.R
\name{summarize_all_qc}
\alias{summarize_all_qc}
\alias{summarise_all_qc}
\alias{summarize_at_qc}
\alias{summarise_at_qc}
\alias{summarize_if_qc}
\alias{summarise_if_qc}
\title{Scoped versions of summarize_qc}
\usage{
summarize_all_qc(.tbl, .funs, ..., .group_check = F)

summarise_all_qc(.tbl, .funs, ..., .group_check = F)

summarize_at_qc(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F)

summarise_at_qc(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F)

summarize_if_qc(.tbl, .predicate, .funs, ..., .group_check = F)

summarise_if_qc(.tbl, .predicate, .funs, ..., .group_check = F)
}
\arguments{
\item{.tbl}{A \code{tbl} object.}

\item{.funs}{A function \code{fun}, a quosure style lambda \code{~ fun(.)} or a list of either form.}

\item{...}{Additional arguments for the function calls in
\code{.funs}. These are evaluated only once, with \link[rlang:dyn-dots]{tidy dots} support.}

\item{.group_check}{a logical value, that when TRUE, will print a table with
each group variable and a column called "missing_vars" that lists which 
variables are missing from the summarized data for each group. Only groups 
with at least one missing variable are listed. This has no effect on the 
returned object, and only prints information. Default is FALSE, to avoid
excess printing. If data is not grouped and .group_check = T, then an error 
is thrown.}

\item{.vars}{A list of columns generated by \code{\link[dplyr:vars]{vars()}},
a character vector of column names, a numeric vector of column
positions, or \code{NULL}.}

\item{.cols}{This argument has been renamed to \code{.vars} to fit
dplyr's terminology and is deprecated.}

\item{.predicate}{A predicate function to be applied to the columns
or a logical vector. The variables for which \code{.predicate} is or
returns \code{TRUE} are selected. This argument is passed to
\code{\link[rlang:as_function]{rlang::as_function()}} and thus supports quosure-style lambda
functions and strings representing function names.}
}
\value{
An object of the same class as \code{.data}. This object will be
  identical to that which is returned when running the scoped variants of
  \code{dplyr::summarize}.
}
\description{
\code{summarize_all_qc}, \code{summarize_at_qc}, and \code{summarize_if_qc}
are used exactly the same as \code{dplyr::summarize_all},
\code{dplyr::summarize_at}, and \code{dplyr::summarize_if}, and require 
all of the same arguments and return identical objects. The only difference
is that the \code{_qc} versions print a message indicating the number of NA
or INFinite values created in the new summary variable(s). This is most
useful when using on a grouped data frame.
}
\section{Grouping}{

  All functions work with grouped data.
}

\section{summarize vs. summarise}{

  There are \code{_qc} versions of \code{summarize} and \code{summarise}.
  But this is America, use a z!
}

\examples{
practice_data <- 
  data.frame(
  A = c(1:4, NA), 
  B = c(NA, 7:10), 
  C = 21:25,
  G = c(1, 1, 1, 2, 2)
)

# Use the _qc versions just like normal dplyr scoped summarize functions.
summarize_at_qc(
  practice_data, 
  vars(A, C), 
  funs(m = mean(., na.rm = T), s = sum)
)

summarize_all_qc(practice_data, funs(mean))

# Pipes work, just as they always do in dplyr
practice_data \%>\% summarize_if_qc(is.integer, mean)

# Functions work on grouped data, too
grouped_data <- group_by(practice_data, G)
grouped_data \%>\% 
  summarize_at_qc(vars(A, C), funs(m = mean(., na.rm = T), s = sum))

# Setting .group_check = T will print, for each group with a missing value,
# which new variables are missing. 
summarize_all_qc(grouped_data, mean, .group_check = T)

}
\seealso{
\code{\link[dplyr]{summarise_all}}
}
