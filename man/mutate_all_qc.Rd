% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate_scoped_qc.R
\name{mutate_all_qc}
\alias{mutate_all_qc}
\alias{transmute_all_qc}
\alias{mutate_at_qc}
\alias{transmute_at_qc}
\alias{mutate_if_qc}
\alias{transmute_if_qc}
\title{Scoped versions of mutate_qc}
\usage{
mutate_all_qc(.tbl, .funs, ..., .group_check = F)

transmute_all_qc(.tbl, .funs, ..., .group_check = F)

mutate_at_qc(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F)

transmute_at_qc(.tbl, .vars, .funs, ..., .cols = NULL, .group_check = F)

mutate_if_qc(.tbl, .predicate, .funs, ..., .group_check = F)

transmute_if_qc(.tbl, .predicate, .funs, ..., .group_check = F)
}
\arguments{
\item{.tbl}{A \code{tbl} object.}

\item{.funs}{A function \code{fun}, a quosure style lambda \code{~ fun(.)} or a list of either form.}

\item{...}{Additional arguments for the function calls in
\code{.funs}. These are evaluated only once, with \link[rlang:dyn-dots]{tidy dots} support.}

\item{.group_check}{a logical value, that when TRUE, will print a table with
each group variable, and columns called "var_name" and  "n_missing" that 
together indicate, for each group, how many values are missing of newly
created variables. Only variables that contain at least 1 missing value are 
reported. This has no effect on the returned object, and only prints 
information. Default is FALSE, to avoid excess printing. If data is not 
grouped and .group_check = T, then an error is thrown.}

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
  identical to that which is returned when running \code{mutate_all_qc},
  \code{mutate_at_qc}, \code{mutate_if_qc}, and their \code{transmute} 
  equivalents.
}
\description{
\code{mutate_all_qc}, \code{mutate_at_qc}, \code{mutate_if_qc}, and their 
\code{transmute} equivalents return identical objects as the scoped versions
of \code{dplyr::mutate} and \code{dplyr::transmute}.The only difference is
that the \code{_qc} versions print a message indicating the number of NA or
INFinite values created in the new or edited variable(s) after calling 
\code{mutate}.
}
\section{Grouping}{

  All functions work with grouped data.
}

\examples{
practice_data <- 
  data.frame(
  A = c(1:4, NA), 
  B = c(NA, 7:10), 
  C = 21:25,
  G = c("X", "X", "X", "Y", "Y")
)

# Use the _qc versions just like normal dplyr scoped mutate functions.
mutate_at_qc(
  practice_data, 
  vars(A, C), 
  funs(m = mean(., na.rm = T), s = sum)
)

mutate_all_qc(practice_data, funs(as.character))

# Pipes work, just as they always do in dplyr
practice_data \%>\% mutate_if_qc(is.integer, mean)

# Functions work on grouped data, too
grouped_data <- group_by(practice_data, G)
grouped_data \%>\% 
  mutate_at_qc(vars(A, C), funs(m = mean(., na.rm = T), s = sum))

# Setting .group_check = T will also print a table indicating which groups
# have a missing value, on what variable, and how many values are missing.
mutate_at_qc(grouped_data, vars(A, B), funs(mean), .group_check = T)

}
\seealso{
\code{\link[dplyr]{summarize_all}}
}
