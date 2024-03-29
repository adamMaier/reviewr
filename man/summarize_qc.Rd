% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_qc.R
\name{summarize_qc}
\alias{summarize_qc}
\alias{summarise_qc}
\title{Report number of NAs created when performing dplyr summarize}
\usage{
summarize_qc(.data = NULL, ..., .group_check = F)

summarise_qc(.data = NULL, ..., .group_check = F)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a
lazy data frame (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Name-value pairs of
summary functions. The name will be the name of the variable in the result.

The value can be:
\itemize{
\item A vector of length 1, e.g. \code{min(x)}, \code{n()}, or \code{sum(is.na(y))}.
\item A data frame, to add multiple columns from a single expression.
}

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Returning values with size 0 or >1 was
deprecated as of 1.1.0. Please use \code{\link[dplyr:reframe]{reframe()}} for this instead.}

\item{.group_check}{a logical value, that when TRUE, will print a table with
each group variable and a column called "missing_vars" that lists which 
variables are missing from the summarized data for each group. Only groups 
with at least one missing variable are listed. This has no effect on the 
returned object, and only prints information. Default is FALSE, to avoid
excess printing. If data is not grouped and .group_check = T, then an error 
is thrown.}
}
\value{
An object of the same class as \code{.data}. This object will be
  identical to that which is returned when running \code{dplyr::summarise}.
}
\description{
\code{summarize_qc} is used exactly the same as \code{dplyr::summarize} and 
requires all of the same arguments and returns an identical object. The only 
difference is that \code{summarize_qc} prints a message indicating the number
of NA or INFinite values created in the new summary variable(s). This is most
useful when using on a grouped data frame.
}
\section{Scoped variants}{

  There are \code{_qc} versions of the scoped summarize functions. See 
  \code{\link{summarize_at_qc}}, \code{\link{summarize_all_qc}}, or
  \code{\link{summarize_if_qc}}.
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
  G = c("X", "X", "X", "Y", "Y"),
  stringsAsFactors = F
)

summarize_qc(practice_data, new_var_1 = mean(C), sum(A))
summarize_qc(practice_data, new_var_1 = mean(C), sum(A, na.rm = T))

# Pipes work
practice_data \%>\% 
  summarize_qc(practice_data, new_var_1 = mean(C), sum(A, na.rm = T))

# Functions worked on grouped data, too
grouped_data <- dplyr::group_by(practice_data, G)
summarize_qc(grouped_data, new_var_1 = mean(A), mean_b = mean(B), sum(C))

# Setting .group_check = T will print, for each group with a missing value,
which new variables are missing. 
summarize_qc(
  grouped_data, 
  .group_check = T,
  new_var_1 = mean(A),
  mean_b = mean(B),
  sum(C)
)

}
\seealso{
\code{\link[dplyr]{summarise}}
}
