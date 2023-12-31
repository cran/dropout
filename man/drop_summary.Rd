% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop_summary.R
\name{drop_summary}
\alias{drop_summary}
\title{Summarizing Dropouts in Surveys}
\usage{
drop_summary(data, last_col = NULL, section_min = 3)
}
\arguments{
\item{data}{A dataframe or tibble containing the survey data.}

\item{last_col}{The index position or column name of the last survey item. This is optional and is used when there are additional columns in the data frame that are not part of the survey questions you are interested in.}

\item{section_min}{Indicates occurrences of missing values that span at least n consecutive columns (n defaults to 3)}
}
\value{
A dataframe or tibble containing summary statistics about dropouts. Typical columns might include:
\itemize{
\item \code{question_name}: The name of the survey question or column.
\item \code{dropout_count}: The number of dropouts at this question.
\item \code{dropout_percentage}: The percentage of participants who dropped out at this question.
}
}
\description{
\code{drop_summary} function provides a high-level summary of dropout occurrences in the survey data.
It generates key statistics to understand the patterns of participant dropouts across different survey questions.
}
\examples{
# Basic usage
drop_summary(flying, "location_census_region")

# Summarizing dropouts up to a specific column
drop_summary(flying, last_col = "age")

# Read more in the vignette for interpreting summary statistics and plotting dropout trends.

}
\seealso{
See vignette for detailed workflows, tips on interpretation, and practical examples.
}
