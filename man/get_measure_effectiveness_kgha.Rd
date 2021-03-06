% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_averaging.R
\name{get_measure_effectiveness_kgha}
\alias{get_measure_effectiveness_kgha}
\title{Function to get effectiveness of measures over scenarios in kg per ha of implemented measures}
\usage{
get_measure_effectiveness_kgha(df)
}
\arguments{
\item{df}{Data.frame of imported output.*** SWAT file processed with
get_diff_from_baseline function by option of 'a' (absolute difference).}
}
\value{
Data.frame of results showing effectiveness of implementing 1 ha of measure.
}
\description{
This function allows comparing scenarios taking into account areas of implemented measures.
Output of function is how much each parameters is changed my implementation of 1 ha of measure
in scenario for reach or setup or whole country.
}
\examples{
## result <- get_measure_effectiveness_kgha(df)
}
