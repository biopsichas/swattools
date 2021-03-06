% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_averaging.R
\name{get_costs_per_reduction_1unit}
\alias{get_costs_per_reduction_1unit}
\title{Function to get effectiveness of measures over scenarios in euros per 1kg or 1cms of parameters for implemented measures}
\usage{
get_costs_per_reduction_1unit(df, cost_list)
}
\arguments{
\item{df}{Data.frame of imported output.*** SWAT file processed with
get_diff_from_baseline function by option of 'a' (absolute difference).}

\item{cost_list}{A list of measure names and costs. Example, cost_list <- list(measure_30 = 248,
measure_33 = 4, measure_34 = 2, measure_37 = 1, measure_39 = 22, measure_40 = 58)}
}
\value{
Data.frame of results showing cost-effectiveness of implementing measure in euros
per one unit (1 kg or 1 cms) of reduction (negative values) or increase (positive values).
}
\description{
This function allows comparing scenarios taking into account areas and costs of implemented measures.
Output of function is how much  it cost to decrease (negative values) or increase (positive values)
each in parameters by one unit (either 1 kg or 1 cubic meter of water) in scenario for reach or
setup or whole country.
}
\examples{
## result <- get_costs_per_reduction_1unit(df, cost_list)
}
