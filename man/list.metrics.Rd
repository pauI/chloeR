% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{list.metrics}
\alias{list.metrics}
\title{List available metrics for the chloe library}
\usage{
list.metrics(type = NULL, process = NULL)
}
\arguments{
\item{type}{vector of strings (optional). Types of metrics to include in the output.
Available types include: "value", "couple", "quantitative", "patch",
"slope", "continuity", "erosion", "degaterosion".}

\item{process}{vector of strings (optional). Process modes associated with the desired metrics.
.       Available modes include: "value", "couple" and "other"
       Metrics of type "value" and "couple" must be completed with specific values,
       e.g., "pNV_1" for value metrics or "pNC_1-2" for couple metrics.}
}
\value{
A data frame containing the requested metrics.
}
\description{
Returns a data frame containing the available landscape metrics, optionally filtered
by type or associated process.
}
\examples{
\dontrun{
list.metrics() # Returns all available metrics
list.metrics(type = "value") # Filters by type
list.metrics(process = "couple") # Filters by process
}

}
