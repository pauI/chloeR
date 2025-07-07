#####
#' List available metrics for the chloe library
#'
#' Returns a data frame containing the available landscape metrics, optionally filtered
#' by type or associated process.
#'
#' @param type vector of strings (optional). Types of metrics to include in the output.
#'        Available types include: "value", "couple", "quantitative", "patch",
#'        "slope", "continuity", "erosion", "degaterosion".
#' @param process vector of strings (optional). Process modes associated with the desired metrics.
#'.       Available modes include: "value", "couple" and "other"
#'        Metrics of type "value" and "couple" must be completed with specific values,
#'        e.g., "pNV_1" for value metrics or "pNC_1-2" for couple metrics.
#'
#' @return A data frame containing the requested metrics.
#'
#' @examples
#' \dontrun{
#' list.metrics() # Returns all available metrics
#' list.metrics(type = "value") # Filters by type
#' list.metrics(process = "couple") # Filters by process
#' }
#'
#' @export
list.metrics <- function(type = NULL, process = NULL) {
  print(system.file("data/metrics.csv", package = "chloe"))
  metrics <- read.csv(system.file("data/metrics.csv", package = "chloe"), header = TRUE, sep = ";")

  if (!is.null(type)) {
    metrics <- metrics[metrics$type == type, ]
  }

  if (!is.null(process)) {
    metrics <- metrics[metrics$process == process, ]
  }

  return(metrics[1:3])
}

#####
#' Generate Value Metrics
#'
#' Generates a single string containing all combinations of metric names
#' and their associated values, separated by semicolons.
#'
#' @param metrics vector of strings. Names of the metrics to combine with values.
#' @param values numeric vector. Values to associate with the given metrics.
#'
#' @return A single string with all metric-value combinations separated by semicolons.
#'
#' @examples
#' \dontrun{
#' metrics <- c("NV", "pNV")
#' values <- c(1, 2, 3, 4, 5, 6)
#' res <- generate.value.metrics(metrics, values)
#' }
#'
#' @export
generate.value.metrics <- function(metrics,values){
  value.metrics = ""
  for(m in metrics)
    for(v in values)
      value.metrics = paste0(value.metrics,";",m,"_",v)
  return(substring(value.metrics,2))
}

#####
#' Generate Couple Metrics
#'
#' Generates a single string containing all unique combinations of two different
#' values for each given metric, separated by semicolons.
#'
#' @param metrics vector of strings. Names of the metrics to combine with value pairs.
#' @param values numeric vector. Values to generate unique pairs from.
#'
#' @return A single string with all metric-couple combinations separated by semicolons.
#'
#' @examples
#' \dontrun{
#' metrics <- c("pNC")
#' values <- c(1, 2, 3, 7, 8)
#' res <- generate.couple.metrics(metrics, values)
#' }
#'
#' @export
generate.couple.metrics <- function(metrics,values){
  couple.metrics = ""
  for(m in metrics)
    for(v1 in values)
      for(v2 in values)
        if(v2>v1)
          couple.metrics = paste0(couple.metrics,";",m,"_",v1,"-",v2)
  return(substring(couple.metrics,2))
}


