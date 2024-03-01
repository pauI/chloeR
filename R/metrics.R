#' List available metrics for chloe library
#'
#' This function returns
#' a data frame with the requested subset of metrics based on the 'type' and
#' 'process' arguments. If no filters are provided, all metrics will be returned.
#'
#' @param type A character vector specifying the type(s) of metric(s) to include
#'            in the output (defaults to NULL, which means no filtering by type).
#'            Available types are : "value", "couple", "quantitative", "patch",
#'            "slope", "continuity", "erosion", "degaterosion"
#' @param process A character vector specifying the process(es) associated with
#'               the desired metrics (defaults to NULL, which means no filtering
#'               by process). The metrics associated with "value" and "couple"
#'               processes have to be fullfiled with specific values eg.
#'               "pNV" value metric has to be associated with a value, eg. pNV_1,
#'               "pNC" couple metric has to be associated with two values, eg. pNC_1-2
#'
#' @return A data frame containing the requested metrics.
#' @examples
#' \dontrun{
#' list.metrics() # Returns all metrics from the 'metrics.csv' file
#' list.metrics(type = "value") # Returns only value metrics
#' list.metrics(process = "couple") # Returns only metrics needing
#'                                  # the specification of a couple of values
#' }
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

#' Generate Value Metrics {#generate.value.metrics}
#'
#' Generates a single string containing all combinations of metric names and their corresponding values
#' @param metrics A vector of strings representing metric names
#' @param values A vector of numbers representing the values associated with the given metrics
#' @return A single string consisting of all combinations of metric names and their corresponding values
#'          separated by semicolon ";"
#' @examples
#' \dontrun{
#'   metrics <- c("pNV","pNV")
#'   values <- c(1,2,3,4,5,6,7,8,9,10)
#'   res <- generate.value.metrics(metrics, values)
#' }
#' @export

generate.value.metrics <- function(metrics,values){
  value.metrics = ""
  for(m in metrics)
    for(v in values)
      value.metrics = paste0(value.metrics,";",m,"_",v)
  return(value.metrics)
}

#' Generate Couple Metrics {#generate.couple.metrics}
#'
#' Generates a single string containing all unique combinations of two different values of a given list of metrics
#'
#' @param metrics A vector of strings representing metric names
#' @param values A vector of numbers representing the values associated with the given metrics
#' @return A single string consisting of all unique combinations of two different values of the given metrics
#'         separated by semicolon ";"
#' @examples
#' \dontrun{
#'   metrics <- c("pNC")
#'   values <- c(1,2,3,4,5,6,7,8,9,10)
#'   res <- generate.couple.metrics(metrics, values)
#' }
#' @export

generate.couple.metrics <- function(metrics,values){
  couple.metrics = ""
  for(m in metrics)
    for(v1 in values)
      for(v2 in values)
        if(v2>v1)
          couple.metrics = paste0(couple.metrics,";",m,"_",v1,"-",v2)
  return(couple.metrics)
}


