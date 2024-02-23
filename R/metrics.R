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

#' Perform sliding window analysis on raster datasets {#sliding.windows}
#'
#' This function performs a sliding window analysis on a raster file, calculating user-specified metrics over specified window sizes. The results can optionally be saved as both TIF images and CSV tables.
#'
#' @param input.raster A string specifying the path to the input raster dataset(s) separated by commas.
#' @param metrics A character vector specifying the names of the metrics to calculate within each window.
#' @param sizes An integer vector specifying the size(s) of the moving windows to use. For example, c(101,201) would consider two sets of windows, those with diameters of 101 pixels and 201 pixels.
#' @param distance.type One of the following values : "FAST_GAUSSIAN"(Default), "FAST_SQUARE", "THRESHOLD" and "WEIGHTED". "WEIGHTED" can use the definition of distance.function parameter.
#' @param output.dir A directory name where processed rasters should be stored. Their name will be 'input_raster_filename'_{metric}_{size}.tif (or .asc if input is .asc)
#' @param output.csv A filename for storing the resulting table of calculated metrics. When left blank, no CSV file will be created.
#' @param output.raster A filename for the output raster file if there is only one metric and one window size.
#' @param dep An integer representing the displacement factor for subsequent iterations (default is 1).
#' @param interpolate A boolean indicating whether pixel values should be interpolated between displacement (default is FALSE).
#' @param distance.function When distance.type is defined to "WEIGHTED", a string specifiing the weighting function. If not specified, the gaussian function "exp(-pow(distance, 2)/pow(dmax/2, 2))" is used.
#' @param window.shape When distance.type is defined to "THRESHOLD" or "WEIGHTED", one of the following values : "CIRCLE", "SQUARE", "FUNCTIONAL". If "FUNCTIONAL" is used, parameter friction.file has to be defined.
#' @param friction.file A string specifying the path to the input raster defining friction when "window.shape" is defined to "FUNCTIONAL".
#' @param filters A numerical vector specifying the only values that are considered in the input raster. Use only one of the filters and unfilters parameters.
#' @param unfilters A numerical vector specifying the values that are not considered in the input raster.
#' @param max.missing.values.rate An integer indicating the maximum allowable rate of missing values (in percent) across all rasters when aggregating summaries (default is 100%).
#' @param properties.file A filename for storing metadata about the performed operation. By default, a temporary file is generated but discarded upon completion.
#' @examples
#' \dontrun{
#' sliding.windows(input.raster =system.file("data/sample.tif",
#'                                            package = "chloe"),
#'                 metrics = c("SHDI","HET"),
#'                 sizes = c(51,101),
#'                 output.csv = "sample_metrics.csv",
#'                 dep = 5,
#'                 interpolate = TRUE)
#' }
#' @export

sliding.windows <- function( input.raster, metrics, sizes, distance.type = "FAST_GAUSSIAN", output.dir = NULL, output.csv = NULL, output.raster = NULL, dep = 1, interpolate = FALSE, distance.function = NULL, window.shape = NULL, friction.file = NULL, filters = NULL, unfilters = NULL,max.missing.values.rate = 100, properties.file = NULL){
  # Create the properties file content
  props <- "treatment=sliding\n"
  props <- paste0(props, "input_raster=", input.raster, "\n")
  props <- paste0(props, "metrics=", "{", paste(metrics,collapse=";"), "}", "\n")
  props <- paste0(props, "sizes=", "{", paste(sizes,collapse=";"), "}", "\n")
  if(!is.null(output.dir)) props <- paste0(props, "output_dir=", output.dir, "\n")
  if(!is.null(output.csv)) props <- paste0(props, "output_csv=", output.csv, "\n")
  if(!is.null(output.raster)) props <- paste0(props, "output_raster=", output.csv, "\n")
  props <- paste0(props, "distance_type=", distance.type, "\n")
  props <- paste0(props, "maximum_nodata_value_rate=", max.missing.values.rate, "\n")
  props <- paste0(props, "displacement=", dep, "\n")
  props <- paste0(props, "interpolation=", if(interpolate) "true" else "false", "\n")
  if(!is.null(distance.function)) props <- paste0(props, "distance_function=", distance.function, "\n")
  if(!is.null(window.shape)) props <- paste0(props, "window_shape=", window.shape, "\n")
  if(!is.null(friction.file)) props <- paste0(props, "friction_file=", friction.file, "\n")
  if(!is.null(filters)) props <- paste0(props, "filters=", "{", paste(filters,collapse=";"), "}", "\n")
  if(!is.null(unfilters)) props <- paste0(props, "unfilters=", "{", paste(unfilters,collapse=";"), "}", "\n")

  run(props, properties.file)
}
