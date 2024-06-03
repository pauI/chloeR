#####
# SLIDING
#' Perform sliding window analysis on raster datasets {#sliding.window}
#'
#' This function performs a sliding window analysis on a raster file, calculating user-specified metrics over specified window sizes. The results can optionally be saved as both TIF images and CSV tables.
#'
#' @param input_raster A string specifying the path to the input raster dataset(s) separated by commas.
#' @param metrics A character vector specifying the names of the metrics to calculate within each window.
#' @param sizes An integer vector specifying the size(s) of the moving windows to use. For example, c(101,201) would consider two sets of windows, those with diameters of 101 pixels and 201 pixels.
#' @param distance_type One of the following values : "THRESHOLD"(Default), "WEIGHTED", "FAST_GAUSSIAN" and "FAST_SQUARE". "WEIGHTED" can use the definition of distance.function parameter.
#' @param distance_function When distance.type is defined to "WEIGHTED", a string specifing the weighting function. If not specified, the gaussian function "exp(-pow(distance, 2)/pow(dmax/2, 2))" is used.
#' @param friction_raster A string specifying the path to the input raster defining friction when "window_shape" is defined to "FUNCTIONAL".
#' @param shape When distance.type is defined to "THRESHOLD" or "WEIGHTED", one of the following values : "CIRCLE"(default), "SQUARE", "FUNCTIONAL". If "FUNCTIONAL" is used, parameter friction_raster has to be defined.
#' @param displacement An integer representing the displacement factor for subsequent iterations (default is 1).
#' @param interpolation A boolean indicating whether pixel values should be interpolated between displacement (default is FALSE).
#' @param filters A numerical vector specifying the only values that are considered in the input raster. Use only one of the filters and unfilters parameters.
#' @param unfilters A numerical vector specifying the values that are not considered in the input raster.
#' @param maximum_rate_nodata_value An integer indicating the maximum allowable rate of missing values (in percent) across all rasters when aggregating summaries (default is 100%).
#' @param output_raster A filename for the output raster file if there is only one metric and one window size.
#' @param output_csv A filename for storing the resulting table of calculated metrics. When left blank, no CSV file will be created.
#' @param output_folder A directory name where processed rasters should be stored. Their name will be 'input_raster_filename'_{metric}_{size}.tif (or .asc if input is .asc)
#' @param properties_file A filename for storing metadata about the performed operation. By default, a temporary file is generated but discarded upon completion.
#' @examples
#' sliding.window(input_raster =system.file("data/sample.tif",
#'                                            package = "chloe"),
#'                 metrics = c("SHDI","HET"),
#'                 sizes = c(51,101),
#'                 output_csv = "sample_metrics.csv",
#'                 displacement = 5,
#'                 interpolation = TRUE)
#' @export
sliding.window <- function(
    input_raster,
    metrics,
    sizes,
    distance_type = "THRESHOLD",
    distance_function = NULL,
    friction_raster = NULL,
    shape = "CIRCLE",
    displacement = 1,
    interpolation = FALSE,
    filters = NULL,
    unfilters = NULL,
    maximum_rate_nodata_value = 100,
    output_raster = NULL,
    output_csv = NULL,
    output_folder = NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=sliding\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "metrics=", "{", paste(metrics,collapse=";"), "}", "\n")
  props <- paste0(props, "sizes=", "{", paste(sizes,collapse=";"), "}", "\n")
  if(distance_type=="FAST_GAUSSIAN" | distance_type=="FAST_SQUARE"){
    props <- paste0(props, "distance_type=", distance_type, "\n")
  }else{
    if(!is.null(distance_function)){
      props <- paste0(props, "distance_type=WEIGHTED\n")
      props <- paste0(props, "distance_function=", distance_function, "\n")
    }else{
      props <- paste0(props, "distance_type=", distance_type, "\n")
    }
    if(!is.null(friction_raster)){
      props <- paste0(props, "shape=FUNCTIONAL\n")
      props <- paste0(props, "friction_raster=", friction_raster, "\n")
    }else if(shape=="FUNCTIONAL"){
      props <- paste0(props, "shape=CIRCLE\n")
    }else{
      props <- paste0(props, "shape=", shape, "\n")
    }
  }
  if(displacement != 1) props <- paste0(props, "displacement=", displacement, "\n")
  if(displacement != 1) props <- paste0(props, "interpolation=", if(interpolation) "true" else "false", "\n")
  if(!is.null(filters)) props <- paste0(props, "filters=", "{", paste(filters,collapse=";"), "}", "\n")
  if(!is.null(unfilters)) props <- paste0(props, "unfilters=", "{", paste(unfilters,collapse=";"), "}", "\n")
  if(maximum_rate_nodata_value != 100) props <- paste0(props, "maximum_rate_nodata_value=", maximum_rate_nodata_value, "\n")
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")
  if(!is.null(output_folder)) props <- paste0(props, "output_folder=", output_folder, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
# SELECTED
#' Perform selected window analysis on raster datasets {#selected.window} using a points file ('ID';'X';'Y') as header
#'
#' This function performs a selected window analysis on a raster file, calculating user-specified metrics over specified window sizes. The results can be saved as CSV tables.
#'
#' @param input_raster A string specifying the path to the input raster dataset(s) separated by commas.
#' @param metrics A character vector specifying the names of the metrics to calculate within each window.
#' @param shape When distance.type is defined to "THRESHOLD" or "WEIGHTED", one of the following values : "CIRCLE"(default), "SQUARE", "FUNCTIONAL". If "FUNCTIONAL" is used, parameter friction_raster has to be defined.
#' @param sizes An integer vector specifying the size(s) of the moving windows to use. For example, c(101,201) would consider two sets of windows, those with diameters of 101 pixels and 201 pixels.
#' @param points A string specifying the path to the points dataset(s) separated by commas and containing fields "ID"(String identifiyng the point), "X"(abscissa) adn "Y"(ordinate)
#' @param distance_type One of the following values : "THRESHOLD"(Default), "WEIGHTED", "FAST_GAUSSIAN" and "FAST_SQUARE". "WEIGHTED" can use the definition of distance.function parameter.
#' @param distance_function When distance.type is defined to "WEIGHTED", a string specifing the weighting function. If not specified, the gaussian function "exp(-pow(distance, 2)/pow(dmax/2, 2))" is used.
#' @param friction_raster A string specifying the path to the input raster defining friction when "window_shape" is defined to "FUNCTIONAL".
#' @param output_raster A filename for the output raster file if there is only one metric and one window size.
#' @param output_csv A filename for storing the resulting table of calculated metrics. When left blank, no CSV file will be created.
#' @param output_folder A directory name where processed rasters should be stored. Their name will be 'input_raster_filename'_{metric}_{size}.tif (or .asc if input is .asc)
#' @param windows_path A directory name where stickers rasters would be stored.
#' @param properties_file A filename for storing metadata about the performed operation. By default, a temporary file is generated but discarded upon completion.
#' @export
selected.window <- function(
    input_raster,
    metrics,
    sizes,
    points,
    shape = "CIRCLE",
    distance_type = "THRESHOLD",
    distance_function = NULL,
    friction_raster = NULL,
    output_raster = NULL,
    output_csv = NULL,
    output_folder = NULL,
    windows_path = NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=selected\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "metrics=", "{", paste(metrics,collapse=";"), "}", "\n")
  props <- paste0(props, "sizes=", "{", paste(sizes,collapse=";"), "}", "\n")
  props <- paste0(props, "points=", points, "\n")
  if(!is.null(distance_function)){
    props <- paste0(props, "distance_type=WEIGHTED\n")
    props <- paste0(props, "distance_function=", distance_function, "\n")
  }else{
    props <- paste0(props, "distance_type=", distance_type, "\n")
  }
  if(!is.null(friction_raster)){
    props <- paste0(props, "shape=FUNCTIONAL\n")
    props <- paste0(props, "friction_raster=", friction_raster, "\n")
  }else if(shape=="FUNCTIONAL"){
    props <- paste0(props, "shape=CIRCLE\n")
  }else{
    props <- paste0(props, "shape=", shape, "\n")
  }
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")
  if(!is.null(output_folder)) props <- paste0(props, "output_folder=", output_folder, "\n")
  if(!is.null(windows_path)) props <- paste0(props, "windows_path=", windows_path, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
# GRID
#' Perform grid window analysis on raster datasets {#grid.window}
#'
#' This function performs a grid window analysis on a raster file, calculating user-specified metrics over specified window sizes. The results can optionally be saved as both TIF images and CSV tables.
#'
#' @param input_raster A string specifying the path to the input raster dataset(s) separated by commas.
#' @param metrics A character vector specifying the names of the metrics to calculate within each window.
#' @param sizes An integer vector specifying the size(s) of the moving windows to use. For example, c(101,201) would consider two sets of windows, those with diameters of 101 pixels and 201 pixels.
#' @param maximum_rate_nodata_value An integer indicating the maximum allowable rate of missing values (in percent) across all rasters when aggregating summaries (default is 100%).
#' @param output_raster A filename for the output raster file if there is only one metric and one window size.
#' @param output_csv A filename for storing the resulting table of calculated metrics. When left blank, no CSV file will be created.
#' @param output_folder A directory name where processed rasters should be stored. Their name will be 'input_raster_filename'_{metric}_{size}.tif (or .asc if input is .asc)
#' @param properties_file A filename for storing metadata about the performed operation. By default, a temporary file is generated but discarded upon completion.
#' @export
grid.window <- function(
    input_raster,
    metrics,
    sizes,
    maximum_rate_nodata_value = 100,
    output_raster = NULL,
    output_csv = NULL,
    output_folder = NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=grid\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "metrics=", "{", paste(metrics,collapse=";"), "}", "\n")
  props <- paste0(props, "sizes=", "{", paste(sizes,collapse=";"), "}", "\n")
  if(maximum_rate_nodata_value != 100) props <- paste0(props, "maximum_rate_nodata_value=", maximum_rate_nodata_value, "\n")
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")
  if(!is.null(output_folder)) props <- paste0(props, "output_folder=", output_folder, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}
#####
# MAP
#' Perform map window analysis on raster datasets {#map.window}
#'
#' This function performs a sliding window analysis on a raster file, calculating user-specified metrics over specified window sizes. The results can optionally be saved as both TIF images and CSV tables.
#'
#' @param input_raster A string specifying the path to the input raster dataset(s) separated by commas.
#' @param metrics A character vector specifying the names of the metrics to calculate within each window.
#' @param output_csv A filename for storing the resulting table of calculated metrics. When left blank, no CSV file will be created.
#' @param properties_file A filename for storing metadata about the performed operation. By default, a temporary file is generated but discarded upon completion.
#' @export
map.window <- function(
    input_raster,
    metrics,
    output_csv = NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=map\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "metrics=", "{", paste(metrics,collapse=";"), "}", "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}
