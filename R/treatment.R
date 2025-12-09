#####
# SLIDING
#' Perform sliding window analysis on raster datasets
#'
#' Applies a sliding window analysis to one or more raster datasets, calculating specified landscape metrics
#' over defined window sizes and exporting results to raster files or CSV tables.
#'
#' @param input_raster String. Path to the input raster dataset(s), with multiple paths separated by commas.
#' @param input_tile_raster String (optional). Path to a folder containing tiled raster datasets.
#' @param metrics Vector of strings. Names of the landscape metrics to calculate within each window.
#' @param sizes Integer vector. Size(s) of the moving windows in pixels (e.g., \code{c(101, 201)} for diameters of 101 and 201 pixels).
#' @param distance_type String. Distance weighting method. One of: "THRESHOLD" (default), "WEIGHTED", "FAST_GAUSSIAN", "FAST_SQUARE".
#' @param distance_function String (optional). Defines the weighting function (used only if \code{distance_type = "WEIGHTED"}). Defaults to Gaussian weighting: \code{exp(-distance^2 / (dmax/2)^2)}.
#' @param friction_raster String (optional). Path to a raster defining friction values, required if \code{shape = "FUNCTIONAL"}.
#' @param shape String. Window shape: "CIRCLE" (default), "SQUARE", or "FUNCTIONAL". If "FUNCTIONAL" is used, \code{friction_raster} must be specified.
#' @param displacement Integer. Displacement step in pixels between windows (default: 1).
#' @param interpolation Logical. If TRUE, interpolates pixel values between displacement steps (default: FALSE).
#' @param values Numeric vector (optional). Specific values to extract from the raster.
#' @param filters Numeric vector (optional). Values to include in the analysis. Use either \code{filters} or \code{unfilters}, not both.
#' @param unfilters Numeric vector (optional). Values to exclude from the analysis.
#' @param maximum_rate_nodata_value Integer. Maximum allowable percentage of NoData values in the window (default: 100).
#' @param output_raster String (optional). File name for the single resulting raster if only one metric and one window size are specified.
#' @param type_mime String (optional). Output raster format, e.g., "GEOTIFF" (default) or "ASCII_GRID".
#' @param output_csv String (optional). File name for the CSV file with calculated metrics. If NULL, no CSV is created.
#' @param output_folder String (optional). Directory where output rasters will be saved. Files will be named \code{input_raster_filename}_{metric}_{size}.tif (or .asc if input is ASCII).
#' @param properties_file String (optional). File name to store metadata about the operation. Defaults to a temporary file that is discarded.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @examples
#' sliding.window(
#'   input_raster = system.file("data/sample.tif", package = "chloe"),
#'   metrics = c("SHDI", "HET"),
#'   sizes = c(51, 101),
#'   output_csv = "sample_metrics.csv",
#'   displacement = 5,
#'   interpolation = TRUE
#' )
#'
#' @export
sliding.window <- function(
    input_raster = NULL,
    input_tile_raster = NULL,
    metrics,
    sizes,
    distance_type = "THRESHOLD",
    distance_function = NULL,
    friction_raster = NULL,
    shape = "CIRCLE",
    displacement = 1,
    interpolation = FALSE,
    values = NULL,
    filters = NULL,
    unfilters = NULL,
    maximum_rate_nodata_value = 100,
    output_raster = NULL,
    type_mime = NULL, # GEOTIFF (default) or ASCII_GRID
    output_csv = NULL,
    output_folder = NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=sliding\n"
  if(!is.null(input_raster)){
    props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  }else if(!is.null(input_tile_raster)){
    props <- paste0(props, "input_tile_raster=", input_tile_raster, "\n")
  }
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
  if(!is.null(values)) props <- paste0(props, "values=", "{", paste(values,collapse=";"), "}", "\n")
  if(!is.null(filters)) props <- paste0(props, "filters=", "{", paste(filters,collapse=";"), "}", "\n")
  if(!is.null(unfilters)) props <- paste0(props, "unfilters=", "{", paste(unfilters,collapse=";"), "}", "\n")
  if(maximum_rate_nodata_value != 100) props <- paste0(props, "maximum_rate_nodata_value=", maximum_rate_nodata_value, "\n")
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(type_mime)) props <- paste0(props, "type_mime=", type_mime, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")
  if(!is.null(output_folder)) props <- paste0(props, "output_folder=", output_folder, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
# SELECTED
#' Perform selected window analysis on raster datasets using a points file
#'
#' Applies a selected window analysis to raster datasets by calculating specified landscape metrics
#' within windows centered on specified point locations. These point locations are stored in a CSV file with 'ID';'X';'Y' header.
#' Results are saved as CSV tables and optional rasters.
#'
#' @param input_raster String. Path to the input raster dataset(s), with multiple paths separated by commas.
#' @param metrics Vector of strings. Names of the landscape metrics to calculate within each window.
#' @param sizes Integer vector. Size(s) of the moving windows in pixels (e.g., \code{c(101, 201)} for diameters of 101 and 201 pixels).
#' @param points String. Path to a CSV file listing point coordinates with columns "ID", "X", and "Y".
#' @param values Numeric vector (optional). Specific values to extract from the raster.
#' @param shape String. Window shape: "CIRCLE" (default), "SQUARE", or "FUNCTIONAL". If "FUNCTIONAL" is used, \code{friction_raster} must be specified.
#' @param distance_type String. Distance weighting method. One of: "THRESHOLD" (default), "WEIGHTED", "FAST_GAUSSIAN", "FAST_SQUARE".
#' @param distance_function String (optional). Defines the weighting function (used only if \code{distance_type = "WEIGHTED"}). Defaults to Gaussian weighting: \code{exp(-distance^2 / (dmax/2)^2)}.
#' @param friction_raster String (optional). Path to a raster defining friction values, required if \code{shape = "FUNCTIONAL"}.
#' @param output_raster String (optional). File name for the single resulting raster if only one metric and one window size are specified.
#' @param output_csv String (optional). File name for the CSV file with calculated metrics. If NULL, no CSV is created.
#' @param output_folder String (optional). Directory where output rasters will be saved. Files will be named \code{input_raster_filename}_{metric}_{size}.tif (or .asc if input is ASCII).
#' @param windows_path String (optional). Directory where window mask rasters will be saved.
#' @param properties_file String (optional). File name to store metadata about the operation. Defaults to a temporary file that is discarded.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @examples
#' selected.window(
#'   input_raster = system.file("data/sample.tif", package = "chloe"),
#'   metrics = c("SHDI", "HET"),
#'   sizes = c(51, 101),
#'   points = "points.csv",
#'   output_csv = "selected_metrics.csv"
#' )
#'
#' @export
selected.window <- function(
    input_raster,
    metrics,
    sizes,
    points,
    values = NULL,
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
  if(!is.null(values)) props <- paste0(props, "values=", "{", paste(values,collapse=";"), "}", "\n")
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")
  if(!is.null(output_folder)) props <- paste0(props, "output_folder=", output_folder, "\n")
  if(!is.null(windows_path)) props <- paste0(props, "windows_path=", windows_path, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
# GRID
#' Perform grid window analysis on raster datasets
#'
#' Applies a grid window analysis to raster datasets, calculating specified landscape metrics
#' over defined window sizes and exporting results to raster files or CSV tables.
#'
#' @param input_raster String. Path to the input raster dataset(s), with multiple paths separated by commas.
#' @param metrics Vector of strings. Names of the landscape metrics to calculate within each window.
#' @param sizes Integer vector. Size(s) of the moving windows in pixels (e.g., \code{c(101, 201)} for diameters of 101 and 201 pixels).
#' @param maximum_rate_nodata_value Integer. Maximum allowable percentage of NoData values in the window (default: 100).
#' @param output_raster String (optional). File name for the single resulting raster if only one metric and one window size are specified.
#' @param output_csv String (optional). File name for the CSV file with calculated metrics. If NULL, no CSV is created.
#' @param output_folder String (optional). Directory where output rasters will be saved. Files will be named \code{input_raster_filename}_{metric}_{size}.tif (or .asc if input is ASCII).
#' @param properties_file String (optional). File name to store metadata about the operation. Defaults to a temporary file that is discarded.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @examples
#' grid.window(
#'   input_raster = system.file("data/sample.tif", package = "chloe"),
#'   metrics = c("SHDI", "HET"),
#'   sizes = c(51, 101),
#'   output_csv = "grid_metrics.csv"
#' )
#'
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
#' Perform map window analysis on raster datasets
#'
#' Applies a map-wide analysis to raster datasets, calculating specified landscape metrics
#' over the entire extent without moving windows. Results are exported as CSV tables.
#'
#' @param input_raster String. Path to the input raster dataset(s), with multiple paths separated by commas.
#' @param metrics Vector of strings. Names of the landscape metrics to calculate over the map extent.
#' @param output_csv String (optional). File name for the CSV file with calculated metrics. If NULL, no CSV is created.
#' @param properties_file String (optional). File name to store metadata about the operation. Defaults to a temporary file that is discarded.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @examples
#' map.window(
#'   input_raster = system.file("data/sample.tif", package = "chloe"),
#'   metrics = c("SHDI", "HET"),
#'   output_csv = "map_metrics.csv"
#' )
#'
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
