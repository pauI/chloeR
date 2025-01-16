#####
#SEARCH AND REPLACE
#' Replace pixel values in a raster based on a given set of pairs
#'
#' This function modifies an input raster by replacing pixels with specific
#' values according to the provided list of pairs.
#'
#' @param input_raster A path to the input raster file as a character string or
#'   a Raster* object from which the filename will be extracted using \code{input_raster@filename}.
#'
#' @param changes A list of integer pairs where each pair consists of two elements:
#'   the original value (first element) and its replacement value (second element).
#'
#' @param output_raster A path to the output raster file as a character string.
#'
#' @param nodata_value An optional numeric value representing NoData cells in both
#'   input and output rasters. If not specified, NoData cells won't be modified.
#'
#' @param properties_file An optional path to a properties file used to store
#'   metadata about the processing performed by this function. If not specified, no
#'   properties file will be generated.
#'
#' @return Invisible null value invisibly when successful. Otherwise, throws an error.
#'
#' @examples
#' \dontrun{
#' search.replace("path/to/input_raster.tif", list(c(1,5), c(7,3)), "path/to/output_raster.tif")
#' }
#' @export
search.replace <- function(
    input_raster,
    changes,
    nodata_value= NULL,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=search_and_replace\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "changes={")
  sc=""
  for(c in changes) {
    props <- paste0(props, sc, "(", c[1], ",", c[2], ")")
    if(sc=="") sc=";"
  }
  props <- paste0(props, "}\n")
  if(!is.null(nodata_value))  props <- paste0(props, "nodata_value=", nodata_value, "\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#CLASSIFICATION
#' Classify Pixel Values Based on Domain Intervals
#'
#' This function takes an input raster and applies custom classifications defined by
#' intervals of pixel values. Each interval is represented by a vector containing
#' two numbers - minimum and maximum values - followed by a single number indicating
#' the classification value.
#'
#' @param input_raster A path to the input raster file as a character string or
#'   a Raster* object from which the filename will be extracted using \code{input_raster@filename}.
#'
#' @param domains A list of vectors where each vector contains three integers:
#'   the lower bound of the range (minimum), the upper bound of the range (maximum),
#'   and the new value assigned to all pixels within this range.
#'
#' @param output_raster A path to the output raster file as a character string.
#'
#' @param properties_file An optional path to a properties file used to store
#'   metadata about the processing performed by this function. If not specified, no
#'   properties file will be generated.
#'
#' @return Invisible null value invisibly when successful. Otherwise, throws an error.
#'
#' @examples
#' \dontrun{
#' classification("path/to/input_raster.tif", list(c(1, 5, 9), c(6, 10, 4)), "path/to/output_raster.tif")
#' }
#' @export
classification <- function(
    input_raster,
    domains,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=classification\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")
  props <- paste0(props, "domains={")
  sc=""
  for(d in domains) {
    props <- paste0(props, sc, "(", d[1], "-", d[2], ")")
    if(sc=="") sc=";"
  }
  props <- paste0(props, "}\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#COMBINE
#' Combine Multiple Factored Rasters Using Expression
#'
#' This function combines multiple factored rasters into one output raster using
#' a predefined mathematical expression involving their labels. The labels should
#' correspond to those specified in the 'factors' argument.
#'
#' @param factors A list of pairs consisting of a factor name (a unique identifier
#'   for each raster) and a corresponding input raster file path.
#'
#' @param combination A textual representation of a valid arithmetic expression
#'   combining the factor names with appropriate operators (+, -, *, /). Parentheses
#'   can be used to group subexpressions together. Existing functions such as min(),
#'   max(), sum(), mean(), median(), sd(), var(), log(), exp(), sqrt(), sin(), cos(),
#'   tan(), abs(), floor(), ceil(), round(), sign(), etc. can also be applied to
#'   individual factors or entire expressions.
#'
#' @param output_raster A path to the output raster file as a character string.
#'
#' @param properties_file An optional path to a properties file used to store
#'   metadata about the processing performed by this function. If not specified, no
#'   properties file will be generated.
#'
#' @examples
#' \dontrun{
#' combine(list(c('factor1', 'path/to/factor1_raster.tif'),
#'               c('factor2', 'path/to/factor2_raster.tif')),
#'         combination = '"factor1" * 2 + "factor2"^2',
#'         output_raster = 'path/to/combined_raster.tif')
#' }
#' @export
combine <- function(
    factors,
    combination,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=combine\n"
  props <- paste0(props, "factors={")
  sc=""
  for(f in factors) {
    props <- paste0(props, sc, "(", f[1], ",", f[2], ")")
    if(sc=="") sc=";"
  }
  props <- paste0(props, "}\n")
  props <- paste0(props, "combination=", combination, "\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#CLUSTER
cluster <- function(
    input_raster,
    cluster_sources,
    cluster_type="QUEEN",
    distance_raster=NULL,
    max_distance=NULL,
    output_raster = NULL,
    output_csv = NULL,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=cluster\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster, collapse=";"), "}", "\n")
  props <- paste0(props, "cluster_sources=", "{", paste(cluster_sources, collapse=";"), "}", "\n")
  props <- paste0(props, "cluster_type=", cluster_type, "\n")
  if(cluster_type == "DISTANCE"){
    props <- paste0(props, "distance_raster=", distance_raster, "\n")
    props <- paste0(props, "max_distance=", max_distance, "\n")
  }
  if(!is.null(output_raster)) props <- paste0(props, "output_raster=", output_raster, "\n")
  if(!is.null(output_csv)) props <- paste0(props, "output_csv=", output_csv, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#OVERLAY
overlay <- function(
    input_raster,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=overlay\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster, collapse=";"), "}", "\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#DISTANCE
#' Compute Distance From Source Pixels
#'
#' This function calculates the distance between every pixel in the input raster
#' and a set of source pixels, whose indices are stored in 'distance_sources'.
#' Distances can be calculated Euclidean ('EUCLIDEAN') or Functional ('FUNCTIONAL').
#' When computing functional distances, they can be optionally weighted by a
#' separate friction raster. Additionally, a maximum distance threshold can be
#' imposed via the 'max_distance' parameter.
#'
#' @param input_raster A path to the input raster file as a character string or
#'   a Raster* object from which the filename will be extracted using \code{input_raster@filename}.
#'
#' @param distance_sources A list of values specifying the source pixels
#'   in the input raster.
#'
#' @param distance_type Either "EUCLIDEAN" or "FUNCTIONAL". Specifies whether to
#'   compute Euclidean or Functional distances. Defaults to "EUCLIDEAN".
#'
#' @param friction_raster Optional path to a Friction Raster, which provides weights
#'   affecting the cost of moving through different areas during the calculation
#'   of Functional distances. Must match the dimensions and coordinate reference
#'   system of the input raster. Set to NULL for EUCLIDEAN calculations.
#'
#' @param max_distance Optionally limit the computed distance to a certain value.
#'   Ignored for 'EUCLIDEAN' computations. Setting it to a positive value enables
#'   limiting the Functional distance computation up to the given limit.
#'
#' @param output_raster A path to the output raster file as a character string.
#'
#' @param properties_file An optional path to a JSON properties file used to store
#'   metadata about the processing performed by this function. If not specified, no
#'   properties file will be generated.
#'
#' @examples
#' \dontrun{
#' distance("path/to/input_raster.tif", c(1, 2, 3, 4), max_distance = 250, output_raster = "path/to/output_raster.tif")
#' }
#' @export
distance <- function(
    input_raster,
    distance_sources,
    distance_type="EUCLIDIAN",
    friction_raster=NULL,
    max_distance=NULL,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=distance\n"
  props <- paste0(props, "input_raster=", "{", paste(input_raster, collapse=";"), "}", "\n")
  props <- paste0(props, "distance_sources=", "{", paste(distance_sources, collapse=";"), "}", "\n")
  props <- paste0(props, "distance_type=", distance_type, "\n")
  if(distance_type == "FUNCTIONAL"){
    props <- paste0(props, "friction_raster=", friction_raster, "\n")
  }
  if(!is.null(max_distance)) props <- paste0(props, "max_distance=", max_distance, "\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#RASTER FROM CSV
raster.from.csv <- function(
    input_csv,
    variables,
    entete = NULL,
    ref_raster = NULL,
    width = NULL,
    height = NULL,
    xmin = NULL,
    ymin = NULL,
    cellsize = NULL,
    nodata_value = NULL,
    crs = NULL,
    output_raster = NULL,
    output_folder = NULL,
    output_prefix = NULL,
    type_mime = "GEOTIFF",
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=raster_from_csv\n"
  props <- paste0(props, "input_csv=", input_csv, "\n")
  props <- paste0(props, "variables=", "{", paste(variables, collapse=";"), "}", "\n")
  if(!is.null(entete)) {
    props <- paste0(props, "entete=", entete, "\n")
  }else if(!is.null(ref_raster)) {
    props <- paste0(props, "ref_raster=", ref_raster, "\n")
  }else{
    props <- paste0(props, "width=", width, "\n")
    props <- paste0(props, "height=", height, "\n")
    props <- paste0(props, "xmin=", xmin, "\n")
    props <- paste0(props, "ymin=", ymin, "\n")
    props <- paste0(props, "cellsize=", cellsize, "\n")
    if(!is.null(nodata_value)) props <- paste0(props, "nodata_value=", nodata_value, "\n")
    if(!is.null(crs)) props <- paste0(props, "crs=", crs, "\n")
  }
  if(!is.null(output_raster)) {
    props <- paste0(props, "output_raster=", output_raster, "\n")
  }else{
    props <- paste0(props, "output_folder=", output_folder, "\n")
    if(!is.null(output_prefix)) props <- paste0(props, "output_prefix=", output_prefix, "\n")
    props <- paste0(props, "type_mime=", type_mime, "\n")
  }

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#RASTER FROM SHAPEFILE
raster.from.shapefile <- function(
    input_shapefile,
    attribute,
    entete = NULL,
    ref_raster = NULL,
    xmin = NULL,
    xmax = NULL,
    ymin = NULL,
    ymax = NULL,
    cellsize = NULL,
    fill_value = NULL,
    nodata_value = NULL,
    output_raster,
    properties_file = NULL) {

  # Create the properties file content
  props <- "treatment=raster_from_shapefile\n"
  props <- paste0(props, "input_shapefile=", input_shapefile, "\n")
  props <- paste0(props, "attribute=", attribute, "\n")
  if(!is.null(entete)) {
    props <- paste0(props, "entete=", entete, "\n")
  }else if(!is.null(ref_raster)) {
    props <- paste0(props, "ref_raster=", ref_raster, "\n")
  }else{
    props <- paste0(props, "xmin=", xmin, "\n")
    props <- paste0(props, "xmax=", xmax, "\n")
    props <- paste0(props, "ymin=", ymin, "\n")
    props <- paste0(props, "ymax=", ymax, "\n")
    props <- paste0(props, "cellsize=", cellsize, "\n")
  }
  if(!is.null(fill_value)) props <- paste0(props, "fill_value=", fill_value, "\n")
  if(!is.null(nodata_value)) props <- paste0(props, "nodata_value=", nodata_value, "\n")
  props <- paste0(props, "output_raster=", output_raster, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}
