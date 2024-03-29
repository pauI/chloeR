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
#' @param properties_file An optional path to a JSON properties file used to store
#'   metadata about the processing performed by this function. If not specified, no
#'   properties file will be generated.
#'
#' @return Invisible null value invisibly when successful. Otherwise, throws an error.
#'
#' @examples
#' \dontrun{
#' search_and_replace("path/to/input_raster.tif", list(c(1,5), c(7,3)), "path/to/output_raster.tif")
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
#DISTANCE
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
