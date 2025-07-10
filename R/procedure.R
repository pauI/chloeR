#####
#' Calculate bocage grain indicators
#'
#' Runs a procedure to compute the bocage grain and related metrics over a territory,
#' considering hedgerow planting/removal scenarios, thresholds, and functional clustering.
#'
#' @param territory String (optional). Path to a shapefile defining the analysis territory.
#' @param envelope String (optional). Path to a shapefile defining an envelope for clipping.
#' @param buffer_area Numeric. Buffer distance in meters (default: 0).
#' @param bocage String (optional). Path to an input hedgerow shapefile.
#' @param wood_removal String (optional). Path to shapefile defining hedgerows to be removed.
#' @param wood_planting String (optional). Path to shapefile defining new hedgerows to be planted.
#' @param height_planting_attribute String. Field name in `wood_planting` specifying height. Default: "hauteur".
#' @param wood_height Numeric (optional). Default height of planted hedgerows if no attribute is defined.
#' @param wood_type String (optional). Type of hedgerows to consider.
#' @param influence_distance Numeric (optional). Influence distance for metrics calculation.
#' @param thresholds Numeric vector. Threshold values for classification. Default: c(0.20, 0.33, 0.45).
#' @param threshold Numeric. Selected threshold value. Default: 0.33.
#' @param grain_bocager_window_radius Numeric. Window radius for grain calculation. Default: 250.
#' @param grain_bocager_cellsize Numeric. Cell size for raster outputs. Default: 5.
#' @param grain_bocager String (optional). Output path for grain result.
#' @param grain_bocager_4classes String (optional). Output path for 4-class grain result.
#' @param functional_grain_bocager String (optional). Output path for functional grain result.
#' @param functional_grain_bocager_clustering String (optional). Output path for functional clustering result.
#' @param issues_window_radius Numeric. Window radius for global issues calculation. Default: 1000.
#' @param issues_cellsize Numeric. Cell size for global issues outputs. Default: 50.
#' @param functional_grain_bocager_proportion String (optional). Output path for proportion result.
#' @param functional_grain_bocager_fragmentation String (optional). Output path for fragmentation result.
#' @param output_folder String (optional). Directory to store output files.
#' @param output_prefix String. Prefix to add to output filenames.
#' @param properties_file String (optional). File name to store metadata about the operation.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @export
grain.bocager <- function(
    territory = NULL,
    envelope = NULL,
    buffer_area = 0,
    bocage = NULL,
    wood_removal = NULL,
    wood_planting = NULL,
    height_planting_attribute = "hauteur",
    wood_height = NULL,
    wood_type = NULL,
    influence_distance = NULL,
    thresholds = c(0.20, 0.33, 0.45),
    threshold = 0.33,
    grain_bocager_window_radius = 250,
    grain_bocager_cellsize = 5,
    grain_bocager = NULL,
    grain_bocager_4classes = NULL,
    functional_grain_bocager = NULL,
    functional_grain_bocager_clustering = NULL,
    issues_window_radius = 1000,
    issues_cellsize = 50,
    functional_grain_bocager_proportion = NULL,
    functional_grain_bocager_fragmentation = NULL,
    output_folder = NULL,
    output_prefix = "",
    properties_file = NULL){

  # Create the properties file content
  props <- "procedure=grain_bocager\n"
  if(!is.null(bocage)){
    props <- paste0(props, "bocage=", bocage, "\n")
    if(!is.null(envelope)) props <- paste0(props, "envelope=", envelope, "\n")
    else if(!is.null(territory)) props <- paste0(props, "territory=", territory, "\n")
  }
  if(!is.null(wood_height)) {
    props <- paste0(props, "wood_height=", wood_height, "\n")
    treatment <- "wood_height_recovery"
  }
  if(!is.null(wood_removal)) props <- paste0(props, "wood_removal=", wood_removal, "\n")
  if(!is.null(wood_planting)){
    props <- paste0(props, "wood_planting=", wood_planting, "\n")
    props <- paste0(props, "height_planting_attribute=", height_planting_attribute, "\n")
  }
  if(!is.null(wood_type)) {
    props <- paste0(props, "wood_type=", wood_type, "\n")
    treatment <- "wood_type_detection"
  }
  if(!is.null(influence_distance)) {
    props <- paste0(props, "influence_distance=", influence_distance, "\n")
    treatment <- "influence_distance_calculation"
  }
  if(!is.null(grain_bocager)) {
    props <- paste0(props, "grain_bocager=", grain_bocager, "\n")
    treatment <- "grain_bocager_calculation"
  }
  props <- paste0(props, "thresholds=", "{", paste(thresholds,collapse=";"), "}", "\n")
  props <- paste0(props, "threshold=", threshold, "\n")
  props <- paste0(props, "grain_bocager_window_radius=", grain_bocager_window_radius, "\n")
  props <- paste0(props, "grain_bocager_cellsize=", grain_bocager_cellsize, "\n")
  if(!is.null(grain_bocager_4classes)) props <- paste0(props, "grain_bocager_4classes=", grain_bocager_4classes, "\n")
  if(!is.null(functional_grain_bocager_clustering)) {
    props <- paste0(props, "functional_grain_bocager_clustering=", functional_grain_bocager_clustering, "\n")
    treatment <- "functional_clustering"
  }
  if(!is.null(functional_grain_bocager)) {
    props <- paste0(props, "functional_grain_bocager=", functional_grain_bocager, "\n")
    treatment <- "functional_clustering"
  }
  if(!is.null(functional_grain_bocager_proportion)) {
    props <- paste0(props, "functional_grain_bocager_proportion=", functional_grain_bocager_proportion, "\n")
    treatment <- "global_issues_calculation"
  }
  if(!is.null(functional_grain_bocager_fragmentation)) {
    props <- paste0(props, "functional_grain_bocager_fragmentation=", functional_grain_bocager_fragmentation, "\n")
    treatment <- "global_issues_calculation"
  }
  if(!is.null(output_folder)){
    props <- paste0(props, "output_folder=", output_folder, "\n")
    props <- paste0(props, "output_prefix=", output_prefix, "\n")
    treatment <- "global_issues_calculation"
  }
  if(treatment == "global_issues_calculation"){
    props <- paste0(props, "issues_window_radius=", issues_window_radius, "\n")
    props <- paste0(props, "issues_cellsize=", issues_cellsize, "\n")
  }

  props <- paste0(props, "treatment=", treatment, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}


#####
# ECOLANDSCAPE
#' Perform ecolandscape analysis
#'
#' Runs an analysis computing landscape metrics over multiple scales and classes,
#' exporting results to specified output folders.
#'
#' @param treatment String. Name of the last procedure to execute (default: "mapping", possible values, in order of execution : "calcul_metrics", "standardization", "clustering", "gradient", "mapping", "rupture").
#' @param input_raster String. Path to the input land use raster file.
#' @param scales Numeric vector. Scale values (e.g., moving window sizes).
#' @param classes vector of strings. Numbers of ecolandscapes to produce.
#' @param codes vector of strings (optional). Land cover values to include in the analysis. By default : all.
#' @param output_folder String. Directory to store output files.
#' @param xy_file String (optional). Path to a CSV file with XY locations for selected outputs.
#' @param displacement Numeric (optional). Displacement value between windows in pixels.
#' @param factor Numeric vector (optional). Scaling factor.
#' @param window_distance_type String (optional). Type of distance function to use for windows : "gaussian" (default) or "square".
#' @param properties_file String (optional). File name to store metadata about the operation.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @export
eco.landscape <- function(
    treatment = "mapping",
    input_raster,
    scales,
    classes,
    codes = NULL,
    unfilters = NULL,
    output_folder,
    xy_file = NULL,
    displacement = NULL,
    factor = NULL,
    window_distance_type =NULL,
    properties_file = NULL){

  # Create the properties file content
  props <- "procedure=ecolandscape\n"
  props <- paste0(props, "input_raster=", input_raster, "\n")
  props <- paste0(props, "scales=", "{", paste(scales,collapse=";"), "}", "\n")
  props <- paste0(props, "classes=", "{", paste(classes,collapse=";"), "}", "\n")
  if(!is.null(codes)){
    props <- paste0(props, "codes=", "{", paste(codes,collapse=";"), "}", "\n")
  }
  if(!is.null(unfilters)){
    props <- paste0(props, "unfilters=", "{", paste(unfilters,collapse=";"), "}", "\n")
  }
  if(!is.null(output_folder)){
    props <- paste0(props, "output_folder=", output_folder, "\n")
  }
  if(!is.null(xy_file))
    props <- paste0(props, "xy_file=", xy_file, "\n")
  if(!is.null(displacement))
    props <- paste0(props, "displacement=", displacement, "\n")
  if(!is.null(factor))
    props <- paste0(props, "factor=", factor, "\n")
  if(!is.null(window_distance_type))
    props <- paste0(props, "window_distance_type=", window_distance_type, "\n")

  props <- paste0(props, "treatment=", treatment, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#erosion
#' Perform erosion analysis
#'
#' Computes erosion risk or sediment delivery using infiltration, erodibility,
#' elevation, and land cover data for a specified territory.
#'
#' @param treatment String. Name of the procedure to execute (default: "erosion_calculation").
#' @param infiltration_map_file String. Path to the infiltration csv file to map infiltration coeficients to land use codes.
#' @param erodibility_map_file String. Path to the erodibility csv file to map infiltration erodibility to land use codes.
#' @param displacement Numeric. Displacement value for processing steps. Default: 1.
#' @param territory_shape String. Path to the territory boundary shapefile.
#' @param territory_id_attribute String. Field name identifying territory IDs.
#' @param territory_id_values vector of strings. IDs of the territories to include.
#' @param elevation_folder vector of strings. Paths to folders with elevation data.
#' @param os_source String. Path to the land use raster file.
#' @param surface_wood_shape vector of strings. Paths to surface wood shapefiles.
#' @param surface_wood_attribute String. Field name for surface wood classification.
#' @param surface_wood_code vector of strings. Code mappings for surface wood categories.
#' @param linear_wood_shape vector of strings. Paths to linear wood shapefiles.
#' @param linear_wood_code String. Code for linear wood features.
#' @param linear_road_shape vector of strings. Paths to linear road shapefiles.
#' @param linear_road_attribute String. Field name for road classification.
#' @param linear_road_code vector of strings. Code mappings for road categories.
#' @param linear_train_shape vector of strings. Paths to railway line shapefiles.
#' @param linear_train_code String. Code for railway features.
#' @param surface_water_shape vector of strings. Paths to surface water shapefiles.
#' @param surface_water_code String. Code for surface water features.
#' @param linear_water_shape vector of strings. Paths to linear watercourse shapefiles.
#' @param linear_water_code String. Code for linear watercourse features.
#' @param output_folder String. Directory to store output files.
#' @param output_prefix String. Prefix to add to output filenames.
#' @param properties_file String (optional). File name to store metadata about the operation.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @export
erosion <- function(
    treatment = "erosion_calculation",
    infiltration_map_file,
    erodibility_map_file,
    displacement = 1,
    territory_shape,
    territory_id_attribute,
    territory_id_values,
    elevation_folder,
    os_source,
    surface_wood_shape,
    surface_wood_attribute,
    surface_wood_code,
    linear_wood_shape,
    linear_wood_code,
    linear_road_shape,
    linear_road_attribute,
    linear_road_code,
    linear_train_shape,
    linear_train_code,
    surface_water_shape,
    surface_water_code,
    linear_water_shape,
    linear_water_code,
    output_folder,
    output_prefix,
    properties_file = NULL){

  # Create the properties file content
  props <- "procedure=erosion\n"
  props <- paste0(props, "infiltration_map_file=", infiltration_map_file, "\n")
  props <- paste0(props, "erodibility_map_file=", erodibility_map_file, "\n")
  props <- paste0(props, "displacement=", displacement, "\n")
  props <- paste0(props, "territory_shape=", territory_shape, "\n")
  props <- paste0(props, "territory_id_attribute=", territory_id_attribute, "\n")
  props <- paste0(props, "territory_id_values=", "{", paste(territory_id_values,collapse=";"), "}", "\n")
  props <- paste0(props, "elevation_folder=", "{", paste(elevation_folder,collapse=";"), "}", "\n")
  props <- paste0(props, "os_source=", os_source, "\n")
  props <- paste0(props, "surface_wood_shape=", paste(surface_wood_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "surface_wood_attribute=", surface_wood_attribute, "\n")
  props <- paste0(props, "surface_wood_code={")
  sc=""
  for(c in surface_wood_code) {
    props <- paste0(props, sc, "(", c[1], ",", c[2], ")")
    if(sc=="") sc=";"
  }
  props <- paste0(props, "}\n")
  props <- paste0(props, "linear_wood_shape=", paste(linear_wood_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "linear_wood_code=", linear_wood_code, "\n")
  props <- paste0(props, "linear_road_shape=", paste(linear_road_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "linear_road_attribute=", linear_road_attribute, "\n")
  props <- paste0(props, "linear_road_code={")
  sc=""
  for(c in linear_road_code) {
    props <- paste0(props, sc, "(", c[1], ",", c[2], ")")
    if(sc=="") sc=";"
  }
  props <- paste0(props, "}\n")
  props <- paste0(props, "linear_train_shape=", paste(linear_train_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "linear_train_code=", linear_train_code, "\n")
  props <- paste0(props, "surface_water_shape=", paste(surface_water_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "surface_water_code=", surface_water_code, "\n")
  props <- paste0(props, "linear_water_shape=", paste(linear_water_shape,collapse=";"), "}", "\n")
  props <- paste0(props, "linear_water_code=", linear_water_code, "\n")
  props <- paste0(props, "output_folder=", output_folder, "\n")
  props <- paste0(props, "output_prefix=", output_prefix, "\n")

  props <- paste0(props, "treatment=", treatment, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}

#####
#regulation biologique Ephestia Toulouse
#' Perform biological regulation for Ephestia validated on Toulouse study area
#'
#' @param cubist_model String. Cubist statistical model to compute regulation
#' @param data_cover String. Landcover map LULC
#' @param data_farm String. Farm map
#' @param system_file String. File to define which farm is organic (3) or conventional (1)
#' @param ift_file String. File to define amount of treatment frequency index for each cover type
#' @param meteo_file String. File to define fixed weather variables
#' @param model_output String. Path to the output model map
#' @param displacement Numeric (optional). Displacement value for processing steps. Default: 1.
#' @param properties_file String (optional). File name to store metadata about the operation.
#'
#' @return No R object is returned. Output is written to disk.
#'
#' @export
regulation.ephestia.toulouse <- function(
    cubist_model,
    data_cover,
    data_farm,
    system_file,
    ift_file,
    meteo_file,
    model_output,
    displacement = 1,
    properties_file = NULL){

  # Create the properties file content
  props <- "procedure=ephestia_toulouse\n"
  props <- paste0(props, "cubist_model=", cubist_model, "\n")
  props <- paste0(props, "data_cover=", data_cover, "\n")
  props <- paste0(props, "data_farm=", data_farm, "\n")
  props <- paste0(props, "system_file=", system_file, "\n")
  props <- paste0(props, "ift_file=", ift_file, "\n")
  props <- paste0(props, "meteo_file=", meteo_file, "\n")
  props <- paste0(props, "model_output=", model_output, "\n")
  props <- paste0(props, "displacement=", displacement, "\n")

  # write and launch
  run.chloe(write.params(props, properties_file))
}
