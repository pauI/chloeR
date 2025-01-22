#####
#' @title Grain bocager
#' @description Calculates the bocage grain.
#' @param wood_removal Path to ESRI shapefile delimiting area where hedgerows would be removed.
#' @param wood_planting Path to ESRI shapefile defining hedgerows that would be added to the territory.
#' @param height_planting_attribute Field from 'wood_planting' shapefile that defines the height of hedgerows. Use param wood_height if no field exists
#' @param wood_height height of planted hedgerows if height_planting_attribute not defined. Default height : 10 meters
#' @details TODO
#' @return None.
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
eco.landscape <- function(
    treatment = "mapping",
    input_raster,
    scales,
    classes,
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
