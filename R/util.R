#####
#SEARCH AND REPLACE
search.replace <- function(
    input_raster,
    changes,
    nodata_value = NULL,
    output_raster,
    properties_file = NULL){

  # Create the properties file content
  props <- "treatment=search_and_replace\n"

  props <- paste0(props, "input_raster=", "{", paste(input_raster,collapse=";"), "}", "\n")


  # write and launch
  run.chloe(write.params(props, properties_file))
}

