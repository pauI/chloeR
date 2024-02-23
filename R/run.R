
#' @keywords internal
run <- function(props,properties.file = NULL) {
  if( is.null(properties.file))
    properties.file = tempfile(pattern = "chloe-", fileext = ".properties")
  write(paste0("# ", Sys.time(), "\n", props), file=properties.file)
  jar.file = system.file("bin/Chloe5-0.0.1.jar", package = "chloe");
  chloe_cmd <- paste0("java -jar ",jar.file," ",properties.file);
  system(chloe_cmd);
}
