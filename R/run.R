
#' @keywords internal
getConfigFile <- function(){
  return(paste0(tools::R_user_dir("chloe", which = "config"),"/chloe.conf"))
}


#' @keywords internal
writeConfigParam <- function(paramName, paramValue) {
  configFileName = getConfigFile()
  # Check if the config file exists already
  if (!file.exists(configFileName)) {
    # Initialize empty list to store parameters
    paramsList <- list()
  } else {
    # Read existing config file
    paramsList <- readLines(con<-file(configFileName))
    close(con)
    names(paramsList) <- gsub("\\=.+$","",paramsList)
  }

  # Add or update specified parameter
  paramsList[[paramName]] <- paste(paramName,"=",paramValue,sep="")

  # Write updated config file
  con <- file(configFileName, open="wt")
  writeLines(unlist(paramsList), con)
  close(con)
}

#' @keywords internal
readConfigParam <- function(paramName) {
  configFileName = getConfigFile()
  # Check if the config file exists
  if (!file.exists(configFileName)) return("")

  # Read config file
  paramsList <- readLines(con<-file(configFileName))
  close(con)
  names(paramsList) <- gsub("\\=.+$","",paramsList)

  # Return requested parameter value
  return(unlist(strsplit(paramsList[[paramName]], split="="))[2])
}

get.java <- function() {
  java_path = readConfigParam("java_path")
  if(java_path=="") {
    if (.Platform$OS.type == "windows")
      java_path = as.vector(Sys.which("java.exe"))
    else
      java_path = as.vector(Sys.which("java"))
  }
  if(java_path=="")
    stop("Java path not configured. Install Java if it is not already installed. Else use 'set.java()' to configure the java executable path.")
  return(java_path)
}

#' @title Set Java Path
#' @description Function to set the Java path in the configuration file.
#' @param java.path Character string specifying the Java path.
#' @details This function writes the provided Java path to the chloe package configuration file.
#' @return None.
#' @export
set.java <- function(java.path){
  writeConfigParam("java_path",java.path)
}

#' @keywords internal
run <- function(props,properties.file = NULL) {
  if( is.null(properties.file))
    properties.file = tempfile(pattern = "chloe-", fileext = ".properties")
  write(paste0("# ", Sys.time(), "\n", props), file=properties.file)
  jar.file = system.file("bin/Chloe5-0.0.1.jar", package = "chloe");
  chloe_cmd <- paste0(get.java()," -jar ",jar.file," ",properties.file);
  print(chloe_cmd);
  system(chloe_cmd);
}
