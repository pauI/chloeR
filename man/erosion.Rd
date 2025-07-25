% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procedure.R
\name{erosion}
\alias{erosion}
\title{Perform erosion analysis}
\usage{
erosion(
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
  properties_file = NULL
)
}
\arguments{
\item{treatment}{String. Name of the procedure to execute (default: "erosion_calculation").}

\item{infiltration_map_file}{String. Path to the infiltration csv file to map infiltration coeficients to land use codes.}

\item{erodibility_map_file}{String. Path to the erodibility csv file to map infiltration erodibility to land use codes.}

\item{displacement}{Numeric. Displacement value for processing steps. Default: 1.}

\item{territory_shape}{String. Path to the territory boundary shapefile.}

\item{territory_id_attribute}{String. Field name identifying territory IDs.}

\item{territory_id_values}{vector of strings. IDs of the territories to include.}

\item{elevation_folder}{vector of strings. Paths to folders with elevation data.}

\item{os_source}{String. Path to the land use raster file.}

\item{surface_wood_shape}{vector of strings. Paths to surface wood shapefiles.}

\item{surface_wood_attribute}{String. Field name for surface wood classification.}

\item{surface_wood_code}{vector of strings. Code mappings for surface wood categories.}

\item{linear_wood_shape}{vector of strings. Paths to linear wood shapefiles.}

\item{linear_wood_code}{String. Code for linear wood features.}

\item{linear_road_shape}{vector of strings. Paths to linear road shapefiles.}

\item{linear_road_attribute}{String. Field name for road classification.}

\item{linear_road_code}{vector of strings. Code mappings for road categories.}

\item{linear_train_shape}{vector of strings. Paths to railway line shapefiles.}

\item{linear_train_code}{String. Code for railway features.}

\item{surface_water_shape}{vector of strings. Paths to surface water shapefiles.}

\item{surface_water_code}{String. Code for surface water features.}

\item{linear_water_shape}{vector of strings. Paths to linear watercourse shapefiles.}

\item{linear_water_code}{String. Code for linear watercourse features.}

\item{output_folder}{String. Directory to store output files.}

\item{output_prefix}{String. Prefix to add to output filenames.}

\item{properties_file}{String (optional). File name to store metadata about the operation.}
}
\value{
No R object is returned. Output is written to disk.
}
\description{
Computes erosion risk or sediment delivery using infiltration, erodibility,
elevation, and land cover data for a specified territory.
}
