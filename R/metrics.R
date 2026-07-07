#####
#' List available metrics for the chloe library
#'
#' Returns a data frame containing the available landscape metrics, optionally filtered
#' by type or associated process.
#'
#' @param type vector of strings (optional). Types of metrics to include in the output.
#'        Available types include: "value", "couple", "quantitative", "patch",
#'        "slope", "continuity", "erosion", "degaterosion".
#' @param process vector of strings (optional). Process modes associated with the desired metrics.
#'.       Available modes include: "value", "couple" and "other"
#'        Metrics of type "value" and "couple" must be completed with specific values,
#'        e.g., "pNV_1" for value metrics or "pNC_1-2" for couple metrics.
#'
#' @return A data frame containing the requested metrics.
#'
#' @examples
#' \dontrun{
#' list.metrics() # Returns all available metrics
#' list.metrics(type = "value") # Filters by type
#' list.metrics(process = "couple") # Filters by process
#' }
#'
#' @export
list.metrics <- function(type = NULL, process = NULL) {
  print(system.file("data/metrics.csv", package = "chloe"))
  metrics <- utils::read.csv(system.file("data/metrics.csv", package = "chloe"), header = TRUE, sep = ";")

  if (!is.null(type)) {
    metrics <- metrics[metrics$type == type, ]
  }

  if (!is.null(process)) {
    metrics <- metrics[metrics$process == process, ]
  }

  return(metrics[1:3])
}

#####
#' Generate Value Metrics
#'
#' Generates a single string containing all combinations of metric names
#' and their associated values, separated by semicolons.
#'
#' @param metrics vector of strings. Names of the metrics to combine with values.
#' @param values numeric vector. Values to associate with the given metrics.
#'
#' @return A single string with all metric-value combinations separated by semicolons.
#'
#' @examples
#' \dontrun{
#' metrics <- c("NV", "pNV")
#' values <- c(1, 2, 3, 4, 5, 6)
#' res <- generate.value.metrics(metrics, values)
#' }
#'
#' @export
generate.value.metrics <- function(metrics,values){
  value.metrics = ""
  for(m in metrics)
    for(v in values)
      value.metrics = paste0(value.metrics,";",m,"_",v)
  return(substring(value.metrics,2))
}

#####
#' Generate Couple Metrics
#'
#' Generates a single string containing all unique combinations of two different
#' values for each given metric, separated by semicolons.
#'
#' @param metrics vector of strings. Names of the metrics to combine with value pairs.
#' @param values numeric vector. Values to generate unique pairs from.
#'
#' @return A single string with all metric-couple combinations separated by semicolons.
#'
#' @examples
#' \dontrun{
#' metrics <- c("pNC")
#' values <- c(1, 2, 3, 7, 8)
#' res <- generate.couple.metrics(metrics, values)
#' }
#'
#' @export
generate.couple.metrics <- function(metrics,values){
  couple.metrics = ""
  for(m in metrics)
    for(v1 in values)
      for(v2 in values)
        if(v2>v1)
          couple.metrics = paste0(couple.metrics,";",m,"_",v1,"-",v2)
  return(substring(couple.metrics,2))
}

#####
#' Select Relevant Couple Metrics From Raster Adjacencies
#'
#' Counts adjacent pixels with distinct values in one or more rasters, then selects
#' relevant value pairs before generating couple metric names. For each pixel, only
#' the right and bottom neighbours are considered, so each adjacency is counted once.
#'
#' Selection keeps the most represented pairs globally.
#'
#' @param input_raster character vector. Path(s) to input raster file(s).
#' @param metrics character vector. Names of the couple metrics to combine with
#'   selected pairs.
#' @param top_rate numeric. Proportion of pairs to keep. If greater than or equal
#'   to 1, it is interpreted as a percentage. If between 0 and 1, as a ratio.
#'   Defaults to 10 percent.
#' @param values numeric vector, optional. Restricts counting and selection to these
#'   raster values.
#' @param nodata_value numeric, optional. Value to ignore in addition to raster NA.
#' @param return character. Output format: \code{"metrics"} returns a semicolon
#'   separated metric string, \code{"couples"} returns selected pairs and counts,
#'   \code{"counts"} returns all counted pairs.
#'
#' @return Depending on \code{return}, either a metric string or a data frame with
#'   columns \code{value1}, \code{value2}, \code{count}, and \code{rate}.
#'
#' @examples
#' \dontrun{
#' select.couple.metrics("path/to/landuse.tif", metrics = c("pNC"), top_rate = 5)
#' }
#'
#' @export
select.couple.metrics <- function(
    input_raster,
    metrics,
    top_rate = 10,
    values = NULL,
    nodata_value = NULL,
    return = c("metrics", "couples", "counts")) {

  return <- match.arg(return)

  counts <- count.adjacent.couples(input_raster, values = values, nodata_value = nodata_value)
  if (nrow(counts) == 0) {
    if (return == "metrics") return("")
    return(counts)
  }

  if (return == "counts") return(counts)

  rate <- .as.rate(top_rate)
  n <- max(1, ceiling(nrow(counts) * rate))
  selected <- counts[seq_len(min(n, nrow(counts))), ]

  if (return == "couples") return(selected)

  couple.metrics <- ""
  for (m in metrics) {
    for (i in seq_len(nrow(selected))) {
      couple.metrics <- paste0(couple.metrics, ";", m, "_", selected$value1[i], "-", selected$value2[i])
    }
  }
  return(substring(couple.metrics, 2))
}

#####
#' Count Adjacent Distinct-Value Pixel Pairs
#'
#' Counts horizontal and vertical adjacencies between distinct values in one or more
#' rasters. Horizontal adjacencies use the right neighbour and vertical adjacencies
#' use the bottom neighbour.
#'
#' @param input_raster character vector. Path(s) to input raster file(s).
#' @param values numeric vector, optional. Restricts counting to these raster values.
#' @param nodata_value numeric, optional. Value to ignore in addition to raster NA.
#'
#' @return A data frame with columns \code{value1}, \code{value2}, \code{count},
#'   and \code{rate}, sorted by decreasing count. \code{value1} is always lower
#'   than \code{value2}.
#'
#' @export
count.adjacent.couples <- function(input_raster, values = NULL, nodata_value = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required to read raster files.", call. = FALSE)
  }

  input_raster <- unlist(input_raster)
  if (length(input_raster) == 0) stop("input_raster must contain at least one raster path.", call. = FALSE)

  all_pairs <- data.frame(value1 = numeric(0), value2 = numeric(0))
  for (raster_path in input_raster) {
    raster <- terra::rast(raster_path)
    if (terra::nlyr(raster) > 1) raster <- raster[[1]]
    raster_values <- terra::values(raster, mat = FALSE)
    raster_values <- matrix(raster_values, nrow = terra::nrow(raster), ncol = terra::ncol(raster), byrow = TRUE)

    right_pairs <- .adjacent.value.pairs(
      raster_values[, -terra::ncol(raster), drop = FALSE],
      raster_values[, -1, drop = FALSE],
      values,
      nodata_value
    )
    bottom_pairs <- .adjacent.value.pairs(
      raster_values[-terra::nrow(raster), , drop = FALSE],
      raster_values[-1, , drop = FALSE],
      values,
      nodata_value
    )
    all_pairs <- rbind(all_pairs, right_pairs, bottom_pairs)
  }

  if (nrow(all_pairs) == 0) {
    return(data.frame(value1 = numeric(0), value2 = numeric(0), count = integer(0), rate = numeric(0)))
  }

  counts <- stats::aggregate(rep(1L, nrow(all_pairs)), all_pairs, sum)
  names(counts) <- c("value1", "value2", "count")
  counts <- counts[order(-counts$count, counts$value1, counts$value2), ]
  row.names(counts) <- NULL
  counts$rate <- counts$count / sum(counts$count)
  return(counts)
}

.adjacent.value.pairs <- function(a, b, values, nodata_value) {
  keep <- !is.na(a) & !is.na(b) & a != b
  if (!is.null(nodata_value)) keep <- keep & a != nodata_value & b != nodata_value
  if (!is.null(values)) keep <- keep & a %in% values & b %in% values

  a <- as.vector(a[keep])
  b <- as.vector(b[keep])
  if (length(a) == 0) return(data.frame(value1 = numeric(0), value2 = numeric(0)))

  data.frame(value1 = pmin(a, b), value2 = pmax(a, b))
}

.as.rate <- function(rate) {
  if (length(rate) != 1 || is.na(rate) || rate < 0) stop("Rates must be positive numeric values.", call. = FALSE)
  if (rate >= 1) rate <- rate / 100
  min(rate, 1)
}
