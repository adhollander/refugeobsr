library(sp)
library(rgdal)
library(data.table)

#' Read in AKN files into data frame
#'
#' @param csvfiles List of csv files to convert to data frame.
#' @return A data frame combining the loaded csv files.
#' @export
readAKNcsv <- function(csvfiles) {
  dflist <- lapply(csvfiles, function(x) read.delim(x, quote=""))
  alldf <- do.call(rbind, dflist)
  alldf
}

#' Read refuge shapefile into sp data object
#'
#' @param refugedsn File location of refuge shapefile (.shp)
#' @param layer Name of layer within shapefile
#' @return sp polygon for refuge
#' @export
readrefuge <- function(refugedsn, layer) {
  rgdal::readOGR(dsn=refugedsn, layer=layer)
}

#' Return sp points of AKNs within the refuge
#'
#' @param refuge Refuge sp data object
#' @param AKNs Data frame giving AKN locations
#' @return Data frame of observation points within refuge
#' @export
refugeAKNs <- function(refuge, AKNs) {
  coordinates(AKNs) <- c("LONGITUDE", "LATITUDE")
  proj4string(AKNs) <- sp::proj4string(refuge)
  AKNs$refuge <- sp::over(AKNs, refuge)$LABEL_NAME
  inside_refuge <- !is.na(sp::over(AKNs, as(refuge, "SpatialPolygons")))
  refugepts <- AKNs[inside_refuge,]
  refugepts
}

#' Return proportion of AKNs that are within the refuge
#'
#' @param refuge Refuge sp data object
#' @param AKNs Data frame giving AKN locations
#' @return Scalar proportion of AKNs within refuge
#' @export
refugeAKNfraction <- function(refuge, AKNs) {
  coordinates(AKNs) <- c("LONGITUDE", "LATITUDE")
  proj4string(AKNs) <- sp::proj4string(refuge)
  inside_refuge <- !is.na(sp::over(AKNs, as(refuge, "SpatialPolygons")))
  mean(inside_refuge)
}

#' Write out AKNs as a csv file
#'
#' @param AKNs Data frame giving AKN locations
#' @param fname Output filename
#' @return (empty)
#' @export
writeAKNcsv <- function(AKNs, fname) {
  write.csv(AKNs@data, file=fname, row.names=FALSE)
}

#' Write out AKNs as shapefile
#'
#' @param AKNs Data frame giving AKN locations
#' @param layer Output layer name
#' @param fname output filename
#' @return (empty)
#' @export
writeAKNshp <- function(AKNs, layer, fname) {
  writeOGR(AKNs, layer=layer, dsn=fname, driver="ESRI Shapefile")
}

#' Return most recent AKNs as a data table
#'
#' @param AKNs Data frame giving AKN locations
#' @return Data frame with most recent AKNs
#' @export
recentAKNs <- function(AKNs) {
  AKNdt <- data.table::data.table(AKNs)
  result <- AKNdt[,list(max = tail(OBSERVATION.DATE, 1), count = length(OBSERVATION.DATE)),by = list(refuge, COMMON.NAME, SCIENTIFIC.NAME, SUBSPECIES.COMMON.NAME, SUBSPECIES.SCIENTIFIC.NAME, CATEGORY)]
  result
}
