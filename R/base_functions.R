require(sp)
require(rgdal)
require(data.table)

# read in AKN files into data frame
readAKNcsv <- function(csvfiles) {
  dflist <- lapply(csvfiles, function(x) read.delim(x, quote=""))
  alldf <- do.call(rbind, dflist)
  alldf
}

# read refuge shapefile into sp data object
readrefuge <- function(refugedsn, layer) {
  readOGR(dsn=refugedsn, layer=layer)
}

# return sp points of AKNs within the refuge
refugeAKNs <- function(refuge, AKNs) {
  coordinates(AKNs) <- c("LONGITUDE", "LATITUDE")
  proj4string(AKNs) <- proj4string(refuge)
  AKNs$refuge <- over(AKNs, refuge)$LABEL_NAME
  inside_refuge <- !is.na(over(AKNs, as(refuge, "SpatialPolygons")))
  refugepts <- AKNs[inside_refuge,] 
  refugepts
}

# return proportion of AKNs that are within the refuge
refugeAKNfraction <- function(refuge, AKNs) {
  coordinates(AKNs) <- c("LONGITUDE", "LATITUDE")
  proj4string(AKNs) <- proj4string(refuge)
  inside_refuge <- !is.na(over(AKNs, as(refuge, "SpatialPolygons")))
  mean(inside_refuge)
}

# write out AKNs as a csv file
writeAKNcsv <- function(AKNs, fname) {
  write.csv(AKNs@data, file=fname, row.names=FALSE)
}

# write out AKNs as shapefile
writeAKNshp <- function(AKNs, layer, fname) {
  writeOGR(AKNs, layer=layer, dsn=fname, driver="ESRI Shapefile")
}

# return most recent AKNs as a data table
recentAKNs <- function(AKNs) {
  AKNdt <- data.table(AKNs)
  result <- AKNdt[,list(max = tail(OBSERVATION.DATE, 1), count = length(OBSERVATION.DATE)),by = list(refuge, COMMON.NAME, SCIENTIFIC.NAME, SUBSPECIES.COMMON.NAME, SUBSPECIES.SCIENTIFIC.NAME, CATEGORY)]
  result
}
