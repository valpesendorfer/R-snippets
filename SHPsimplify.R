#' @title Helper function for simplifying shapefiles
#' @description
#' Simplifying shapefile until maximum vertex threshold is not exeeded
#' @param AOI area of interest in shapefile format
#' @return Simplified shapefile
SHPsimplify <- function(AOI,nmin){
  
  n <- raster::nrow(AOI@polygons[[1]]@Polygons[[1]]@coords)
  
  p <- 0.001
  t <- 0
  
  while (n > nmin){
    
    t <- t+p
    AOI <- rgeos::gSimplify(AOI,tol=t)
    n <- raster::nrow(AOI@polygons[[1]]@Polygons[[1]]@coords)
  }
  return(AOI)
}
