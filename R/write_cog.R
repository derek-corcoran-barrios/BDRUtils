#' Write Cloud Optimized Geotiff (COG)
#'
#' This function takes a SpatRaster object and saves it as a Cloud Optimized Geotiff (COG).
#' COGs are geospatial files optimized for efficient cloud storage and retrieval.
#'
#' @param SpatRaster A SpatRaster object (class SpatRaster) representing the raster data to be saved as a COG.
#' @param Name The desired name for the COG file, including the ".tif" extension.
#' @return A Cloud Optimized Geotiff saved at the specified location.
#'
#' @examples
#' # Load required libraries if not already loaded
#' # library(terra)
#'
#' # Create a sample SpatRaster
#' r <- terra::rast(nrows = 5, ncols = 5, vals = 1:25)
#'
#' # Specify a filename for the COG file
#' cog_filename <- "test.tif"
#' file.remove("test.tif")
#' file.remove("test.tfw")
#' # Save the SpatRaster as a COG
#' write_cog(SpatRaster = r, Name = cog_filename)
#'
#' @importFrom terra writeRaster
#' @export
#'

write_cog <- function(SpatRaster, Name){
  terra::writeRaster(x = SpatRaster,
                     filename = Name, overwrite=TRUE,
                     gdal=c("COMPRESS=DEFLATE", "TFW=YES","of=COG"))
}
