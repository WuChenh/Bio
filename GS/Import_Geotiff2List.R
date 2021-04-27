Import_Geotiff2List <- function(directory) {
  library(raster)
  files <- list.files(path = directory, pattern = '.tif$')
  out_list <- list()
  for (f in 1:length(files)) {
    file_path <- paste(directory, files[f], sep = "")
    out_list[[f]] <- raster(file_path)
  }
  names(out_list) <- files
  return(out_list)
}
