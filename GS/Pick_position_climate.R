Pick_position_climate <- function(geotiff, sampleSet) {
  num_sample <- nrow(sampleSet[[3]])
  whichColLong <- which(colnames(sampleSet[[3]])=="Longitude")
  whichColLati <- which(colnames(sampleSet[[3]])=="Latitude")
  num_coords <- nrow(geotiff[[1]]@coords)
  num_climate_category <- length(geotiff)
  nam_climate_category <- names(geotiff)
  samplePosition <- matrix(NA, num_sample, num_climate_category,
                           dimnames = list(NULL, nam_climate_category))
  y_unique <- unique(geotiff[[1]]@coords[,2])
  d2 <- abs(as.numeric((y_unique[3] - y_unique[2])*0.5))
  for (s in 1:num_sample) {
    if (as.character(sampleSet[[3]][s, whichColLong])=="Not available"){next}
    if (as.character(sampleSet[[3]][s, whichColLati])=="Not available"){next}
    s_x <- as.numeric(as.character(sampleSet[[3]][s, whichColLong]))
    s_y <- as.numeric(as.character(sampleSet[[3]][s, whichColLati]))
    ##Binary Search y BEGIN
    guess_y_row <- floor(num_coords/2)
    guess_y_row_sub = floor(guess_y_row/2)
    ifexist <- 0
    repeat{
      if (guess_y_row_sub < 1) { break }
      if (as.numeric(abs(geotiff[[1]]@coords[guess_y_row, 2] - s_y)) <= d2) {
        ifexist <- 1
        break
      }
      if (s_y < (geotiff[[1]]@coords[guess_y_row, 2] - d2)) {
        guess_y_row <- ceiling(guess_y_row + guess_y_row_sub)
      }
      else if (s_y > (geotiff[[1]]@coords[guess_y_row, 2] + d2)){
        guess_y_row <- floor(guess_y_row - guess_y_row_sub)
      }
      guess_y_row_sub = guess_y_row_sub/2
    }
    if (ifexist==0) { next }
    ##Binary Search y END
    #y lower
    for (y_r in guess_y_row:1) {
      if (y_r==1) {
        y_r_begin <- y_r
        break
      }
      if (as.numeric(abs(geotiff[[1]]@coords[y_r, 2] - s_y)) > d2) {
        y_r_begin <- y_r + 1
        break
      }
    }
    #y upper
    for (y_r in guess_y_row:num_coords) {
      if (y_r==num_coords) {
        y_r_end <- y_r
        break
      }
      if (as.numeric(abs(geotiff[[1]]@coords[y_r, 2] - s_y)) > d2) {
        y_r_end <- y_r - 1
        break
      }
    }
    ##x BEGIN
    ifexist <- 0
    for (r_x in y_r_begin:y_r_end) {
      if (abs(geotiff[[1]]@coords[r_x, 1] - s_x) <= d2){
        guess_x_row <- r_x
        ifexist <- 1
        break
      }
    }
    if (ifexist==0) { next }
    #x END
    samplePosition[s, 1] <- geotiff[[1]]@data[guess_x_row, 1]
    for (ca in 2:num_climate_category) {
      samplePosition[s, ca] <- geotiff[[ca]]@data[guess_x_row, 1]
    }
  }
  return(samplePosition)
}

