tmp <- list()
len <- length(geotiff_bio)
namelist <- c()
for (i in 1:len) {
  i_th_name <- paste(paste("bio_", i, sep = ""), ".tif", sep = "")
  namelist <- c(namelist, paste("wc_", paste("bio_", i, sep = ""), sep = ""))
  tmp[[i]] <- geotiff_bio[[grep(i_th_name, names(geotiff_bio))]]
}
names(tmp) <- namelist
geotiff_bio <- tmp
rm(tmp,i,i_th_name,len,namelist)