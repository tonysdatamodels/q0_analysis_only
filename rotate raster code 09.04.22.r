#q0 two different decades
decade1 <- raster ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\5. Research Assistant\\GIS\\World bands\\1988 to 1997\\Average\\Combined Average 1988 to 1997.tif")
decade2 <- raster ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\5. Research Assistant\\GIS\\World bands\\2008 to 2017\\Average\\Average 2008 to 2017.tif") # newly calculated to get 90 NS latitude required for rotation

#find difference in q0 between decades and write as a new raster file
diffhcq0 <- overlay(decade1, decade2, fun=function(r1, r2){return(r2-r1)}) # find difference between values
writeRaster(diffhcq0, overwrite = TRUE, ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\9. PhD GIS\\Table\\1988 to 2017 difference.tif" ))

#need older version of raster package to rotate "new" raster 
require(devtools) 
packageURL <- "https://cran.r-project.org/src/contrib/Archive/raster/raster_3.0-12.tar.gz" #older raster package
install.packages(packageURL,repos=NULL, type="source")
library(raster)

#rotate new raster using this code
your_raster <- raster ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\9. PhD GIS\\Table\\1988 to 2017 difference.tif")
rotated_raster <- rotate(your_raster) #might need 2019 version raster package
writeRaster(rotated_raster,overwrite = TRUE, ("C:\\Users\\tonyb\\OneDrive - Queen's University Belfast\\9. PhD GIS\\Table\\1988 to 2017 difference rotate.tif"))
plot(rotated_raster)