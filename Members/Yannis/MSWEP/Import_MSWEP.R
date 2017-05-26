nc.path = "C:/Users/markonis/Documents/R/Projects/RUserGroup/Members/Yannis/MSWEP"
shp.path = "C:/Users/markonis/Documents/R/Projects/OWDA"
out.path = "C:/Users/markonis/Documents/R/Projects/RUserGroup/Members/Yannis/MSWEP"
my.crs = "+proj=longlat +ellps=WGS84"

library(data.table)
library(raster)
library(RCurl) 

login.id = "e2o_guest"
login.pwd = "oowee3WeifuY1aeb"
lat = c(34.25, 60.25) 
lon = c(-10.75, 38.25) 
cz.cords = c(12.25 , 19.25, 48.25, 51.25)
mswep.url = paste("ftp://",login.id,":",login.pwd,"@wci.earth2observe.eu/data/primary/public/jrc/MSWEP_V1.2/3hourly/", sep = "")
ftp.files = getURL(mswep.url, dirlistonly = TRUE) #Get all filenames in ftp address
ftp.files = unlist(strsplit(ftp.files, "\r\n")) 

#Download each month and crop Europe
for(i in 1:length(ftp.files)){
  download.file(paste(mswep.url, ftp.files[i], sep =""), "imp_month.nc", mode = "wb")  # binary file types are transferred with mode = "wb".
  mswep.ras.eu = nc.brick('imp_month.nc', lon, lat)
  writeRaster(mswep.ras.eu, ftp.files[i], varname= "precipitation", varunit = "mm",  zname = "time", overwrite=TRUE) 
  print(i)
}

#Create ncdf for Czech Republic
mswep.ras.cz = brick('data/197901.nc')
mswep.ras.cz = crop(mswep.ras.cz, extent(cz.cords))
for(i in 2:length(ftp.files)){
  mswep.ras.cz.tmp = brick(paste('data/', ftp.files[i], sep = ""))
  mswep.ras.cz.tmp = crop(mswep.ras.cz.tmp, extent(cz.cords))
  mswep.ras.cz= addLayer(mswep.ras.cz, mswep.ras.cz.tmp)
  print(i)
}
save(mswep.ras.cz, file = "mswep_cz.Rdata")
writeRaster(mswep.ras.cz, 'mswep_cz.nc', varname= "precipitation", varunit = "mm",  zname = "time", overwrite=TRUE) 