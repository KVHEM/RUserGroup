local_path = "C:/Users/markonis/Documents/Data/Precipitation/MSWEP/24h_ver2.0/" 

library(data.table)
library(raster)
library(RCurl) 
library(ncdf4) 
<<<<<<< HEAD

# Version 2.01
#################################################################################

mswep_url_3h = "http://hydrology.princeton.edu/data/hylkeb/MSWEP_V2.01/3hourly_010deg/" #3h at 0.1 deg
mswep_url_d = "http://hydrology.princeton.edu/data/hylkeb/MSWEP_V2.01/daily_050deg/" #daily at 0.5 deg

file_list = vector() #Create file names
=======
library(ncdf.tools) 
library(RNetCDF) 

# Version 2.0
#################################################################################

local_path = "C:/Users/markonis/Documents/Data/Precipitation/MSWEP/24h_ver2.0/" 
mswep_url_3h = "http://hydrology.princeton.edu/data/hylkeb/MSWEP_V2.0/3hourly_010deg/" #3h at 0.1 deg
mswep_url_d = "http://hydrology.princeton.edu/data/hylkeb/MSWEP_V2.0/daily_050deg/" #daily at 0.5 deg

file_list = vector()
>>>>>>> ccc0cbd3638197f4b90ae80eb17b71a244a83e78
for(i in 1979:2016){
  for(j in 1:9){
    file_list[j+(i-1979)*12] = paste0(i, "0", j, ".nc")
  }
  for(j in 10:12){
    file_list[j+(i-1979)*12] = paste0(i, j, ".nc")
  }
}

<<<<<<< HEAD
lowres_lat = seq(-89.75, 89.75, 5)    #Coarse resolution grid coordinates
lowres_lon = seq(-197.75, 197.75, 5) 

for(i in 1:length(file_list)){        #Downloader to local path
  fname = paste0(mswep_url_d, file_list[i])
  download.file(fname, paste0(local_path, file_list[i]), mode = "wb", quiet = T)  # binary file types are transferred with mode = "wb".
}  

############ Daily coarse 
mswep_nc = nc_open(paste0(local_path, file_list[1]))  #Standard raster approach did not work
=======
lowres_lat = seq(-89.75, 89.75, 5) 
lowres_lon = seq(-197.75, 197.75, 5) 

############ Daily coarse
mswep_nc = nc_open(paste0(local_path, file_list[1]))
>>>>>>> ccc0cbd3638197f4b90ae80eb17b71a244a83e78
mswep = ncvar_get(mswep_nc, "precipitation")
dimnames(mswep)[[3]] = mswep_nc$dim$time$vals
dimnames(mswep)[[2]] = mswep_nc$dim$lat$vals 
dimnames(mswep)[[1]] = mswep_nc$dim$lon$vals
kk = nc_close(mswep_nc)
mswep_lowres = data.table(melt(mswep, varnames=c("lon","lat","time"), value.name="precip"))
mswep_lowres = mswep_lowres[lat %in% lowres_lat & lon %in% lowres_lon]
rm(kk, mswep, mswep_nc); gc()

for(i in 2:length(file_list)){
  mswep_nc = nc_open(paste0(local_path, file_list[i]))
  mswep = ncvar_get(mswep_nc, "precipitation")
  dimnames(mswep)[[3]] = mswep_nc$dim$time$vals
  dimnames(mswep)[[2]] = mswep_nc$dim$lat$vals 
  dimnames(mswep)[[1]] = mswep_nc$dim$lon$vals
  kk = nc_close(mswep_nc)
  mswep_lowres_temp = data.table(melt(mswep, varnames=c("lon","lat","time"), value.name="precip"))
  mswep_lowres_temp = mswep_lowres_temp[lat %in% lowres_lat & lon %in% lowres_lon]
  mswep_lowres = rbind(mswep_lowres, mswep_lowres_temp)
  rm(kk, mswep, mswep_nc); gc()
  print(i)
}

saveRDS(mswep_lowres, file = "MSWEP_5x5_day.Rds")

<<<<<<< HEAD
############ 3h coarse for single file - loop is need here
coarse.grid = expand.grid(lowres_lon, lowres_lat)

download.file(paste0(mswep_url_3h, file_list[1], sep =""), "imp_month_3h.nc", mode = "wb")  
=======
############ 3h coarse
coarse.grid = expand.grid(lowres_lon, lowres_lat)

download.file(paste(mswep.url, ftp.files[1], sep =""), "imp_month_3h.nc", mode = "wb")  # binary file types are transferred with mode = "wb".
>>>>>>> ccc0cbd3638197f4b90ae80eb17b71a244a83e78
mswep.ras.coarse = brick(x = 'imp_month_3h.nc', path = '"C:/Users/markonis/Documents/R/Projects/MSWEP"')

bb = t(extract(mswep.ras.coarse, SpatialPoints(coarse.grid)))

nnames = apply(coarse.grid, 1, paste, collapse = " ")
colnames(bb) = nnames

bb.t = data.table(melt(bb))
bb.t = bb.t[complete.cases(bb.t)]
bb.t[, year := as.numeric(substr(Var1, 2, 5))]
bb.t[, month := as.numeric(substr(Var1, 7, 8))]
bb.t[, day := as.numeric(substr(Var1, 10, 11))]
bb.t[, precip_day := sum(value), list(year, month, day, Var2)]
bb.t[, Var2 := as.character(Var2)]
bb.t[, lon := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
bb.t[, lat := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]

<<<<<<< HEAD
saveRDS(bb.t, file = "MSWEP_5x5_3h.Rds")
=======

>>>>>>> ccc0cbd3638197f4b90ae80eb17b71a244a83e78

# Version 1.2
#################################################################################

login.id = "e2o_guest"
login.pwd = "oowee3WeifuY1aeb"
lat = c(34.25, 60.25) 
lon = c(-10.75, 38.25) 
cz.cords = c(12.25 , 19.25, 48.25, 51.25)
mswep.url = paste("ftp://",login.id,":",login.pwd,"@wci.earth2observe.eu/data/primary/public/jrc/MSWEP_V1.2/3hourly/", sep = "")
ftp.files = getURL(mswep.url, ftp.use.epsv = FALSE, dirlistonly = TRUE) #Get all filenames in ftp address
ftp.files = unlist(strsplit(ftp.files, "\r\n")) 

#Download each month and get 5 x 5 grid
grid.lat = seq(-90,90,5) 
grid.lon = seq(-180,180,5) 
coarse.grid = expand.grid(grid.lat, grid.lon)

download.file(paste(mswep.url, ftp.files[1], sep =""), "imp_month.nc", mode = "wb")  # binary file types are transferred with mode = "wb".
mswep.ras.coarse = brick(x = 'imp_month.nc', path = 'C:/Users/markonis/Documents/R/Projects/RUserGroup')

bb = t(extract(mswep.ras.coarse, SpatialPoints(coarse.grid)))
bb[bb<0.5] = 0
nnames = apply(coarse.grid,1,paste, collapse = " ")
colnames(bb) = nnames

for(i in 2:length(ftp.files)){
  download.file(paste(mswep.url, ftp.files[i], sep =""), "imp_month.nc", mode = "wb")  # binary file types are transferred with mode = "wb".
  mswep.ras.coarse = brick(x = 'imp_month.nc', path = 'C:/Users/markonis/Documents/R/Projects/RUserGroup')
  aa = t(extract(mswep.ras.coarse, SpatialPoints(coarse.grid)))
  aa[aa<0.5] = 0
  bb = rbind(bb, aa)
  print(i)
}

saveRDS(bb.dt, file = "MSWEP_5x5_int.Rds")

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