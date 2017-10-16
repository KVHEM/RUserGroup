library(data.table)
library(raster)
library(ncdf4)
library(reshape2)
library(trend)
library(maps)
library(maptools)
library(zoo)
library(latticeExtra)
library(scalegram)
library(geosphere)
data(wrld_simpl)

local_path = "C:/Users/markonis/Documents/Data/Precipitation/MSWEP/" 
devtools::install_git("https://github.com/imarkonis/scalegram.git", branch = "develop")

#Raster approach
#mswep_month_ras = brick(x = 'MSWEP_monthly_050deg.nc')
#coarse.grid = expand.grid(grid.lat, grid.lon)
#mswep_month_coarse = t(extract(mswep_month_ras[[1]], SpatialPoints(coarse.grid)))

mswep_lowres = readRDS(paste0(local_path, "MSWEP_5x5_day.Rds"))

#EDA: Prepare data

mswep_lowres[, id := .GRP, by = list(lat, lon)]
mswep_lowres[, time.abs :=time]
mswep_lowres[, time := as.Date("1900/1/1") + time.abs - 1]
mswep_lowres[, counts := .N, by = id]
mswep_lowres = mswep_lowres[counts > 30]
mswep_lowres[, year := year(time)]
mswep_lowres[, month := month(time)]
mswep_lowres[, day := yday(time)]

#saveRDS(mswep_lowres, file = paste0(local_path, "MSWEP_5x5_day_basic.Rds"))

mswep_lowres[, pr_mon := sum(precip), by = list(id, month, year)]
mswep_lowres[, pr_mon_75 := quantile(precip, 0.75), by = list(id, month, year)]
mswep_lowres[, pr_year := sum(precip), by = list(id, year)]
mswep_lowres[, pr_24h_max := max(precip), by = list(year, id)]
mswep_lowres[, pr_24h_75 := quantile(precip, 0.75), by = list(year, id)]
mswep_lowres[precip == 0, dry_days := .N, by = list(id, year)]

##land estimation
lon = mswep_lowres[, unique(lon)]
lat = mswep_lowres[, unique(lat)]
mswep_points <- expand.grid(lon, lat)
pts <- SpatialPoints(points, proj4string=CRS(proj4string(wrld_simpl)))
land <- !is.na(over(pts, wrld_simpl)$FIPS)
mswep_points = data.table(cbind(mswep_points, land))
colnames(mswep_points)[1:2] = c("lon", "lat")
mswep_points$id = mswep_lowres[, unique(id)]
setcolorder(mswep_points, c("id", "lat", "lon", "land"))

plot(wrld_simpl)
points(pts, col=1+land, pch=16)

for(i in 1:nrow(mswep_points)){ #estimate area of each 0.5 grid cell
  temp = with(mswep_points[i,], expand.grid(c(lon, lon + 0.5 ), c(lat, lat + 0.5)))
  temp = temp[c(4, 2, 1,3),]
  mswep_points$area[i] = areaPolygon(temp)/1000000 # area in km^2
}

##estiamtion of areal precipitation
mswep_lowres = merge(mswep_lowres, mswep_points)
mswep_lowres = mswep_lowres[order(mswep_lowres$id),]
mswep_lowres[, pr_areal := precip * 0.001 * area] # m^3 x 10^6
mswep_lowres[, pr_areal_year := sum(pr_areal),  list(id, year) ] 
mswep_lowres[, pr_intens_year := pr_areal_year/(365-dry_days), by = list(id, year)]

#saveRDS(mswep_lowres, file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))
mswep_lowres = readRDS(file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))

rm(mswep_lowres)

