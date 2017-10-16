library(data.table)
library(ncdf4)
library(reshape2)
library(maps)
library(maptools)
library(zoo)
library(scalegram)
library(geosphere)

local_path = "C:/Users/markonis/Documents/Data/Precipitation/MSWEP/" 

#Raster approach
#library(raster)
#mswep_month_ras = brick(x = 'MSWEP_monthly_050deg.nc')
#coarse.grid = expand.grid(grid.lat, grid.lon)
#mswep_month_coarse = t(extract(mswep_month_ras[[1]], SpatialPoints(coarse.grid)))

mswep_lowres = readRDS(paste0(local_path, "MSWEP_5x5_day.Rds"))

#Prepare data

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
mswep_lowres[precip != 0, pr_mon_75 := quantile(precip, 0.75), by = list(id, month, year)]
mswep_lowres[, pr_year := sum(precip), by = list(id, year)]
mswep_lowres[, pr_year_mean := mean(pr_year), by = id]
mswep_lowres[, pr_24h_max := max(precip), by = list(year, id)]
mswep_lowres[precip != 0, pr_24h_75 := quantile(precip, 0.75), by = list(year, id)] # Quantile 0.75
mswep_lowres[precip >= pr_24h_75, pr_24h_75_year := sum(precip, na.rm = T), by = list(year, id)] # Sum of precip above 0.75 quantile
mswep_lowres[, hvy_pr_per_year := pr_24h_75_year/pr_year, by = list(year, id)]
mswep_lowres[precip == 0, dry_days_a := .N, by = list(id, year)]
mswep_lowres[, dry_days := mean(dry_days_a, na.rm=T), list(id, year)]
mswep_lowres[, dry_days_a :=NULL]
mswep_lowres[, pr_intens_year := pr_year/(365-dry_days), by = list(id, year)]
mswep_lowres[is.na(pr_intens_year) | pr_intens_year < 0 | is.infinite(pr_intens_year), pr_intens_year :=0, by = list(id, year)]

##Land estimation

lon = mswep_lowres[, unique(lon)]
lat = mswep_lowres[, unique(lat)]
mswep_points <- expand.grid(lon, lat)
pts <- SpatialPoints(mswep_points, proj4string=CRS(proj4string(wrld_simpl)))
land <- !is.na(over(pts, wrld_simpl)$FIPS)
mswep_points = data.table(cbind(mswep_points, land))
colnames(mswep_points)[1:2] = c("lon", "lat")
mswep_points$id = mswep_lowres[, unique(id)]
setcolorder(mswep_points, c("id", "lat", "lon", "land"))

for(i in 1:nrow(mswep_points)){ #estimate area of each 0.5 grid cell
  temp = with(mswep_points[i,], expand.grid(c(lon, lon + 0.5 ), c(lat, lat + 0.5)))
  temp = temp[c(4, 2, 1,3),]
  mswep_points$area[i] = areaPolygon(temp)/1000000 # area in km^2 * 100 -> Area of Earth [validated for total and land]
}

##Estimation of areal precipitation
mswep_lowres = merge(mswep_lowres, mswep_points)
mswep_lowres = mswep_lowres[order(mswep_lowres$id),]
mswep_lowres[, pr_areal := precip * 0.001 * area] # m^3 x 10^6
mswep_lowres[, pr_areal_year := sum(pr_areal),  list(id, year) ] 


#saveRDS(mswep_lowres, file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))
#saveRDS(mswep_points, file = paste0(local_path, "MSWEP_5x5_day_points.Rds"))

rm(mswep_lowres)

