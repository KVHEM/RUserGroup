library(data.table)
library(ncdf4)
library(reshape2)
library(maps)
library(maptools)
library(zoo)
library(scalegram)
library(geosphere)

local_path = "C:/Users/markonis/Documents/Data/Precipitation/MSWEP/" 
data(wrld_simpl)

#Raster approach
#library(raster)
#mswep_month_ras = brick(x = 'MSWEP_monthly_050deg.nc')
#coarse.grid = expand.grid(grid.lat, grid.lon)
#mswep_month_coarse = t(extract(mswep_month_ras[[1]], SpatialPoints(coarse.grid)))

mswep_eu = readRDS(paste0(local_path, "MSWEP_eu_day.Rds"))
mswep_eu[, time := as.Date("1900/1/1") + time - 1]
mswep_eu[, day := yday(time)]
mswep_eu[, year := year(time)]
mswep_eu[, time := NULL]
mswep_eu[, id := .GRP, by = list(lat, lon)]

##Land estimation | Use only land points
lon = mswep_eu[, unique(lon)]
lat = mswep_eu[, unique(lat)]
mswep_points_eu <- expand.grid(lon, lat)
pts <- SpatialPoints(mswep_points_eu, proj4string=CRS(proj4string(wrld_simpl)))
land <- !is.na(over(pts, wrld_simpl)$FIPS)
mswep_points_eu = data.table(cbind(mswep_points_eu, land))
colnames(mswep_points_eu)[1:2] = c("lon", "lat")
mswep_points_eu$id = mswep_eu[, unique(id)]
setcolorder(mswep_points_eu, c("id", "lat", "lon", "land"))

for(i in 1:nrow(mswep_points_eu)){ #estimate area of each 0.5 grid cell
  temp = with(mswep_points_eu[i,], expand.grid(c(lon, lon + 0.5 ), c(lat, lat + 0.5)))
  temp = temp[c(4, 2, 1,3),]
  mswep_points_eu$area[i] = areaPolygon(temp)/1000000 # area in km^2 * 100 -> Area of Earth [validated for total and land]
}

mswep_eu = merge(mswep_eu, mswep_points_eu)
mswep_eu = mswep_eu[land == T]
mswep_eu = mswep_eu[order(mswep_eu$id),]
mswep_eu[, land := NULL]

mswep_points_eu = mswep_points_eu[land == T]
mswep_points_eu[,land := NULL]

#saveRDS(mswep_eu, file = paste0(local_path, "MSWEP_eu_day_basic.Rds"))

#Prepare data
mswep_eu[, pr_year := sum(precip), by = list(id, year)]
mswep_eu[, pr_year_mean := mean(pr_year), by = id]
mswep_eu[precip != 0, pr_24h_75 := quantile(precip, 0.75), by = list(year, id)] # Quantile 0.75
mswep_eu[precip >= pr_24h_75, pr_24h_75_year := sum(precip, na.rm = T), by = list(year, id)] # Sum of precip above 0.75 quantile
mswep_eu[, hvy_pr_per_year := pr_24h_75_year/pr_year, by = list(year, id)]
mswep_eu[precip == 0, dry_days_a := .N, by = list(id, year)]
mswep_eu[, dry_days := mean(dry_days_a, na.rm=T), list(id, year)]
mswep_eu[, dry_days_a :=NULL]
mswep_eu[, pr_intens_year := pr_year/(365-dry_days), by = list(id, year)]
mswep_eu[is.na(pr_intens_year) | pr_intens_year < 0 | is.infinite(pr_intens_year), pr_intens_year :=0, by = list(id, year)]

##Estimation of areal precipitation
mswep_eu[, pr_areal := precip * 0.001 * area] # m^3 x 10^6
mswep_eu[, pr_areal_year := sum(pr_areal),  list(id, year) ] 

saveRDS(mswep_eu, file = paste0(local_path, "MSWEP_eu_analysis.Rds"))
saveRDS(mswep_points_eu, file = paste0(local_path, "MSWEP_eu_day_points.Rds"))

rm(mswep_eu)

