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


## 
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

mswep_lowres = merge(mswep_lowres, mswep_points)
mswep_lowres = mswep_lowres[order(mswep_lowres$id),]
mswep_lowres[, pr_areal := precip * 0.001 * area] # m^3 x 10^6
mswep_lowres[, pr_areal_year := sum(pr_areal),  list(id, year) ] 
mswep_lowres[, pr_intens_year := pr_areal_year/(365-dry_days), by = list(id, year)]

#saveRDS(mswep_lowres, file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))
mswep_lowres = readRDS(file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))


###############
#Petit tests
###############

#Annual

test.break = mswep_lowres[, .(id, lat, lon, year, precip.year)]
test.break = unique(test.break) 
test.break[, br.pt := pettitt.test(precip.year)$estimate[1], id]
test.break[, br.pt.p := pettitt.test(precip.year)$p.value[1], id]
test.break.pet = test.break[br.pt.p <0.05, .(lat, lon, br.pt)]
test.break.pet = unique(test.break.pet) 
table(test.break.pet$br.pt)
barplot(table(test.break.pet$br.pt))
map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, test.break.pet[br.pt <= 22 & br.pt >= 16], col="red", pch=16)

#Day max

test.break = mswep_lowres[, .(id, lat, lon, year, precip.24h.max)]
test.break = unique(test.break) 
test.break[, br.pt := pettitt.test(precip.24h.max)$estimate[1], id]
test.break[, br.pt.p := pettitt.test(precip.24h.max)$p.value[1], id]
test.break.pet = test.break[br.pt.p <0.05, .(lat, lon, br.pt)]
test.break.pet = unique(test.break.pet) 
table(test.break.pet$br.pt)
barplot(table(test.break.pet$br.pt))
map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, test.break.pet[br.pt <= 22 & br.pt >= 16], col="red", pch=16)

#0.75 quantile

test.break = mswep_lowres[, .(id, lat, lon, year, precip.24h.75)]
test.break = unique(test.break) 
test.break[, br.pt := pettitt.test(precip.24h.75)$estimate[1], id]
test.break[, br.pt.p := pettitt.test(precip.24h.75)$p.value[1], id]
test.break.pet = test.break[, .(lat, lon, br.pt, br.pt.p)]
test.break.pet = unique(test.break.pet) 
table(test.break.pet$br.pt)
barplot(table(test.break.pet$br.pt))

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, test.break.pet[br.pt <= 22 & br.pt >= 19 & br.pt.p < 0.1], col="orange ", pch=16)
points(lat~lon, test.break.pet[br.pt <= 22 & br.pt >= 19 & br.pt.p < 0.05], col="red", pch=16)
points(lat~lon, test.break.pet[br.pt <= 22 & br.pt >= 19 & br.pt.p < 0.01], col="dark red", pch=16)

#Comparison between -2000 and 2000- monthly means

test.mon = unique(mswep_lowres[, .(id, lat, lon, time, precip.mon, precip.mon.75)])
test.mon[, time:= as.yearmon(time)]
test.mon = test.mon[!duplicated(test.mon)]

test.mon$time <- as.Date(test.mon$time)
id.number = length(unique(test.mon$id))
alldates  = seq.Date(min(test.mon$time), max(test.mon$time), by="month")
alldates  = data.table(expand.grid(alldates, unique(test.mon$id))) 
colnames(alldates) = c("time", "id")
test.mon = merge(test.mon,  alldates, by=c("time", "id"), all.y = TRUE)
test.mon[is.na(precip.mon), precip.mon := 0]
test.mon[is.na(precip.mon.75), precip.mon.75 := 0]
test.mon[, lat := max(lat, na.rm = T), id] 
test.mon[, lon := max(lon, na.rm = T), id] 
test.mon[, year:= year(time)] 

test.mon.mean.compare = test.mon[year < 2000, mean(precip.mon, na.rm = T), list(lat,lon)]
aa =  test.mon[year >= 2000, mean(precip.mon, na.rm = T), list(lat,lon)]
test.mon.mean.compare$V1 = aa$V1/ test.mon.mean.compare$V1 
colnames(test.mon.mean.compare)[3] = "mean.ratio"

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))

points(lat~lon, test.mon.mean.compare[mean.ratio<0.8],  col="orange", pch=16)

points(lat~lon, test.mon.mean.compare[mean.ratio>1.25],  col="orange", pch=16)
points(lat~lon, test.mon.mean.compare[mean.ratio>1.5],  col="red", pch=16)
points(lat~lon, test.mon.mean.compare[mean.ratio>2],  col="dark red", pch=16)

#Comparison between -2000 and 2000- monthly variances

test.mon.var.compare = test.mon[year < 2000, var(precip.mon, na.rm = T), list(lat,lon)]
aa =  test.mon[year >= 2000, var(precip.mon, na.rm = T), list(lat,lon)]
test.mon.var.compare$V1 = aa$V1/ test.mon.var.compare$V1 
colnames(test.mon.var.compare)[3] = "var.ratio"

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, test.mon.var.compare[var.ratio>1.25],  col="orange", pch=16)
points(lat~lon, test.mon.var.compare[var.ratio>1.5],  col="red", pch=16)
points(lat~lon, test.mon.var.compare[var.ratio>2],  col="dark red", pch=16)

#Comparison between -2000 and 2000- monthly quantiles 0.75

test.mon.q75.compare = test.mon[year < 2000, mean(precip.mon.75, na.rm = T), list(lat,lon)]
aa =  test.mon[year >= 2000, mean(precip.mon.75, na.rm = T), list(lat,lon)]
test.mon.q75.compare$V1 = aa$V1/ test.mon.q75.compare$V1 
colnames(test.mon.q75.compare)[3] = "q75.ratio"

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, test.mon.q75.compare[q75.ratio>1.25],  col="orange", pch=16)
points(lat~lon, test.mon.q75.compare[q75.ratio>1.5],  col="red", pch=16)
points(lat~lon, test.mon.q75.compare[q75.ratio>2],  col="dark red", pch=16)

#Combination of all tests & statistics of valid points

valid.points = merge(test.mon.mean.compare, test.mon.var.compare, by = c("lon", "lat"), all = T)
valid.points = merge(valid.points, test.mon.q75.compare, all = T)
valid.points = merge(valid.points, test.break.pet, all = T)

hist(valid.points$mean.ratio[valid.points$mean.ratio<1.25 & valid.points$mean.ratio > 0.75 ], breaks = 120, col = "tan", main = "mean", xlab = "ratio")
hist(valid.points$var.ratio[valid.points$var.ratio<1.25 & valid.points$var.ratio> 0.75], breaks = 120, col = "tan", main = "var", xlab = "ratio")
hist(valid.points$q75.ratio[valid.points$q75.ratio<1.25 & valid.points$q75.ratio> 0.75], breaks = 120, col = "tan", main = "q75", xlab = "ratio")

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, valid.points[, .(lat, lon)],  col="red", pch=16)
points(lat~lon, valid.points[mean.ratio < 1.5 & var.ratio < 1.5 & q75.ratio < 1.5, .(lat, lon)],  col="orange", pch=16)
points(lat~lon, valid.points[mean.ratio < 1.25 & var.ratio <  1.25 & q75.ratio <  1.25, .(lat, lon)],  col="green", pch=16)
points(lat~lon, valid.points[mean.ratio < 1.1 & var.ratio < 1.1 & q75.ratio < 1.1, .(lat, lon)],  col="dark green", pch=16)

valid.points.25 = valid.points[mean.ratio < 1.25 & var.ratio <  1.25 & q75.ratio <  1.25, .(lat, lon, br.pt, br.pt.p)]

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, valid.points.25,  col="dark green", pch=16)
points(lat~lon, valid.points.25[br.pt <= 22 & br.pt >= 19 & br.pt.p<0.05],  col="red", pch=16)

##############
#Slopes
##############

slope.fast = function(y, x = 1:length(y)){ 
  x = cbind(x,y)
  x = x[!is.na(x[,2]),]
  sl = lm.fit(x=cbind(x[,1],1), y=x[,2])
  return(sl)
}

slopes = mswep_lowres[,.(id, lat, lon, year, precip.year)]
slopes = unique(slopes)
slopes[, mean_precip := mean(precip.year), id]
slopes[, slopes.year := coef(slope.fast(precip.year, year))[1], by = id]
slopes[, slopes.year_rel := slopes.year/mean_precip]
slopes = slopes[!duplicated(slopes$id)]

names(slopes)[2] = "precip_year"

map("world", fill=TRUE, col="white", bg="lightgrey", ylim=c(-60, 90), mar=c(0,0,0,0))
points(lat~lon, slopes[, .(lat, lon)],  col="grey", pch=16)
points(lat~lon, slopes[slopes.year_rel > 0.0025],  col="light blue", pch=16)
points(lat~lon, slopes[slopes.year_rel > 0.005],  col="blue", pch=16)
points(lat~lon, slopes[slopes.year_rel > 0.01],  col="dark blue", pch=16)
points(lat~lon, slopes[slopes.year_rel < -0.0025],  col="orange", pch=16)
points(lat~lon, slopes[slopes.year_rel < -0.005],  col="red", pch=16)
points(lat~lon, slopes[slopes.year_rel < -0.01],  col="dark red", pch=16)

####################
#Zonal Means
####################

zonal_sum_year = mswep_lowres[, sum(pr_areal), list(year, lat)]
colnames(zonal_sum_year)[3] = "precip"
xyplot(precip~year|lat, zonal_sum_year, type = c('l', 'r'), scales = list(relation = "free"))
xyplot(precip~year, zonal_sum_year, type = c('l', 'r'), groups = lat, scales = list(relation = "free"))

zonal_dry_year = mswep_lowres[, mean(dry_days, na.rm = T), list(year, lat)]
colnames(zonal_dry_year )[3] = "dry_days"
xyplot(dry_days~year|lat, zonal_dry_year, type = c('l', 'r'), scales = list(relation = "free"))

zonal_intens_year = mswep_lowres[, mean(pr_intens_year, na.rm = T), list(year, lat)]
colnames(zonal_intens_year )[3] = "intensity"
xyplot(intensity~year|lat, zonal_intens_year, type = c('l', 'r'), scales = list(relation = "free"))

#Land vs Ocean

zonal_sum_year_land = mswep_lowres[land == T, sum(pr_areal), list(year, lat)]
zonal_sum_year_ocean = mswep_lowres[land == F, sum(pr_areal), list(year, lat)]
xyplot(V1~year|lat, zonal_sum_year_land, type = c('l', 'r'), scales = list(relation = "free"))
xyplot(V1~year|lat, zonal_sum_year_ocean, type = c('l', 'r'), scales = list(relation = "free"))

zonal_dry_year_land = mswep_lowres[land == T & precip == 0, mean(.N), list(year, lat)]


#Scalegram

single_point = mswep_lowres[lat == 0.25 & lon == 2.25, .(year, precip)]
scalegram(single_point, MODE = "s2")

test_dt_scale = single_point[, scalegram(precip), year]
colnames(test_dt_scale)[1] = "variable" 
plot_scalegram(test_dt_scale)

single_zone = mswep_lowres[lon == 2.25 & lat <10 & lat> -10, .(lat, precip)]
test_dt_scale = single_zone[, scalegram(precip, threshold = 100), lat]
plot_scalegram(test_dt_scale)

single_zone = mswep_lowres[lat == 0.25, precip := mean(precip), list(year, lon)]
single_zone =single_zone[,.(year, precip)]
colnames(single_zone)[1] = "variable" 
colnames(single_zone)[2] = "y_scale" 
plot_scalegram(single_zone, MODE = "s2")


rm(mswep_lowres)

