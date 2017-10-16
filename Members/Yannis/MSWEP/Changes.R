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

