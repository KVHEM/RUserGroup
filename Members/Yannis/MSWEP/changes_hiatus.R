mswep_lowres = readRDS(file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))
mswep_points = readRDS(file = paste0(local_path, "MSWEP_5x5_day_points.Rds"))

slope_fast = function(y, x = 1:length(y)){
  sl = NA
  x = cbind(x,y)
  x = x[!is.na(x[,2]),]
  sl = try(lm.fit(x=cbind(x[,1],1), y=x[,2]), silent = TRUE)
  return(sl)
}

slope_fast_coef = function(y, x = 1:length(y)){
  sl = NA
  x = cbind(x,y)
  x = x[!is.na(x[,2]),]
  sl = try(lm.fit(x=cbind(x[,1],1), y=x[,2]), silent = TRUE)
  return(coef(sl)[1])
}


mswep_change_hiatus = mswep_lowres[,.(id, lat, lon, year, pr_year, pr_year_mean, pr_areal_year, 
                               dry_days, hvy_pr_per_year, pr_intens_year, land, area)]

mswep_change_hiatus = mswep_change_hiatus[year>=1998 & year <= 2013]

mswep_change_hiatus = mswep_change_hiatus[complete.cases(mswep_change_hiatus)]
mswep_change_hiatus = unique(mswep_change_hiatus)

mswep_change_hiatus_global = mswep_change_hiatus[, sum(pr_areal_year) * 100, year] # P x 10 x 10 since we have taken only one grid per 5deg
mswep_change_hiatus_land = mswep_change_hiatus[land == T, sum(pr_areal_year) * 100, year] # Orders of magnitude validated [m3 * 10^6]
mswep_change_hiatus_ocean = mswep_change_hiatus[land == F, sum(pr_areal_year) * 100, year]
mswep_change_hiatus_land_ratio = mswep_change_hiatus_land[, .(year, V1 = mswep_change_hiatus_land$V1/mswep_change_hiatus_global$V1)]

mswep_change_hiatus[, slopes_pr_year := coef(slope_fast(pr_year, year))[1], by = id]
mswep_change_hiatus[, slopes_pr_year_rel := slopes_pr_year/pr_year_mean]
mswep_change_hiatus[, slopes_dry_days := coef(slope_fast(dry_days, year))[1], by = id]
mswep_change_hiatus[, slopes_hvy_pr_year := coef(slope_fast(hvy_pr_per_year , year))[1], by = id]
mswep_change_hiatus[, slopes_intens_pr_year := coef(slope_fast(pr_intens_year, year))[1], by = id] 

mswep_change_hiatus_points = mswep_change_hiatus[!duplicated(mswep_change_hiatus$id)]
mswep_change_hiatus_points = merge(mswep_change_hiatus_points, mswep_points, by = "id")

mswep_change_hiatus_global_plot = rbind(cbind(mswep_change_hiatus_global, "global"), 
                            cbind(mswep_change_hiatus_ocean, "ocean"),
                            cbind(mswep_change_hiatus_land, "land"),
                            cbind(mswep_change_hiatus_land_ratio, "land/global")) 
colnames(mswep_change_hiatus_global_plot) = c("Year", "Precipitation", "Type") 
mswep_change_hiatus_global_plot$Type = factor(mswep_change_hiatus_global_plot$Type, levels=c("global", "ocean", "land", "land/global"))

ggplot() +
  geom_line(data = mswep_change_hiatus_global_plot, aes(x = Year, y = Precipitation), col = "grey20") +
  geom_point(data = mswep_change_hiatus_global_plot, aes(x = Year, y = Precipitation)) +
  geom_smooth(data = mswep_change_hiatus_global_plot, aes(x = Year, y = Precipitation), method = "lm") +
  facet_wrap(~Type, ncol = 1, scales="free") +
  theme_bw()

#For comparison with Trenberth paper
mswep_change_hiatus_global_comp = mswep_change_hiatus[year>2001 & year<2009, sum(pr_areal_year) * 100, year] # P x 10 x 10 since we have taken only one grid per 5deg
mswep_change_hiatus_land_comp = mswep_change_hiatus[land == T & year>2001 & year<2009, sum(pr_areal_year) * 100, year] # Orders of magnitude validated [m3 * 10^6]
mswep_change_hiatus_ocean_comp = mswep_change_hiatus[land == F & year>2001 & year<2009, sum(pr_areal_year) * 100, year]

##############
#Slopes: Mean
##############
ggplot(countries, aes(long, lat, group=group)) + #Mean annual precipitation
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = pr_year_mean, group = NULL), interpolate = T) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_hiatus$pr_year_mean), 700, max(mswep_change_hiatus$pr_year_mean))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("mean_annual_pr.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Relative slopes of annual precipitation
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = slopes_pr_year_rel, group = NULL), interpolate = F) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_hiatus$slopes_pr_year_rel), 0, max(mswep_change_hiatus$slopes_pr_year_rel))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_rel_annual_pr.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Absolute slopes of annual precipitation
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = slopes_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_hiatus$mswep_change_hiatus_year), 0, max(mswep_change_hiatus$slopes_year))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggplot(countries, aes(long, lat, group=group)) + #Slopes of dry days
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = slopes_dry_days, group = NULL), interpolate = F) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = rev(gradient_OrBu), 
                       values = rescale(c(min(mswep_change_hiatus$slopes_dry_days), 0, max(mswep_change_hiatus$slopes_dry_days))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_dry_days.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Slopes of heavy precipitation percentage to annual precipitation
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = slopes_hvy_pr_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_hiatus$slopes_hvy_pr_year), 0, max(mswep_change_hiatus$slopes_hvy_pr_year))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_pr_intensity.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Slopes of precipitation intensity
  geom_raster(data = mswep_change_hiatus, aes(lon, lat, fill = slopes_intens_pr_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_hiatus$slopes_intens_pr_year), 0, max(mswep_change_hiatus$slopes_intens_pr_year))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_heavy_pr.png",  width=12.5, height=8.25, dpi=72) 

####################
#ITCZ
####################

itcz_change = mswep_change_hiatus[lat > -4 & lat < 26 & lon == -107.75, sum(pr_year), list(lat,year)]
ggplot(data = itcz_change, aes(x = lat, y = V1, group = year, col = year)) +
  geom_line(alpha=0.7)+
  theme_bw()

itcz_change = mswep_change_hiatus[lat > -26 & lat < 26 & lon > -170 & lon < -90, mean(slopes_pr_year_rel), list(lat, lon)]
ggplot(data = itcz_change, aes(x = lat, y = V1, group = lon, col = lon)) +
  geom_line(alpha=0.7)+
  theme_bw()


####################
#Changes in the distribution
####################

#Global
global = mswep_lowres[precip != 0, .(id, year, day, precip)]
global_distr_change = plot_distr_change(ceu)

global_land = mswep_lowres[precip != 0 & land ==T, .(id, year, day, precip)]
global_land_distr_change = plot_distr_change(global_land)

global_ocean = mswep_lowres[precip != 0 & land ==F, .(id, year, day, precip)]
global_ocean_distr_change = plot_distr_change(global_ocean)

ggplot() +
  geom_density(data = region_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

#ITCZ
itcz = mswep_lowres[lat > -4 & lat < 26 & precip != 0, .(id, year, day, precip)]
itcz_distr_change = plot_distr_change(itcz)

ggplot() +
  geom_density(data = itcz_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

#Europe 
medit_coarse = mswep_lowres[precip != 0 & lat > 30 & lat < 45 & lon > -20 & lon < 30, .(id, year, day, precip)]
ceu_coarse = mswep_lowres[lat > 45 & lat > 60 &  lon > 5 & lon < 30 & precip != 0, .(id, year, day, precip)]
europe_coarse = mswep_lowres[lat > max(eu_lat) & lat > eu_lat[1]  &  lon > eu_lon[1] & lon < max(eu_lon) & precip != 0, .(id, year, day, precip)]

europe_coarse_distr_change = plot_distr_change(europe_coarse)
ceu_coarse_distr_change = plot_distr_change(ceu_coarse)
medit_coarse_distr_change = plot_distr_change(medit_coarse)

ggplot() +
  geom_density(data = europe_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

####################
#Zonal Means
####################

precip_tropic_year = unique(mswep_lowres[lat == 40.25, .(pr_areal_year, lon, year)])
#xyplot(pr_areal_year~lon, precip_tropic_year, groups = year, type = 'l')
boxplot(pr_areal_year~lon, precip_tropic_year)

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

