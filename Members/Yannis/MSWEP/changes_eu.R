mswep_eu = readRDS(file = paste0(local_path, "MSWEP_eu_day_analysis.Rds"))
mswep_points_eu = readRDS(file = paste0(local_path, "MSWEP_eu_day_points.Rds"))

mswep_change_eu = mswep_eu[,.(id, lat, lon, year, pr_year, pr_year_mean, pr_areal_year, 
                               dry_days, hvy_pr_per_year, pr_intens_year, area)]
mswep_change_eu = mswep_change_eu[complete.cases(mswep_change_eu)]
mswep_change_eu = unique(mswep_change_eu)

mswep_change_eu[, slopes_pr_year := coef(slope_fast(pr_year, year))[1], by = id]
mswep_change_eu[, slopes_pr_year_rel := slopes_pr_year/pr_year_mean]
mswep_change_eu[, slopes_dry_days := coef(slope_fast(dry_days, year))[1], by = id]
mswep_change_eu[, slopes_hvy_pr_year := coef(slope_fast(hvy_pr_per_year , year))[1], by = id]
mswep_change_eu[, slopes_intens_pr_year := coef(slope_fast(pr_intens_year, year))[1], by = id] 

mswep_change_eu_points = mswep_change_eu[!duplicated(mswep_change_eu$id)]
mswep_change_eu_points = merge(mswep_change_eu_points, mswep_points_eu, by = "id")

mswep_change_eu_sum = mswep_change_eu[, sum(pr_areal_year), year] 
colnames(mswep_change_eu_sum) = c("Year", "Precipitation")

ggplot() +
  geom_line(data = mswep_change_eu_sum, aes(x = Year, y = Precipitation), col = "grey20") +
  geom_point(data = mswep_change_eu_sum, aes(x = Year, y = Precipitation)) +
  geom_smooth(data = mswep_change_eu_sum, aes(x = Year, y = Precipitation), method = "lm") +
  theme_bw()

##############
#Slopes: Mean
##############
ggplot(countries, aes(long, lat, group=group)) + #Mean annual precipitation
  geom_raster(data = mswep_change_eu, aes(lon, lat, fill = pr_year_mean, group = NULL), interpolate = F) +
  geom_path(size=1.0) + 
  coord_equal() + 
  coord_cartesian(xlim = eu_lon, ylim = eu_lat) +
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_eu$pr_year_mean), 700, max(mswep_change_eu$pr_year_mean))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("mean_annual_pr.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Relative slopes of annual precipitation
  geom_raster(data = mswep_change_eu, aes(lon, lat, fill = slopes_pr_year_rel, group = NULL), interpolate = F) +
  geom_path(size=1.0) + 
  coord_equal() + 
  coord_cartesian(xlim = eu_lon, ylim = eu_lat) +
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_eu$slopes_pr_year_rel), 0, max(mswep_change_eu$slopes_pr_year_rel))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_rel_annual_pr.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Slopes of dry days
  geom_raster(data = mswep_change_eu, aes(lon, lat, fill = slopes_dry_days, group = NULL), interpolate = F) +
  geom_path(size=1.0) + 
  coord_equal() + 
  coord_cartesian(xlim = eu_lon, ylim = eu_lat) +
  theme_bw() +
  theme_opts +
  scale_fill_gradientn(colours = rev(gradient_OrBu), 
                       values = rescale(c(min(mswep_change_eu$slopes_dry_days), 0, max(mswep_change_eu$slopes_dry_days))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_dry_days.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Slopes of precipitation intensity
  geom_raster(data = mswep_change_eu, aes(lon, lat, fill = slopes_intens_pr_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() +
  coord_cartesian(xlim = eu_lon, ylim = eu_lat) +
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_eu$slopes_intens_pr_year), 0, max(mswep_change_eu$slopes_intens_pr_year))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_heavy_pr.png",  width=12.5, height=8.25, dpi=72) 

ggplot(countries, aes(long, lat, group=group)) + #Slopes of heavy precipitation percentage to annual precipitation
  geom_raster(data = mswep_change_eu, aes(lon, lat, fill = slopes_hvy_pr_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() + 
  coord_cartesian(xlim = eu_lon, ylim = eu_lat) +
  theme_opts +
  scale_fill_gradientn(colours = gradient_OrBu, 
                       values = rescale(c(min(mswep_change_eu$slopes_hvy_pr_year), 0, max(mswep_change_eu$slopes_hvy_pr_year))))+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")

ggsave("slopes_pr_intensity.png",  width=12.5, height=8.25, dpi=72) 

####################
#Changes in the distribution
####################
#All Europe
europe = mswep_eu[precip != 0 , .(id, year, day, precip)]
europe_distr_change = plot_distr_change(europe)

ggplot() +
  geom_density(data = region_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

#Central Europe
ceu = mswep_eu[lat > 45 & lat > 60 &  lon > 5 & lon < 30 & precip != 0 , .(id, year, day, precip)]
ceu_distr_change = plot_distr_change(ceu)

ggplot() +
  geom_density(data = ceu_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

#Mediterranean 
medit = mswep_eu[precip != 0 & lat < 45 & lon > -20 & lon < 30, .(id, year, day, precip)]
medit_distr_change = plot_distr_change(medit)

ggplot() +
  geom_density(data = medit_qq_slopes, aes(x = slope, group = quantile, col = quantile)) +
  scale_colour_grey(start = 0.9, end = 0) +
  theme_bw()

####################
#Zonal Means
####################
region = europe
region_qq = qq_slopes(region)

precip_tropic_year = unique(mswep_eu[lat == 40.25, .(pr_areal_year, lon, year)])
#xyplot(pr_areal_year~lon, precip_tropic_year, groups = year, type = 'l')
boxplot(pr_areal_year~lon, precip_tropic_year)

zonal_sum_year = mswep_eu[, sum(pr_areal), list(year, lat)]
colnames(zonal_sum_year)[3] = "precip"
xyplot(precip~year|lat, zonal_sum_year, type = c('l', 'r'), scales = list(relation = "free"))
xyplot(precip~year, zonal_sum_year, type = c('l', 'r'), groups = lat, scales = list(relation = "free"))

zonal_dry_year = mswep_eu[, mean(dry_days, na.rm = T), list(year, lat)]
colnames(zonal_dry_year )[3] = "dry_days"
xyplot(dry_days~year|lat, zonal_dry_year, type = c('l', 'r'), scales = list(relation = "free"))

zonal_intens_year = mswep_eu[, mean(pr_intens_year, na.rm = T), list(year, lat)]
colnames(zonal_intens_year )[3] = "intensity"
xyplot(intensity~year|lat, zonal_intens_year, type = c('l', 'r'), scales = list(relation = "free"))

