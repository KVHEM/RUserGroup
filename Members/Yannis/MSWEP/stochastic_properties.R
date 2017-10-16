
mswep_lowres = readRDS(file = paste0(local_path, "MSWEP_5x5_day_analysis.Rds"))
mswep_points = readRDS(file = paste0(local_path, "MSWEP_5x5_day_points.Rds"))

agg = function(x, agg.scale, FUN = mean) {
  aa = as.numeric(tapply(x, (seq_along(x) - 1) %/% agg.scale, FUN, na.rm = T))
  return(aa[1:(length(aa)-1)])
}

acf.1 = function(x, ...) {
  tryCatch(acf(x[!is.na(x)], plot = F, ...)$acf[2], error = function(e) NA)
}

##Determination of ACF structure

mswep_lowres[, acf_1_year := acf.1(unique(pr_year)), id]
mswep_points = merge(mswep_points, unique(mswep_lowres[, .(id, acf_1_year)]))

ggplot(countries, aes(long, lat, group=group)) + 
  geom_raster(data = mswep_points, aes(lon, lat, fill = acf_1_year, group = NULL)) +
  geom_path(size=1.0) + 
  coord_equal() + 
  theme_opts +
  scale_fill_gradientn(colours = gradient_RdBu)+
  geom_path(data=grat, aes(long, lat, group=group), linetype="dashed", color="grey50")




