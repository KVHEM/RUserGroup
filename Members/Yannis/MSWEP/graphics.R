library(rgdal)
library(ggplot2)
library(gstat)
library(sp)

map_path = "C:/Users/markonis/Documents/R/Projects/geodata/"

countries <- readOGR(paste0(map_path,"ne_110m_admin_0_countries"), layer="ne_110m_admin_0_countries") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer="ne_110m_graticules_15") 
countries = fortify(countries)
grat <- fortify(grat)

gradient_RdBu = colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#fef0d9','#e0f3f8','#abd9e9','#74add1','#4575b4')), space = "rgb")(100)
gradient_OrBu = colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), interpolate = "spline", space = "rgb")(100)

my.col = colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027",  "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")(10)
rgb.palette.Qualitative.2 = colorRampPalette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",     "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
rgb.palette.Qualitative.3 = colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",     "#cab2d6", "#6a3d9a", "#ffed6f", "#b15928"))

theme_opts <- list(theme(axis.ticks.length=unit(-0.1, "cm"),  
                               axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
                               axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"))))

plot_distr_change = function(region, kendal_thres = 0.05){
    region_qq_slopes = qq_slopes(region)
  region_qq_slopes = region_qq_slopes[kendal< kendal_thres]
  
  region_qq_slopes[, mean_slope:= mean(slope), quantile]
  region_qq_slopes[, stdev:= sd(slope), quantile]
  region_qq_slopes[, lower := mean_slope - stdev]
  region_qq_slopes[, upper := mean_slope + stdev]
  
  region_qq_slopes_plot = region_qq_slopes[, .(quantile, mean_slope, lower, upper)]
  region_qq_slopes_plot = unique(region_qq_slopes_plot)
  region_qq_slopes_plot$quantile = 1:17
  
  ggplot(data = region_qq_slopes_plot, aes(quantile)) +
    scale_x_continuous(breaks = 1:17, labels = seq(0.1, 0.9, 0.05))+
    geom_smooth(aes(y = mean_slope), col = "black", se = F) +
    geom_point(aes(y = mean_slope)) +
    geom_line(aes(y = lower), linetype = 2, color = "black")+
    geom_line(aes(y = upper), linetype = 2, color = "black")+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)+
    theme_bw()+
    theme_opts
}

plot_zonal_distr_change = function(region, direction = "lat", kendal_thres = 1){
  region_qq_slopes = qq_slopes(region)
  region_qq_slopes = region_qq_slopes[kendal <= kendal_thres]
  
  region_qq_slopes_map = merge(region_qq_slopes, mswep_change_eu_points[, .(id, lat, lon)], by = "id")
  region_qq_slopes_map[, mean_lat := mean(slope), list(quantile, lat)]
  region_qq_slopes_map[, mean_lon := mean(slope), list(quantile, lon)]
  if(direction == "lat"){
    ggplot() +
      geom_raster(data = region_qq_slopes_map, aes(x = quantile, y = lat, fill = mean_lat)) + 
      
      scale_fill_gradientn(colours = gradient_OrBu, 
                           values = rescale(c(min(region_qq_slopes_map$mean_lat), 0, max(region_qq_slopes_map$mean_lat)))) +
      theme_bw()+
      theme_opts
  } else {
    ggplot() +
      geom_raster(data = region_qq_slopes_map, aes(x = quantile, y = lon, fill = mean_lon)) + 
      
      scale_fill_gradientn(colours = gradient_OrBu, 
                           values = rescale(c(min(region_qq_slopes_map$mean_lon), 0, max(region_qq_slopes_map$mean_lon)))) +
      theme_bw()+
      theme_opts
  }
}
