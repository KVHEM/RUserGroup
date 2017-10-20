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
gradient_OrBu = colorRampPalette(c('tan4','#8c510a','#f5f5f5','skyblue4', 'purple3'), interpolate = "spline", space = "rgb")(100)

gradient_OrBu = colorRampPalette(c('#5B4E33','#93886C','#f5f5f5','#405874', '#1F3144'), interpolate = "spline", space = "rgb")(100)

my.col = colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027",  "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")(10)
rgb.palette.Qualitative.2 = colorRampPalette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",     "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
rgb.palette.Qualitative.3 = colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",     "#cab2d6", "#6a3d9a", "#ffed6f", "#b15928"))

theme_opts <- list(theme(axis.ticks.length=unit(-0.1, "cm"),  
                               axis.text.x = element_text(margin=unit(c(0.5,0.5,0.2,0.2), "cm")), 
                               axis.text.y = element_text(margin=unit(c(0.5,0.5,0.2,0.2), "cm")),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank())) 

single_site = mswep_lowres[precip != 0 & lat == 45.25 & lon == 52.25, .(id, year, day, precip)]

plot_distr_change_single = function(single_site, kendal_thres = 0.1){
  region_qq_slopes = qq_slopes(single_site)
  region_qq_slopes$quantile = 1:17
  kendal_signif = region_qq_slopes[kendal < kendal_thres, .(quantile, slope)]
  ggplot(data = region_qq_slopes, aes(quantile)) +
    scale_x_continuous(breaks = seq(1,17,2), labels = seq(0.1, 0.9, 0.1))+
    geom_line(aes(y = slope), col = "slategray3", size = 1.5) +
    geom_point(data = kendal_signif, aes(x = quantile, y = slope), size = 7, col = my.col[4]) +
    geom_point(aes(y = slope), size = 4, col = "slategray3") +
    theme_bw()+
    labs(x = "Quantile", y = "Slope") +
    theme(axis.ticks.length=unit(-0.1, "cm"),  
          axis.text.x = element_text(size = 14, margin=unit(c(0.5,0.5,0.2,0.2), "cm")), 
          axis.text.y = element_text(size = 14, margin=unit(c(0.5,0.5,0.2,0.2), "cm")),
          axis.title.x = element_text(size = 16),   axis.title.y = element_text(size = 16))
}

plot_distr_change = function(region, kendal_thres = 0.05){
    region_qq_slopes = qq_slopes(region)
  region_qq_slopes = region_qq_slopes[kendal< kendal_thres]
  
  region_qq_slopes[, mean_slope:= mean(slope), quantile]
  region_qq_slopes[, stdev:= sd(slope), quantile]
  region_qq_slopes[, lower := mean_slope - stdev]
  region_qq_slopes[, upper := mean_slope + stdev]
  
  region_qq_slopes_plot = region_qq_slopes[, .(quantile, mean_slope, lower, upper)]
  region_qq_slopes_plot = unique(region_qq_slopes_plot)

    ggplot(data = region_qq_slopes_plot, aes(quantile)) +
    scale_x_continuous(breaks = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%"))+
    geom_smooth(aes(y = mean_slope), col = "black", se = F) +
    geom_point(aes(y = mean_slope)) +
    geom_line(aes(y = lower), linetype = 2, color = "black")+
    geom_line(aes(y = upper), linetype = 2, color = "black")+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)+
    theme_bw()+
    theme_opts
}

plot_zonal_distr_change = function(region, direction = "lat", kendal_thres = 1, interpolate = F){
  region_qq_slopes = qq_slopes(region)
  region_qq_slopes = merge(region_qq_slopes, unique(region[,.(id, lat,lon)]), by = 'id')
  region_qq_slopes = region_qq_slopes[kendal <= kendal_thres]
  region_qq_slopes[, mean_lat := mean(slope), list(quantile, lat)]
  region_qq_slopes[, mean_lon := mean(slope), list(quantile, lon)]
  region_qq_slopes_lat = region_qq_slopes[, .(lat, quantile, mean_lat)]
  region_qq_slopes_lon = region_qq_slopes[, .(lon, quantile, mean_lon)]
  if(direction == "lat"){
    ggplot() +
      geom_raster(data = region_qq_slopes_lat, aes(x = quantile, y = lat, fill = mean_lat), interpolate = interpolate) + 
      labs(x = "Quantile", y = "Latitude") + 
      scale_fill_gradientn(colours = gradient_OrBu, name = "Slope", 
                           values = rescale(c(min(region_qq_slopes_lat$mean_lat), 0, max(region_qq_slopes_lat$mean_lat)))) +
      theme_bw()+
      theme(axis.text.x = element_text(size = 13), axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 14))+
      scale_x_discrete(breaks = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%"), expand = c(0,0))+
      scale_y_continuous(breaks = seq(-60, 60, by = 30), expand = c(0,0)) +
      geom_vline(xintercept = c(4, 9, 14), col = "grey40") +
      geom_hline(yintercept = seq(-60, 60, by = 30) , col = "grey40", linetype = 2) +
      theme_opts
  } else {
    ggplot() +
      geom_raster(data = region_qq_slopes_lon, aes(x = quantile, y = lon, fill = mean_lon), interpolate = interpolate) + 
      labs(x = "Quantile", y = "Longitude") + 
      scale_fill_gradientn(colours = gradient_OrBu, name = "Slope", 
                           values = rescale(c(min(region_qq_slopes_lon$mean_lon), 0, max(region_qq_slopes_lon$mean_lon)))) +
      theme_bw()+
      theme(axis.text.x = element_text(size = 13), axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 14))+
      scale_x_discrete(breaks = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%"), expand = c(0,0))+
      scale_y_continuous(breaks = seq(min(region_qq_slopes_lon$lon), max(region_qq_slopes_lon$lon), by = 30) + 2.75, expand = c(0,0)) +
      geom_vline(xintercept = c(4, 9, 14), col = "grey40") +
      geom_hline(yintercept = seq(min(region_qq_slopes_lon$lon), max(region_qq_slopes_lon$lon), by = 30) + 2.75, col = "grey40", linetype = 2) +
      theme_opts
  }
}

compare_two_hist_example = function(x1, x2, name1 = "hist1", name2 = "hist2", title = "", 
                            xlab = "what is this?", lim = c(0, 0.2), ...) {
  cukes <- data.frame(x = x1)
  carrots <- data.frame(x = x2)
  
  cukes$Legend <- name1
  carrots$Legend <- name2
  vegLengths <- rbind(cukes, carrots)
  
  ggplot(vegLengths, aes(x, fill = Legend)) + 
    ggtitle(title) + 
    geom_density(alpha = 0.4) + 
    xlab(xlab) + 
    scale_fill_manual(values = c("orange", "tan4")) + 
    scale_x_continuous(limits = lim) + 
    theme(plot.title = element_text(size = 20, face = "bold"), 
          axis.title.x = element_text(size = 16, face = "bold"), 
          axis.title.y = element_text(size = 16, face = "bold"))
}

          
          
          
