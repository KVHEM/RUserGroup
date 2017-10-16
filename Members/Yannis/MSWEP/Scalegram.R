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