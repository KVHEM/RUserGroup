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