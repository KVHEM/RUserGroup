nc.path = "C:/Users/markonis/Documents/R/Projects/RUserGroup/Members/Yannis/MSWEP"
shp.path = "C:/Users/markonis/Documents/R/Projects/OWDA"
out.path = "C:/Users/markonis/Documents/R/Projects/RUserGroup/Members/Yannis/MSWEP"
my.crs = "+proj=longlat +ellps=WGS84"

library(data.table)
library(raster)

transform.dt.to.brick = function(my.dt, variable.name) {
  array.from.dt = acast(my.dt, lat ~ lon ~ time, value.var = variable.name)
  my.brick = brick(array.from.dt, ymn = min(as.numeric(rownames(array.from.dt))), 
                   ymx = max(as.numeric(rownames(array.from.dt))), xmn = min(as.numeric(colnames(array.from.dt))), 
                   xmx = max(as.numeric(colnames(array.from.dt))))
  my.brick = flip(my.brick, direction = "2")
  return(my.brick)
}

load(paste(nc.path, "/mswep_cz.Rdata", sep = ""))

mswep.dt.cz = data.table(rasterToPoints(mswep.ras.cz))
rm(mswep.brk.cz);rm(mswep.brk.eu);rm(mswep.ras.cz);gc()

nlayers = ncol(mswep.dt.cz)-2
num.of.gpoints = max(nrow(mswep.dt.cz[,1]))

mswep.dt.cz[,id := factor(1:nrow(mswep.dt.cz[,1]))]
names(mswep.dt.cz)[3:(nlayers+2)] = 1:nlayers

mswep.dt.cz = data.table(melt(mswep.dt.cz, id.vars = c("id","x","y"), variable.factor = FALSE))
names(mswep.dt.cz) = c("id", "lon", "lat", "month", "precipitation")
mswep.dt.cz[, precipitation := round(precipitation, 2)]

seq_count <- dim(mswep.dt.cz)[1]
my.dates = seq(c(ISOdate(1978,12,31,23)), by = "3 hour", length.out = seq_count/num.of.gpoints)
mswep.dt.cz[, time := rep(my.dates, each = num.of.gpoints)]

saveRDS(mswep.dt.cz, "MSWEP_CZ.Rds")

#Testing the result

mswep.dt.cz = readRDS(paste(nc.path, "/MSWEP_CZ.RDS", sep = "")) 
mswep.dt.cz[precipitation >40,]
test = mswep.dt.cz[year(time) == 1998 & month(time) == 7 & (mday(time) == 28 | mday(time) == 27),]

test.brk = transform.dt.to.brick(test, variable.name = "precipitation")
plot(test.brk)

#Matrix to dt
mswep.dt = readRDS("MSWEP_5x5_int.Rds")
rownames(mswep.dt) = as.character(seq(c(ISOdate(1978,12,31,23)), by = "3 hour", length.out = dim(mswep.dt)[1]))
mswep.dt = data.table(melt(mswep.dt))
mswep.dt.nz =  mswep.dt[value != 0]
mswep.dt.nz[,year := year(Var1)]
coords = data.table(mswep.dt.nz[, stringr::str_split_fixed(as.character(Var2), " ", 2)])
colnames(coords) = c("lat", "lon")
mswep.dt.nz = cbind(mswep.dt.nz, coords )
mswep.dt.nz[, Var2 := NULL]
colnames(mswep.dt.nz)[1:2] = c("time", "prcp")
setcolorder(mswep.dt.nz, c("year", "lat", "lon", "time", "prcp"))
saveRDS(mswep.dt, "MSWEP_dt_nz_full.Rds")

mswep.dt[,year := year(Var1)]
coords = data.table(mswep.dt[, stringr::str_split_fixed(as.character(Var2), " ", 2)])
colnames(coords) = c("lat", "lon")
mswep.dt = cbind(mswep.dt, coords )
mswep.dt[, Var2 := NULL]
colnames(mswep.dt)[1:2] = c("time", "prcp")
setcolorder(mswep.dt, c("year", "lat", "lon", "time", "prcp"))
saveRDS(mswep.dt, "MSWEP_dt_nz_full.Rds")



