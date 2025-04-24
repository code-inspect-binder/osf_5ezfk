# Plot flow calibration SWAT-CUP
require(zoo)
require(tidyverse)

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/SantaLuciaPSO.PSO.SwatCup")

# Calibration results
Dates <- seq.Date(as.Date("2002-01-01"), 
                  as.Date("2004-12-31"), 
                  by = 1)
## Flow only
flow_pred <- read.table("iterations/Calibration/pso.out/95ppu.txt",
                        skip = 1,
                        header = T, nrows = 1096)
head(flow_pred)

# create a zoo data.frame
flow_pred_z <- zoo(flow_pred,order.by=Dates)

# Now make a plot
fp <- ggplot(flow_pred_z, 
             aes(x = time(flow_pred_z), 
                 y = observed))
fp <- fp + geom_line(colour = "darkblue", size=1.05)
fp <- fp + geom_line(aes(x = time(flow_pred_z), 
                         y = Best_Sim),
                     colour = "red")
fp <- fp + geom_line(aes(x = time(flow_pred_z), 
                         y = L95PPU),
                     colour = "green", 
                     linetype=2)
fp <- fp + geom_line(aes(x = time(flow_pred_z), 
                         y = U95PPU),
                     colour = "green", 
                     linetype=2)
fp <- fp + xlab("Date")
fp
# demonstrate how you show plot on external window
windows() # or X11() on mac
fp
#dev.off()

# Now plot timeseries: follow E_ExtractingETCalibrationSWATCUP.pdf
require(hydroGOF)

# quick detour, show hydroGOF
# use NSE to calculate flow NSE
NSE(flow_pred_z$observed, flow_pred_z$Best_Sim)
gof(flow_pred_z$observed, flow_pred_z$Best_Sim)

# Load the "extract_subbasin" function
source("../UruguayCourse/functions/Extract_subbasin.R")

# Demonstrate
test <- extract.sub(getwd(),sb=1)

# skip over plotting ET for calibration only in the document,
# I have not yet ETdataSL.RDS with observed ET data for
# Santa Lucia
# from page 4

# create an empty list for the results
ET_all <- list()
# sequence of dates
Dates <- c(seq.Date(as.Date("2002-01-01"),
                    as.Date("2002-12-31"),8),
           seq.Date(as.Date("2003-01-01"),
                    as.Date("2003-12-31"),8),
           seq.Date(as.Date("2004-01-01"),
                    as.Date("2004-12-31"),8))

# read in the "best_sim.txt" file from PSO.out
foo_bar <- "iterations/Calibration/pso.out/best_sim.txt"
foo <- file(foo_bar, "r+")
test <- readLines(foo)
close(foo)

# now search through the file for all the ET data
# we have 27 subbasins and each ET data set is 138 lines long
for (i in 1:27) {
  # use only 1st in grep because of 1 and 10, 11 etc
  lineno <- grep(paste("ET_",i, sep=""),test)[1]
  # read in specific lines
  ETdata <- fread(foo_bar, data.table=T, skip = lineno, 
                  nrows=138, header=T)
  # combine with Dates and store in list()
  ET_all[[i]] <- data.frame(Dates=Dates, ET_obs= ETdata$observed,
                            ET_pred = ETdata$simulated, 
                            Point = rep(i,length(Dates)))
}

# "unlist" the long list into a stacked dataframe using 'do.call()'
ET_fin <- do.call(rbind, ET_all)

# combined plot to show subbasins
p <- ggplot(ET_fin, aes(x = Dates, y = ET_pred)) +
  geom_line(colour="blue") + 
  geom_point(aes(x = Dates,
                 y = ET_obs), colour="red") +
  facet_wrap(~Point) + xlab("Date")
p

# Plotting the points in space: subbasin shape file
require(raster)
require(maptools)
require(rgdal)


# Reading the shape file of the catchment: move this to the Inputdata dir
SantaLucia <- readShapePoly("../uruguaycourse/data/SL_Shape/subcuencaSantaLucia.shp")
crs(SantaLucia) <-  "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
SantaLucia_ll<-spTransform(SantaLucia,CRS("+proj=longlat +datum=WGS84 +no_defs"))


gp <- ggplot(SantaLucia_ll, aes(x = long, y = lat)) + 
  geom_polygon(fill="gray75") +
  coord_equal()
gp


# reading in the subbasin centroids
Subbasins <- read.csv("../Uruguaycourse/data/Subbasins_SantaLucia.csv")

# sub_b_sp <- SpatialPoints(cbind(Subbasins$Long_, Subbasins$Lat),
#                           proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
gp <- gp + geom_point(data = Subbasins, 
                      aes(x = Long_, y = Lat),
                      col = "red")
windows()
gp

# now read in the results for ET calibration
sim_res <- read_table(
        "iterations/Calibration/PSO.OUT/summary_stat.txt",
                      skip=3)

KGE_sub <- data.frame(Point= Subbasins[1:27,1],
                      long = Subbasins[1:27,3],
                      lat = Subbasins[1:27,2],
                      R2 = sim_res$R2[2:28],
                      KGE = sim_res$KGE[2:28])

# Now plot this in space
gp <- ggplot(SantaLucia_ll, aes(x = long, y = lat)) + 
  geom_polygon(fill="gray75") +
  coord_equal()
gp <- gp + geom_point(data = KGE_sub, 
                      aes(x = long, y = lat,
                          col=R2, size = R2))
windows()
gp
