# SWAT-CUP files for Cotter catchment

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/UruguayCourse")
# need the zoo package
require(zoo)

# read in flow data
flowdata <- readRDS(file="data/Discharge_data_2000_2017.RDS")
head(flowdata)
flowdata <- flowdata[,c(1,3)]
# original Q is in m^3/sec
# SWAT needs cumecs

# get the swatcup functions
source("functions/SWATCUPfunctions.R")

# Now reorganise the flow data to a format needed for 
# swatcup_ETformat()
# 
# flowdata <- data.frame(Date = time(SantaLucia),
#                        flow = coredata(SantaLucia$Q))

# write observed_rch.txt
swatcup_ETformat(flowdata,df_flow=NULL,
                 date.format = "%Y-%m-%d",
                 "2002-01-01", "2004-12-31",
                 "data/observed_rch.txt" ,
                 "data/observed_rch.txt", 6,
                 Flow = TRUE)

# Create a single file with all the MODIS ET data for all points
ET_Data <- MODIS_ts("MODIS/Cotter")
# show the data
head(ET_Data)

# write observed_sub.txt
swatcup_ETformat(ET_Data, df_flow = NULL, date.format = "%Y-%m-%d",
                 "2006-01-01", "2011-12-31",
                 "data/observed_sub.txt" ,
                 "data/observed_sub.txt", 6, weight= 0.1)
# write observed.txt
swatcup_ETformat(ET_Data, df_flow = flowdata,
                 date.format = "%Y-%m-%d",
                 "2006-01-01", "2011-12-31",
                 "data/observed.txt" ,
                 "data/observed.txt", 14, Flow = TRUE,
                 weight = 0.1)