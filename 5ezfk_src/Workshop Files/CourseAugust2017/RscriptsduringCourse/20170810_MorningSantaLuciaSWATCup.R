# Create SWAT-CUP files for Santa Lucia catchment

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/UruguayCourse")
# need the zoo package
require(zoo)

# read in flow data
load(file="data/SantaLucia.RData")
# loads an object called "SantaLucia"
head(SantaLucia)
flowdata <- data.frame(Date=time(SantaLucia),
                       flow=coredata(SantaLucia$Q))

head(flowdata)
# original Q is in m^3/sec
# SWAT needs cumecs

# optional save as "RDS" datafile
#saveRDS(flowdata,"data/SantaLucia.RDS")
# this can be read in as:
#flowdata <- readRDS("data/SantaLucia.RDS")

# get the swatcup functions
source("functions/SWATCUPfunctions.R")

# if just calibrating on flow, you only need to write
# observed_rch.txt
# write observed_rch.txt
swatcup_ETformat(flowdata,df_flow=NULL,
                 date.format = "%Y-%m-%d",
                 st.date = "2002-01-01", end.date="2004-12-31",
                 outfile = "data/observed_rch.txt" ,
                 infile = "data/observed_rch.txt", nlines = 6,
                 Flow = TRUE)

# Read in the MODIS ET data for Santa Lucia
# Create a single file with all the MODIS ET data for all points
ET_Data <- MODIS_ts("MODIS/SantaLucia")
# show the data
head(ET_Data)

swatcup_ETformat(ET_Data, df_flow = NULL, 
                 date.format = "%Y-%m-%d",
                 "2002-01-01", "2004-12-31",
                 "data/observed_sub.txt" ,
                 "data/observed_sub.txt", 6, weight= 0.1)
# write observed.txt
swatcup_ETformat(ET_Data, df_flow = flowdata,
                 date.format = "%Y-%m-%d",
                 "2002-01-01", "2004-12-31",
                 "data/observed.txt" ,
                 "data/observed.txt", 14, Flow = TRUE,
                 weight = 0.1)

