# Read in the flow data and write observed_rch.txt
# Demonstrate

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/UruguayCourse")

# read in flow data
flowdata <- readRDS(file="data/Discharge_data_2000_2017.RDS")
head(flowdata)

# get the swatcup functions
source("functions/SWATCUPfunctions.R")

# write observed_rch.txt
swatcup_ETformat(flowdata[,c(1,3)],df_flow=NULL,
                 date.format = "%Y-%m-%d",
                 "2006-01-01", "2011-12-31",
                 "data/observed_rch.txt" ,
                 "data/observed_rch.txt", 6,
                 Flow = TRUE)
