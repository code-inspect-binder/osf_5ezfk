# Using MODIS data in hydromad

require(zoo)
require(tidyverse)
require(hydromad)


setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/UruguayCourse")

#source("functions/MODIS_ts.R")

MODIS_ts <- function(MODISdir="MODIS",patt=".asc"){
  # read in all the file names
  x1 <- list.files(path=MODISdir, pattern=patt)
  # each "asc" file stores all the values in time for 1 location including the QA data
  # the number of rows is important as this is all the time steps
  # divide nrows by 2 as second part is QC data
  n <- nrow(read.csv(paste(MODISdir,x1[1],sep="/"),header=F))/2
  # Create storage for the data, Jdate is Julian date
  Store <- data.frame(Year = numeric(length=n),
                      JDay = numeric(length=n),
                      ET = numeric(length=n),
                      Point = numeric(length = n),
                      Quality = numeric(length = n))
  # Create a list to store the different pixels (each with a Store)
  Store1 <- list()
  # run a loop over the list of file names
  for (i in 1:length(x1)) {
    Mdata <- read.csv(paste(MODISdir,x1[i],sep="/"),header=F)
    # do some substringing
    Store[,1] <- as.numeric(substr(Mdata[1:n,8],2,5))
    Store[,2] <- as.numeric(substr(Mdata[1:n,8],6,8))
    Store[,3] <- Mdata[1:n,11]/10
    # 0.1 scaling factor (see MODIS read_me)
    Store[,4] <- i
    Store[,5] <- Mdata[(n+1):nrow(Mdata),11]
    Store1[[i]] <- Store
  }
  # converting from list back to a data.frame
  ts.data <- do.call(rbind,Store1)
  # Now make the date from the Year and Jdate
  ts.data$Date <- as.Date(paste(ts.data$Year,ts.data$JDay,
                                sep = "/"), "%Y/%j")
  return(ts.data)
}

# Now we can use the function
Cotter_ET <- MODIS_ts(MODISdir = "MODIS/Cotter")
# Chk the data
head(Cotter_ET)

point1 <- Cotter_ET[Cotter_ET$Point==1,]

# Make a plot of a few points
plot(subset(Cotter_ET, Point == 1)[,c("Date","ET")],
     type = "b", pch = 16, col = "blue",
     xlab = "8-day time series", ylab = "8-day ET in mm",
      ylim=c(0,50), xlim=c(as.Date("2000-01-01"),as.Date("2002-12-31")))
points(subset(Cotter_ET, Point == 2)[,c("Date","ET")],
       type = "b", pch = 16, col = "red")
points(subset(Cotter_ET, Point == 3)[,c("Date","ET")],
       type = "b", pch = 16, col = "grey60")
points(subset(Cotter_ET, Point == 4)[,c("Date","ET")],
       type = "b", pch = 16, col = "black")
points(subset(Cotter_ET, Point == 5)[,c("Date","ET")],
       type = "b", pch = 16, col = "green")

# Simple histogram in time and space
hist(Cotter_ET$ET, xlab="MODIS ET")
# for a single timestep:
hist(Cotter_ET[Cotter_ET$Date==as.Date("2014-01-01"),"ET"])
# midwinter
hist(Cotter_ET[Cotter_ET$Date>=as.Date("2014-07-01") &
                 Cotter_ET$Date<=as.Date("2014-07-10")
                 ,"ET"])

# Aggregate across the catchment
ET.mean <- aggregate(Cotter_ET$ET,
                     list(JDay=Cotter_ET$JDay,
                          Year=Cotter_ET$Year), 
                     FUN = mean,na.rm=T)
# Check the data
head (ET.mean)
colnames(ET.mean)[3] <- "ET"

# Create a date column from Jdate
ET.mean$Date <- as.Date(paste(ET.mean$Year,ET.mean$JDay,
                              sep = "/"), "%Y/%j")
# Now, make a plot of the mean 8 daily ET
plot(ET.mean$Date,ET.mean$ET, xlab = "Time (8-daily)",
     ylab = "Basin average ET", type="l")


ET.mean_z <- zoo(ET.mean,order.by=ET.mean$Date)


# Hydromad and calibrate with ET
Cotter_ET <- MODIS_ts(MODISdir = "MODIS/Cotter")
# get rid of quality column
Cotter_ET <- Cotter_ET[,-5]
# In here, first convert to wide format, use tidyverse
Cotter_ET_w <- spread(Cotter_ET, key=Point, value=ET)
# Converting date to date format
#Cotter_ET_w$Date <- as.Date(Cotter_ET_w$Date)
names(Cotter_ET_w)

# Calculating the catchment average ET 
# need to start at column 5 because of added quality data
Cotter_avgET <- data.frame(Date = Cotter_ET_w$Date,
                           ETa = apply(Cotter_ET_w[,4:ncol(Cotter_ET_w)],
                                       1, mean, na.rm = T))
# Converting to a zoo format to work with hydromad
Cotter_MODISET <- zoo(Cotter_avgET$ETa,
                      order.by=Cotter_avgET$Date)
# we can plot to see the data
plot(Cotter_MODISET, xlab="Date",
     ylab="8-day summed MODIS ET (mm)")

# Source specific functions
source("functions/leapfun.R")
source("functions/ETa.merge.R")
source("functions/plot.ET.R")
source("functions/ETfit.objectives.R")

load("data/Cotter.Rdata") # this is the flow data
Cotter$Q <- convertFlow(Cotter$Q, from="ML", area.km2=130)
# discard the data before 2000
Cotter <- window(Cotter, start="2000-01-01")

Cotter_Sat <- ETa.merge(Flowdata=Cotter,ETdata=Cotter_MODISET)
# Make a plot
xyplot(Cotter_Sat)


# Calibrate without ET data
# Data period for calibration
data_cal <- window(Cotter, start = "2005-01-01",end = "2010-12-31")
# Data for validation period
data_val <- window(Cotter, start = "2011-01-01",end = "2014-12-31")
# Define the model, important to define return_state=T
Cotter_mod <- hydromad(DATA=data_cal,
                       sma = "gr4j", routing = "gr4jrouting",
                       x1 = c(500,2500), x2 = c(-30,20), x3 = c(5,500),
                       x4 = c(0.5,10), etmult=c(0.01,0.5),
                       return_state=TRUE)
# Using shuffled complex evolution algorithm for fitting
Cotter_fit<- fitByOptim(Cotter_mod,
                      objective= hmadstat("r.squared"))
# Extract the coefficients and the summary
summary(Cotter_fit)
# plot
xyplot(Cotter_fit, with.P = TRUE)


# Calibrate with the satellite data
# remake the calibration data
data_modis_cal <- window(Cotter_Sat, start = "2005-01-01",
                         end = "2010-12-31")
# also make the validation data
data_modis_val <- window(Cotter_Sat, start = "2011-01-01",
                         end = "2014-12-31")

# Because we have rebuilt data.cal, redefine the model
Cotter_mod_Modis <- hydromad(DATA=data_modis_cal,
                             sma = "gr4j", routing = "gr4jrouting",
                             x1 = c(500,3000), x2 = c(-30,20),
                             x3 = c(5,500), x4 = c(0.5,10),
                             etmult=c(0.01,0.5),
                             return_state=TRUE)
# fit both ET and Q using special objective function
Cotter_Fit_Modis <- fitBySCE(Cotter_mod_Modis,
      objective=~hmadstat("JointQandET")(Q,X,w=0.5,
        DATA=DATA,model=model,objf = hmadstat("viney")))
# check the model fit
summary(Cotter_Fit_Modis)

# plotting the result
xyplot(Cotter_Fit_Modis, with.P = TRUE)

# plot the ET
plot.ET(caldata=data_modis_cal,Cotter_Fit_Modis)
# How the coefficients have changed
coef(Cotter_fit)

# with MODIS
coef(Cotter_Fit_Modis)

# check validation
# updating the model data for the validation
sim_val <- update(Cotter_fit, newdata = data_val)
sim_val_modis <- update(Cotter_Fit_Modis, 
                        newdata = data_modis_val)
# runlist
allMods <- runlist(calibration=Cotter_fit, validation=sim_val,
                   calibrationET=Cotter_Fit_Modis,
                   validationET= sim_val_modis)
# Get the summary results
round(summary(allMods),2)
