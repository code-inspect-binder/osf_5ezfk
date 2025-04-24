#R as a calculator
3*5
x <- 5
y <- 2
x *y; x/y

# a vector
x <- c(1,2,5,7,8,15,3,12,11,19)
y <-1:10

x/y
ls()

foo = 0.5*x^2 -3*x+2
foo

# data frame
Rainfall <-  data.frame(City = 
                          c("Montevideo","New York",
                            "Amsterdam","Sydney",
                            "Moscow", "Hong Kong"),
                        Rain_mm =c(950, 1174, 838,
                                   1215, 707, 2400))

# call a column
Rainfall$City
Rainfall["Rain_mm"]
Rainfall[,2]
Rainfall[1:2,2]
Rainfall[1,]
Rainfall[Rainfall["City"]=="Montevideo"]

# find a subset
lots <- Rainfall[Rainfall["Rain_mm"] > 1000,]
lots

# working directory
setwd("C:/users/rver4657/Documents")
dir()[1:10]

# read in data
setwd("D:/UruguayCourse")
UR_flow <- read.csv("data/UruguayRiver_ConcordiaSt.csv")
PA_flow <- read.csv("data/Parana_CorrientesSt.csv")

head(UR_flow)
names(UR_flow)

# packages
require(tidyverse)
require(zoo)

# Statistical functions
# average monthly flow
mean(UR_flow$Flow, na.rm=T)
sd(UR_flow$Flow, na.rm=T)

# subset two years and correlate
flow1969 <- UR_flow[UR_flow$Year==1969,]
flow1970 <- UR_flow[UR_flow$Year==1970,]
cor(flow1969$Flow,flow1970$Flow)

# aggregate
# aggregate to annual flow
annual_flow <- aggregate(UR_flow,
                          list(Year=UR_flow$Year),
                          sum)
# aggregates everything
annual_flow
# if you want only the flow column
(annual_flow <- aggregate(UR_flow$Flow,
                          list(Year=UR_flow$Year),
                          sum))
# change the column name from "x" back to "Flow"
names(annual_flow)[2] <- "Flow"

# if statements
#We will first generate the data frame:
x <- UR_flow
# now wrote loop
if (nrow(x) > 10) {
  print("the dataframe is LONG")
} else {
  print("the dataframe is SHORT")
}

# ifelse()
x <- UR_flow
# add a column which identifies whether the flow < 5000
x[,4] <- ifelse(x[,3] > 5000, "large", "small")
# this creates a fourth column
tail(x,10)

# corrected version
x[,5] <- ifelse(x[,3] > 2500,ifelse(x[,3] > 10000,
                  "large", "intermediate"), "small")
tail(x,10)

# add a column by creating a new name
x$othercolumn <- ifelse(x[,3] > 2500,ifelse(x[,3] > 10000,
                                    "large", "intermediate"), "small")
tail(x,10)

# for loop
# Hello world
for (i in 1:5) {
  print(paste(i, "hello world"))
}

# flow data
for (i in 1:5) {
  print(paste(UR_flow$Flow[i], "is the flow (ML/day)"))
}

for (i in 1:5) {
  print(paste("in Year", UR_flow$Year[i],"and month", 
              UR_flow$Month[i], 
              "the flow is", UR_flow$Flow[i], "(ML/day)"))
}

# double loop
for (i in 1:5) {
  for (j in c(1,3)) {
    print(paste(UR_flow[i,j], colnames(UR_flow)[j]))
  }
}

# zoo and ggplot
UR_flow$Dates <- as.Date(paste(UR_flow$Year,
                               UR_flow$Month,
                               "01",sep="-"))
UR_flow[1:10,]

UR_flow_z <- zoo(UR_flow$Flow, 
                 order.by = UR_flow$Dates)
head(UR_flow_z)
plot(UR_flow_z)
plot(UR_flow$Flow)

# ggplot
p <- ggplot(UR_flow_z, aes(x=time(UR_flow_z), y = UR_flow_z)) +
  geom_line()
print(p)

# Functions
HW <- function(n, outtext) {
  for (i in 1:n) {
    print(outtext)
  }
  #return("nothing")
}
# test
HW(5, "Hello World")
# switch inputs
HW(outtext ="I can switch the inputs", n = 3)


# Exercise

ThisFun <- function(x,a,b) {
    y <- a*x + b
  return(y)
}
# test
ThisFun(2,0.5,3)

x <- 1:10
ThisFun(x,0.2,0.3)

x <- 1:10
b = 0.2
a = 0.3
out <- ThisFun(x,b,a)

# use browser() for function testing
TestFun <- function(x,a,b) {
   browser()
       y <- a*x + b
   return(y)
}
TestFun(3,0.2,0.5)

