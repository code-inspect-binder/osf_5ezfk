# hydromad
library(hydromad)
# or require(hydromad)
data(Cotter)
xyplot(Cotter)
head(Cotter)


# specify a model structure (GR4J) with some arbritrary parameters
CMod <- hydromad(Cotter[1:1000,], sma="gr4j", 
                 routing="gr4jrouting",etmult=0.15,
                 x1 = 665, x2 = 10, x3 = 90, x4 = 3.8,
                 S_0 = 0.6, R_0 = 0.7)
# in other words, this model is purely arbitrary to demonstrate
# the different components
xyplot(predict(CMod, return_state = TRUE, 
               return_components = TRUE),
       strip = FALSE, strip.left = TRUE)

# summaries of the model
print(CMod)
summary(CMod)
coefficients(CMod)


# define some calibration data (6 years)
# split the data to use for calibration
Data_Cal<- window(Cotter,
                  start = "1970-01-01",
                  end = "1975-12-31")
head(Data_Cal)

# redefine the model
CMod <- hydromad(Data_Cal, sma="gr4j", 
                 routing="gr4jrouting",
                 etmult=c(0.05,0.5),x1 = c(1000,3000), 
                 x2 = c(-3,20),
                 x3 =c(5,500), x4 = c(0.5,10))

# calibrate
set.seed(100)
CotterFit <- fitByOptim(CMod,objective=~hmadstat("r.squared")(Q,X),
                        samples=1000,method="PORT")

summary(CotterFit)
coef(CotterFit)
# plot observed vs modelled with the rainfall (Figure 5)
xyplot(CotterFit, with.P=TRUE, 
       xlim=as.Date(c("1970-01-01", "1975-01-01")))


# Load the Santa Lucia data
load("data/SantaLucia.Rdata")
head(SantaLucia)
# original Q is in m^3/sec
# convert to mm/day
SantaLucia$Q <- SantaLucia$Q/(5171*10^6)*86400*1000

# split the data to use for calibration
Data_CalSL<- window(SantaLucia,
                  start = "2001-01-01",
                  end = "2006-12-31")
head(Data_CalSL)
xyplot(Data_CalSL)

# define the Santa Lucia model
SL_Mod <- hydromad(Data_CalSL, sma="gr4j", 
                 routing="gr4jrouting",
                 etmult=c(0.05,0.5),x1 = c(1000,3000), 
                 x2 = c(-3,20),
                 x3 =c(5,500), x4 = c(0.5,10))
# optimise
SLFit <- fitByOptim(SL_Mod,
                    objective=~hmadstat("r.squared")(Q,X),
                        samples=1000,method="PORT")

summary(SLFit)

# Some analysis
annual <- aggregate(Data_CalSL,
                    list(Year=format(time(Data_CalSL),"%Y")),
                    sum,na.rm=T)

head(annual)
# correct maxT to convert to Potential ET
annual$E <- annual$E*0.15

balance = annual$P - annual$E - annual$Q
balance

# different calibration 
SLFit <- fitByOptim(SL_Mod,
                    #objective=~hmadstat("r.squared")(Q,X),
                    samples=1000,method="PORT")

summary(SLFit)
