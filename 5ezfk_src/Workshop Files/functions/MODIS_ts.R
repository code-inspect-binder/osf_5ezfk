# Using MODIS data in hydromad

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