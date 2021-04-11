# Math23c project
# Commodities: initial histograms and pdfs
# add modeling with a polynomial?
# Sharon Tai 04/11

# gold, oil, soybeans, wheat, beef, rubber, Hershey’s 

#********************************
#*Set-up and cleaning up the data
#********************************

library(ggplot2)
library(zoo)

# To clean up the data
# Fix the commas in the numbers: as.numeric(gsub("," , "" , df_comm$Gold..USD...ozt.))
# See how the data's been stored: str(df_comm)

projdata <- read.csv("/Users/stai/Math23cproject/math23c-rproject/Math 23c term project RR + ST - Data.csv", stringsAsFactors=FALSE); head(projdata)
HSY_data <- read.csv("/Users/stai/Math23cproject/math23c-rproject/HSY.csv", stringsAsFactors=FALSE); head(HSY_data)

# Create a dataframe
df_comm <- data.frame(projdata); head(df_comm)
df_comm$Gold..USD...ozt. <- as.numeric(gsub(",", "", df_comm$Gold..USD...ozt.)) ;

# Change the month column to dates we can use to do calculations. Need the zoo package.
time = df_comm$Month 
# Converts the column into humanly readable dates. The as.Date function allows R to actually operate on it.
df_comm$Date = as.Date(as.yearmon(time)) ;
# Use zoo to convert dataframe to time series (easier to define price changes)
df_ts = as.ts(read.zoo(df_comm, FUN = as.yearmon)) ;

# Time-series: 
# first-order differences: comparing the differences between the months (e.g. velocity change) 
# second-order differences: actually comparing the differences in the changes themselves (e.g the acceleration changes)
# Hypothesis: these will differ in pre-recession periods; the distribution of changes could differ between pre-recession
# periods, due to the type
# look up auto-correlation and stationarity. Note: the S&P's 3-year returns over a long period of time
# exhibit stationarity; no matter where in time the distribution is the same.
# Compare this to gold, where the price changes are exponential

# using log for two reasons:
# (1) allows for comparisons across differently scaled time-series (e.g. price of gold/oz vs price of beef/kg)
# (2) also deals with the fact that long term price trends in these time series are likely to be exponential
#      over time

# first-order  diffs (length is N-1) : diff(log(df_comm$Gold..USD...ozt.))
# second-order diffs (length is N-2) : diff(diff(log(df_comm$Gold..USD...ozt.)))

# ex 1.
# plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# This is why we should use logs

# ex 2.
# plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), type = 'l')

plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)))

# logs, As line graphs
plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), 'l')
plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), type = 'l')
plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
plot(df_comm$Date, df_comm$Beef..USD...kg., type = 'l')
plot(df_comm$Date, df_comm$Crude.oil..USD...bbl., type = 'l')
plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
hist(df_comm$Gold..USD...ozt.)
hist(log(df_comm$Gold..USD...ozt.))
hist(diff(log(df_comm$Gold..USD...ozt.)))
hist(df_comm$Gold..USD...ozt.[1:120])
hist(df_comm$Gold..USD...ozt.[121:240])
hist(df_comm$Gold..USD...ozt.[1:120])
hist(df_comm$Gold..USD...ozt.[121:240])

#*****************
#*Gold
#*****************
#*


length(df_comm$Gold..USD...ozt.)
#241 observations

#histogram of purely values, not time series
hist(df_comm$Gold..USD...ozt., breaks=50)
# Bimodal, no apparent skewness

#Time series of prices over time

#Time series of price-changes over time

#create a new dataframe of price differences

#*****************
#*Oil
#*****************
hist(df_comm$Crude.oil..USD...bbl., breaks=50)


#*****************
#*Soybeans
#*****************
hist(df_comm$Soybeans..USD...1000kg., breaks=50)

#*****************
#*Wheat
#*****************
hist(df_comm$Wheat..USD...1000kg., breaks=50)

# Initially: more skewness

#*****************
#*Beef
#*****************
hist(df_comm$Beef..USD...kg., breaks=50)
#bimodal



#*****************
#*Rubber
#*****************
hist(df_comm$Rubber..USD...kg., breaks=50)

# More skewness as well