# Math23c project
# Commodities: initial histograms and pdfs
# add modeling with a polynomial?

# 04/13 
# gold, oil, soybeans, wheat, beef, rubber, Hershey’s 

# 04/17
# all goods



#********************************
#*Set-up and cleaning up the data
#********************************

library(ggplot2)
library(zoo)
setwd("~/Dropbox (Personal)/Work 2021/- Math 23c/term project")
chiSqTest <- dget("chiSqTest.R")

setwd("/Users/stai/math23c-rproject")

# To clean up the data
# Fix the commas in the numbers: as.numeric(gsub("," , "" , df_comm$Gold..USD...ozt.))
# See how the data's been stored: str(df_comm)

projdata <- read.csv("/Users/stai/math23c-rproject/source_data/commodities data.csv", stringsAsFactors=FALSE); head(projdata)
HSY_data <- read.csv("/Users/stai/math23c-rproject/source_data/HSY.csv", stringsAsFactors=FALSE); head(HSY_data)
rec_indic <-read.csv("/Users/stai/math23c-rproject/source_data/monthly recession indicator.csv", stringsAsFactors=FALSE); head(rec_indic)

# Create a dataframe
df_comm <- data.frame(projdata); head(df_comm)
df_comm$Gold..USD...ozt. <- as.numeric(gsub(",", "", df_comm$Gold..USD...ozt.)) ;

# Append the recession indicator 
# NOTE NEED TO FIX: this because originally indicator had March 2021 as well
df_comm$rec_indic <- rec_indic$USREC[1:241]

# Change the month column to dates we can use to do calculations. Need the zoo package.
time = df_comm$Month 
# Converts the column into humanly readable dates. The as.Date function allows R to actually operate on it.
df_comm$Date = as.Date(as.yearmon(time)) ;
# Use zoo to convert dataframe to time series (easier to define price changes)
df_ts = as.ts(read.zoo(df_comm, FUN = as.yearmon)) ;

#df_comm$Date[which(df_comm$rec_indic == 1)]
#which(df_comm$rec_indic == 1)

df_comm$rec2001 <- df_comm$Date >= df_comm$Date[3] & df_comm$Date <= df_comm$Date[10]
df_comm$rec2008 <- df_comm$Date >= df_comm$Date[84] & df_comm$Date <= df_comm$Date[101]
df_comm$rec2020 <- df_comm$Date >= df_comm$Date[230] & df_comm$Date <= df_comm$Date[241]

# Note: need to think about how few data points we have for these time periods.
plot(df_comm$Date, df_comm$rec2001)
plot(df_comm$Date, df_comm$rec2008)
plot(df_comm$Date, df_comm$rec2020)


#*************************************
# Time-series: NOTES
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
#plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# This is why we should use logs

# ex 2.
# plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), type = 'l')

#plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)))

# logs, As line graphs
# plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), 'l')
# plot(df_comm$Date[2:length(df_comm$Date)], diff(log(df_comm$Gold..USD...ozt.)), type = 'l')
# plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# plot(df_comm$Date, df_comm$Beef..USD...kg., type = 'l')
# plot(df_comm$Date, df_comm$Crude.oil..USD...bbl., type = 'l')
# plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# hist(df_comm$Gold..USD...ozt.)
# hist(log(df_comm$Gold..USD...ozt.))
# hist(diff(log(df_comm$Gold..USD...ozt.)))
# hist(df_comm$Gold..USD...ozt.[1:120])
# hist(df_comm$Gold..USD...ozt.[121:240])
# hist(df_comm$Gold..USD...ozt.[1:120])
# hist(df_comm$Gold..USD...ozt.[121:240])

#*****************
#*Gold ____****______
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

# First half of the dataset, which is ordered according to time
hist(df_comm$Gold..USD...ozt.[1:120])
# Second half (of the time frame)
hist(df_comm$Gold..USD...ozt.[121:240])

# price changes/difference in prices
gold_price_change1 <- diff(log(df_comm$Gold..USD...ozt.[1:120]))
hist(gold_price_change1)

gold_price_change2 <- diff(log(df_comm$Gold..USD...ozt.[121:240]))
hist(gold_price_change2)

# differences in price changes
gold_diff_diff1 <- diff(diff(log(df_comm$Gold..USD...ozt.[1:120])))
hist(gold_diff_diff1)

gold_diff_diff2 <- diff(diff(log(df_comm$Gold..USD...ozt.[121:240])))
hist(gold_diff_diff2)

#Gold distribution
pval_gold_diff_diff1 <- chiSqTest(gold_diff_diff1)
# [1] "Chi-sq test statistic:"
# [1] "377.769491525424"
# [1] "p-value with df = {nbins - 2}:"
# [1] "1.06109053748395e-76"

pval_gold_diff_diff2 <- chiSqTest(gold_diff_diff2)
# [1] "Chi-sq test statistic:"
# [1] "377.769491525424"
# [1] "p-value with df = {nbins - 2}:"
# [1] "1.06109053748395e-76"

# QQ plots to see how distribution compares to normal distribution
# REFER TO https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot for a review

# Log Price Changes/percentage changes
qqnorm(gold_price_change1)
qqline(gold_price_change1) 

qqnorm(gold_price_change2)
qqline(gold_price_change2) 

# Changes in log price changes/percentage changes ("acceleration")
qqnorm(gold_diff_diff1)
qqline(gold_diff_diff1) 

qqnorm(gold_diff_diff2)
qqline(gold_diff_diff2) 

#~~~~~~~~~~~~
#Comparing during a recession (without distinguishing what kind of recession)
#~~~~~~~~~~~~

# df_comm$Date[which(df_comm$rec_indic == 1)]
#which(df_comm$rec_indic == 1)

# Do we need daily data on this...
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 0)], breaks=50)
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 1)], breaks=50)

hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 0)])
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 1)])

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
#*Beef *******####****
#*****************
hist(df_comm$Beef..USD...kg., breaks=50)
#bimodal

length(df_comm$Beef..USD...kg.)
#241 observations

#histogram of purely values, not time series
hist(df_comm$Beef..USD...kg.)
# Bimodal, no apparent skewness

#Time series of prices over time

#Time series of price-changes over time

#create a new dataframe of price differences

# First half of the dataset, which is ordered according to time
hist(df_comm$Beef..USD...kg.[1:120])
# Second half (of the time frame)
hist(df_comm$Beef..USD...kg.[121:240])

# price changes/difference in prices
beef_price_change1 <- diff(log(df_comm$Beef..USD...kg.[1:120]))
hist(beef_price_change1)

beef_price_change2 <- diff(log(df_comm$Beef..USD...kg.[121:240]))
hist(beef_price_change2)

# differences in price changes
beef_diff_diff1 <- diff(diff(log(df_comm$Beef..USD...kg.[1:120])))
hist(beef_diff_diff1)

beef_diff_diff2 <- diff(diff(log(df_comm$Beef..USD...kg.[121:240])))
hist(beef_diff_diff2)

#Beef distribution
pval_beef_diff_diff1 <- chiSqTest(beef_diff_diff1)
# [1] "Chi-sq test statistic:"
# [1] "379.125423728814"
# [1] "p-value with df = {nbins - 2}:"
# [1] "5.44450985295908e-77"

pval_beef_diff_diff2 <- chiSqTest(beef_diff_diff2)
# [1] "Chi-sq test statistic:"
# [1] "379.908474576271"
# [1] "p-value with df = {nbins - 2}:"
# [1] "3.70335576819339e-77"

# QQ plots to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(beef_price_change1)
qqline(beef_price_change1) 

qqnorm(beef_price_change2) #Note what's happening in the right and left tails.
qqline(beef_price_change2) 

# Changes in log price changes/percentage changes ("acceleration")
qqnorm(beef_diff_diff1)
qqline(beef_diff_diff1) 

qqnorm(beef_diff_diff2)
qqline(beef_diff_diff2) # Note what's happening in the right and left tails

#~~~~~~~~~~~~
#Comparing during a recession (without distinguishing what kind of recession)
#~~~~~~~~~~~~

# df_comm$Date[which(df_comm$rec_indic == 1)]
#which(df_comm$rec_indic == 1)

# Do we need daily data on this...
hist(df_comm$Beef..USD...kg.[which(df_comm$rec_indic == 0)])
hist(df_comm$Beef..USD...kg.[which(df_comm$rec_indic == 1)])

# Refined with more bins
hist(df_comm$Beef..USD...kg.[which(df_comm$rec_indic == 0)], breaks=50)
hist(df_comm$Beef..USD...kg.[which(df_comm$rec_indic == 1)], breaks=50)



#*****************
#*Rubber
#*****************
hist(df_comm$Rubber..USD...kg., breaks=50)

# More skewness as well