# Math 23C: Final project
# Daily data analyses: Goods prices and the different recessions
# Monthly data analyses
set.seed(24)

#************************
#*Data set-up
#************************

#library(ggplot2)
#library(zoo)
# setwd("~/Dropbox (Personal)/Work 2021/- Math 23c/term project")
#setwd("/Users/stai/math23c-rproject")


# Get the data. Make sure to set the file paths to your local file path

# Monthly data and recession indicators (monthly and daily)
projdata <- read.csv("/Users/stai/math23c-rproject/source_data/commodities data.csv", stringsAsFactors=FALSE); head(projdata)
HSY_data <- read.csv("/Users/stai/math23c-rproject/source_data/HSY.csv", stringsAsFactors=FALSE); head(HSY_data)
rec_indic <-read.csv("/Users/stai/math23c-rproject/source_data/monthly recession indicator.csv", stringsAsFactors=FALSE); head(rec_indic)
daily_rec_indic <-read.csv("/Users/stai/math23c-rproject/source_data/daily_USRECInd.csv", stringsAsFactors=FALSE); head(daily_rec_indic)

# Daily data for goods
dailyWTI <- read.csv("/Users/stai/math23c-rproject/source_data/dailyWTI.csv", stringsAsFactors=FALSE); head(dailyWTI)
dailySUG <- read.csv("/Users/stai/math23c-rproject/source_data/CANE_daily.csv", stringsAsFactors=FALSE); head(dailySUG)
dailySOYB <- read.csv("/Users/stai/math23c-rproject/source_data/SOYB_daily.csv", stringsAsFactors=FALSE); head(dailySOYB)
dailyWEAT <- read.csv("/Users/stai/math23c-rproject/source_data/WEAT_daily.csv", stringsAsFactors=FALSE); head(dailyWEAT)
dailyGOLD <- read.csv("/Users/stai/math23c-rproject/source_data/GOLD_daily.csv", stringsAsFactors=FALSE); head(dailyGOLD)
dailyCOCOA <- read.csv("/Users/stai/math23c-rproject/source_data/COCOA_daily.csv", stringsAsFactors=FALSE); head(dailyCOCOA)


#********************************
#* Clean up data and create dataframes
#********************************


#---------------------
# Monthly data dataframe
#---------------------

# Create a dataframe for monthly data
df_comm <- data.frame(projdata); head(df_comm)
df_comm$Gold..USD...ozt. <- as.numeric(gsub(",", "", df_comm$Gold..USD...ozt.)) ;


#---------------------
# Daily data dataframe
#---------------------

# Create a dataframe for the daily data; standardize all the date columns 

# Standardize the dates column to create equal-length vectors for each good
dailyCOCOA$stdDATE = dailyCOCOA$Date
dailyWEAT$stdDATE = dailyWEAT$Date
dailySOYB$stdDATE = dailySOYB$Date
dailySUG$stdDATE = dailySUG$Date
dailyWTI$stdDATE = dailyWTI$Date
dailyGOLD$stdDATE = dailyGOLD$Date
HSY_data$stdDATE = HSY_data$Date

#Relabel the prices column to indicate which good
dailyCOCOA$priceCOCOA = dailyCOCOA$Adj.Close
dailyWEAT$priceWEAT = dailyWEAT$Adj.Close
dailySOYB$priceSOYB = dailySOYB$Adj.Close
dailySUG$priceSUG = dailySUG$Adj.Close
dailyWTI$priceOIL = dailyWTI$DCOILWTICO
dailyGOLD$priceGOLD = dailyGOLD$GOLDAMGBD228NLBM
HSY_data$priceHYS = HSY_data$Adj.Close

#Create the first step of the dataframe
dailydata <- data.frame(cbind(dailyGOLD$DATE, dailyGOLD$priceGOLD, 
                              dailyWTI$priceOIL, dailyCOCOA$priceCOCOA, 
                              dailySUG$priceSUG, HSY_data$priceHYS, 
                              dailyWEAT$priceWEAT, dailySOYB$priceSOYB))

#Rename the columns
names(dailydata)[names(dailydata) == "X1"] <- "Date"
names(dailydata)[names(dailydata) == "X2"] <- "priceGOLD"
names(dailydata)[names(dailydata) == "X3"] <- "priceWTI"
names(dailydata)[names(dailydata) == "X4"] <- "priceCOCOA"
names(dailydata)[names(dailydata) == "X5"] <- "priceSUG"
names(dailydata)[names(dailydata) == "X6"] <- "priceHYS"
names(dailydata)[names(dailydata) == "X7"] <- "priceWEAT"
names(dailydata)[names(dailydata) == "X8"] <- "priceSOYB"

#head(dailydata)


#---------------------------------------------
#Cleaning and adding the recession indicators.
# These indicators only include information 
# on whether or not there was a recession.
#---------------------------------------------

#Create recession indicator dataframe
daily_rec_indicdf <- data.frame(daily_rec_indic)

#Find the data that are the same
daily_rec_indicdf$DATE %in% dailydata$Date
# summary(daily_rec_indicdf$DATE %in% dailydata$Date)

# Drop the weekend dates, as the goods prices don't include those because the 
# market's closed on those days.

rec_inds_use <- daily_rec_indicdf[which((daily_rec_indicdf$DATE %in% dailydata$Date)==TRUE),]
summary(rec_inds_use)
# Length should be 5259  

# The data vectors match; the dates are aligned. 
# Bind the recession indicators to the dailydata dataframe, renaming it.
dailydata2 <- data.frame(cbind(dailydata, rec_inds_use$USRECD))

#---------------------------------------------
#Cleaning and adding the indicators for 
# recession type.
# TYPE and INDICATOR
# no recession: 0
# dot-com crash: 1
# subprime mortgage recession: 2
# COVID-19 recession: 3
#---------------------------------------------

#Duplicate the daily recession indicator data from the prior section
rec_type <- daily_rec_indicdf

#Change the indicator from 1 to 2 or 3 for the housing crisis and covid recessions.
rec_type$USRECD["2008-01-01" <= rec_type$DATE & rec_type$DATE<= "2009-06-30" & rec_type$USRECD == "1"] <- "2"
# 547 values
#second runthrough: 391

rec_type$USRECD["2020-03-01" <= rec_type$DATE & rec_type$DATE<= "2021-02-28" & rec_type$USRECD == "1"] <- "3"
# 365 values

#dotcom crash
# 244 values
# Second runthrough 435

#no type of recession: 4433


#Dates and rows for the recessions
#Dotcom crash: keep as indicator 1
# 91   2001-04-01      1
# 334  2001-11-30      1

#Housing crisis: turn to indicator 2
# 2557 2008-01-01      1
# 3103 2009-06-30      1

#COVID-19 pandemic turn to indicator 3
# 7000 2020-03-01      1
# 7364 2021-02-28      1


# Equalize the vector lengths by dropping the weekend dates
# Duplicating the dataframe to make this change so that we don't lose any data
# that we may want later
rec_type_df <- rec_type
rec_types_use <- rec_type_df[which((rec_type_df$DATE %in% dailydata2$Date)==TRUE),]

#Bind the recession type indicators to create
# the data frame with all goods, recession indicators, and recession type indicators
dailydata_ALL <- data.frame(cbind(dailydata2, rec_types_use$USRECD))

#latest version on Git: "dailydata updated"


#-----------------------------
# Monthly recession indicators
# for monthly data's dataframe
#-----------------------------


# Drop the March 2021 datapoint because we are only investigating to the end of Feb 2021.
df_comm$rec_indic <- rec_indic$USREC[1:241]

# Change the month column to dates we can use to do calculations. 
# Make sure to download the the zoo package.
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

# Plots for each recession's monthly indicators, left as 0 and 1
plot(df_comm$Date, df_comm$rec2001)
plot(df_comm$Date, df_comm$rec2008)
plot(df_comm$Date, df_comm$rec2020)


#************************
#*ANALYSES
#************************


#-----------------
#Code for tests
#-----------------

#chiSqTest code we wrote for this. 
# Should be in accompanying file. Comment out once you've used.
#chiSqTest <- dget("chiSqTest.R")


#---------------------------------------------
# GOLD
#---------------------------------------------

#~~~~~~~~~~~~~
# Monthly
#~~~~~~~~~~~~~

# Basic values; no time-series

length(df_comm$Gold..USD...ozt.)
#241 observations

# Histogram of prices
hist(df_comm$Gold..USD...ozt., breaks=50)
# Bimodal, no apparent skewness

#Line graphs
plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# Random walk

# Log comparisons of gold prices to rescale
hist(log(df_comm$Gold..USD...ozt.))
# Not a normal distribution; negative skewness?

#Difference in log values over time
hist(diff(log(df_comm$Gold..USD...ozt.)))
# Normal-ish distribution

# Dataset is organized chronologically
# First half of the two decades
hist(df_comm$Gold..USD...ozt.[1:120])
summary(df_comm$Gold..USD...ozt.[1:120])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 260.5   359.6   552.4   626.0   888.9  1390.5
var(df_comm$Gold..USD...ozt.[1:120])
# Greater variance than the second half of the time period: see below.
# 98757.43
sd(df_comm$Gold..USD...ozt.[1:120])
# 314.2569


# Second half 
hist(df_comm$Gold..USD...ozt.[121:240])
summary(df_comm$Gold..USD...ozt.[121:240])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1076    1244    1318    1406    1592    1969 
var(df_comm$Gold..USD...ozt.[121:240])
# Variance: 47899.8
sd(df_comm$Gold..USD...ozt.[121:240])
# 218.8602

#Conclusions about the two halves of the two decades:
# The greater variance and standard deviation of the first half
# of the decade indicates greater volatility in gold prices. 
# To consider: why is gold seemingly more stable in the second half of the time period,
# when the mean price of gold was also higher? Why was there less volatility? 
# See below for consideration of the price changes themselves.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monthly: Differences in price changes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gold_price_change_Feb01Jan2011 <- diff(log(df_comm$Gold..USD...ozt.[1:120]))
hist(gold_price_change_Feb01Jan2011)
summary(gold_price_change_Feb01Jan2011)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.12480 -0.01037  0.01447  0.01385  0.04141  0.10217 
var(gold_price_change_Feb01Jan2011)
#0.001555529
sd(gold_price_change_Feb01Jan2011)
#0.03944019

gold_price_change_Feb11Jan21 <- diff(log(df_comm$Gold..USD...ozt.[121:240]))
hist(gold_price_change_Feb11Jan21)
summary(gold_price_change_Feb11Jan21)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0683367 -0.0183982 -0.0004282  0.0025723  0.0239885  0.1119255 
var(gold_price_change_Feb11Jan21)
# 0.001152209
sd(gold_price_change_Feb11Jan21)
#0.0339442


# difference in price changes
gold_diff_diffFeb01Jan2011 <- diff(diff(log(df_comm$Gold..USD...ozt.[1:120])))
hist(gold_diff_diffFeb01Jan2011)
# Tight normal distribution?
summary(gold_diff_diffFeb01Jan2011)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.2255643 -0.0347651  0.0030388 -0.0002228  0.0316488  0.1858971 
var(gold_diff_diffFeb01Jan2011)
#0.003063077
sd(gold_diff_diffFeb01Jan2011)
#0.05534507

gold_diff_diffFeb11Jan21 <- diff(diff(log(df_comm$Gold..USD...ozt.[121:240])))
hist(gold_diff_diffFeb11Jan21)
summary(gold_diff_diffFeb11Jan21)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.1044888 -0.0275081 -0.0001921 -0.0002554  0.0248185  0.1041309 
var(gold_diff_diffFeb11Jan21)
#0.001758639
sd(gold_diff_diffFeb11Jan21)
#0.04193613

# The intraperiod prices changes during the two periods is very similar.
# The differences in price changes, however, is less similar. There is
# greater variance in the first decade's difference in price changes.
# This again confirms the need to question why the first decade was
# more volatile for gold prices. See the RMD file for comparison across goods.

#***Chi-square tests on normal distribution
#*
#*Use the code in the accompanying file chiSqTest.R

# Differences in the price changes
pval_gold_diff_diffFeb01Jan2011 <- chiSqTest(gold_diff_diffFeb01Jan2011)
"Chi-sq test statistic:"
"377.769491525424"
"p-value with df = {nbins - 2}:"
"1.06109053748395e-76"
# The Chi-sq test confirms this is a normal distribution,
# with a tiny p-value. This normal distribution has a very
# very small chance of having occurred by chance.

pval_gold_diff_diffFeb11Jan21 <- chiSqTest(gold_diff_diffFeb11Jan21)
"Chi-sq test statistic:"
"377.769491525424"
"p-value with df = {nbins - 2}:"
"1.06109053748395e-76"
# The Chi-sq test confirms this is a normal distribution,
# with a tiny p-value. This normal distribution has a very
# very small chance of having occurred by chance.


#QQ plots test of normality

# QQ plots to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(gold_price_change_Feb01Jan2011)
qqline(gold_price_change_Feb01Jan2011) 

qqnorm(gold_diff_diffFeb11Jan21)
qqline(gold_diff_diffFeb11Jan21) 

# Changes in log price changes/percentage changes
qqnorm(gold_diff_diffFeb01Jan2011)
qqline(gold_diff_diffFeb01Jan2011) 
# Looks normal, but need to look more closely at the tails.
# There is divergence from the x=y line at the tails, 
# implying that the dataset is heavy-tailed.It's heavier-tailed
# than the monthly data from the second half of the time period.

qqnorm(gold_diff_diffFeb11Jan21)
qqline(gold_diff_diffFeb11Jan21) 
# See above.



# Comparing to relationship with recessions. 
# However, looking at this at the monthly level is likely not granular enough
# for us to conclude anything meaningful beyond the clustering around
# certain prices. This is in line with the heavy-tailed QQ plots.
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 0)])
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 1)])

hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 0)], breaks=50)
hist(df_comm$Gold..USD...ozt.[which(df_comm$rec_indic == 1)], breaks=50)

#~~~~~~~~~~~~~
# Daily: Gold
# Comparing independence of price changes
# and recession
#~~~~~~~~~~~~~
# We look at the relation between gold's prices changes and a recession
# at the daily price level.

#----------------------
# Daily: Basic rundown
#----------------------

# Histogram of prices
# Note: the values are strings, not numbers. So need to not include the 
# missing values.
daily_price_GOLD <- as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")])

hist(as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")]), breaks=50)

#Line graphs
plot(as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")]), type = 'l')


# Log comparisons of gold prices to rescale
hist(log(as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")])))


#Difference in log values over time
hist(diff(log(as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")]))))


#-------------
# Daily: price changes
#------------

daily_gold_price_change <- diff(log(daily_price_GOLD))
hist(daily_gold_price_change)

# differences in price changes
daily_gold_diff_diff <- diff(diff(log(daily_price_GOLD)))
hist(daily_gold_diff_diff)
var(daily_gold_diff_diff)
# 0.0002503008

#***Chi-square tests on normal distribution

# Differences in the price changes
daily_pval_gold_diff_diff<- chiSqTest(daily_gold_diff_diff)
"Chi-sq test statistic:"
"16294.3909251621"
"p-value with df = {nbins - 2}:"
"0"


#QQ plots test of normality

# QQ plots to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(daily_gold_price_change)
qqline(daily_gold_price_change) 
#WHOA, that's not normal. This is 
# a heavy-tailed QQ plot. We saw this slightly in the monthly data, where the 
# tails also did not follow a normal distribution, veering away from the 
# y = x line. With the daily data, it is even clearer the tails are heavy.
# See below for analysis that removes data points with a z-score > |2.5|


# Changes in log price changes/percentage changes
qqnorm(daily_gold_diff_diff)
qqline(daily_gold_diff_diff) 
#WHOA, that's not normal either. While the 
# middle of the Longer dark spot than price changes.
# We saw this slightly in the monthly data, where the 
# tails also did not follow a normal distribution, veering away from the 
# y = x line. 


#Localized QQ plot analysis
# Use the subset function


# Comparing to relationship with recessions. 
hist(daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 0)])
hist(daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 1)])

#frequency
hist(daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 0)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 1)], breaks=50)

#Types of recessions
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)])
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)])
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)])
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)])

#frequency
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)], breaks=50)

#-------------------------------------
# Daily: Gold: Pareto distribution 
# Using code from pset #5 R homework
#-------------------------------------

# Given the density function: 
paretopdf <- function(y) 4*y^(-5)

# Distribution function from integrating pdf from 1 to y: 
# 1 - (1/y^4).

# Quantile 
# q = 1 - (1/y^4)
CDF <- function(y) 1 - (1/y^4)
# y = 1 / (1-q)^(1/4)
invCDF <- function(q) 1 / (1-q)^(1/4)

# Generating 10000 uniform random numbers to be the quantiles
quantiles <- runif(10000)
pareto_draws <- invCDF(quantiles)

hist(pareto_draws, prob=TRUE)
curve(paretopdf, col="darkblue", lwd=3.2, add=TRUE)
# The curve of the pareto's density function matches the values that were
# randomly drawn according to the distribution function's inverse.

# b) 
library(fitdistrplus)
# Leave the library commands; comment out the installs.


# Use a qq plot to see if the claims follow Pareto distribution with different parameters
# (Thanks to Samuel Moy for the idea to use a qq plot)
# alpha = 3-5 still created a steep curve

# The 1.25 creates a straight line between the theoretical quantiles and sample ones.
CDF <- function(y) 1 - (1/y^1.25)

#-----
# generating quantiles for the number of data points in the sample 
# e.g. if 100 data points, then [1/100, 2/100, 3/100, ..., 100/100]
sample_quantiles <- (1:length(dailydata_ALL$priceGOLD)) / length(danishuni$Loss) 

# sorting the data set to compute each datapoint's theoretical quantile if it followed
# the given distribution function. e.g. pareto with parameter of alpha.
theoretical_quantiles <- CDF(sort(danishuni$Loss))

# This illustrates how well the theoretical distribution matches the empirical distribution.
plot(theoretical_quantiles, sample_quantiles)

# Another possible method, due to the extreme differences in scale, could be to look
# at the distribution of the log of the losses. 

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(danishuni$Loss), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)

# This looks pretty good! The curve fits the historgram. 


#-------------------------------------
# Daily: Gold: Levy distribution 
#-------------------------------------

