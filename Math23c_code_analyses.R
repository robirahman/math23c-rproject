# Math 23C: Final project
# Distribution analyses
# Daily data analyses: Goods prices and the different recessions
# Monthly data analyses
set.seed(24)

#************************
#*Data set-up
#************************

#library(ggplot2)
#library(zoo)
# setwd("~/Dropbox (Personal)/Work 2021/- Math 23c/term project")
# setwd("/Users/stai/math23c-rproject")


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
chiSqTest <- dget("chiSqTest.R")


#---------------------------------------------
# GOLD: Data from the St. Louis Fed
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
# We continue the gold analysis by breaking the monthly data into two time
# periods because the bimodality appears to align with the two decades.

#Line graphs
plot(df_comm$Date, df_comm$Gold..USD...ozt., type = 'l')
# Looks like a random walk

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
curve(dnorm(x, 
            mean(gold_price_change_Feb01Jan2011), 
            sd=sqrt(var(gold_price_change_Feb01Jan2011))), 
      add=TRUE, col = "red")
#The normal distribution is not a good fit.

gold_price_change_Feb11Jan21 <- diff(log(df_comm$Gold..USD...ozt.[121:240]))
hist(gold_price_change_Feb11Jan21)
summary(gold_price_change_Feb11Jan21)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0683367 -0.0183982 -0.0004282  0.0025723  0.0239885  0.1119255 
var(gold_price_change_Feb11Jan21)
# 0.001152209
sd(gold_price_change_Feb11Jan21)
#0.0339442
curve(dnorm(x, 
            mean(gold_price_change_Feb11Jan21), 
            sd=sqrt(var(gold_price_change_Feb11Jan21))), 
      add=TRUE, col = "red")
# Normal distribution is not a good fit


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
curve(dnorm(x, 
            mean(gold_diff_diffFeb01Jan2011), 
            sd=sqrt(var(gold_diff_diffFeb01Jan2011))), 
      add=TRUE, col = "red")
# Not a good fit

gold_diff_diffFeb11Jan21 <- diff(diff(log(df_comm$Gold..USD...ozt.[121:240])))
hist(gold_diff_diffFeb11Jan21)
summary(gold_diff_diffFeb11Jan21)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.1044888 -0.0275081 -0.0001921 -0.0002554  0.0248185  0.1041309 
var(gold_diff_diffFeb11Jan21)
#0.001758639
sd(gold_diff_diffFeb11Jan21)
#0.04193613
curve(dnorm(x, 
            mean(gold_diff_diffFeb11Jan21), 
            sd=sqrt(var(gold_diff_diffFeb11Jan21))), 
      add=TRUE, col = "red")
# Not a good fit

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
# The Chi-sq test confirms this is not a normal distribution.
# With such a small p-value, we reject the null hypothesis of this being a normal distribution. 
# This non-normal distribution has a very very small chance of having occurred by chance.

pval_gold_diff_diffFeb11Jan21 <- chiSqTest(gold_diff_diffFeb11Jan21)
"Chi-sq test statistic:"
"377.769491525424"
"p-value with df = {nbins - 2}:"
"1.06109053748395e-76"
# The Chi-sq test confirms this is not a normal distribution.
# With such a small p-value, we reject the null hypothesis of this being a normal distribution. 
# This non-normal distribution has a very very small chance of having occurred by chance.


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

# Note: the values are strings, not numbers. So need to not include the 
# missing values.
daily_price_GOLD <- as.numeric(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")])

# Recession variables for gold
gold_no_rec <- daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 0)]
gold_any_rec <- daily_price_GOLD[which(dailydata_ALL$rec_inds_use.USRECD == 1)]

#Types of recessions variables for gold
gold_no_rec_type <- daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)]
gold_dotcom <- daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)]
gold_GreatRec <- daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)]
gold_COVID <- daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)]

#------------------------------------

# Histogram of prices
hist(daily_price_GOLD, breaks=50)
summary(daily_price_GOLD)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#256.7   544.8  1173.2  1015.8  1327.8  2061.5
var(daily_price_GOLD)
#228427.9
sd(daily_price_GOLD)
#477.9413

# Comparing prices during recessions
summary(gold_no_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 256.7   547.7  1209.2  1038.0  1336.8  2061.5 
hist(gold_no_rec)
# bimodal, likely due to the price of gold being so much higher in the second decade
var(gold_no_rec)
# 223408.9
sd(gold_no_rec)
# 472.6615

# summary(gold_any_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 257.0   290.6   890.1   867.1   949.5  1957.5     166 
# There are 166 values that are NAs. These appear to be week days where 
# the market was not open, such as holidays.
# Rerun without these dates for the analysis because any effect of these
# holidays on the prices will be captured in the prices of the first open
# market day after the holiday. Rename gold_any_rec.
gold_any_rec <- dailydata_ALL$priceGOLD[which(dailydata_ALL$rec_inds_use.USRECD == 1 & dailydata_ALL$priceGOLD != ".")]
# Cast the variable to ensure usage as numbers, not strings
gold_any_rec <- as.numeric(gold_any_rec)
summary(gold_any_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 257.0   773.3   913.9  1048.5  1716.5  2061.5
hist(gold_any_rec)
# clustered around the prices during these time periods
var(gold_any_rec)
# 331198.8
sd(gold_any_rec)
# 575.4987


summary(gold_dotcom)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 257.0   267.4   273.1   273.5   277.7   292.9 
hist(gold_dotcom)
# normal with heavy tails; possible positive skewness
var(gold_dotcom)
# 69.66963
sd(gold_dotcom)
# 8.346833

summary(gold_GreatRec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 692.5   864.0   910.0   894.5   938.8  1020.5
hist(gold_GreatRec)
# negative skewness
var(gold_GreatRec)
# 4508.915
sd(gold_GreatRec)
# 67.14845

# summary(gold_COVID)
   #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   # 1765    1834    1858    1858    1884    1957     166 
# Redo without the weekday holidays
gold_COVID <- dailydata_ALL$priceGOLD[which(dailydata_ALL$rec_types_use.USRECD == 3 & dailydata_ALL$priceGOLD != ".")]
gold_COVID <- as.numeric(gold_COVID)
summary(gold_COVID)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1472    1728    1837    1814    1897    2062 
hist(gold_COVID)
# Heavy and long tails
var(gold_COVID)
# 13042.65
sd(gold_COVID)
# 114.2044

#Line graphs
plot(daily_price_GOLD, type = 'l')
plot(gold_no_rec, type = 'l')

plot(gold_any_rec, type= 'l')
plot(gold_dotcom, type = 'l')
# Two great peaks, overall very slowly increasing trend

plot(gold_GreatRec, type='l')
# One giant dip, followed by increasing trend

plot(gold_COVID, type='l')
# Overall decreasing trend

# Log comparisons of gold prices to rescale
hist(log(daily_price_GOLD))

hist(log(gold_no_rec))
# negative skewness

hist(log(gold_any_rec))
#three clusters around certain prices, aligned with the log price of gold during
# each recession

hist(log(gold_dotcom))
#heavy-tailed normal

hist(log(gold_GreatRec))
#negative skewness

hist(log(gold_COVID))
#positive skewness

#Difference in log values over time
hist(diff(log(daily_price_GOLD)))


#-------------
# Daily: price changes
# Note: using log values to rescale
#------------

daily_gold_price_change <- diff(log(daily_price_GOLD))
daily_gold_price_change_no_rec <- diff(log(gold_no_rec))
daily_gold_price_change_rec <- diff(log(gold_any_rec))
daily_gd_price_chng_dotcom <- diff(log(gold_dotcom))
daily_gd_price_chng_GR <- diff(log(gold_GreatRec))
daily_gd_price_chng_C19 <- diff(log(gold_COVID))

#-------------------------------------

hist(daily_gold_price_change)
summary(daily_gold_price_change)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-0.0891278 -0.0052370  0.0004333  0.0003667  0.0061726  0.0955416 
var(daily_gold_price_change)
# 0.0001224745
sd(daily_gold_price_change)
# 0.01106682

# difference in price changes
daily_gold_diff_diff <- diff(diff(log(daily_price_GOLD)))
hist(daily_gold_diff_diff)
summary(daily_gold_diff_diff)
#   Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-1.270e-01 -8.403e-03 -1.802e-04 -2.300e-07  8.609e-03  1.151e-01
var(daily_gold_diff_diff)
# 0.0002503008
sd(daily_gold_diff_diff)
# 0.0158209

#****************

hist(daily_gold_price_change_rec)
summary(daily_gold_price_change_rec)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0857106 -0.0062178  0.0002771  0.0024089  0.0078989  1.1186148 

# difference in price changes
daily_gold_diff_diff_rec <- diff(daily_gold_price_change_rec)
hist(daily_gold_diff_diff_rec)
summary(daily_gold_diff_diff_rec)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -1.0897751 -0.0113805 -0.0004483 -0.0000163  0.0111433  1.1184327 
var(daily_gold_diff_diff_rec)
# 0.004291922
sd(daily_gold_diff_diff_rec)
# 0.06551276

#****************

hist(daily_gd_price_chng_dotcom)
summary(daily_gd_price_chng_dotcom)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0279777 -0.0040967  0.0001821  0.0003715  0.0038493  0.0580054 

# difference in price changes
daily_gold_diff_diff_dotcom <- diff(daily_gd_price_chng_dotcom)
hist(daily_gold_diff_diff_dotcom)
summary(daily_gold_diff_diff_dotcom)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -7.762e-02 -6.404e-03  1.195e-03  2.605e-05  6.374e-03  5.672e-02 
var(daily_gold_diff_diff_dotcom)
# 0.0001772934
sd(daily_gold_diff_diff_dotcom)
# 0.01331516

#****************

hist(daily_gd_price_chng_GR)
summary(daily_gd_price_chng_GR)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0657935 -0.0084565 -0.0004112  0.0002834  0.0098430  0.0955416 

# difference in price changes
daily_gold_diff_diff_GR <- diff(daily_gd_price_chng_GR)
hist(daily_gold_diff_diff_GR)
summary(daily_gold_diff_diff_GR)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -1.270e-01 -1.407e-02 -5.823e-04 -7.180e-06  1.457e-02  8.819e-02 
var(daily_gold_diff_diff_GR)
# 0.0007015153
sd(daily_gold_diff_diff_GR)
# 0.02648613

#****************

hist(daily_gd_price_chng_C19)
summary(daily_gd_price_chng_C19)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.0540095 -0.0055388  0.0006364  0.0003672  0.0071458  0.0678994 

# difference in price changes
daily_gold_diff_diff_C19 <- diff(daily_gd_price_chng_C19)
hist(daily_gold_diff_diff_C19)
summary(daily_gold_diff_diff_C19)
#    Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -5.458e-02 -1.006e-02 -6.350e-04 -3.417e-05  9.685e-03  7.454e-02 
var(daily_gold_diff_diff_C19)
# 0.0002724171
sd(daily_gold_diff_diff_C19)
# 0.01650506




#***Chi-square tests on normal distribution

# Differences in the price changes
daily_pval_gold_diff_diff<- chiSqTest(daily_gold_diff_diff)
"Chi-sq test statistic:"
"16294.3909251621"
"p-value with df = {nbins - 2}:"
"0"
# We reject the null hypothesis that 
# the daily prices follow a normal distribution, with a [near] 0
# chance of this result having happened by chance.

#QQ plots test of normality to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(daily_gold_price_change)
qqline(daily_gold_price_change) 
#WHOA, that's not normal. This is 
# a heavy-tailed QQ plot. We saw this with the monthly data as well, where the 
# tails also did not follow a normal distribution, veering away from the 
# qq line. With the daily data, it is even clearer the tails are long and heavy.


# Changes in log price changes/percentage changes
qqnorm(daily_gold_diff_diff)
qqline(daily_gold_diff_diff) 
# This confirms as well that the differences in daily price
# changes do not follow a normal distribution.

#****************************************
#*Recap: Histograms and the good's normality
#****************************************

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

# To recap: none of these follows a normal distribution. We next test to see if 
# the prices or price changes follow a Pareto distribution.

#-------------------------------------
# Daily: Gold: Pareto distribution 
# Using code and notes from STai's PSet #5 R homework
#-------------------------------------
# Let's assess whether the distribution of gold's prices
# follows a Pareto distribution instead. A Pareto distribution
# can more closely model stock prices. 

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
# Note that the curve of the pareto's density function matches the values that were
# randomly drawn according to the distribution function's inverse.

library(fitdistrplus)

# Use a qq plot to see if the claims follow Pareto distribution with different parameters

# The 1.25 creates a straight line between the theoretical quantiles and sample ones.
CDF <- function(y) 1 - (1/y^1.25)

#-----
# generating quantiles for the number of data points in the sample 
# e.g. if 100 data points, then [1/100, 2/100, 3/100, ..., 100/100]
Gold_noNA <- dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")]
length_GOLD_noNA <- length(dailydata_ALL$priceGOLD[which(dailydata_ALL$priceGOLD != ".")])
sample_quantiles_gold <- (1:length_GOLD_noNA) / length(length_GOLD_noNA) 

# sorting the data set to compute each datapoint's theoretical quantile if it followed
# the given distribution function. e.g. pareto with parameter of alpha.
theoretical_quantiles_gold <- CDF(sort(as.numeric(Gold_noNA)))

# This QQ plot illustrates how well the theoretical distribution matches the empirical distribution.
plot(theoretical_quantiles_gold, sample_quantiles_gold)

# Rescale using log

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(as.numeric(Gold_noNA)), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)

# Does the curve fit the histogram? It appears not, at least not for the price of gold

# Quick assessment of gold's price changes
Gold_delt_noNA <- diff(as.numeric(Gold_noNA))
Gold_sample_quantiles_delta <- (1:length(Gold_delt_noNA)) / length(Gold_delt_noNA)
gold_delt_th_quant <- CDF(sort(Gold_delt_noNA))
plot(gold_delt_th_quant, Gold_sample_quantiles_delta)
# Definitely does not follow a 45 degree line; these values do not appear to follow a Pareto distribution.

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(Gold_delt_noNA), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# The Pareto distribution does not lie over the log(price changes of gold) histogram very well at all. 
# The scale of price changes can't be attributed to only a small proportion of the price changes.



#*************************************************************
#* Crude oil: WTI prices. Data from the St. Louis Fed.
#*************************************************************

#----------------------
# Daily: Basic rundown
#----------------------

# Note: the values are strings, not numbers. So need to not include the 
# missing values.
daily_price_WTI <- as.numeric(dailydata_ALL$priceWTI[which(dailydata_ALL$priceWTI != ".")])

# Recession variables
wti_no_rec <- daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 0)]
wti_any_rec <- daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 1)]

#Types of recessions variables
wti_no_rec_type <- daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 0)]
wti_dotcom <- daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 1)]
wti_GreatRec <- daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 2)]
wti_COVID <- daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 3)]

#------------------------------------

# Histogram of prices
hist(daily_price_WTI, breaks=50)
# Definitely not normal
summary(daily_price_WTI)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -36.98   42.80   58.64   62.27   82.46  145.31
var(daily_price_WTI)
#665.2304
sd(daily_price_WTI)
#25.79206

# Comparing prices during recessions
summary(wti_no_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -36.98   44.13   59.17   62.49   83.25  114.80 
hist(wti_no_rec)
# Very even and wide distribution during a recession;
# the price distribution looks almost uniform for frequencies of 400.
# Seems to confirm that oil is an inelastic good: no matter the cost,
# the people buy at the usual amounts.
# There is still a peak around 50 dollars/barrel.

var(wti_no_rec)
# 593.0466

sd(wti_no_rec)
# 24.35255

wti_any_rec <- dailydata_ALL$priceWTI[which(dailydata_ALL$rec_inds_use.USRECD == 1 & dailydata_ALL$priceWTI != ".")]
# Cast the variable to ensure usage as numbers, not strings
wti_any_rec <- as.numeric(wti_any_rec)
summary(wti_any_rec)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -36.98   28.82   43.17   57.48   75.14  145.31 
hist(wti_any_rec)
# clustered around the $40 - $50 mark, with a long and heavy upper tail
var(wti_any_rec)
# 1214.918
sd(wti_any_rec)
# 34.85568


summary(wti_dotcom)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.50   22.29   26.94   25.39   27.91   29.96 
hist(wti_dotcom)
# the value of wti weighs heavily on the upper end during the dotcom crash
var(wti_dotcom)
# 11.5367
sd(wti_dotcom)
# 3.396572

summary(wti_GreatRec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 30.28   51.51   69.34   77.75  108.69  145.31 
hist(wti_GreatRec)
# bimodal, with a larger bump at the lower end
var(wti_GreatRec)
# 4508.915
sd(wti_GreatRec)
# 31.82328

wti_COVID <- dailydata_ALL$priceWTI[which(dailydata_ALL$rec_types_use.USRECD == 3 & dailydata_ALL$priceWTI != ".")]
wti_COVID <- as.numeric(wti_COVID)
summary(wti_COVID)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -36.98   36.47   40.53   39.24   45.42   63.43
hist(wti_COVID)
# Centered around upper end, with the outlier of -40. If one recalls the news
# during this time, this should bring to mind the moment when the OPEC countries
# were at a loss on what to do.
var(wti_COVID)
# 138.5191
sd(wti_COVID)
# 11.76941

#Line graphs
plot(daily_price_WTI, type = 'l')
plot(wti_no_rec, type = 'l')
plot(wti_any_rec, type= 'l')
plot(wti_dotcom, type = 'l')
plot(wti_GreatRec, type='l')
plot(wti_COVID, type='l')
# These all follow a random walk. No periodicity.

# Log comparisons to rescale
hist(log(daily_price_WTI))
hist(log(wti_no_rec))
#both of these are heavy on the right

hist(log(wti_any_rec))
# no recognizable distribution at first glance
hist(log(wti_dotcom))
# Possibly a beta distribution?

hist(log(wti_GreatRec))
hist(log(wti_COVID))

#Difference in log values over time
hist(diff(log(daily_price_WTI)))


#-------------
# Daily: price changes
#
#------------

# Warning: min(daily_price_WTI) = -36.98; there are 
# some negative values that are being used in log commands
# because of oil market idiosyncrasies. Using non-rescaled prices instead.
daily_wti_price_change <- diff(daily_price_WTI)
daily_wti_price_change_no_rec <- diff(wti_no_rec)
daily_wti_price_change_rec <- diff(wti_any_rec)
daily_wti_price_chng_dotcom <- diff(wti_dotcom)
daily_wti_price_chng_GR <- diff(wti_GreatRec)
daily_wti_price_chng_C19 <- diff(wti_COVID)

#-------------------------------------

hist(daily_wti_price_change)
summary(daily_wti_price_change)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -55.29000  -0.69000   0.05000   0.00677   0.75000  45.89000
var(daily_wti_price_change)
# 3.163682
sd(daily_wti_price_change)
# 1.778674

# difference in price changes
daily_wti_diff_diff <- diff(diff(daily_price_WTI))
hist(daily_wti_diff_diff)
# Very small differences in price changes themselves! Always clustered around 0
summary(daily_wti_diff_diff)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-53.7800  -1.0700  -0.0200  -0.0005   1.0300 101.1800 
var(daily_wti_diff_diff)
# 7.419232
sd(daily_wti_diff_diff)
# 2.723827

#****************

hist(daily_wti_price_change_rec)
summary(daily_wti_price_change_rec)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -55.29000  -0.85500   0.07000   0.04509   0.93000  80.18000 

# difference in price changes
daily_wti_diff_diff_rec <- diff(daily_wti_price_change_rec)
hist(daily_wti_diff_diff_rec)
# Even during a recession the difference in price changes cluster around
# 0. The variance is larger, but remains reasonably-sized, with a standard
# deviation of 6.849487.
summary(daily_wti_diff_diff_rec)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -80.65000  -1.37750  -0.06500  -0.00356   1.28500 101.18000  
var(daily_wti_diff_diff_rec)
# 46.91547
sd(daily_wti_diff_diff_rec)
# 6.849487

#****************

hist(daily_wti_price_chng_dotcom)
summary(daily_wti_price_chng_dotcom)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.00000 -0.40250 -0.04000 -0.05046  0.33000  2.71000 

# difference in price changes
daily_wti_diff_diff_dotcom <- diff(daily_wti_price_chng_dotcom)
hist(daily_wti_diff_diff_dotcom)
summary(daily_wti_diff_diff_dotcom)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -4.360000 -0.560000 -0.010000  0.001503  0.550000  4.250000 
var(daily_wti_diff_diff_dotcom)
# 1.098891
sd(daily_wti_diff_diff_dotcom)
# 1.04828

#****************

hist(daily_wti_price_chng_GR)
summary(daily_wti_price_chng_GR)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -14.76000  -1.68250  -0.01500  -0.09482   1.51500  18.56000 

# difference in price changes
daily_wti_diff_diff_GR <- diff(daily_wti_price_chng_GR)
hist(daily_wti_diff_diff_GR)
# There is a longer lower tail for this due to the value < -30.
# Much more volatility during the Great Recession than there was in the dotcom crash.
# The greater variance confirms this.
summary(daily_wti_diff_diff_GR)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -33.32000  -2.29000  -0.05000   0.00149   2.21000  14.89000  
var(daily_wti_diff_diff_GR)
# 17.40762
sd(daily_wti_diff_diff_GR)
# 4.172244

#****************

hist(daily_wti_price_chng_C19)
summary(daily_wti_price_chng_C19)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -55.29000  -0.51000   0.18000   0.05932   0.87000  45.89000  

# difference in price changes
daily_wti_diff_diff_C19 <- diff(daily_wti_price_chng_C19)
hist(daily_wti_diff_diff_C19)
# During COVID-19, there was much greater volatility. 
# Note the two long tails, due to the extreme outliers of
# changes in price changes at both the upper and lower ends.
# The difference in price changes has a greater variance.
summary(daily_wti_diff_diff_C19)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-53.78000  -1.11250  -0.13500  -0.00956   0.95000 101.18000 
var(daily_wti_diff_diff_C19)
# 65.61629
sd(daily_wti_diff_diff_C19)
# 8.100388





#***Chi-square tests on normal distribution

# Differences in the price changes
daily_pval_wti_diff_diff<- chiSqTest(daily_wti_diff_diff)
# "Chi-sq test statistic:"
# "1536.02550919517"
# "p-value with df = {nbins - 2}:"
# "0"
# We definitely reject the null hypothesis that oil's changes in price changes
# follow a normal distribution.

#QQ plots test of normality to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(daily_wti_price_change)
qqline(daily_wti_price_change) 
# The data usually follow the normal distribution, but the outliers 
# of price changes creates heavy tails. The distribution is therefore not
# normal.


# Changes in price changes
qqnorm(daily_wti_diff_diff)
qqline(daily_wti_diff_diff) 
# Changes in price changes are even less normal, and even more heavily
# tailed than the price changes.

#****************************************
#*Recap: Histograms and the good's normality
#****************************************

# Comparing to relationship with recessions. 
hist(daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 0)])
hist(daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 1)])

#Binned
hist(daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 0)], breaks=50)
hist(daily_price_WTI[which(dailydata_ALL$rec_inds_use.USRECD == 1)], breaks=50)

#Types of recessions
hist(daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 0)])
hist(daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 1)])
hist(daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 2)])
hist(daily_price_WTI[which(dailydata_ALL$rec_types_use.USRECD == 3)])

#Types of recessions; binned
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)], breaks=50)

# None of these is normal, as demonstrated above. The 
# prices during the different recessions also appear to follow different distributions.

#-------------------------------------
# Daily: Oil: Pareto distribution 
# Using code and notes from STai's PSet #5 R homework
#-------------------------------------
# Let's assess whether the distribution of oil's prices
# follows a Pareto distribution instead. A Pareto distribution
# can more closely model stock prices. 

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
# Note that the curve of the pareto's density function matches the values that were
# randomly drawn according to the distribution function's inverse.

library(fitdistrplus)

# Use a qq plot to see if the claims follow Pareto distribution with different parameters

# The 1.25 creates a straight line between the theoretical quantiles and sample ones.
CDF <- function(y) 1 - (1/y^1.25)

#-----
# generating quantiles for the number of data points in the sample 
# e.g. if 100 data points, then [1/100, 2/100, 3/100, ..., 100/100]
wti_noNA <- dailydata_ALL$priceWTI[which(dailydata_ALL$priceWTI != ".")]
length_wti_noNA <- length(dailydata_ALL$priceWTI[which(dailydata_ALL$priceWTI != ".")])
sample_quantiles_wti <- (1:length_wti_noNA) / length_wti_noNA

# sorting the data set to compute each datapoint's theoretical quantile if it followed
# the given distribution function. e.g. pareto with parameter of alpha.
theoretical_quantiles_wti <- CDF(sort(as.numeric(wti_noNA)))

# This QQ plot illustrates how well the theoretical distribution matches the empirical distribution.
plot(theoretical_quantiles_wti, sample_quantiles_wti)
# It doesn't create a line; the Pareto distribution is not a good model

# Rescale using log

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(as.numeric(wti_noNA)), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)

# The curve does not fit the histogram.

# Quick assessment of oil's price changes
wti_delt_noNA <- diff(as.numeric(wti_noNA))
wti_sample_quantiles_delta <- (1:length(wti_delt_noNA)) / length(wti_delt_noNA)
wti_delt_th_quant <- CDF(sort(wti_delt_noNA))
plot(wti_delt_th_quant, wti_sample_quantiles_delta)
# Definitely does not follow a line; no Pareto distribution is established.

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(wti_delt_noNA), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# Rescaled logarithmically, the Pareto distribution fits the upper end of oil's price changes much better
# than it fits the prices themselves. It fits the upper end of the data
# for the log of the price changes. The lower end's end's data is not modeled well
# by the Pareto distribution, however. The data's variance is not captured.
# The upper end of price changes of oil, that is, price changes >0, possibly follows a Pareto distribution.
# This is, however, with some log(negative value) datapoints, which produces NaNs.

#Rescaled to using all data
alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(wti_delt_noNA, prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# Fits the data less well, missing the price changes > 0.


#*************************************************************
#* Sugar prices: Teucrium Sugar Fund prices from Yahoo Finance
#*************************************************************
#----------------------
# Daily: Basic rundown
#----------------------

# Note: the values are strings, not numbers. So need to not include the 
# missing values.
daily_price_SUG <- as.numeric(dailydata_ALL$priceSUG[which(dailydata_ALL$priceSUG != ".")])

# Recession variables
sug_no_rec <- daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 0)]
sug_any_rec <- daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 1)]

#Types of recessions variables
sug_no_rec_type <- daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 0)]
sug_dotcom <- daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 1)]
sug_GreatRec <- daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 2)]
sug_COVID <- daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 3)]

#------------------------------------

# Histogram of prices
hist(daily_price_SUG, breaks=50)
# Possibly follows a gamma distribution, with greatest frequency between 5 and 10
summary(daily_price_SUG)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.92    7.54   11.22   12.37   15.28   26.31 
var(daily_price_SUG)
#28.22823
sd(daily_price_SUG)
#5.313025

# Comparing prices during recessions
summary(sug_no_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.92    7.93   11.02   12.17   14.81   26.31
hist(sug_no_rec)
# Definitely not normal. Looks similar to wind speeds' distribution, but sugar
# prices do not fit the usual use case for a Weibull distribution.
var(sug_no_rec)
# 24.92819
sd(sug_no_rec)
# 4.992814

sug_any_rec <- dailydata_ALL$priceSUG[which(dailydata_ALL$rec_inds_use.USRECD == 1 & dailydata_ALL$priceSUG != ".")]
# Cast the variable to ensure usage as numbers, not strings
sug_any_rec <- as.numeric(sug_any_rec)
summary(sug_any_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.92    6.93   16.13   13.50   19.29   24.66
hist(sug_any_rec)
# Extremely bimodal. It appears to be 50/50 split in prices < $7 and prices >$14.
var(sug_any_rec)
# 44.49444
sd(sug_any_rec)
# 6.670416

summary(sug_dotcom)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.37   19.80   21.73   21.62   23.35   24.66 
hist(sug_dotcom)
# A lot of fluctuation during the dotcom recession.
# More evenly distributed prices than during no recession period.
# Comparisons to other recessions below.
var(sug_dotcom)
# 3.594999
sd(sug_dotcom)
# 1.896048

summary(sug_GreatRec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.920   6.445   6.900   6.724   7.270   7.810 
hist(sug_GreatRec)
# long lower tail, negative skewness.
var(sug_GreatRec)
# 0.4793186
sd(sug_GreatRec)
#  0.6923284
# The variance during the Great Recession is much lower than during
# the dotcom recession.

sug_COVID <- dailydata_ALL$priceSUG[which(dailydata_ALL$rec_types_use.USRECD == 3 & dailydata_ALL$priceSUG != ".")]
sug_COVID <- as.numeric(sug_COVID)
summary(sug_COVID)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.78   17.04   17.84   18.21   19.25   21.80
hist(sug_COVID)
# Heavier upper tail than during Great Recession; has a slight positive skewness.  
var(sug_COVID)
# 2.385077
sd(sug_COVID)
# 1.544369

#Line graphs
plot(daily_price_SUG, type = 'l')
# Upswings around recessionary periods. 
plot(sug_no_rec, type = 'l')
# No recession looks very similar to overall prices pattern
plot(sug_any_rec, type= 'l')
# Huge trough 
plot(sug_dotcom, type = 'l')
# downward trend; steep decline
plot(sug_GreatRec, type='l')
# Steep decline in the latter months before climbing back up.
plot(sug_COVID, type='l')
# During COVID recession months, there is a consistent downward trend, with no extreme declines.
# All follow random walk non-patterns

# Log comparisons to rescale
hist(log(daily_price_SUG))
hist(log(sug_no_rec))
#Very fat curves

hist(log(sug_any_rec))
# mean(log(sug_any_rec)) is 2.462173
# Bimodal around the mean.
hist(log(sug_dotcom))
# Similarly, bimodal around the mean
# mean(log(sug_dotcom)) = 3.069934 

hist(log(sug_GreatRec))
# Bimodal around the mean 
# mean(log(sug_GreatRec)) = 1.899976
hist(log(sug_COVID))
# Not bimodal, unlike the others. A very fat curve with a small peak
# mean(log(sug_COVID)) = 2.898559
curve(dnorm(x, mean(log(sug_COVID)), sd = sqrt(var(log(sug_COVID)))), add=TRUE, col = "red")
# Does not fit well; ot a normal distribution

#Difference in log values over time
hist(diff(log(daily_price_SUG)))
# Very small, nothing that stands out


#-------------
# Daily: price changes
#
#------------

daily_sug_price_change <- diff(daily_price_SUG)
daily_sug_price_change_no_rec <- diff(sug_no_rec)
daily_sug_price_change_rec <- diff(sug_any_rec)
daily_sug_price_chng_dotcom <- diff(sug_dotcom)
daily_sug_price_chng_GR <- diff(sug_GreatRec)
daily_sug_price_chng_C19 <- diff(sug_COVID)

#-------------------------------------

hist(daily_sug_price_change)
summary(daily_sug_price_change)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -2.219999 -0.090000  0.000000 -0.001721  0.070000 16.810001 
var(daily_sug_price_change)
# 0.1608409
sd(daily_sug_price_change)
# 0.4010498

# difference in price changes
daily_sug_diff_diff <- diff(diff(daily_price_SUG))
hist(daily_sug_diff_diff)
# Very small differences in price changes themselves! Always clustered around 0
summary(daily_sug_diff_diff)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -16.350002  -0.140000   0.000000  -0.000074   0.130002  16.810001 
var(daily_sug_diff_diff)
# 0.3365551
sd(daily_sug_diff_diff)
# 0.5801336

#****************

hist(daily_sug_price_change_rec)
summary(daily_sug_price_change_rec)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -11.430001  -0.090000   0.000000  -0.008352   0.050000  15.960000

# difference in price changes
daily_sug_diff_diff_rec <- diff(daily_sug_price_change_rec)
hist(daily_sug_diff_diff_rec)
# Changes in price changes for sugar cluster around 0, but there
# are long thin tails
summary(daily_sug_diff_diff_rec)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-16.050001  -0.130000   0.000000   0.000085   0.122500  16.030000   
var(daily_sug_diff_diff_rec)
# 1.128211
sd(daily_sug_diff_diff_rec)
# 1.062173

#****************

hist(daily_sug_price_chng_dotcom)
# Long thin tails with a tight curve
summary(daily_sug_price_chng_dotcom)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2.22000 -0.18000  0.00000 -0.02448  0.10750  2.51000

# difference in price changes
daily_sug_diff_diff_dotcom <- diff(daily_sug_price_chng_dotcom)
hist(daily_sug_diff_diff_dotcom)
# During the dotcom crash, we see that the changes
# in price changes have very long and thin tails; there is a little volatility.
# However, most of the changes are still clustered around 0, and the variance is low.
summary(daily_sug_diff_diff_dotcom)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.73000 -0.25000  0.00000  0.00185  0.23000  2.51000 
var(daily_sug_diff_diff_dotcom)
# 0.4446871
sd(daily_sug_diff_diff_dotcom)
# 0.6668486

#****************
hist(daily_sug_price_chng_GR)
hist(daily_sug_price_chng_GR, breaks = 50)
# A much more normal-looking distribution than compared to the others.
curve(dnorm(x, mean(daily_sug_price_chng_GR), sd = sqrt(var(daily_sug_price_chng_GR))), add=TRUE, col = "red")
# However, we see that the normal distribution for this mean and this standard deviation
# does not well-match the histogram for daily sugar price changes during the Great Recession.

summary(daily_sug_price_chng_GR)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.310000 -0.060000  0.000000 -0.003821  0.050000  0.380000  

# difference in price changes
daily_sug_diff_diff_GR <- diff(daily_sug_price_chng_GR)
hist(daily_sug_diff_diff_GR)
hist(daily_sug_diff_diff_GR, breaks = 100)
curve(dnorm(x, mean(daily_sug_diff_diff_GR), sd = sqrt(var(daily_sug_diff_diff_GR))), add=TRUE, col = "red")
# The curve is too tightly clustered around the mean 
# for this to follow the normal distribution.
summary(daily_sug_diff_diff_GR)
#  Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.4200000 -0.0800000 -0.0100000 -0.0002571  0.0700000  0.4400000   
var(daily_sug_diff_diff_GR)
# 0.01612983
sd(daily_sug_diff_diff_GR)
# 0.1270033

#****************

hist(daily_sug_price_chng_C19)
# The price changes remain clustered around the near-0 mean with a very long and thin
# tail due to the extremes.
summary(daily_sug_price_chng_C19)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2.22000 -0.15000  0.00000 -0.02189  0.08500  2.51000  

# difference in price changes
daily_sug_diff_diff_C19 <- diff(daily_sug_price_chng_C19)
hist(daily_sug_diff_diff_C19)
# Continues to cluster around the near-0 mean for differences in price changes.
# Continues to have very long and thin tails for the occasional outliers.
summary(daily_sug_diff_diff_C19)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.73000 -0.20750 -0.00500  0.00062  0.20000  2.51000 
var(daily_sug_diff_diff_C19)
# 0.2405248
sd(daily_sug_diff_diff_C19)
# 0.4904333





#***Chi-square tests on normal distribution

# Differences in the price changes
daily_pval_sug_diff_diff<- chiSqTest(daily_sug_diff_diff)
# "Chi-sq test statistic:"
# "9888.67053452539"
# "p-value with df = {nbins - 2}:"
# "0"
# We definitely reject the null hypothesis that sugar's changes in price changes
# follow a normal distribution.

#QQ plots test of normality to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(daily_sug_price_change)
qqline(daily_sug_price_change) 
# The data usually follow the normal distribution, but the outliers 
# of price changes creates heavy tails. The distribution is therefore not
# normal. This matches what we have seen with gold, considered a safe haven good, and
# oil, a price-inelastic good. Sugar is traditionally not thought of as either of these
# types of goods, and yet it similarly has non-normal price changes, with heavy tails.
# This seems to be a trait native to prices themselves, regardless of type of good.


# Changes in price changes
qqnorm(daily_sug_diff_diff)
qqline(daily_sug_diff_diff) 
# For sugar, changes in price changes have "lighter" tails, unlike for oil.

#****************************************
#*Recap: Histograms and sugar prices' normality
#****************************************

# Comparing to relationship with recessions. 
hist(daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 0)])
hist(daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 1)])

#Binned
hist(daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 0)], breaks=50)
hist(daily_price_SUG[which(dailydata_ALL$rec_inds_use.USRECD == 1)], breaks=50)

#Types of recessions
hist(daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 0)])
hist(daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 1)])
hist(daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 2)])
hist(daily_price_SUG[which(dailydata_ALL$rec_types_use.USRECD == 3)])

#Types of recessions; binned
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)], breaks=50)

# None of these is normal, as demonstrated above. The 
# prices during the different recessions also appear to follow different distributions
# with different skewness. The Great Recession is negatively skewed; COVID recession
# is closer to normal with a wide variance; dotcom crash has a slight positive skewness.

#-------------------------------------
# Daily: Sugar: Pareto distribution 
# Using code and notes from STai's PSet #5 R homework
#-------------------------------------
# Let's assess whether the distribution of sugar's prices
# follows a Pareto distribution instead. A Pareto distribution
# can more closely model stock prices. 

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
# Note that the curve of the pareto's density function matches the values that were
# randomly drawn according to the distribution function's inverse.

library(fitdistrplus)

# Use a qq plot to see if the claims follow Pareto distribution with different parameters

# The 1.25 creates a straight line between the theoretical quantiles and sample ones.
CDF <- function(y) 1 - (1/y^1.25)

#-----
# generating quantiles for the number of data points in the sample 
# e.g. if 100 data points, then [1/100, 2/100, 3/100, ..., 100/100]
sug_noNA <- dailydata_ALL$priceSUG[which(dailydata_ALL$priceSUG != ".")]
length_sug_noNA <- length(dailydata_ALL$priceSUG[which(dailydata_ALL$priceSUG != ".")])
sample_quantiles_sug <- (1:length_sug_noNA) / length_sug_noNA

# sorting the data set to compute each datapoint's theoretical quantile if it followed
# the given distribution function. e.g. pareto with parameter of alpha.
theoretical_quantiles_sug <- CDF(sort(as.numeric(sug_noNA)))

# This QQ plot illustrates how well the theoretical distribution matches the empirical distribution.
plot(theoretical_quantiles_sug, sample_quantiles_sug)
# Sugar prices' Pareto theoretical vs. sample quantiles has more of a linear relationship than
# the other goods prices' Pareto theoretical vs. sample quantils' relationship!

# Rescale using log

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(as.numeric(sug_noNA)), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# The curve does not fit the histogram.

# Quick assessment of sugar's price changes
sug_delt_noNA <- diff(as.numeric(sug_noNA))
sug_sample_quantiles_delta <- (1:length(sug_delt_noNA)) / length(sug_delt_noNA)
sug_delt_th_quant <- CDF(sort(sug_delt_noNA))
plot(sug_delt_th_quant, sug_sample_quantiles_delta)
# Definitely does not follow a line; no Pareto distribution is established.

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(sug_delt_noNA), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# Rescaled logarithmically, the Pareto distribution fits the shape of the logs of sugar
# price changes' histogram. However, the Pareto curve does not overlay the
# histogram.


#*************************************************************
# Wheat prices: Teucrium Wheat Fund prices from Yahoo Finance
#*************************************************************

#----------------------
# Daily: Basic rundown
#----------------------

# Note: the values are strings, not numbers. So need to not include the 
# missing values.
daily_price_WEAT <- as.numeric(dailydata_ALL$priceWEAT[which(dailydata_ALL$priceWEAT != ".")])

# Recession variables
weat_no_rec <- daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 0)]
weat_any_rec <- daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 1)]

#Types of recessions variables
weat_no_rec_type <- daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 0)]
weat_dotcom <- daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 1)]
weat_GreatRec <- daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 2)]
weat_COVID <- daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 3)]

#------------------------------------

# Histogram of prices
hist(daily_price_WEAT, breaks=50, main="Daily Wheat Prices from Jan 2001—Feb 2021", 
     xlab = "Prices in USD", ylab = "Frequency", col="yellow")
# Historically, prices have been between $5-$25, with greater frequency between $5-$7.
# Note that the most information that can be gleaned from historical data is exactly that:
# this is only historical data. 
summary(daily_price_WEAT)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.86    6.23    9.10   11.76   17.41   25.35  
var(daily_price_WEAT)
#41.45526
sd(daily_price_WEAT)
#6.438576

# Comparing prices during recessions
summary(weat_no_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.86    6.38    9.05   11.35   16.23   25.35
hist(weat_no_rec, main=c("Daily Wheat Prices from Jan 2001—Feb 2021", "Non-Recessionary Periods"), 
     xlab = "Prices in USD", ylab = "Frequency", col="yellow")
# Definitely not normal. Looks similar to wind speeds' distribution. 
# Purpose of this is to compare with recessionary periods.
var(weat_no_rec)
# 35.52707
sd(weat_no_rec)
# 5.960459

weat_any_rec <- dailydata_ALL$priceWEAT[which(dailydata_ALL$rec_inds_use.USRECD == 1 & dailydata_ALL$priceWEAT != ".")]
# Cast the variable to ensure usage as numbers, not strings
weat_any_rec <- as.numeric(weat_any_rec)
summary(weat_any_rec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.860   5.463  18.730  13.994  21.348  25.350
hist(weat_any_rec, main=c("Daily Wheat Prices from Jan 2001—Feb 2021", "Recessionary Periods"), 
     xlab = "Prices in USD", ylab = "Frequency", col="yellow")
# Extremely bimodal. It appears to be 50/50 split in prices < $7 and prices >$15.
var(weat_any_rec)
# 67.43344
sd(weat_any_rec)
# 8.211787


summary(weat_dotcom)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.60   20.30   21.12   21.56   22.36   25.30
hist(weat_dotcom, main=c("Daily Wheat Prices Post-DotCom Bubble", "Recession: Apr 2001 — Nov 2001"), 
     xlab = "Prices in USD", ylab = "Frequency", col="orange")
# Prices during this recession: negative skewness
var(weat_dotcom)
# 3.055292
sd(weat_dotcom)
# 1.747939

summary(weat_GreatRec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.860   5.260   5.450   5.487   5.715   6.150 
hist(weat_GreatRec, main=c("Daily Wheat Prices: Great Recession", "Recession: Jan 2008 — Jun 2009"), 
     xlab = "Prices in USD", ylab = "Frequency", col="dark orange")
# Prices of wheat during the Great Recession have lower variance; more clustered around the mean,
# less skewness. Not normal though.
curve(dnorm(x, mean(weat_GreatRec), sd = sqrt(var(weat_GreatRec))), col="dark green", lwd=3.2, add=TRUE)
var(weat_GreatRec)
# 0.09694789
sd(weat_GreatRec)
#  0.3113646
# The variance during the Great Recession is lower than during
# the dotcom recession.

weat_COVID <- dailydata_ALL$priceWEAT[which(dailydata_ALL$rec_types_use.USRECD == 3 & dailydata_ALL$priceWEAT != ".")]
weat_COVID <- as.numeric(weat_COVID)
summary(weat_COVID)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.91   19.43   21.33   21.70   23.96   25.35
hist(weat_COVID, main=c("Daily Wheat Prices during COVID-19 Recession", "Recession: Mar 2020 — Feb 2021"), 
     xlab = "Prices in USD", ylab = "Frequency", col="light yellow")
# Bimodal during pandemic  
var(weat_COVID)
# 5.13271
sd(weat_COVID)
# 2.265548

#Line graphs
plot(daily_price_WEAT, type = 'l', main=c("Daily Wheat Prices", "Jan 2001 — Feb 2021"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# Upswings around recessionary periods. 
plot(weat_no_rec, type = 'l', main=c("Daily Wheat Prices","Non-Recessionary Periods", "Jan 2001 — Feb 2021"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# No recession looks very similar to overall prices pattern
plot(weat_any_rec, type= 'l', main=c("Daily Wheat Prices","Recessionary Periods", "Jan 2001 — Feb 2021"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# Huge trough 
plot(weat_dotcom, type = 'l', main=c("Daily Wheat Prices","DotCom Crash", "Apr 2001 — Nov 2001"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# Steep increase in the latter half
plot(weat_GreatRec, type='l', main=c("Daily Wheat Prices","Great Recession", "Jan 2008 — Jun 2009"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# Volatile, with extreme ups and downs
plot(weat_COVID, type='l', main=c("Daily Wheat Prices","COVID-19 Recession", "Mar 2020 — Feb 2021"), 
     xlab="Index: Daily", ylab = "Prices in USD")
# There is a steep incline; the peak covers the middle portion before
# prices fall again.
# All follow random walk non-patterns

# Log comparisons to rescale
hist(log(daily_price_WEAT), main=c("Log of Daily Wheat Prices", "Jan 2001 — Feb 2021"), 
     xlab = "Prices in USD", ylab = "Frequency", col="orange")
hist(log(weat_no_rec), main=c("Log of Daily Wheat Prices", "Non-Recessionary Periods", "Jan 2001 — Feb 2021"), 
     xlab = "Prices in USD", ylab = "Frequency", col="orange")
# Greater frequency in lower half

hist(log(weat_any_rec), main=c("Log of Daily Wheat Prices", "Recessionary Periods", "Jan 2001 — Feb 2021"), 
     xlab = "Prices in USD", ylab = "Frequency", col="dark orange")
# mean(log(weat_any_rec)) is 2.421868
# Bimodal, but not clustered around the mean.
hist(log(weat_dotcom), main=c("Log of Daily Wheat Prices", "DotCom Crash", "Apr 2001 — Nov 2001"), 
     xlab = "Prices in USD", ylab = "Frequency", col="orange")
# Slight positive skewness, long and heavy tails
# mean(log(weat_dotcom)) = 3.067471

hist(log(weat_GreatRec), main=c("Log of Daily Wheat Prices", "Great Recession", "Jan 2008 — Jun 2009"), 
     xlab = "Prices in USD", ylab = "Frequency", col="dark orange")
# Clustered around the mean, very very fat tails 
# mean(log(weat_GreatRec)) = 1.700779
hist(log(weat_COVID), main=c("Log of Daily Wheat Prices", "COVID-19 Recession", "Mar 2020 — Feb 2021"), 
     xlab = "Prices in USD", ylab = "Frequency", col="light yellow")
# Bimodal, heavier left half
# mean(log(weat_COVID)) = 3.071734

#Difference in log values over time
hist(diff(log(daily_price_WEAT)), main=c("Differences Between Daily Wheat Prices", "Jan 2008 — Jun 2009"), 
     xlab = "Prices in USD", ylab = "Frequency", col="orange")
# Very small, very small differences, clustered around 0.


#-------------
# Daily: price changes
#
#------------

daily_weat_price_change <- diff(daily_price_WEAT)
daily_weat_price_change_no_rec <- diff(weat_no_rec)
daily_weat_price_change_rec <- diff(weat_any_rec)
daily_weat_price_chng_dotcom <- diff(weat_dotcom)
daily_weat_price_chng_GR <- diff(weat_GreatRec)
daily_weat_price_chng_C19 <- diff(weat_COVID)

#-------------------------------------

hist(daily_weat_price_change)
summary(daily_weat_price_change)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -2.219999 -0.090000  0.000000 -0.001721  0.070000 16.810001 
var(daily_weat_price_change)
# 0.1608409
sd(daily_weat_price_change)
# 0.4010498

# difference in price changes
daily_weat_diff_diff <- diff(diff(daily_price_WEAT))
hist(daily_weat_diff_diff)
# Very small differences in price changes themselves! Always clustered around 0
summary(daily_weat_diff_diff)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -16.350002  -0.140000   0.000000  -0.000074   0.130002  16.810001 
var(daily_weat_diff_diff)
# 0.3365551
sd(daily_weat_diff_diff)
# 0.5801336

#****************

hist(daily_weat_price_change_rec)
summary(daily_weat_price_change_rec)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -11.430001  -0.090000   0.000000  -0.008352   0.050000  15.960000

# difference in price changes
daily_weat_diff_diff_rec <- diff(daily_weat_price_change_rec)
hist(daily_weat_diff_diff_rec)
# Changes in price changes for weatar cluster around 0, but there
# are long thin tails
summary(daily_weat_diff_diff_rec)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-16.050001  -0.130000   0.000000   0.000085   0.122500  16.030000   
var(daily_weat_diff_diff_rec)
# 1.128211
sd(daily_weat_diff_diff_rec)
# 1.062173

#****************

hist(daily_weat_price_chng_dotcom)
# Long thin tails with a tight curve
summary(daily_weat_price_chng_dotcom)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2.22000 -0.18000  0.00000 -0.02448  0.10750  2.51000

# difference in price changes
daily_weat_diff_diff_dotcom <- diff(daily_weat_price_chng_dotcom)
hist(daily_weat_diff_diff_dotcom)
# During the dotcom crash, we see that the changes
# in price changes have very long and thin tails; there is a little volatility.
# However, most of the changes are still clustered around 0, and the variance is low.
summary(daily_weat_diff_diff_dotcom)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.73000 -0.25000  0.00000  0.00185  0.23000  2.51000 
var(daily_weat_diff_diff_dotcom)
# 0.4446871
sd(daily_weat_diff_diff_dotcom)
# 0.6668486

#****************
hist(daily_weat_price_chng_GR)
hist(daily_weat_price_chng_GR, breaks = 50)
# A much more normal-looking distribution than compared to the others.
curve(dnorm(x, mean(daily_weat_price_chng_GR), sd = sqrt(var(daily_weat_price_chng_GR))), add=TRUE, col = "red")
# However, we see that the normal distribution for this mean and this standard deviation
# does not well-match the histogram for daily weatar price changes during the Great Recession.

summary(daily_weat_price_chng_GR)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.310000 -0.060000  0.000000 -0.003821  0.050000  0.380000  

# difference in price changes
daily_weat_diff_diff_GR <- diff(daily_weat_price_chng_GR)
hist(daily_weat_diff_diff_GR)
hist(daily_weat_diff_diff_GR, breaks = 100)
curve(dnorm(x, mean(daily_weat_diff_diff_GR), sd = sqrt(var(daily_weat_diff_diff_GR))), add=TRUE, col = "red")
# The curve is too tightly clustered around the mean 
# for this to follow the normal distribution.
summary(daily_weat_diff_diff_GR)
#  Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -0.4200000 -0.0800000 -0.0100000 -0.0002571  0.0700000  0.4400000   
var(daily_weat_diff_diff_GR)
# 0.01612983
sd(daily_weat_diff_diff_GR)
# 0.1270033

#****************

hist(daily_weat_price_chng_C19)
# The price changes remain clustered around the near-0 mean with a very long and thin
# tail due to the extremes.
summary(daily_weat_price_chng_C19)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -2.22000 -0.15000  0.00000 -0.02189  0.08500  2.51000  

# difference in price changes
daily_weat_diff_diff_C19 <- diff(daily_weat_price_chng_C19)
hist(daily_weat_diff_diff_C19)
# Continues to cluster around the near-0 mean for differences in price changes.
# Continues to have very long and thin tails for the occasional outliers.
summary(daily_weat_diff_diff_C19)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.73000 -0.20750 -0.00500  0.00062  0.20000  2.51000 
var(daily_weat_diff_diff_C19)
# 0.2405248
sd(daily_weat_diff_diff_C19)
# 0.4904333





#***Chi-square tests on normal distribution

# Differences in the price changes
daily_pval_weat_diff_diff<- chiSqTest(daily_weat_diff_diff)
# "Chi-sq test statistic:"
# "9888.67053452539"
# "p-value with df = {nbins - 2}:"
# "0"
# We definitely reject the null hypothesis that weatar's changes in price changes
# follow a normal distribution.

#QQ plots test of normality to see how distribution compares to normal distribution

# Log Price Changes/percentage changes
qqnorm(daily_weat_price_change)
qqline(daily_weat_price_change) 
# The data usually follow the normal distribution, but the outliers 
# of price changes creates heavy tails. The distribution is therefore not
# normal. This matches what we have seen with gold, considered a safe haven good, and
# oil, a price-inelastic good. Sugar is traditionally not thought of as either of these
# types of goods, and yet it similarly has non-normal price changes, with heavy tails.
# This seems to be a trait native to prices themselves, regardless of type of good.


# Changes in price changes
qqnorm(daily_weat_diff_diff)
qqline(daily_weat_diff_diff) 
# For weatar, changes in price changes have "lighter" tails, unlike for oil.

#****************************************
#*Recap: Histograms and weatar prices' normality
#****************************************

# Comparing to relationship with recessions. 
hist(daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 0)])
hist(daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 1)])

#Binned
hist(daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 0)], breaks=50)
hist(daily_price_WEAT[which(dailydata_ALL$rec_inds_use.USRECD == 1)], breaks=50)

#Types of recessions
hist(daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 0)])
hist(daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 1)])
hist(daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 2)])
hist(daily_price_WEAT[which(dailydata_ALL$rec_types_use.USRECD == 3)])

#Types of recessions; binned
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 0)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 1)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 2)], breaks=50)
hist(daily_price_GOLD[which(dailydata_ALL$rec_types_use.USRECD == 3)], breaks=50)

# None of these is normal, as demonstrated above. The 
# prices during the different recessions also appear to follow different distributions
# with different skewness. The Great Recession is negatively skewed; COVID recession
# is closer to normal with a wide variance; dotcom crash has a slight positive skewness.

#-------------------------------------
# Daily: Sugar: Pareto distribution 
# Using code and notes from STai's PSet #5 R homework
#-------------------------------------
# Let's assess whether the distribution of weatar's prices
# follows a Pareto distribution instead. A Pareto distribution
# can more closely model stock prices. 

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
# Note that the curve of the pareto's density function matches the values that were
# randomly drawn according to the distribution function's inverse.

library(fitdistrplus)

# Use a qq plot to see if the claims follow Pareto distribution with different parameters

# The 1.25 creates a straight line between the theoretical quantiles and sample ones.
CDF <- function(y) 1 - (1/y^1.25)

#-----
# generating quantiles for the number of data points in the sample 
# e.g. if 100 data points, then [1/100, 2/100, 3/100, ..., 100/100]
weat_noNA <- dailydata_ALL$priceWEAT[which(dailydata_ALL$priceWEAT != ".")]
length_weat_noNA <- length(dailydata_ALL$priceWEAT[which(dailydata_ALL$priceWEAT != ".")])
sample_quantiles_weat <- (1:length_weat_noNA) / length_weat_noNA

# sorting the data set to compute each datapoint's theoretical quantile if it followed
# the given distribution function. e.g. pareto with parameter of alpha.
theoretical_quantiles_weat <- CDF(sort(as.numeric(weat_noNA)))

# This QQ plot illustrates how well the theoretical distribution matches the empirical distribution.
plot(theoretical_quantiles_weat, sample_quantiles_weat)
# Sugar prices' Pareto theoretical vs. sample quantiles has more of a linear relationship than
# the other goods prices' Pareto theoretical vs. sample quantils' relationship!

# Rescale using log

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(as.numeric(weat_noNA)), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# The curve does not fit the histogram.

# Quick assessment of weatar's price changes
weat_delt_noNA <- diff(as.numeric(weat_noNA))
weat_sample_quantiles_delta <- (1:length(weat_delt_noNA)) / length(weat_delt_noNA)
weat_delt_th_quant <- CDF(sort(weat_delt_noNA))
plot(weat_delt_th_quant, weat_sample_quantiles_delta)
# Definitely does not follow a line; no Pareto distribution is established.

alpha = 1.25
pdf = function(y) alpha*exp(y)^(-alpha-1)
hist(log(weat_delt_noNA), prob=TRUE)
curve(pdf, col="darkblue", lwd=3.2, add=TRUE)
# Rescaled logarithmically, the Pareto distribution fits the shape of the logs of weatar
# price changes' histogram. However, the Pareto curve does not overlay the
# histogram.


