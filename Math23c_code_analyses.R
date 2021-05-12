# Math 23C: Final project
# Daily data analyses: Goods prices and the 
# Monthly data analyses


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
rec_type$USRECD["2020-03-01" <= rec_type$DATE && rec_type$DATE<= "2021-02-28"]<-3
# 365 values
#dotcom crash
# 244 values


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
#*Code for tests
#-----------------

#chiSqTest code. Should be in accompanying file. Comment out once you've used.
#chiSqTest <- dget("chiSqTest.R")
