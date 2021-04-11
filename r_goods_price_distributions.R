# This script shows the distributions of month-over-month price changes of our commodities.
library(eeptools) # to import the decomma() function


commodity_prices <- read.csv("source_data/commodities data.csv")
recession_dates <- read.csv("source_data/monthly recession indicator.csv")

recession_dates <- recession_dates[c(-242),] # Removed the entry for March 2021 so that the datasets have congruent date ranges.
commodity_prices[,9] <- decomma(commodity_prices[,9]) # Remove commas from the price of gold column.

goods <- c("Month","Crude_oil", "Sugar", "Soybeans", "Wheat", "Beef", "Rubber", "Cocoa_beans", "Gold", "USD_EUR", "Ice_cream", "Unemployment")
names(commodity_prices) <- goods


# Adding a column to distinguish the three different recessions in our date range.
recession_dates$which_recession <- recession_dates$USREC
recession_dates[84:101,3] <- 2*recession_dates[84:101,3]
recession_dates[230:241,3] <- 3*recession_dates[230:241,3]

# Find month-over-month price changes for each commodity
price_changes <- commodity_prices
for (c in 2:11) {
  for (r in 2:241) {
    price_changes[r,c] <- commodity_prices[r,c] / commodity_prices[r-1,c] - 1
  }
}
price_changes$recession_bool <- recession_dates$USREC
price_changes$which_recession <- recession_dates$which_recession
price_changes <- price_changes[c(-1),]

write.csv(price_changes, "output_files/price changes.csv")

# Display a histogram of the price changes
for (i in 2:11) {
  name <- goods[i]
  hist(price_changes[,i], main=name)
}

# Repeating that, but during recessions only
for (i in 2:11) {
  name <- paste(goods[i],"(recession)")
  values <- price_changes[,c(i,13)]
  values <- values[values[,2] == 1,]
  hist(values[,1], main=name)
}

# Repeat for months not in recession
for (i in 2:11) {
  name <- paste(goods[i],"(non-recession)")
  values <- price_changes[,c(i,13)]
  values <- values[values[,2] == 0,]
  hist(values[,1], main=name)
}


# Robi's goods: sugar, cocoa, usd:eur, ice cream, unemployment
# Sharon's goods: gold, oil, soybeans, wheat, beef, rubber

