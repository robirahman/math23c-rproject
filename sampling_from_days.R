daily_data <- read.csv("output_files/daily data.csv")[,2:11]


# Got the daily values. Let's sample them to try to estimate monthly or yearly values.

length(daily_data$priceCOCOA)
sum(daily_data$priceCOCOA == ".")
sum(daily_data$priceGOLD == ".")
sum(daily_data$priceSUG == ".")
sum(daily_data$priceHYS == ".")
sum(daily_data$priceWEAT == ".")
sum(daily_data$priceSOYB == ".")

# Cocoa, sugar, and Hershey had 
