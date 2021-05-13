library(vars) # vector autoregression library

price_changes <- read.csv("output_files/price changes.csv")[,2:15]

var_aic <- VAR(price_changes[,2:13], type = "none", lag.max = 5, ic = "AIC")
summary(var_aic)

# Trying to predict recessions from price history of all our goods is futile.
# None of the variables, nor their histories, are significant contributors to
# the likelihood of recession in the next month, except for whether there was a
# recession during the previous month.

# Sources used:
# https://www.rdocumentation.org/packages/vars/versions/1.5-3/topics/VAR
# https://www.r-econometrics.com/timeseries/varintro/
# https://en.wikipedia.org/wiki/Vector_autoregression

# However, recessions and crude oil prices are significantly predictive of
# unemployment! (Maybe we can forecast unemployment using these.)