commodity_prices <- read.csv("commodities data.csv")
recession_dates <- read.csv("monthly recession indicator.csv")

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

# Display a histogram of the price changes
for (i in 2:11) {
  name <- paste(goods[i],"(all months)")
  hist(price_changes[,i], main=name, col="lightblue")
}

# Repeating that, but during recessions only
for (i in 2:11) {
  name <- paste(goods[i],"(recession)")
  values <- price_changes[,c(i,13)]
  values <- values[values[,2] == 1,]
  hist(values[,1], main=name, col="lightblue")
}

# Repeat for months not in recession
for (i in 2:11) {
  name <- paste(goods[i],"(non-recession)")
  values <- price_changes[,c(i,13)]
  values <- values[values[,2] == 0,]
  hist(values[,1], main=name, col="lightblue")
}



# Logistic modeling to estimate probability of a recession in a given month,
# based on the changes in commodity prices during that month:
soybeans_regression <- glm(recession_bool ~ Soybeans, data = price_changes, family = 'binomial')
summary(soybeans_regression)
plot(price_changes$Soybeans, price_changes$recession_bool,
     xlab="Monthly price change of soybeans", ylab="Recession indicator",
     main="Soybean price changes in non-recession and recession periods")


gold_regression <- glm(recession_bool ~ Gold, data = price_changes, family = 'binomial')
summary(gold_regression)
plot(price_changes$Gold, price_changes$recession_bool,
     xlab="Monthly price change of gold", ylab="Recession indicator",
     main="Gold price changes in non-recession and recession periods")


goods_vs_recession_logistic_model <- glm(
  recession_bool ~ Crude_oil + Sugar + Soybeans + Wheat + Beef + Rubber + Cocoa_beans + Gold + USD_EUR + Ice_cream,
  data = price_changes, family = 'binomial')
summary(goods_vs_recession_logistic_model)

negative_goods_regression <- glm(recession_bool ~ Crude_oil + Wheat + Rubber + Ice_cream, data = price_changes, family = 'binomial')
summary(negative_goods_regression)
# Probability of a recession if all of these goods lost 100% of their value this month:
inv.logit(-1.6582+0.2640+0.7643+0.8586+1.5616)
# Probability of a recession if all of these goods doubled in price last month:
inv.logit(-1.6582-0.2640-0.7643-0.8586-1.5616)

icecream_regression <- glm(recession_bool ~ Ice_cream, data = price_changes, family = 'binomial')
summary(icecream_regression)

plot(price_changes$Ice_cream, price_changes$recession_bool,
     xlab="Monthly price change of ice cream", ylab="Recession indicator",
     main="Ice cream price changes in non-recession and recession periods")
curve(inv.logit(-1.5900*x-1.6687), add=TRUE)


icecream_predictions <- as.factor(predict(icecream_regression, newdata=price_changes, type='response') > inv.logit(-1.6687))
confusionMatrix(icecream_predictions, reference = as.factor(price_changes$recession_bool==1))


recession_predictions <- as.factor(predict(goods_vs_recession_logistic_model, newdata=price_changes, type='response') > inv.logit(-1.7006))
confusionMatrix(recession_predictions, reference = as.factor(price_changes$recession_bool==1))



# It looks like there's not much of a correlation between recessions and positive
# vs negative price changes of goods. Let's investigate using a contingency table.

# unpack the dataframe price_changes into two columns
change_vs_recession <- data.frame(change=numeric(2400),recession_bool=rep(price_changes$recession_bool,10))

for (c in 2:11) {
  for (r in 2:241) {
    change_vs_recession[r-1+240*(c-2),1] <- price_changes[r-1,c]
  }
}

hist(change_vs_recession$change, col="lightblue", main="Frequency of price changes (all goods, 2001-21)")
table(change_vs_recession$change >= 0, change_vs_recession$recession_bool)

# During recessions: price diff >= 0 212 times; price diff < 0 168 times
# During other times: price diff >= 0 1130 times; price diff < 0 890 times

212/(212+168) # Price goes up 55.8% of the time during recessions
1130/(1130+890) # Price goes up 55.9% of the time without recession




# Repeat the above analysis, but break up the recession category into three.
change_vs_recession <- data.frame(change=numeric(2400),which_recession=rep(price_changes$which_recession,10))

for (c in 2:11) {
  for (r in 2:241) {
    change_vs_recession[r-1+240*(c-2),1] <- price_changes[r-1,c]
  }
}

table(change_vs_recession$change >= 0, change_vs_recession$which_recession)

1130/(890+1130) # Prices increased 55.9% of the time outside of recessions
38/(38+42) # # Prices increased 47.5% of the time during the dotcom recession
101/(101+79) # Prices increased 56.1% of the time during the housing crisis
73/(73+47) # Prices increased 60.8% of the time during the COVID pandemic



df <- tibble::tribble(
  ~x, ~y, ~price,
  "non-recession", 1130,  "same/increased",
  "non-recession", -890,  "decreased",
  
  "recession", 212,  "same/increased",
  "recession", -168,  "decreased",
  
)
ggplot(data = df, aes(x, y, group = price)) +
  geom_col(aes(fill = price), position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  xlab("") + ylab("") + ggtitle("Likelihood of price increases vs recession")

df <- tibble::tribble(
  ~x, ~y, ~price,
  
  "dotcom crash", 38,"same/increased",
  "dotcom crash", -42,  "decreased",
  
  "financial crisis", 101,"same/increased",
  "financial crisis", -79,  "decreased",
  
  
  "COVID pandemic", 73,"same/increased",
  "COVID pandemic", -47,  "decreased",
)
ggplot(data = df, aes(x, y, group = price)) +
  geom_col(aes(fill = price), position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = 0) +
  xlab("") + ylab("") + ggtitle("Likelihood of price increases vs recession")




# Make a graphical display to see if any commodities are correlated to each other.
pairs.panels(price_changes[,c(2:9,11)])

# Looks like nothing is correlated to anything else except for R=0.45 between
# soybeans and wheat. Maybe we can compare some commodities to the recession
# indicator, the Euro exchange rate, and unemployment.
pairs.panels(price_changes[,8:13])
# There aren't really any correlations between these, looks like they're all
# orthogonal! I tried this with the entire dataframe too, with the same results.




# This bootstrap test determines the difference between unemployment during
# recessions and periods of economic growth using a simulation technique.
set.seed(2021)

unemployment_vs_recession <- price_changes[,12:13]

total_recession <- sum(unemployment_vs_recession[,2]) # 38
total_nonrecession <- 240-total_recession # 202

indicator <- function(x) x == 1
not_indicator <- function(x) x == 0

avg_unemp_in_recession <- sum(unemployment_vs_recession[,1]*indicator(unemployment_vs_recession[,2]))/38
avg_unemp_not_recession <- sum(unemployment_vs_recession[,1]*not_indicator(unemployment_vs_recession[,2]))/202

observed_difference <- avg_unemp_in_recession - avg_unemp_not_recession


diffs <- numeric(1000)
for (i in 1:1000) {
  unemps <- sample(unemployment_vs_recession[,1])
  recession_avg <- sum(unemps*indicator(unemployment_vs_recession[,2]))/38
  nonrecession_avg <- sum(unemps*not_indicator(unemployment_vs_recession[,2]))/202
  diffs[i] <- recession_avg - nonrecession_avg
}

hist(diffs, col="lightblue", main="Shuffled differences in unemployment")
abline(v=observed_difference, col="red")

pvalue <- sum(diffs > observed_difference)/1000
# p-value = 0.004, so there is a significant difference in unemployment during
#                  recessions and non-recession periods!




set.seed(2022)

recession_unemployment_rates <- unemployment_vs_recession$Unemployment[unemployment_vs_recession$recession_bool==1]
nonrecession_unemployment_rates <- unemployment_vs_recession$Unemployment[unemployment_vs_recession$recession_bool==0]

rur <- mean(recession_unemployment_rates)
nur <- mean(nonrecession_unemployment_rates)
rsd <- sd(recession_unemployment_rates)
nsd <- sd(nonrecession_unemployment_rates)

recession_samples <- sample(recession_unemployment_rates,size=16,replace=TRUE)
nonrecession_samples <- sample(nonrecession_unemployment_rates,size=16,replace=TRUE)

hist(recession_unemployment_rates, probability = TRUE, col="lightblue", main="Histogram of monthly unemployment rates during recessions")
curve(dnorm(x,mean=rur,sd=rsd),add=TRUE, col="red")
abline(v=rur, col="red")
abline(v=rur-1.96*rsd/sqrt(16), col="red")
abline(v=rur+1.96*rsd/sqrt(16), col="red")

hist(nonrecession_unemployment_rates, probability = TRUE, col="lightblue", main="Histogram of monthly unemployment rates outside of recessions")
curve(dnorm(x,mean=nur,sd=nsd),add=TRUE, col="red")
abline(v=nur, col="red")
abline(v=nur-1.96*nsd/sqrt(16), col="red")
abline(v=nur+1.96*nsd/sqrt(16), col="red")

t.test(recession_samples,nonrecession_samples,alternative="two.sided")
# So the classical statistical two-sided t-test finds that the difference in means
# is not significant, when the bootstrap test indicates that there is a difference!
# However, the classical test may be inaccurate because it relies on the assumptions
# that both categories are normally distributed, and have the same variance





# Here are some advanced regression techniques implemented in cool R packages!

# Stepwise regression:
library(MASS)
library(car)


# Let's see if we can make a model that predicts unemployment from the other variables.
unemployment_reg <- lm(Unemployment ~ ., data=price_changes[,2:13])

vif(unemployment_reg)
# All of the values are less than 2, indicating that the predictors are not
# related to each other, and the multilinear regression does not suffer from
# multicollinearity. Therefore, all of these variables may be used
# in a multiple linear regression model of recessions.

summary(unemployment_reg)
# As it turns out, none of the variables are significant predictors of
# unemployment except for the recession indicator.

# Let's see if we can improve the model using a stepwise regression to optimize
# the Akaike information criterion and remove unnecessary variables.

stepAIC(unemployment_reg)

# Stepwise regression shows that the model is optimized when all price variables
# are eliminated except for the recession indicator and the price change of crude oil.


# Let's try again, but to predict recessions:
recession_reg <- lm(recession_bool ~ ., data=price_changes[,2:13])

vif(recession_reg)
# Again, nothing over 2, so no multicollinearity issues.

summary(recession_reg)
# Nothing is relevant in a model to predict recessions, except for unemployment.



# But we shouldn't be using linear models: logistic is more accurate since
# recession is a boolean, so the y-values are 0 or 1.

recession_reg <- glm(recession_bool ~ ., data=price_changes[,2:13])

stepAIC(recession_reg)

# The model for recession predictions does best with nothing but unemployment rate.






# Vector autoregression:
library(vars)

# This lets us model future values of the target variable using past
# as well as present values of the predictors.
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
# unemployment! You could try using them to forecast next month's unemployment.












