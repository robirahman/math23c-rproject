# This script will do a logistic regression of the recession boolean column
# based on the price changes of our goods.

library(boot)
library(caret)

price_changes <- read.csv("source_data/price changes.csv")

soybeans_regression <- glm(recession_bool ~ Soybeans, data = price_changes, family = 'binomial')
summary(soybeans_regression)
plot(price_changes$Soybeans, price_changes$recession_bool)


gold_regression <- glm(recession_bool ~ Gold, data = price_changes, family = 'binomial')
summary(gold_regression)
plot(price_changes$Gold, price_changes$recession_bool)


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

plot(price_changes$Ice_cream, price_changes$recession_bool)
curve(inv.logit(-1.5900*x-1.6687), add=TRUE)


icecream_predictions <- as.factor(predict(icecream_regression, newdata=price_changes, type='response') > inv.logit(-1.6687))
confusionMatrix(icecream_predictions, reference = as.factor(price_changes$recession_bool==1))


recession_predictions <- as.factor(predict(goods_vs_recession_logistic_model, newdata=price_changes, type='response') > inv.logit(-1.7006))
confusionMatrix(recession_predictions, reference = as.factor(price_changes$recession_bool==1))

# Need to add contingency tables.
# They will probably show no correlation between [monthly price change > 0] and [recession_bool=True]
