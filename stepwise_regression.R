library(MASS)
library(car)

price_changes <- read.csv("output_files/price changes.csv")[,2:15]

# Let's see if we can make a model that predicts unemployment from the other variables.
multilin_reg <- lm(Unemployment ~ ., data=price_changes[,2:13])

vif(multilin_reg)
# All of the values are less than 2, indicating that the predictors are not
# related to each other, and the multilinear regression does not suffer from
# multicollinearity. Therefore, it's appropriate to use all of these variables
# in a multiple linear regression model of recessions.

summary(multilin_reg)
# As it turns out, none of the variables are significant predictors of
# unemployment except for the recession indicator.

# Let's see if we can improve the model using a stepwise regression to optimize
# the Akaike information criterion and remove unnecessary variables.

stepAIC(multilin_reg)

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