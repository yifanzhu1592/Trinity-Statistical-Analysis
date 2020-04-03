#1)Plot the residuals density
faithful.lm <- lm(waiting ~ eruptions, data=faithful)
print(faithful.lm)

faithful.res <- resid(faithful.lm)

plot(faithful$eruptions, faithful.res, 
     ylab="Residuals", xlab="Eruptions", 
     main="Faithful Linear Model") 
abline(0, 0)                  # the horizon

plot(density(faithful.res), main="Density Plot: residuals", ylab="Frequency") 



#2)Use the plot function to generate 4 graphs of the residuals vs fitted values, etc. in a single plot
par(mfrow=c(2,2))
plot(faithful.lm)



#3)Comment on whether or not you feel the model is appropriate, 
#  given both what you have seen this week in terms of the residuals plots, 
#  and what you saw last week in terms of the results of the model. 
#  Particularly, comment on the appearance of the density plots of the two variables that you made last week 
#  as compared to the density plots of the residuals.

# I feel the model is appropriate.
# Because it satisfies the assumptions set for regression model:
# 1. The mean of residuals is zero (If you look at the Density Plot: residuals)
# 2. The residuals should be normally distributed (If you look at the Density Plot: residuals and the Normal Q-Q plot)
# 3. Homoscedasticity (equal variance) of residuals (If you look at the Residuals vs Fitted plot and the Scale-Location plot)
# 4. The X variable and residuals are uncorrelated (If you look at the Faithful Linear Model)

# The shapes of the density plots of the two vairables I made last week is different from the shape of the density plots of the residuals
