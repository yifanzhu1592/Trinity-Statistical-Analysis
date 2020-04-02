#1)Check the correlation between the variables eruptions and waiting
cor(faithful$eruptions, faithful$waiting)

#2)Create a plot that shows both the points and a smoothed line of the points 
scatter.smooth(x=faithful$eruptions, y=faithful$waiting, main="Waiting ~ Eruptions")  # scatterplot

#3)Create box plots for the variables
# box plot for 'eruptions'
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))
# box plot for 'waiting'
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))

#4)Create graphs of the densities of the variables
# density plot for 'eruptions'
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")
# density plot for 'waiting'
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency")

#5)Fit a simple linear model that predicts eruptions from waiting
# build linear regression model on full data
faithful.lm <- lm(waiting ~ eruptions, data=faithful)
print(faithful.lm)

#6)Visualise the resulting regression line on a scatterplot of the data
plot(faithful$eruptions, faithful$waiting)
abline(faithful.lm)
# inspect the results
summary(faithful.lm)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  33.4744     1.1549   28.98   <2e-16 ***
#  eruptions    10.7296     0.3148   34.09   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#7)In comments in your code, write an equation that describes the linear model you have fitted
# waiting = 33.4744 + 10.7296 * eruptions

#8)At a significance level of 0.05, does there appear to be a statistically significant relationship 
#  between eruptions and waiting? Explain your answer in comments in your code.
# p-value < 2e-16, p-value < significance, therefore it is significant
# So rejecting the null hypothesis, and accepting the alternative hypothesis
# So there is a statistically significant relationship between eruptions and waiting

#9)Why are the p-values for the variable waiting and the overall F test so similar for this model?
# Because the t test and the F test are testing the same hypothesis.
