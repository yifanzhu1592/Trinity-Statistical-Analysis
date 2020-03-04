##code for a one-sample hypothesis test function
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  #return the list
  return( ret )
}



#----1)----
#H0: the mean of the population is equal to 0. 
#HA: the mean of the population is not equal to 0. 
#Use a Z-test with the true population SD. Use alpha = 0.05
# the sample
sample1 <- rnorm(100, 4, 5)
# the size of the sample
length1 <- length(sample1)
# the mean of the sample
mean1 <- mean(sample1)
# the standard deriviation of the sample
sd1 <- sd(sample1)
# the test statistics
test_stat1 <- (mean1-0)/(sd1/sqrt(length1))
test_stat1
# the function
OneSampTest("z","two",0.05,0,length1,mean1,sd1)
# test statistic: 7.34303
# P: 2.088117e-13
# p value is less than alpha, therefore it is significant
# So rejecting the null hypothesis, and accepting the alternative hypothesis

#----2)----
#H0: the mean of the population is greater than or equal to 4.2. 
#HA: the mean of the population is less than 4.2. 
#Use a t-test with the sample SD. Use alpha = 0.025
# the sample
sample2 <- rnorm(100, 4, 5)
# the size of the sample
length2 <- length(sample2)
# the mean of the sample
mean2 <- mean(sample2)
# the standard deriviation of the sample
sd2 <- sd(sample2)
# the test statistics
test_stat2 <- (mean2-4.2)/(sd2/sqrt(length2))
test_stat2
# the function
OneSampTest("t","left",0.025,4.2,length2,mean2,sd2)
# test statistic: -1.304278
# P: 0.09758143
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----3)----
#Repeat the previous task using the t.test function in the package {stats}
t.test(x=sample2, y=NULL, alternative=c("less"), mu=4.2, paired=FALSE, var.equal=FALSE, conf.level=0.975)
# test statistic: 0.50671
# P: 0.6933
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----4)----
#H0: the population proportion is equal to 0.28. 
#HA: the population proportion is not equal to 0.28. 
#Use alpha = 0.05
#sd = sqrt((prop)(1-prop))
# the sample
sample3 <- rbern(100, 0.3)
# the size of the sample
length3 <- length(sample3)
# the mean of the sample
mean3 <- mean(sample3)
# the standard deriviation of the sample
sd3 <- sqrt((0.28)*(1-0.28))
# the test statistics
test_stat3 <- (mean3-4.2)/(sd3/sqrt(length3))
test_stat3
# the function
OneSampTest("z","two",0.05,0.28,length3,mean3,sd3)
# test statistic: -86.19175
# P: 0.2654558
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----5)----
#H0: the population proportion is less than or equal to 0.35. 
#HA: the population proportion is greater than 0.35. 
#Use alpha = 0.05
#sd = sqrt((prop)(1-prop))
# the sample
sample4 <- rbern(100, 0.3)
# the size of the sample
length4 <- length(sample4)
# the mean of the sample
mean4 <- mean(sample4)
# the standard deriviation of the sample
sd4 <- sqrt((0.35)*(1-0.35))
# the test statistics
test_stat4 <- (mean4-4.2)/(sd4/sqrt(length4))
test_stat4
# the function
OneSampTest("z","right",0.05,0.35,length4,mean4,sd4)
# test statistic: -80.50828
# P: 0.4169677
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis
