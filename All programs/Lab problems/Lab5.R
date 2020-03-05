##code for a two-sample hypothesis test function
TwoSampTest <-function(type=NULL, tails=NULL, alpha, mu, n1, n2, x_bar1, x_bar2, sd)
{
  #calculate the standard error
  se = sd * sqrt(1/n1 + 1/n2)
  
  #calculate the test statistic
  test_stat <- (x_bar1-x_bar2-mu)/se
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n1+n2-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- 2*pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t or proportion")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("Two Sample", type, "test.", tails, "tailed"), mu=mu, n1=n1, n2=n2, x_bar1=x_bar1, x_bar2=x_bar2, se=se, p = p_val, alpha = alpha, significance = sig )
  #return the list
  return( ret )
}



#----1)----
#H0: the difference in means of the populations are equal to 1. 
#HA: the difference in means of the populations are not equal to 1.
#Use a Z-test with the true population SDs. Use alpha = 0.05
# the samples
sample1 <- rnorm(100, 4, 5)
sample2 <- rnorm(80, 3.5, 2)
# the sizes of the samples
length1 <- length(sample1)
length2 <- length(sample2)
# the means of the samples
mean1 <- mean(sample1)
mean2 <- mean(sample2)
# the pooled standard deriviation of the sample
sd1 <- sd(sample1)
sd2 <- sd(sample2)
sd <- sqrt(((length1-1)*sd1*sd1+(length2-1)*sd2*sd2) / (length1+length2-2))
# the test statistics
test_stat1 <- (mean1-mean2-1)/(sd * sqrt(1/length1 + 1/length2))
test_stat1
# the function
TwoSampTest("z","two",0.05,1,length1,length2,mean1,mean2,sd)
# test statistic: -1.366369
# P: 0.1718233
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----2)----
#H0: the means of the populations are equal.
#HA: the means of the populations are not equal.
#Use a Z-test with the sample SDs. Use alpha = 0.05
# the test statistics
test_stat2 <- (mean1-mean2-0)/(sd * sqrt(1/length1 + 1/length2))
test_stat2
# the function
TwoSampTest("z","two",0.05,0,length1,length2,mean1,mean2,sd)
# test statistic: 0.2008788
# P: 0.8407933
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----3)----
#H0: the means of the populations are equal.
#HA: the means of the populations are not equal.
#Use a t-test. Use alpha = 0.05.
# the test statistics
test_stat2 <- (mean1-mean2-0)/(sd * sqrt(1/length1 + 1/length2))
test_stat2
# the function
TwoSampTest("t","two",0.05,0,length1,length2,mean1,mean2,sd)
# test statistic: 0.2008788
# P: 0.8410214
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----4)----
#Repeat the previous task using the t.test function in the package {stats}
t.test(x=sample1, y=sample2, alternative=c("two.sided"), mu=0, paired=FALSE, var.equal=TRUE, conf.level=0.95)
# test statistic: 0.20088
# P: 0.841
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----5)----
#Repeat the previous task using the t.test function in the package {stats}, without the assumption of equal variance.
t.test(x=sample1, y=sample2, alternative=c("two.sided"), mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95)
# test statistic: 0.2172
# P: 0.8284
# p value is greater than alpha, therefore it is not significant
# So failing to reject the null hypothesis

#----6)----
#H0: the population proportions are equal.
#HA: the population proportions are not equal.
#Use alpha = 0.05
# the samples
sample3 <- rbern(100, 0.3)
sample4 <- rbern(85, 0.7)
# the sizes of the samples
length3 <- length(sample3)
length4 <- length(sample4)
# the means of the samples
mean3 <- mean(sample3)
mean4 <- mean(sample4)
# the standard deriviation of the sample
prob <- (0.3*length3+0.7*length4) / (length3+length4)
sd3 <- sqrt(prob*(1-prob))
# the test statistics
test_stat3 <- (mean3-mean4-0) / (sd3 * sqrt(1/length3 + 1/length4))
test_stat3
# the function
TwoSampTest("z","two",0.05,0,length3,length4,mean3,mean4,sd3)
# test statistic: -6.909571
# P: 4.86121e-12
# p value is less than alpha, therefore it is significant
# So rejecting the null hypothesis, and accepting the alternative hypothesis

#----7)----
#write code to calculate the 95% Confidence Interval for your estimate of the difference 
#between the two population proportions.
# 95% Confidence Interval, which means 2.5% each tail
# the z_score
z_score <- qnorm(0.975)
# standard error
se <- sd3 * sqrt(1/length3 + 1/length4)
# upper bound of confidence interval
ci1 <- (mean3-mean4) + z_score * se
ci1
# lower bound of confidence interval
ci2 <- (mean3-mean4) - z_score * se
ci2
# ci1: -0.3649124
# ci2: -0.6539111
