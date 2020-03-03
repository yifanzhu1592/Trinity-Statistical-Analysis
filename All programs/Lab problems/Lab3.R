#Simulate a population of 10,000 with a variable from a Normal(4,5) distribution 
#and a population of 10,000 with a variable from an Exponential(1) distribution 
#[use the command rexp(n,lambda) ]
x_normal <- rnorm(10000, 4, 5)
x_exp <- rexp(10000, 1)

# record the size, mean and sd of x_normal
N_normal <- length(x_normal)
mean_normal <- mean(x_normal)
sd_normal <- sd(x_normal)

# record the size, mean and sd of x_exp
N_exp <- length(x_exp)
mean_exp <- mean(x_exp)
sd_exp <- sd(x_exp)

#Take a random sample of size 10, 50, 500 from x_normal
x_normal_10 <- sample(x_normal, 10)
x_normal_50 <- sample(x_normal, 50)
x_normal_500 <- sample(x_normal, 500)

#Take a random sample of size 10, 50, 500 from x_exp
x_exp_10 <- sample(x_exp, 10)
x_exp_50 <- sample(x_exp, 50)
x_exp_500 <- sample(x_exp, 500)

# record the size, mean and sd of x_normal_50
n_normal_50 <- length(x_normal_50)
mean_normal_50 <- mean(x_normal_50)
sd_normal_50 <- sd(x_normal_50)

# record the size, mean and sd of x_exp_50
n_exp_50 <- length(x_exp_50)
mean_exp_50 <- mean(x_exp_50)
sd_exp_50 <- sd(x_exp_50)

#calculate the standard error of mean_normal_50 using known and estimated sd_normal
se_kn_normal <- sd_normal/sqrt(n_normal_50)
se_unkn_normal <- sd_normal_50/sqrt(n_normal_50)

#calculate the standard error of mean_exp_50 using known and estimated sd_exp
se_kn_exp <- sd_exp/sqrt(n_exp_50)
se_unkn_exp <- sd_exp_50/sqrt(n_exp_50)





#Draw graphs to help you assess the normality of each population, 
#as well as samples of size 10, 50 and 500 from each population. 

#make a qqplot and a histogram with normal density curve for x_normal
qqnorm(x_normal)
qqline(x_normal)
hist(x_normal, freq = FALSE)
xfit <- seq(min(x_normal), max(x_normal), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_normal), sd = sd(x_normal))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_normal_10
qqnorm(x_normal_10)
qqline(x_normal_10)
hist(x_normal_10, freq = FALSE)
xfit <- seq(min(x_normal_10), max(x_normal_10), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_normal_10), sd = sd(x_normal_10))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_normal_50
qqnorm(x_normal_50)
qqline(x_normal_50)
hist(x_normal_50, freq = FALSE)
xfit <- seq(min(x_normal_50), max(x_normal_50), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_normal_50), sd = sd(x_normal_50))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_normal_500
qqnorm(x_normal_500)
qqline(x_normal_500)
hist(x_normal_500, freq = FALSE)
xfit <- seq(min(x_normal_500), max(x_normal_500), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_normal_500), sd = sd(x_normal_500))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_exp
qqnorm(x_exp)
qqline(x_exp)
hist(x_exp, freq = FALSE)
xfit <- seq(min(x_exp), max(x_exp), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_exp), sd = sd(x_exp))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_exp_10
qqnorm(x_exp_10)
qqline(x_exp_10)
hist(x_exp_10, freq = FALSE)
xfit <- seq(min(x_exp_10), max(x_exp_10), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_exp_10), sd = sd(x_exp_10))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_exp_50
qqnorm(x_exp_50)
qqline(x_exp_50)
hist(x_exp_50, freq = FALSE)
xfit <- seq(min(x_exp_50), max(x_exp_50), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_exp_50), sd = sd(x_exp_50))
lines(xfit, yfit)

#make a qqplot and a histogram with normal density curve for x_exp_500
qqnorm(x_exp_500)
qqline(x_exp_500)
hist(x_exp_500, freq = FALSE)
xfit <- seq(min(x_exp_500), max(x_exp_500), length = 400) 
yfit <- dnorm(xfit, mean = mean(x_exp_500), sd = sd(x_exp_500))
lines(xfit, yfit)





#Calculate confidence intervals for the mean of the sample of size 50 from each distribution, 
#using a) the Z-score and the known population SD, 
#and b) the t-distribution and the sample SD.

#Calculate Z-score for 95% CI
Z_score <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x_normal_50
t_score_normal <- qt(0.975, n_normal_50-1)

#Calculate critical t-value for 95% CI and x_exp_50
t_score_exp <- qt(0.975, n_exp_50-1)

#Calculate 95% CIs for x_normal_50 using the Z-score and the known population SD
left_Z95_kn_normal <- mean_normal_50-Z_score*se_kn_normal
right_z95_kn_normal <- mean_normal_50+Z_score*se_kn_normal

#Calculate 95% CIs for x_exp_50 using the Z-score and the known population SD
left_Z95_kn_exp <- mean_exp_50-Z_score*se_kn_exp
right_z95_kn_exp <- mean_exp_50+Z_score*se_kn_exp

#Calculate 95% CIs for x_normal_50 using the t-distribution and the sample SD
left_t95_unkn_normal <- mean_normal_50-t_score*se_unkn_normal
right_t95_unkn_normal <- mean_normal_50+t_score*se_unkn_normal

#Calculate 95% CIs for x_exp_50 using the t-distribution and the sample SD
left_t95_unkn_exp <- mean_exp_50-t_score*se_unkn_exp
right_t95_unkn_exp <- mean_exp_50+t_score*se_unkn_exp





#show results
paste("var: x_normal", "mean:", mean_normal, "sd:", sd_normal)
paste("var: x_exp", "mean:", mean_exp, "sd:", sd_exp)
paste("var: x_normal_50", "mean:", mean_normal_50, "sd:", sd_normal_50, 
      "se w/known pop sd:", se_kn_normal, "se w/unknown pop sd:", se_unkn_normal)
paste("var: x_exp_50", "mean:", mean_exp_50, "sd:", sd_exp_50, 
      "se w/known pop sd:", se_kn_exp, "se w/unknown pop sd:", se_unkn_exp)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn_normal, right_z95_kn_normal)
paste("Z-distribution w/known pop sd:", left_Z95_kn_exp, right_z95_kn_exp)
paste("t-distribution w/unknown pop sd:", left_t95_unkn_normal, right_t95_unkn_normal)
paste("t-distribution w/unknown pop sd:", left_t95_unkn_exp, right_t95_unkn_exp)
