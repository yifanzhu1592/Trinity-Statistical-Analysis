survey <- read.csv(file="survey.csv", header=TRUE)

tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

#  X-squared = 5.4885
#  df = 6
#  p-value = 0.4828



#  1)How were the degrees of freedom for this test calculated?
#    Df = (#rows-1)*(#cols-1)
#    Exer: Freq, Some, None
#    Smoke: Heavy, Regul, Occas, Never
#    Degrees of freedom = (3-1)*(4-1) = 6


#  2)What are the assumptions of the chi square test?
#    · All observations must contribute to only one cell
#    · In this table, n must be >20, all Expected values must be >1 and
#      no more than 20% of the expected values can be <5.


#  3)Why did the code give an error message?
#    Because there are too few observations (the sample size is too small).
#    chi-square is highly sensitive to sample size.
#    A reasonably strong association may not come up as significant if the sample size is small.


#  4)What is the null hypothesis for this test and the alternative hypothesis for this test?
#    Null hypothesis: There is no relationship between 
#                     if a person exercises and if this person smokes in the population
#    Alternative hypothesis: There is a relationship between 
#                            if a person exercises and if this person smokes in the population


#  5)Using a significance level of 0.05, give your conclusions for the result of this test.
#    p-value > significance level
#    Therefore it is not significant
#    So failing to reject the null hypothesis
#    So There is no relationship between 
#         if a person exercises and if this person smokes in the population
#    The test may be inaccurate due to the issue that 
#       there are too few observations (the sample size is too small)
