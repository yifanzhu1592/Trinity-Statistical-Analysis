#Read in the csv file Lab1.csv using code, not the Import button
Lab1<-read.csv(file="Lab1.csv", header=TRUE)

#Display summary statistics for the variable EARN
summary(Lab1$EARN)

#Display frequencies of the variable Job.class
table(Lab1$Job.class)

#Display a three-way cross-tabulation of the proportions of variables Educational Level, Gender and Job.Class
ftable(Lab1$EDUC, Lab1$Gender, Lab1$Job.class)

#Create a basic histogram of the variable EARN
hist(Lab1$EARN)

#Create a basic boxplot of the variable EARN by Job Class
boxplot(Lab1$EARN~Lab1$Job.class)

#Create a new variable EARNx10000 that is equal to Earnings divided by 10,000
Lab1$EARNx10000 = Lab1$EARN/10000

#Create a scatterplot with EARNx10000 on the x axis and AGE on the Y axis
plot(Lab1$EARNx10000, Lab1$AGE)
