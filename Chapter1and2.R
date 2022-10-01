# This file contains the lessons learned in Chapter 1 and 2 of the Master class  Quantitative methods
# lessons learned:
#   > univariate and multivariate Regression
#   > std error, variance, t tets, p test
# Data is about US Earnings

#install package to read Excel files
install.packages("readxl")
library(readxl)

#Load Data -------------------

#load Excel file of US Earnings data
earnings_data <- read_excel('./Data_Sources/Data_Earnings.xlsx')
head(earnings_data)
attach(earnings_data)

#Plot Data ------------------

#plot how the years of schools influence the hourly earnings 
#create a scatterplot for the Variables S and Earnings, with x- and y-axis labels and  dots as point shape(pch=19)
plot(S, EARNINGS, xlab = "Years of Schooling", ylab = "hourly Earnings in $", pch = 19)

#Model --------------- 

#use a linear regression model to show the relationship between years of schools and hourly earnings
reg1 <- lm(EARNINGS ~ S)
#show results of the model
#Estimates S = 2,4553  means one School year increases the hourly earnings by $2.46
#
summary(reg1)
anova(reg1)

#add visualization of the model to plot
abline(reg1, col = "blue", lwd = 4)

