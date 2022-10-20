# Excercise the Master class Quantitative methods
# Questions to answer:
# 1) What is the fair price of a signed Monet painting with 150cm height and 100cm width?
# 2) In principle, shall I go to Christie's or Sotheby'2 or any other auctioneer
# 3) What is the value of a Monet signature?

install.packages("readxl")
library(readxl)

#load Excel file Monet paintings sales data
monet_data <- read_excel('./Data_Sources/Data_Monet.xlsx')
head(monet_data)

#filter out paintings not sold (PRICE = 0.0)
monet_data[monet_data$PRICE != 0,]
attach(monet_data)

#create additional parameters
size <- HEIGHT*WIDTH

#first look at data
plot(size, PRICE, xlab="SIZE", col="blue", ylab="Price ", pch=10)

#using a linear regression model to estimate the influence of size and signature on the selling price
regSize <- lm(PRICE ~ size + SIGNED)
summary(regSize)
#Results:
# 1 INCH² additional size results in a $2500 higher selling price
# a signature results in a $2,403 million higher selling price
#both statistical highly significant as p-test < 0.01

# 1) What is the fair price of a signed Monet painting with 150cm height and 100cm width?
# Create a Data Frame containing the values needed for the prediction 
prediction_values <- data.frame(size = c(150/2.54*100/2.54),
                                SIGNED = c(1)
                                )
predict(regSize,prediction_values)
# result: price is predicted to be $6.96 million

#alternative solution with height and width separately
#using a linear regression model to estimate the influence of height, width and signature on the selling price
regSize2 <- lm(PRICE ~ HEIGHT + WIDTH + SIGNED)
summary(regSize2)
# 1 INCH additional height results in a $22000 higher selling price
# 1 INCH additional width results in a $21000 higher selling price
# a signature results in a $2,200 million higher selling price
# all 3 statistical highly significant as p-test < 0.01
#F-Score lower than using size 

prediction_values2 <- data.frame(HEIGHT = c(150/2.54),
                                WIDTH = c(100/2.54),
                                SIGNED = c(1)
)
predict(regSize2,prediction_values2)
# result: price is predicted to be $7.11 million for 150x100
prediction_values3 <- data.frame(HEIGHT = c(100/2.54),
                                 WIDTH = c(150/2.54),
                                 SIGNED = c(1)
)
predict(regSize2,prediction_values3)
# result: price is predicted to be $7.45 million for 100x150


regHouse <- lm(PRICE ~ size + SIGNED + HOUSE)
summary(regHouse)



regSigned <- lm(PRICE ~ SIGNED)
summary(regSigned)
