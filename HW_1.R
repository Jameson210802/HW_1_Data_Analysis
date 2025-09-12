# HW 1
# Jamie Anderson netid: jla656

library(ggplot2)
options(scipen = 999)
## part a ##

## summary stats of price ##

summary(kingCountyHouses$price)
summary(kingCountyHouses$sqft)

 # disable scientific notation 

## graphs of price ##


price_box_stats <- boxplot.stats(kingCountyHouses$price)
price_out <- length(price_box_stats$out)

price_out_label <- paste("Number of Outliers:", price_out)

boxplot(kingCountyHouses$price, main="Price of houses",ylab="Price")

mtext(price_out_label,side= 1, line = 0.5)






hist(kingCountyHouses$price,main="Histogram of Price of Houses",xlab="Price")

density_price <- density(kingCountyHouses$price)
plot(density_price, main="Density plot: Price of homes")
polygon(density_price,col="skyblue",border="black")
## Summary stats of Sqft ##
summary(kingCountyHouses$sqft)


density_sqft <- density(kingCountyHouses$sqft)
plot(density_sqft, main="Density plot: Square feet of homes")
polygon(density_sqft,col="skyblue",border="black")

## graphs of sqft ##
boxplot(kingCountyHouses$sqft,main="Square feet of houses",ylab="Square feet")
hist(kingCountyHouses$sqft,main="Histogram of square feet of houses",xlab="Square feet")

plot(kingCountyHouses$sqft,kingCountyHouses$price,ylab="Price",xlab="Square feet")


sqft_box_stats <- boxplot.stats(kingCountyHouses$sqft)
sqft_out <- length(sqft_box_stats$out)

sqft_out_label <- paste("Number of Outliers:", price_out)

boxplot(kingCountyHouses$sqft, main="Square feet of houses",ylab="Square feet")

mtext(sqft_out_label,side= 1, line = 0.5)






## Part b ##



scatter.smooth(x=kingCountyHouses$sqft,y=kingCountyHouses$price,main="Price ~ Square feet",ylab = "Price",xlab = "square feet",lpars = list(col = "red"))
boxplot(kingCountyHouses$price,kingCountyHouses$sqft)

summary(lm(kingCountyHouses$price~kingCountyHouses$sqft))

lm.model1 <- lm(formula = kingCountyHouses$price~kingCountyHouses$sqft)

lm.model1

summary(lm.model1)

summary(lm.model1$residuals)
par(mfrow=c(2,2))
gvlma::gvlma(lm.model1)
plot(lm.model1)




## Part C ##

logprice <- log(kingCountyHouses$price) # log defaults to calculating natural log. 

lm.model2 <- lm(formula = logprice~kingCountyHouses$sqft)

lm.model2


# storing coefficent and intercept from lm.model2

model2_coefficients <- coef(lm.model2)
coef(lm.model2)

intercept_model2 <- model2_coefficients["(Intercept)"]
model2_sqft_coefficent <- model2_coefficients["kingCountyHouses$sqft"]

model2_price_1 <- intercept_model2 + (model2_sqft_coefficent *10000)

model2_price_2 <- intercept_model2 + (model2_sqft_coefficent * 10001)


# part e. check back later. 
diff_price <- log(model2_price_1) - log(model2_price_2)

diff_price




## part f



## part e

logsqft <- log(kingCountyHouses$sqft)

lm.model3 <- lm(formula = kingCountyHouses$price ~ logsqft)

model3_coefficients<- coef(lm.model3)


intercept_model3 <- model3_coefficients["(Intercept)"]
model3_logsqft_coefficent <- model3_coefficients["logsqft"]



model3_regression_equation_1 <-intercept_model3 + (model3_logsqft_coefficent * 10000)
model3_regression_equation_2 <-intercept_model3 + (model3_logsqft_coefficent * 10001)
model3_regression_equation_3 <-intercept_model3 + (model3_logsqft_coefficent * (10000 * 1.01))
lm.model3

model3_regression_equation_1
model3_regression_equation_2
model3_regression_equation_3


summary(lm.model3)


lm.model4 <-lm(logsqft ~ logprice + kingCountyHouses$waterfront)
summary(lm.model4)






