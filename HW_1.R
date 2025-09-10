# HW 1
# Jamie Anderson netid: jla656


## part a ##

## summary stats of price ##

summary(kingCountyHouses$price)

options(scipen = 999) # disable scientific notation 

## graphs of price ##

boxplot(kingCountyHouses$price, main="Price of houses",ylab="Price")
hist(kingCountyHouses$price,main="Histogram of Price of Houses",xlab="Price")



## Summary stats of Sqft ##
summary(kingCountyHouses$sqft)

## graphs of Price ##
boxplot(kingCountyHouses$sqft,main="Square feet of houses",ylab="Square feet")
hist(kingCountyHouses$sqft,main="Histogram of square feet of houses",ylab="Square feet")

plot(kingCountyHouses$sqft,kingCountyHouses$price,ylab="Price",xlab="Square feet")




## Part b ##

lm(formula= kingCountyHouses$price~kingCountyHouses$sqft) # price is our response value and sqft is our explanatory variable



## Part C ##

logprice <- log(kingCountyHouses$price) # log defaults to calculating natural log. 

lm(logprice~kingCountyHouses$sqft)














