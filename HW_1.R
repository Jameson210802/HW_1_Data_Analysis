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



scatter.smooth(x=kingCountyHouses$sqft,y=kingCountyHouses$price,main="Price ~ Square feet")
boxplot(kingCountyHouses$price,kingCountyHouses$sqft)

##summary(lm(kingCountyHouses$price~kingCountyHouses$sqft))

lm.model1 <- lm(formula = kingCountyHouses$price~kingCountyHouses$sqft)

par(mfrow=c(2,2))
gvlma::gvlma(lm.model1)
plot(lm.model1)

##plot(linearpricesqft.mod)






## Part C ##

logprice <- log(kingCountyHouses$price) # log defaults to calculating natural log. 

lm.model2 <- lm(formula = logprice~kingCountyHouses$sqft)

lm.model2


# storing coefficent and intercept from lm.model2

model2_coefficients <- coef(lm.model2)

intercept_model2 <- model2_coefficients["(Intercept)"]
model2_sqft_coefficent <- model2_coefficients["kingCountyHouses$sqft"]

y <- intercept_model2 + (model2_sqft_coefficent *100001)

y












