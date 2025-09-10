# HW 1
# Jamie Anderson netid: jla656

price_val <- (kingCountyHouses$price)
sqft_val <- (kingCountyHouses$sqft)

#ploting scatter plot. 

## NUMBER 1 ##

summary(price_val)
options(scipen = 999)
boxplot(price_val,sqft_val)
boxplot(sqft_val)
density(price_val)

hist(price_val)
hist(sqft_val)
summary(sqft_val)



#TODO double check this 
options(scipen = 999)
plot(sqft_val,price_val,xlab="sqft",ylab="price")


linear_reg_1 <- lm(price~sqft,data=kingCountyHouses)
summary(linear_reg_1)
abline(linear_reg_1, col = "red", lwd =3)
lm()

hist(price_val)

