library(astsa)
library(ggplot2)
library(timeSeries)

# col, name, detail
# 2, BCPI,	 BCPI Total 	
# 3, BCNE,	 BCPI Excluding Energy 				
# 4, ENER,	 BCPI Energy 			
# 5, MTLS,	 BCPI Metals and Minerals 				
# 6, FOPR,	 BCPI Forestry 			
# 7, AGRI,	 BCPI Agriculture					
# 8, FISH,	 BCPI Fish		

## A.____ = annual metric; M.____ = monthly metric

## 1972-01-01 - 2023-10-01 ##

M.BCPI <- as.data.frame(read.csv("M.BCPI.csv"))

total <- ts(M.BCPI[,2], start = 1972, frequency = 12)
tsplot(total)

par(mfrow=c(1,2))
# Use post 1996 prices (crude oil price index changed)
price <- window(total, start = 1996)
tsplot(price, main = "Total Index")

# Look at log transformation 
price.log <- log(price)
tsplot(price.log, main = "Log Price of Index")

# Need to find transformation for stationary distribution
price.tran <- diff((price.log), 1)
par(mfrow = c(1,3))
tsplot(price.tran, ylab = "BCPI", main = "BCPI 1996-2023")
acf(price.tran)
pacf(price.tran)


# Where is the possible level shift ? 
# Look at pre covid prices ? 
price.log.cov = window(price.log, end = 2020)
# See some possible models 

Model1 <- sarima(price.log.cov,0,1,1,details = TRUE,  no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model2 <-sarima(price.log.cov,0,1,2,details = TRUE,  no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model3 <- sarima(price.log.cov,0,1,3,details = TRUE, no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model4 <-sarima(price.log.cov,1,1,1,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))
Model5<- sarima(price.log.cov,1,1,2,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))
Model6<- sarima(price.log.cov,2,1,2,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))

AICr1 <- c(Model1$AIC,Model2$AIC,Model3$AIC,Model4$AIC,Model5$AIC,Model6$AIC)

# best model 

best <- which.min(AICr1) # model 4 is the best





