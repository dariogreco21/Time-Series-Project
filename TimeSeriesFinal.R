library(astsa)
library(ggplot2)
library(timeSeries)

setwd("/Users/dario/Desktop/TSP") # Set your working directory 



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
M.ER <- as.data.frame(read.csv("ER.csv")) 
M.ER <- M.ER[-(1:12),] # Exchange Rate 


yy <- ts(M.BCPI[300:622,2], start = 1997, frequency = 12)
tsplot(y)
dyy <-diff((log(y)), 1)
par(mfrow = c(1,3))
tsplot(dyy, ylab = "BCPI", main = "BCPI 1972-2023")

acf(dyy)
pacf(dyy)


# Where is the level shift ? 


t1 <- 2006; t2 <- 2010
tt <- c(t1:t2)
n0 <-length(tt)
AICinfo = rep(0,n0)
Time <- time(y)
iter <- 1
for(time1 in tt){
  TimeBk <- c(Time>=time1) # Creating the indicator variable 
  Model1 <- sarima(y,2,0,0,details = TRUE, xreg = TimeBk, no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
  AICinfo[iter] <- Model1$AIC
  iter <- iter + 1
  
}

t0 <- tt[AICinfo == min(AICinfo)]
# Fit with time shit at t0 

reg 
Time0 <- c(Time>=2020)

# See some possible models 


Model2 <- sarima(y,0,1,1,details = TRUE,  no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model3 <-sarima(y,0,1,2,details = TRUE,  no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model4 <- sarima(y,0,1,3,details = TRUE, no.constant = TRUE, tol = sqrt(.Machine$double.eps)) 
Model5 <- sarima(y,1,1,1,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))
Model6<- sarima(y,1,1,2,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))
Model7<- sarima(y,2,1,2,details = TRUE,   no.constant = TRUE, tol = sqrt(.Machine$double.eps))

AICr1 <- c(Model2$AIC,Model3$AIC,Model4$AIC,Model5$AIC,Model6$AIC,Model7$AIC)




