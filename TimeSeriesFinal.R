library(astsa)
library(ggplot2)
library(timeSeries)
library(forecast)

# col, name, detail
BCPI = 2
BCNE = 3
ENER = 4
MTLS = 5
FOPR = 6
AGRI = 7


## 1972-01-01 - 2023-10-01 ##

M.BCPI <- as.data.frame(read.csv("M.BCPI.csv"))


total <- ts(M.BCPI[,3], start = 1972, frequency = 12)
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
tsplot(price.log.cov)
# See some possible models 




#Functions to make plotting comparisons easier

#Use STATIC VARIABLE for i, all other parameters can be left empty if desired
#Plots a specific commodity with/without transformations and included acf/pacf plots 

mult_plot <- function(i, DIFF = d, log = FALSE, ACF = FALSE, PACF = FALSE){
  row = 3
  data <- ts(M.BCPI[,i], start = 1980, end = 2020, frequency = 12)
  par(mfrow=c(row,1))
 
  if(log == TRUE){
    data <- log(data)
  }
  
  if(!missing(DIFF) & DIFF>0){
  data <- diff(data, DIFF)
  }
  
  if(ACF == TRUE){#bad coder moment
    row = row + 1
  }
  if(PACF == TRUE){
    row = row + 1
  }
  tsplot(data, main = paste(names(M.BCPI[i])), ylab = "Index" )
  
  if(ACF == TRUE){
    acf(data, 48)
  }
  if(PACF == TRUE){
    pacf(data, 48)
  }
  
}         
#Plots every commodity on separate pages
plot_everything<- function(diff = 1, log = FALSE, acf = FALSE, pacf = FALSE){
  for(i in 2:8){
    mult_plot(i, diff, log, acf, pacf)
  }
}

total.c <-  window(total, end = 2020) # CHANGE TOTAL 


total.l <- log(total.c) # log total
total.l.d <- diff(total.l, 1) # difference log total 
total.l.d.s <- diff(total.l.d, 12) # difference log total seasonal 

## Plotting log ting 
par(mfrow = c(1,3))
tsplot(total.l.d)
acf(total.l.d, lag.max = 50)
pacf(total.l.d, lag.max = 50)


## Looking for seasonal trends 
par(mfrow = c(1,3))
tsplot(total.l.d.s)
acf(total.l.d.s, lag.max = 50)
pacf(total.l.d.s, lag.max = 50)

### NOTES: 

### PACF cuts off tails after ks for k = 1,2,3,... s = 12 => P = 0

### ACF cuts of after Q = 1

## Implies we have seasonal (0,1,1)

## Potential (0,1,12) x (1,1,1)_12??

Model1 <- sarima(total.l, 1,1,0, P = 0, D = 1, Q = 1, S = 12) ## BEST 
Model3 <- sarima(total.l, 1,1,1, P =0, D =1, Q = 1, S = 12) ## SECOND BEST 
Model4 <- sarima(total.l, 0,1,1, P =0, D =1, Q = 1, S = 12)
Model5 <- sarima(total.l, 0,1,2, P = 0, D = 1, Q = 1, S = 12)



