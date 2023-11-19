library(astsa)
library(ggplot2)
library(timeSeries)
library(forecast)

M.BCPI <- as.data.frame(read.csv("M.BCPI.csv"))

total <- ts(M.BCPI[,3], start = 1972, frequency = 12)
tsplot(total)

# total.c <-  window(total, end = 2020)  # Look at Pre-Covid levels 

total.l <- log(total) # log total
total.l.d <- diff(total.l, 1) # difference log total 

## Plotting log ting 
par(mfrow = c(1,3))
tsplot(total.l.d)
acf(total.l.d)
pacf(total.l.d)


### USE Model$ttable to look at significant coefficients 
# NOTE: Starting from a lower p, q value dont pass Ljung -Box test 
M1 <- sarima(total.l, 0,1,12) 
M2 <- sarima(total.l, 12,1,0) 
M3 <- sarima(total.l, 11,1,0)
M4 <- sarima(total.l, 11,1,1) 
M5 <- sarima(total.l, 11,1,2)
M6 <- sarima(total.l, 12,1,2)
M7 <-sarima(total.l, 12, 1, 1) 


# Winner Model 7,6,5, 1
AICv <- c(M1$AIC, M2$AIC, M3$AIC, M4$AIC, M5$AIC, M6$AIC, M7$AIC)
BestAIC <- order(AICv)[1:4] # Returns four best models with AIC criterion 
 # NOTE: MODEL 6 and 5 are similar we just look at model 5 for now 

# Extracts Ljung - Box statistic p -values for an ARMA(p,d,q) 
# NOTE: df = p + q
LJB_pvalues <- function(Model, max.lag, df){
  resd = Model[["fit"]][["residuals"]]
  pv <- c()
  for(i in df+1: max.lag){
    p <- Box.test(resd, i, type = "Ljung-Box", fitdf = df)$p.value
    pv = append(pv, p)
  }
  return(pv)
}

# Extract p-values up to lag 35
p7  = LJB_pvalues(M7, 35, 13); p5 = LJB_pvalues(M5, 35, 14); p1 = LJB_pvalues(M1, 35, 11) 
## p -value >0.05 at higher lags for Model 7,5,1 so we can coninute oto forecasting 


## Forecasting (not looking ta covid effects)

# MAYBE MAKE THIS GG PLOT 
# DENOTED WINNING MODELS Mi by Modeli


Model1 <-arima(total.l, c(12,1,1))
fore1 <- predict(Model1, n.ahead = 24)
ts.plot(log(total), xlim = c(1972, 2026), ylab = "Price Change") # IS IT PRICE CHANGE 

U =fore1$pred + fore1$se
L = fore1$pred - fore1$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore1$pred, type="l", col=2) # MAYBE ADD A SECOND ERROR BAR




