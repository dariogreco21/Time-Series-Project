packages <- c("astsa", "dplyr", "ggplot2", "timeSeries", "forecast",
              "ggfortify", "grid", "ggthemes", "reshape2", "ggforce", 
              "scales", "lubridate", "tidyquant")
lapply(packages, library, character.only = TRUE)
#Saves space


M.BCPI <- as.data.frame(read.csv("M.BCPI"))


# BCPI Total index
total.en <- ts(M.BCPI[,2], start = 1972, frequency = 12)
tsplot(total.en) # The effects of marekt crashes are much more notable 

# BCNE

M.BCPI <- as.data.frame(read.csv("M.BCPI.csv"))
par(mfrow=c(1,2))
# Total Index Including Energy 
total.en <- ts(M.BCPI[,2], start = 1972, frequency = 12)
tsplot(total.en) # The effects of marekt crashes are much more notable 

# Without Energy 

total <- ts(M.BCPI[,3], start = 1972, frequency = 12)
tsplot(total)

### TRANSFORMATIONS ###

total.l <- log(total)
tsplot(total.l)# log total
total.l.d <- diff(total.l, 1) # difference log total 

## Plotting log ting 
par(mfrow = c(1,3))
tsplot(total.l.d)
acf(total.l.d)
pacf(total.l.d)


### USE Model$ttable to look at significant coefficients 
# NOTE: Starting from a lower p, q value dont pass Ljung -Box test 
# NOTE 11,1,2, 12,1,1 are both OVERFIT
M1 <- sarima(total.l, 0,1,12) 
M2 <- sarima(total.l, 0,1,11)
M3 <- sarima(total.l, 11,1,0)
M4 <-sarima(total.l, 12,1,0) 
M5 <- sarima(total.l, 10,1,3)


AICv <- c(M1$AIC, M2$AIC, M3$AIC, M4$AIC, M5$AIC)
BestAIC <- order(AICv)[1:3] ## Models 5,1,3 are best (in order)

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
p5  = LJB_pvalues(M5, 35, 13); p1 = LJB_pvalues(M1, 35, 14); p3 = LJB_pvalues(M3, 35, 11) 
## p -value >0.05 at higher lags for all of Model 5, 1, 3

# MAYBE MAKE THIS GG PLOT 
# DENOTED WINNING MODELS Mi by Modeli

## WERE GONNA HAVE TO UNLOG THIS 
par(mfrow = c(1,1))
Model1 <-arima(total.l, c(10,1,3))
fore1 <- predict(Model1, n.ahead = 24)
exp(fore1$se)
ts.plot(total.l, xlim = c(1972, 2026), ylab = "Price Change") # IS IT PRICE CHANGE 

U =fore1$pred + fore1$se
L = fore1$pred - fore1$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore1$pred, type="l", col=2) # MAYBE ADD A SECOND ERROR BAR


##############ggplot###########


#Does pretty plot with input as index on any specific part of BCPI data
#that we want to look at
timeline_plot <- function(ts, title){ #title in quotes
  autoplot(ts, ts.colour = "black", size = 0.8,  
           ts.linetype = "solid", xlab = "Time", ylab = "Commodity Price Index") +
    ggtitle(title) + 
    theme_calc() + #Theme to make it look pretty
    scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="5 years"), 
                 labels=label_date("%Y")) + # more frequent time intervals
    
    ####Covid####
  #Inserts text onto plot by using x-value as date for more accurate alignment.
  # Add an integer to this val for subtle shifting if required
  geom_text(aes(x = as.Date('2020-03-01') + 580, y = -20), label = "Covid-19", color = "black",
            size = 3, fontface = 'plain') +
    #Creates rectangle that can respond to the alpha parameter correctly (geom_rect wasnt working)
    annotate("rect", xmin=as.Date('2020-03-01'), xmax = as.Date('2022-10-01'),
             ymin = 0, ymax = Inf, fill = "darkred", alpha = 0.2) +
    
    #2008 bubble steve carrel saves the world by being angry ryan gosling is so me
    geom_text(aes(x = as.Date('2008-03-24') + 200, y = -20), label = "Global Financial Crisis", color = "black",
              size = 3, fontface = 'plain') + 
    annotate("rect", xmin=as.Date('2008-04-24'), xmax = as.Date('2009-10-01'),
             ymin = 0, ymax = Inf, fill = "blue", alpha = 0.2) +
    
    # Dot Com Recession
    geom_text(aes(x = as.Date('2000-03-24') + 400, y = -20), label = "Dot Com Crash", color = "black",
              size = 3, fontface = 'plain') +  
    annotate("rect", xmin=as.Date('2000-06-24'), xmax = as.Date('2002-01-01'),
             ymin = 0, ymax = Inf, fill = "darkgreen", alpha = 0.2) +
    
    
    #1979 Energy crisis
    
    geom_text(aes(x = as.Date('1979-01-24') + 200, y = -20), label = "Energy Crisis", color = "black",
              size = 3, fontface = 'plain') + 
    annotate("rect", xmin=as.Date('1979-01-24'), xmax = as.Date('1981-01-24'),
             ymin = 0, ymax = Inf, fill = "cyan", alpha = 0.2)
}

timeline_plot(total.en, "British Columbia Consumer Price Index")
timeline_plot(total, "British Columbia Consumer Price Index, No Energy")
timeline_plot(ener, "British Columbia Consumer Price Index, Only Energy") #No idea why the 0 axis squishes here but 
#if we want this plot i can fuck with it directly




Model1_corrected <-Arima(total, c(10,1,3), lambda = 0) #Use Arima() in order to use
#                                         lambda parameter. Lambda = 0 -> first diff
d.forecast <- forecast(Model1_corrected, level = c(95), h=24) #forecast object for final model

#Zooming in on forecast (COMPLICATED VERSION)

#fortify into a data frame and change all the column names to make sense out of 
#all of this
zoom <- fortify(d.forecast)
zoom$date <- as.Date((zoom$Index))
zoom <- zoom %>% #magic infix operator that i will not touch
select(-Index) %>%
  #Remove this line to get full plot
  filter(date >= as.Date("2022-01-01")) %>%
  
rename("Low95" = "Lo 95",
       "High95" = "Hi 95",
       "Forecast" = "Point Forecast")


# Zoomed in plot

zoomed_plot <-
  ggplot(zoom, aes(x = date)) +
  ggtitle("Prediction model - Zoomed in") +
  xlab("Time") + ylab("Commodity Price Index") +
  geom_ribbon(aes(ymin = Low95, ymax = High95, fill = "95%")) +
  geom_line(aes(y = Data, group = 1, colour = "Data"),
            linetype = "solid", size = 1) +
  geom_line(aes(y = Fitted, group = 2, colour = "Fitted"), size = 1, 
            linetype = 'dashed') +
  geom_line(aes(y = Forecast, group = 3, colour = "Forecast"), size = 1) +
  scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
  scale_fill_brewer(name = "CI Interval") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme_bw(base_size = 14) +
  geom_segment(aes(x = as.Date('2023-10-01'), xend = as.Date("2023-11-01"),
                   y = 418.0750, yend = 414.3796), size = 1, colour = 'orange')



#There was an actually much easier and intuitive way to do this but
# then I don't get the super sick colour packages.
# also legend didnt work.

 # autoplot(d.forecast, ts.colour = "black", size = 0.8,  
 #         ts.linetype = "solid", xlab = "Time", ylab = "Commodity Price Index",
 #         predict.colour = "darkgreen", predict.linetype = 'dashed', conf.int = TRUE,
 #         predict.size = 0.9) +
 #  ggtitle("ARIMA(10,1,3) Forecast") + 
 #   xlab("Time") +
 #   ylab("Commodity Price Index") +
 #   geom_line(aes(y = d.forecast$fitted), col = "orange", size = 1, linetype = ) + 
 #  theme_calc() + #Theme to make it look pretty

# This is how you zoom in
 #  coord_x_date(xlim = c(as.Date('2021-01-01'), as.Date('2025-10-01')))



#Without zoom, literally same code without one line
final <- fortify(d.forecast)
final$date <- as.Date((final$Index))
final <- final %>% #magic infix operator that i will not touch
  select(-Index) %>%
  rename("Low95" = "Lo 95",
         "High95" = "Hi 95",
         "Forecast" = "Point Forecast")

final_plot <- 
  ggplot(final, aes(x = date)) +
  ggtitle("Final Fitted Model") +
  xlab("Time") + ylab("Commodity Price Index") +
  geom_ribbon(aes(ymin = Low95, ymax = High95, fill = "95%")) +
  geom_line(aes(y = Data, group = 1, colour = "Data"),
            linetype = "solid", size = 0.8) +
  geom_line(aes(y = Fitted, group = 2, colour = "Fitted"), size = 1, 
            linetype = 'dashed') +
  geom_line(aes(y = Forecast, group = 3, colour = "Forecast"), size = 1) +
  scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
  scale_fill_brewer(name = "CI Interval") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme_bw(base_size = 14) +
  geom_segment(aes(x = as.Date('2023-10-01'), xend = as.Date("2023-11-01"),
                   y = 418.0750, yend = 414.3796), size = 1, colour = 'orange') +
  scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="5 years"), 
               labels=label_date("%Y")) 


final_plot
zoomed_plot



 

  



  
