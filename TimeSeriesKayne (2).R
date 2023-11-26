packages <- c("astsa", "dplyr", "ggplot2", "timeSeries", "forecast",
              "ggfortify", "grid", "ggthemes", "reshape2", "ggforce", 
              "scales", "lubridate", "tidyquant", "cowplot")
lapply(packages, library, character.only = TRUE)
#Saves space
setwd("C:\\Users\\Kayne\\Desktop\\Time Series Project")

M.BCPI <- as.data.frame(read.csv("M.BCPI.csv"))


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
# Baseline autoplot attributes for ts plots to remove clutter from code

##title,x,y in quotes

graph_aes <- function(data, title, x, y){ 
  if(missing(x)){
    x = "Time"
  }
  if(missing(y)){
    y = "Commodity Price Index"
  }
  
  autoplot(data, data.colour = "black", size = 0.8,  
           data.linetype = "solid", xlab = "x", ylab = "y") +
    ggtitle(title) + 
    theme_calc() + #Theme to make it look pretty
    scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="5 years"), 
                 labels=label_date("%Y"))  # more frequent time intervals
}

######Following ggplot objects to combine into annotated graph########

#Covid19
covid_19_obj <- geom_text(aes(x = as.Date('2020-03-01') + 580, y = -20), 
                          label = "Covid-19", color = "black",
                          size = 3, fontface = 'plain') 
covid_rect <- annotate("rect", xmin=as.Date('2020-03-01'), 
                       xmax = as.Date('2022-10-01'),
                       ymin = 0, ymax = Inf, fill = "darkred", alpha = 0.2)

#2008 Housing Market Crash
glob_cris_obj <- geom_text(aes(x = as.Date('2008-03-24') + 200, y = -20), 
                           label = "Global Financial Crisis", color = "black",
                           size = 3, fontface = 'plain') 
glob_rect <- annotate("rect", xmin=as.Date('2008-04-24'), 
                      xmax = as.Date('2009-10-01'),
                      ymin = 0, ymax = Inf, fill = "blue", alpha = 0.2)

#Dot Com Bubble
dot_com_obj <- geom_text(aes(x = as.Date('2000-03-24') + 400, y = -20), 
                         label = "Dot Com Crash", color = "black",
                         size = 3, fontface = 'plain')
dot_com_rect <- annotate("rect", xmin=as.Date('2000-06-24'), 
                         xmax = as.Date('2002-01-01'),
                         ymin = 0, ymax = Inf, fill = "darkgreen", alpha = 0.2)

  
#Energy Crisis 1979
ener_obj <- geom_text(aes(x = as.Date('1979-01-24') + 200, y = -20), 
                      label = "Energy Crisis", color = "black",
                      size = 3, fontface = 'plain')
ener_rect <- annotate("rect", xmin=as.Date('1979-01-24'), 
                      xmax = as.Date('1981-01-24'),
                      ymin = 0, ymax = Inf, fill = "cyan", alpha = 0.2)
  

############################################################################

#Appends all objects into annotated graph
timeline_plot <- function(ts, title){ 
  graph_aes(ts, title) +
    
  covid_19_obj +
    covid_rect +
  glob_cris_obj +
    glob_rect +
  dot_com_obj +
    dot_com_rect +
  ener_obj +
    ener_rect
    
    
    
}

#Examples
timeline_plot(total.en, "British Columbia Consumer Price Index")
timeline_plot(total, "British Columbia Consumer Price Index, No Energy")
timeline_plot(ener, "British Columbia Consumer Price Index, Only Energy") 

#ALL FOLLOWING FUNCTION CALLS USE THIS. CHANGE MODEL FOR DIFFERENT ARIMA PLOTS
#////////////////////////////////////////////////////////////#
#Use Arima() in order to use lambda parameter. Lambda = 0 -> first diff
Model1_corrected <-Arima(total, c(0,1,12), lambda = 0) 
d.forecast <- forecast(Model1_corrected, level = c(95), h=24) 
#////////////////////////////////////////////////////////////#
#Zooming in on forecast (COMPLICATED VERSION)

#Pure magic don't touch

  
f_final <- fortify(forecast)
f_final$date <- as.Date((f_final$Index))
f_final <- f_final %>% #No touchy
  select(-Index) %>%
     
  
  rename("Low95" = "Lo 95",
          "High95" = "Hi 95",
          "Forecast" = "Point Forecast") %>%
    
    #Pipe magic do not question
  filter(date >= as.Date("2022-01-01")) 
    
  
  

#Fitted model zoomed in on forecast

zoomed_plot <-
  ggplot(f_final, aes(x = date)) +
  ggtitle("ARMA(10,1,3)  Time > 2022") +
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

f_final <- fortify(forecast)
f_final$date <- as.Date((f_final$Index))
f_final <- f_final %>% #No touchy
  select(-Index) %>%
     
  
  rename("Low95" = "Lo 95",
          "High95" = "Hi 95",
          "Forecast" = "Point Forecast") %>%
    
    
  
# Entire plot with fitted model
full_plot <- 
  ggplot(f_final, aes(x = date)) +
  ggtitle("ARMA(10,1,3)") +
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

#Examples
full_plot
zoomed_plot


#Transformation Diagnostics plot
#Using original log/diff data as example.

acf_1 <- ggAcf(total.l.d, lag = 48) + xlab("Lag (Months)") + ggtitle('Log First Diff ACF') +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5))

pacf_1 <- ggPacf(total.l.d, lag = 48)  + xlab("Lag (Months)") + ggtitle('Log First Diff PACF') +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5))

log_1 <- autoplot(total.l, ts.colour = "black", size = 0.8,  
         ts.linetype = "solid", xlab = "Time", ylab = "Log Price Index") +
  ggtitle("Log Transformation") + 
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="15 years"), 
               labels=label_date("%Y"))

diff_1 <- autoplot(total.l.d, ts.colour = "black", size = 0.8,  
                  ts.linetype = "solid", xlab = "Time", ylab = "Log Differenced Data") +
  ggtitle("Log - First Difference") + 
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="15 years"), 
               labels=label_date("%Y"))

#Plots all in neat 4x4 grid
plot_grid(log_1, diff_1, acf_1,pacf_1, labels = c('','', '', ''))  




# Other method of zooming into the forecast plot. Much simpler/intuitive but
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
