packages <- c("astsa", "dplyr", "ggplot2", "timeSeries", "forecast",
              "ggfortify", "grid", "ggthemes", "reshape2", "ggforce", 
              "scales", "lubridate", "tidyquant", "cowplot", "knitr", 
              "kableExtra")
invisible(lapply(packages, library, character.only = TRUE))
# Need to install individually if not installed
#The following functions mainly used for decomposition to make the code
# more readable.


#Template for time series plots
graph_aes <- function(data, title, x, y){ 
  if(missing(x)){
    x = "Time"
  }
  if(missing(y)){
    y = "Commodity Price Index"
  }
  
  autoplot(data, data.colour = "black", size = 0.8,  
           data.linetype = "solid", xlab = x, ylab = y) +
    ggtitle(title) + 
    theme_calc() + #Theme to make it look pretty
    scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="5 years"), 
                 labels=label_date("%Y"))  # more frequent time intervals
}

#Template for ACF/PACF plots
acf_aes <- function(ts, title, p = FALSE){
  x <- ggAcf
  y <- ' ACF'
  if(p == TRUE){
    x <- ggPacf
    y <- 'PACF'
  }
  return(
    x(ts, lag = 48) + xlab("Lag (Months)") + ggtitle(paste(title,y)) +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5))
  )
}
#Template for forecast plots. Includes the ability to zoom around the forecast
# period.
forecast_plot_aes <- function(fs, title, zoom = FALSE){
  
  #Allows me to group original data, fitted, and forecasts for Legend
  forecast <- rename(fortify(fs), "Forecast" = "Point Forecast",
                     "date" = "Index")
  
  fsplot <- ggplot(forecast, aes(x = date)) +
    xlab("Time") + 
    ylab("Commodity Price Index") +
    ggtitle(title) + 
    geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`, fill = "95%")) +
    geom_line(aes(y = Data, group = 1, colour = "Data"),
              linetype = "solid", size = 0.8) +
    geom_line(aes(y = Fitted, group = 2, colour = "Fitted",),
              linetype = "dashed", size = 0.9) +
    geom_line(aes(y = Forecast, group = 3, colour = "Forecast"), size = 1) +
    
    #Connects empty space between data and forecast. 
    geom_segment(aes(x = as.Date('2023-10-01'), xend = as.Date("2023-11-01"),
                     y = 415.01, yend = 414.3796), size = 0.8, colour ="#00BA38" ) +
    #Themes and Legend color themes to make it prettier.
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
    scale_fill_brewer(name = "CI Interval") +
    theme_bw(base_size = 14)   
  
  # Zoom
  if(zoom == TRUE){
    fsplot <- fsplot + 
      coord_x_date(xlim = c(as.Date('2022-01-01'), as.Date('2024-07-01'))) +
      scale_y_continuous(limits = c(300, 700))
  }
  fsplot
  
}


######Following ggplot objects to combine into annotated graph########

#Covid19
covid_19_obj <- annotate("text", x = as.Date('2020-03-01') + 580, y = -20, 
                         label = "Covid-19", color = "black",
                         size = 3, fontface = 'plain') 
covid_rect <- annotate("rect", xmin=as.Date('2020-03-01'), 
                       xmax = as.Date('2022-10-01'),
                       ymin = 0, ymax = Inf, fill = "darkred", alpha = 0.2)

#2008 Housing Market Crash
glob_cris_obj <- annotate("text", x = as.Date('2008-03-24') + 200, y = -20, 
                          label = "Global Financial Crisis", color = "black",
                          size = 3, fontface = 'plain') 
glob_rect <- annotate("rect", xmin=as.Date('2008-04-24'), 
                      xmax = as.Date('2009-10-01'),
                      ymin = 0, ymax = Inf, fill = "blue", alpha = 0.2)

#Dot Com Bubble
dot_com_obj <- annotate("text", x = as.Date('2000-03-24') + 100, y = -20, 
                        label = "Dot Com Crash", color = "black",
                        size = 3, fontface = 'plain')
dot_com_rect <- annotate("rect", xmin=as.Date('2000-06-24'), 
                         xmax = as.Date('2002-01-01'),
                         ymin = 0, ymax = Inf, fill = "darkgreen", alpha = 0.2)


#Energy Crisis 1979
ener_obj <- annotate("text", x = as.Date('1979-01-24') + 200, y = -20, 
                     label = "Energy Crisis", color = "black",
                     size = 3, fontface = 'plain')
ener_rect <- annotate("rect", xmin=as.Date('1979-01-24'), 
                      xmax = as.Date('1981-01-24'),
                      ymin = 0, ymax = Inf, fill = "cyan", alpha = 0.2)

world_events <- c(covid_19_obj, covid_rect, glob_cris_obj, glob_rect, 
                  dot_com_rect, dot_com_obj, ener_rect, ener_obj)
############################################################################



#Dataset and respective indexes for isolated commodity data
BCPI_full <- as.data.frame(read.csv("M.BCPI.csv"))
BCPI <- 2 # Entire basket
BCNE <- 3 # No Energy <- Chosen data
ENER <- 4 # Only Energy
METAL <- 5 # Only metals/minerals
FORSTRY <- 6 # Only forestry
AGRI <- 7 # Only agriculture
FISH <- 8 # Only fish








BCPI_ts <- ts(BCPI_full[,BCPI], start = 1972, frequency = 12)
BCNE_ts <- ts(BCPI_full[,BCNE], start = 1972, frequency = 12) # <-Chosen dataset

#Time series plots

graph_aes(BCPI_ts, "Bank of Canada Commodity Price Index")
graph_aes(BCNE_ts, "Bank of Canada Commodity Price Index, No Energy")

#Annotated graphs highlighting world events and their observed effect on the data
graph_aes(BCPI_ts, "Bank of Canada Commodity Price Index") + world_events
graph_aes(BCNE_ts, "Bank of Canada Commodity Price Index, No Energy") + world_events

#Transformations on BCNE
log_plot <- graph_aes(log(BCNE_ts), "Logged BCPI", y = "Logged CPI") +
 theme_clean() + 
  #Fix time scaling so that it doesn't get squished.
 scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="15 years"),
              labels=label_date("%Y"))
               
log_diff <- graph_aes(diff(log(BCNE_ts)), "Log First Difference", y = "Data") + 
  theme_clean() + 
  scale_x_date(breaks = seq(as.Date("1972-01-01"), as.Date("2023-10-01"), by="15 years"),
               labels=label_date("%Y"))

acf_ld <- acf_aes(diff(log(BCNE_ts)), "Log First Difference")
pacf_ld <- acf_aes(diff(log(BCNE_ts)), "Log First Difference", p = TRUE)

#Plots all four
plot_grid(log_plot, log_diff, acf_ld,pacf_ld, labels = c('','', '', ''))  

#Diagnostics / Potential Models

#Logged BCNE Data will be our chosen data to model
main_ts <- log(BCNE_ts)

#Handful of models that were able to pass diagnostics
#Note: ARIMA(11,1,2), ARIMA(12,1,1) are both OVERFIT


M1 <- sarima(main_ts, 0,1,12, no.constant = TRUE) 
M2 <- sarima(main_ts, 0,1,11, no.constant = TRUE)
M3 <- sarima(main_ts, 11,1,0, no.constant = TRUE)
M4 <- sarima(main_ts, 12,1,0, no.constant = TRUE) 
M5 <- sarima(main_ts, 10,1,3, no.constant = TRUE)

#Differentiate model performance by comparing criterion
AICv <- c(M1$AIC, M2$AIC, M3$AIC, M4$AIC, M5$AIC)
BestAIC <- order(AICv)[1:3] 
## Models M5,M1,M3 are best (in order)

#Some models had Ljung-Box statistics plots with suspicious plots near p=0.05.

# Extracts Ljung - Box statistic p -values for an ARMA(p,d,q) 
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


#Forecasting
#ARIMA(10,1,3)
m5_forecast <- Arima(BCNE_ts, c(10,1,3), lambda = 0) %>%
                forecast(., level= c(95), h = 10)
#MA(12)
m1_forecast <- Arima(BCNE_ts, c(0,1,12), lambda = 0) %>%
                forecast(.,level = c(95), h=10)
#AR(11)
m3_forecast <- Arima(BCNE_ts, c(11,1,0), lambda = 0) %>%
               forecast(.,level = c(95), h=10)

#Sample Forecasts
forecast_plot_aes(m5_forecast, "ARIMA(10,1,3)")
forecast_plot_aes(m5_forecast, "ARIMA(10,1,3), Time > 2022", zoom = TRUE)

forecast_plot_aes(m1_forecast, "ARIMA(0,1,12)")
forecast_plot_aes(m1_forecast, "ARIMA(0,1,12), Time > 2022", zoom = TRUE)

forecast_plot_aes(m3_forecast, "ARIMA(11,1,0)")
forecast_plot_aes(m3_forecast, "ARIMA(11,1,0), Time > 2022", zoom = TRUE)



#Code for tables
AICv <- c(M1$AIC, M2$AIC, M3$AIC, M4$AIC, M5$AIC)
AICCv <- c(M1$AICc, M2$AICc, M3$AICc, M4$AICc, M5$AICc)
BICv <- c(M1$BIC, M2$BIC, M3$BIC, M4$BIC, M5$BIC)
BestAIC <- order(AICv)[1:3] ## Models 5,1,3 are best (in order)
BestAICC <- order(AICCv)[1:3]
BestBIC <- order(BICv)[1:3]
names <- c("ARMA(0,1,12)", "ARMA(1,1,11)", "ARMA(11,1,0)", "ARMA(12,1,0)", "ARMA(10,1,3)")
crit<- data.frame(Model = names, AIC = AICv, AICc = AICCv, BIC = BICv)

table <- kable(crit, format = "html", caption = "<b style='color:black;'>Model Selection</b>") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "arial")  
tablet <- kable(M5$ttable, format = "html", caption = "<b style='color:black;'>ARMA(10,1,3)</b>") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), html_font = "arial") 
tablet2 <- kable(M1$ttable, format = "html", caption = "<b style='color:black;'>ARMA(0,1,12)</b>") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



 


 
