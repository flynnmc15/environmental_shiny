install.packages("forecast")
library(forecast)
dat = env_data[which(env_data$Country=="United States"),]
table(dat$City)
waterbury = dat[dat$City=="Waterbury",]
waterbury = selectByDate(
  waterbury,
  start = "1/1/2000",
  end = "31/12/2012"
)

env_data$Year <- format(as.Date(env_data$dt), "%Y")


yonk = dat[which(dat$City=="Yonkers"),]


fit = auto.arima(y = waterbury$AverageTemperature)

forecastObj = forecast(auto.arima(y = waterbury$AverageTemperature), h = 15)

library(ggplot2)
p <- ggplot(aes(x=dt, y=AverageTemperature), data=waterbury)
p <- p + geom_line()
p + geom_forecast()







########## Piece for forecast button! #############

# needs to be changed from Afghanistan to 
#if the user wants to forecast:

  df2 <- env_data %>%
    filter(Country == "Afghanistan" & year >= 2008)%>%
  group_by(dayTime) %>% 
  mutate(pointEst = mean(AverageTemperature)) %>% 
  select(dayTime, pointEst) %>% 
  distinct(pointEst) %>% 
    as.data.frame() %>% na.omit()
  
  # get forecast
  myForecast = env_data %>%
    filter(Country == "Afghanistan") %>% 
    group_by(dayTime) %>% 
    mutate(pointEst = mean(AverageTemperature)) %>% 
    select(pointEst) %>% 
    distinct(pointEst) %>% 
    ungroup() %>% 
    select(pointEst) %>% 
    auto.arima() %>%
    forecast(h = 5*12 )
  #can be changed to allow person to forecast as
  # far into the future as they want
  
  #turn into data frame
  forecastDF = fortify(myForecast, ts.connect = TRUE) %>% 
    select("Point Forecast", "Lo 95", "Hi 95") %>% na.omit()
  rownames(forecastDF) = NULL
  #need to get this to the point where it takes in the country
  lastMeasuredDate = df2 %>%
    select(dayTime) %>% as.vector()
  rownames(lastMeasuredDate) = NULL
  forecastDF$dayTime = ymd(seq(as.Date(max(lastMeasuredDate$dayTime)), 
                             by = "month", length.out = nrow(forecastDF)))
  names(forecastDF) = c("pointEst", "upperBound", "lowerBound", "dayTime")
  forecastDF$year = as.numeric(format(forecastDF$dayTime,'%Y'))
  
  df2$dayTime = as_date(df2$dayTime, tz = NULL)
  forecastDF$dayTime = as_date(forecastDF$dayTime, tz = NULL)

  combinedDF = forecastDF %>% select(dayTime, pointEst) %>% 
    bind_rows(df2)

  
  ggplot(data = NULL, aes(x = dayTime, y = pointEst)) +
    geom_line(data = df2) +
    geom_line(data = forecastDF)+
     #can add lowess in to the prediction model!!
    geom_errorbar(data = forecastDF, 
                  aes(x = dayTime, y = pointEst,ymin = lowerBound, ymax = upperBound), 
                  color = "light blue") +
      geom_smooth(data = combinedDF, se = FALSE, method = "loess", formula = y~x)+
    labs(x = "Date", y = "Temperature")+
         ggtitle( paste0("Monthly temperatures in ", "Afghanistan", " with predictions"))
  