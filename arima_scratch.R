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

yonk = dat[which(dat$City=="Yonkers"),]


fit = auto.arima(y = waterbury$AverageTemperature)

forecastObj = forecast(fit, h = 15)

library(ggplot2)
p <- ggplot(aes(x=dt, y=AverageTemperature), data=waterbury)
p <- p + geom_line()
p + geom_forecast()


#from a blog, will use as inspiration, plots later to come.
funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}
