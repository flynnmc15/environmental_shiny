library(shiny)
library(tidyverse)
library(tidyr)
library(forecast)
library(maps)
library(lubridate)
library(ggfortify)

# setwd("/Users/flynnmc/Desktop/environmental_shiny")
# env_data <- read.csv("GlobalLandTemperaturesByCity.csv")
# setwd("/Users/flynnmc/Desktop/environmental_shiny")
env_data <- read.csv("~/GlobalLandTemperaturesByCity.csv") %>% 
  select(-X) %>% 
  na.omit()

#make a date time object
foo = env_data %>% select( year, month, day) %>%
  mutate(dayTime = make_datetime(year = year, month = month, day = day))
env_data$dayTime = foo$dayTime


#### IDEAS TO POSSIBLY IMPLEMENT 12/1/22
# 1. geom_forecast is not working. want to be able to pipe time series data in
#     possible solution: create a forecast tab.

#Getting a vector of the country names to add into variable_country selectinput
country_list <- env_data %>% 
  distinct(Country) %>% 
  arrange(Country) %>% 
  pull(Country)

#Getting a list of cities and their countries so the city selectinput can automatically update based off the country the user picks
city_list <- env_data %>% 
  select(Country, City) %>% 
  distinct(City, .keep_all = TRUE) %>% 
  arrange(Country, City)

#formatting column of months and years in env_data
# env_data$year <- as.numeric(format(as.Date(env_data$dt), "%Y"))
# env_data$month = as.numeric(format(as.Date(env_data$dt), "%m"))

#Defining month_year used later in the UI
month_year <- c("Months", "Years")

#Defining pdf_png used later in the UI
pdf_png <- c(".pdf", ".png")

###############################################################UI###############################################################
ui <- fluidPage(
    br(),
    tabPanel(
      "Global Temperature Visualization",
    sidebarPanel(width = 3,
                 br(),
                 h3("Menu Selection"), 
                 selectInput(
                   inputId = "variable_country",   #Loading in country list to select input, allows you to choose country
                   label = "Choose Country",
                   choices = country_list
                 ),
                 conditionalPanel(
                   condition = "input.tab1 == 'By City'",    #Loading in city list to select input, allows you to choose city, contains conditional panel, choices defined in server
                 selectInput(
                   inputId = "variable_city",
                   label = "Choose City",
                   choices = NULL
                 )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By City' & input.month_year == 'Months'",    #UI output for a numeric input to specify the year, contains conditional panel
                   uiOutput(
                     outputId = "select_year"
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By Country'",  #Adding a lowess line option, contains conditional panel, defaults to false
                   checkboxInput(
                     inputId = "lowess_line", 
                     label = "Add Lowess Line?", 
                     value = FALSE, 
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By Country'",  #Adding a lowess line option, contains conditional panel, defaults to false
                   checkboxInput(
                     inputId = "forecast_choice", 
                     label = "Forecast?", 
                     value = FALSE, 
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By Country'",   #Adds an option for forecasting or not.
                 checkboxInput(
                   inputId = "city_color", 
                   label = "Stratify by City?", 
                   value = FALSE, 
                 )),
                 conditionalPanel(
                   condition = "input.city_color == 1 & input.tab1 == 'By Country'",    #Adding a group checkbox so you can turn cities on and off in the country plot, choices defined in server, 
                                                                                        # contains conditional panel
                   checkboxGroupInput(
                     inputId = "city_checkbox",
                     label = "Cities:",
                     choices = NULL
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By City'",    #Asks the user if they want the city temp to be displayed over months or years
                   radioButtons(
                     inputId = "month_year",
                     label = "Display in Months or Years?",
                     choices = month_year
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By Country'",    #UI output for a slider to select a year range for countries, contains conditional panel
                 uiOutput(
                   "year_slider"
                  )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By City' & input.month_year == 'Years'",    #UI output for a slider to select a year range for city years, contains conditional panel
                   uiOutput(
                     "year_slider_2"
                   )),
                conditionalPanel(
                  condition = "input.tab1 == 'By City' & input.month_year == 'Months'",        #UI output for a slider to select a month range, contains conditional panel
                 uiOutput(
                   "month_slider"
                  )),
                radioButtons(
                  inputId = "download_type",      #Allows the user to choose what type of download they want
                  label = "Type of Download?",
                  choices = pdf_png
                ),
                downloadButton(
                  outputId = 'download_plot_1',     #Download button allows user to click button to download last loaded plot
                  label = 'Download Plot'
                ),
    ),
    
    mainPanel(width = 9,
                tabsetPanel(id = "tab1", type = "tabs",
                            tabPanel("By Country", br(), plotOutput("country_plot", height = "600px")),  #Country plot tab
                            tabPanel("By City", br(), plotOutput("city_plot", height = "600px"))),   #City plot tab
              
    ),
  ))






server <- function(input, output, session) {
###############################################################Filling in select inputs###############################################################
  #Selecting a city based off a country, for the city plot
  observeEvent(input$variable_country,
               {
                 updateSelectInput(session, input = "variable_city",
                                   choices = sort(city_list[city_list$Country %in% input$variable_country, "City", drop = TRUE]))
               })
  
  #Creating a reactive to randomly choose cities when cities is greater than 30
  sample_sort <- reactive({
    pre_sort <- sort(city_list[city_list$Country %in% input$variable_country, "City", drop = TRUE])
    
    if(length(pre_sort) > 30) {
      sample(pre_sort, size = 30)
    } else {
      pre_sort
    }
  })
  
  #Filling the check box of cities if you stratify by city, random sample of 30 cities if number of cities exceeds 30, for the country plot
  observeEvent(input$variable_country,
               {
                 updateCheckboxGroupInput(session, input = "city_checkbox", choices = sample_sort(), 
                                          selected = sample_sort())
               })
  
  
###############################################################Outputs for the UI###############################################################
  #Year slider
  output$year_slider <- renderUI({
    
    sliderInput(
      inputId = "year_slider",
      label = "Year Range",
      min = 1900,
      max = 2012,
      value = c(1900, 2012),
      step = NULL,
      ticks = FALSE,
      dragRange = TRUE,
      round = TRUE,
      sep = ""
    )
    
  })
  
  #Year slider for city plot by year
  output$year_slider_2 <- renderUI({
    
    sliderInput(
      inputId = "year_slider_2",
      label = "Year Range",
      min = 1900,
      max = 2012,
      value = c(1900, 2012),
      step = NULL,
      ticks = FALSE,
      dragRange = TRUE,
      round = TRUE,
      sep = ""
    )
    
  })
  
  #Month slider
  output$month_slider <- renderUI({
    
    sliderInput(
      inputId = "month_slider",
      label = "Month Range",
      min = 1,
      max = 12,
      value = c(1, 12),
      step = 2,
      ticks = TRUE,
      dragRange = TRUE,
      round = TRUE
    )
    
  })
  
  #Input a year for cities graph
  output$select_year <- renderUI({
    
    numericInput(
      inputId = "select_year",
      label = "Select Year Between 1900 and 2012",
      min = 1900,
      value = 1998,
      max = 2012
    )
    
  })
  
  
  
  ###############################################################Country Plots###############################################################
  #Changing the y-axis intervals based on how many cities you stratify by
  yaxis_intervals <- reactive({
    pre_sort <- sort(city_list[city_list$Country %in% input$variable_country, "City", drop = TRUE])
  
    if(length(pre_sort) > 15) {
      return(1)
    } else {
      return(0.5)
    }
  })
  
  #Country plot with ifelse logic based on whether or not the user wants to stratify by city and lowess line
  output$country_plot <- renderPlot({
    req(input$year_slider)

    #Getting the max and min year selected by user
    max_year <- max(input$year_slider)
    min_year <- min(input$year_slider)
    
    #Getting the max and min temp of country selected, also changes based on year selected
    country_temp <- env_data %>% 
      filter(Country == input$variable_country & year >= min_year & year <= max_year) %>% 
      group_by(year) %>% 
      summarise(avgtemp = mean(AverageTemperature))
    
    max_temp <- round(max(country_temp$avgtemp, na.rm = TRUE), 0) + 0.5
    min_temp <- round(min(country_temp$avgtemp, na.rm = TRUE), 0) - 0.5
    
    

    
    #Getting the max and min temp of country selected stratified by cities, also changes based on year selected
    country_temp <- env_data %>% 
      filter(Country == input$variable_country & year >= min_year & year <= max_year & City %in% sample_sort()) %>% 
    
      group_by(year, City) %>% 
      summarise(avgtemp = mean(AverageTemperature))
    
    max_temp_strat <- round(max(country_temp$avgtemp, na.rm = TRUE), 0) + 0.5
    min_temp_strat <- round(min(country_temp$avgtemp, na.rm = TRUE), 0) - 0.5
    
    
    
    #Actual country plots with ifelse statements
    if(input$city_color == 1 & input$lowess_line == 1) { #Stratify by cities and add lowess
      env_data %>% 
        filter(Country == input$variable_country & City %in% input$city_checkbox) %>% 
        group_by(year, City) %>% 
        summarise(avgtemp = mean(AverageTemperature)) %>% 
        ggplot(aes(year, avgtemp, color = City)) +
        geom_line() +
        #geom_forecast(forecast(AverageTemperature, h = 5)) + #how do I put in the average temperature???
        theme_bw() +
        scale_y_continuous(limits = c(min_temp_strat, max_temp_strat), breaks = seq(min_temp_strat, max_temp_strat, by = yaxis_intervals())) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 10)) +
        labs(x = "Year", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_country, " Cities"), subtitle = paste0("From ", min(input$year_slider), "-", max(input$year_slider))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 16), legend.title = element_text(size = 16), axis.text = element_text(size = 14)) +
        geom_smooth(se = FALSE)
    } else if(input$city_color == 1 & input$lowess_line == 0) { #Stratify by cities, no lowess
      env_data %>% 
        filter(Country == input$variable_country & City %in% input$city_checkbox) %>% 
        group_by(year, City) %>% 
        summarise(avgtemp = mean(AverageTemperature)) %>% 
        ggplot(aes(year, avgtemp, color = City)) +
        geom_line()+
        geom_forecast() +
        theme_bw() +
        scale_y_continuous(limits = c(min_temp_strat, max_temp_strat), breaks = seq(min_temp_strat, max_temp_strat, by = yaxis_intervals())) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 10)) +
        labs(x = "Year", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_country, " Cities"), subtitle = paste0("From ", min(input$year_slider), "-", max(input$year_slider))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), legend.text = element_text(size = 16), legend.title = element_text(size = 16), axis.text = element_text(size = 14))
      

    } else if(input$city_color == 0 & input$lowess_line == 1) { #No stratify by cities, add lowess
      env_data %>% 
        filter(Country == input$variable_country) %>% 
        group_by(year) %>% 
        summarise(avgtemp = mean(AverageTemperature)) %>% 
        ggplot(aes(year, avgtemp)) +
        geom_line()+
        geom_forecast(h=10) +
        theme_bw() +
        scale_y_continuous(limits = c(min_temp, max_temp), breaks = seq(min_temp, max_temp, by = 0.5)) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 10)) +
        labs(x = "Year", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_country), subtitle = paste0("From ", min(input$year_slider), "-", max(input$year_slider))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14)) +
        geom_smooth(se = FALSE)
      
    
    } else { #No stratify by cities, no lowess
      if(input$forecast_choice == 1){ #if the user wants to forecast:
        df2 <- env_data %>%
          filter(Country == "Afghanistan")%>%
          group_by(year) %>% 
          summarise(mean_temp=mean(AverageTemperature),
                    .groups = 'drop') %>% 
          as.data.frame() %>% na.omit()
       
        #get forecast
        myForecast = env_data %>%
          filter(Country == "Afghanistan") %>% 
          select(AverageTemperature) %>% 
          auto.arima() %>%
          forecast(h = 6*12)
        #can be changed to allow person to forecast as
        # far into the future as they want
        
        #turn into data frame
        forecastDF = fortify(myForecast, ts.connect = TRUE) %>% 
          select("Point Forecast", "Lo 95", "Hi 95") %>% na.omit()
        rownames(forecastDF) = NULL
        #need to get this to the point where it takes in the country
        lastMeasuredDate = env_data %>%
          filter(Country == "Afghanistan") %>% 
          select(dayTime) %>% as.vector()
        rownames(lastMeasuredDate) = NULL
        forecastDF$timePoint = seq(as.Date(max(lastMeasuredDate$dayTime)), 
            by = "month", length.out = nrow(forecastDF))
        names(forecastDF) = c("pointEst", "upperBound", "lowerBound", "timePoint")
        forecastDF$year = as.numeric(format(forecastDF$timePoint,'%Y'))
        
        foo = forecastDF %>% group_by(year) %>% 
          summarise(mean_temp=mean(pointEst),
                    .groups = 'drop') %>% 
          as.data.frame() %>% na.omit()

        ggplot() +               
          geom_line(data = df2, aes(x = year, y = mean_temp), 
                     color = "black")+
          geom_line(data = foo, aes(x = year, y = mean_temp), 
                    color = "red")+
          labs(x = "X-Data", y = "Y-Data")+
          ggtitle("Combined Plot")
      }else{
      env_data %>% 
        filter(Country == input$variable_country) %>% 
        group_by(year) %>% 
        summarise(avgtemp = mean(AverageTemperature)) %>% 
        ggplot(aes(year, avgtemp)) +
        geom_line()+
        geom_forecast() +
        theme_bw() +
        scale_y_continuous(limits = c(min_temp, max_temp), breaks = seq(min_temp, max_temp, by = 0.5)) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 10)) +
        labs(x = "Year", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_country), subtitle = paste0("From ", min(input$year_slider), "-", max(input$year_slider))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14))
    }
    }
  })
  
  
  ###############################################################City Plots###############################################################
  output$city_plot <- renderPlot({
    req(input$month_slider)
    req(input$year_slider_2)
    
    #Getting the max and min month selected by user
    max_month <- max(input$month_slider)
    min_month <- min(input$month_slider)
    
    #Getting the monthly max and min temp of city selected, also changes based on month selected
    city_temp <- env_data %>% 
      filter(City == input$variable_city & month >= min_month & month <= max_month & year == input$select_year)
    
    max_temp_month <- round(max(city_temp$AverageTemperature, na.rm = TRUE), 0) + 0.5
    min_temp_month <- round(min(city_temp$AverageTemperature, na.rm = TRUE), 0) - 0.5
    
    
    
    #Getting the max and min month selected by user
    max_year <- max(input$year_slider_2)
    min_year <- min(input$year_slider_2)
    
    #Getting the yearly max and min temp of city selected, also changes based on year selected
    city_temp_year <- env_data %>% 
      filter(City == input$variable_city & year >= min_year & year <= max_year) %>% 
      group_by(year) %>% 
      summarise(avgtemp = mean(AverageTemperature))
    
    max_temp_year <- round(max(city_temp_year$avgtemp, na.rm = TRUE), 0) + 0.5
    min_temp_year <- round(min(city_temp_year$avgtemp, na.rm = TRUE), 0) - 0.5
    
    
    #Actual city plots with ifelse statements
    if(input$month_year == "Months") { #User selected for cities to be displayed in months
    env_data %>% 
      filter(City == input$variable_city & year == input$select_year) %>% 
      ggplot(aes(month, AverageTemperature)) +
      geom_line()+
      # autoplot() + geom_forecast(h = 5)+
      scale_x_continuous(limits = c(min_month, max_month), breaks = seq(min_month, max_month, by = 1)) +
      scale_y_continuous(limits = c(min_temp_month, max_temp_month), breaks = seq(min_temp_month, max_temp_month, by = 1)) +
      theme_bw() +
      labs(x = "Month", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_city, " in ", input$select_year), subtitle = "On the First of Every Month") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
            axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14))
      
    } else { #User selected for cities to be displayed in years
      env_data %>% 
        filter(City == input$variable_city) %>% 
        group_by(year) %>% 
        summarise(avgtemp = mean(AverageTemperature)) %>% 
        ggplot(aes(year, avgtemp)) +
        geom_forecast() +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 10)) +
        scale_y_continuous(limits = c(min_temp_year, max_temp_year), breaks = seq(min_temp_year, max_temp_year, by = 1)) +
        theme_bw() +
        labs(x = "Year", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_city), 
             subtitle = paste0("From ", min(input$year_slider), "-", max(input$year_slider))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14))
    }
  })
  
  
  ###############################################################Downloading Plots###############################################################
  
  #Used in downloadHandler filename
  download_type_1 <- reactive(
    if(input$download_type == ".pdf") {
      ".pdf"
    }
    else {
      ".png"
    }
  )
  
  #Used in downloadHandler content
  download_type_2 <- reactive(
    if(input$download_type == ".pdf") {
      "pdf"
    }
    else {
      "png"
    }
  )
  
  #Putting downloadHandler together with download button
  output$download_plot_1 <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), download_type_1(), sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = download_type_2(), dpi = 600, width = 10, height = 8, units = "in")
    }
  )
  
}

#Run the app
shinyApp(ui = ui, server = server)
