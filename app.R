library(shiny)
library(tidyverse)
library(tidyr)
library(forecast)
library(maps)
library(lubridate)
library(ggfortify)
library(data.table)

setwd("/Users/flynnmc/Desktop/environmental_shiny")

#A much faster way to read in the data
# env_data <- as.data.frame(fread("GlobalLandTemperaturesByCity.csv", showProgress = F)) %>%
#   select(-V1) %>%
#   na.omit()

#Make a date time object
foo = env_data %>% select(year, month, day) %>%
  mutate(dayTime = make_datetime(year = year, month = month, day = day))
env_data$dayTime = foo$dayTime


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

#Formatting column of months and years in env_data
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
                     label = "Add Lowess Line", 
                     value = FALSE, 
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'Forecast'",  #Adding a lowess line option for forecasting, contains conditional panel, defaults to false
                   checkboxInput(
                     inputId = "forecast_lowess", 
                     label = "Add Lowess Line", 
                     value = FALSE, 
                   )),
                 conditionalPanel(
                   condition = "input.tab1 == 'By Country'",   #Adds an option to stratify by city, contains conditional panel, defaults to falsee
                 checkboxInput(
                   inputId = "city_color", 
                   label = "Stratify by City", 
                   value = FALSE, 
                 )),
                 conditionalPanel(
                   condition = "input.city_color == 1 & input.tab1 == 'By Country'",    #Adding a group checkbox so you can turn cities on and off in the country plot, choices defined in server, contains conditional panel
                   checkboxGroupInput(
                     inputId = "city_checkbox",
                     label = "Cities:",
                     choices = NULL
                   )),
                 br(), #Adds a small break in between action button and radio button
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
                   condition = "input.tab1 == 'By City' & input.month_year == 'Months'",                #Allows user to select a different angle for city graph displaying months
                   sliderInput(
                     inputId = "axis_angle",
                     label = "Angle of Month Label",
                     min = 0,
                     max = 30,
                     value = 0,
                     round = TRUE,
                     step = 5,
                     ticks = TRUE,
                     width = "100%"
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
                            tabPanel("By City", br(), plotOutput("city_plot", height = "600px")),        #City plot tab
                            tabPanel("Forecast", br(), plotOutput("forecast_plot", height = "600px"))),      #Forecast tab
              
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
  })
  
  
  ###############################################################City Plots###############################################################
  output$city_plot <- renderPlot({
    req(input$year_slider_2)
    
    #Getting the monthly max and min temp of city selected, also changes based on month selected
    city_temp <- env_data %>% 
      filter(City == input$variable_city & year == input$select_year)
    
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
      mutate(month_cat = as.factor(case_when(month == 1 ~ "January", month == 2 ~ "February", month == 3 ~ "March", month == 4 ~ "April", month == 5 ~ "May", month == 6 ~ "June",
                                               month == 7 ~ "July", month == 8 ~ "August", month == 9 ~ "September", month == 10 ~ "October", month == 11 ~ "November", 
                                               month == 12 ~ "December"))) %>% 
      mutate(month_cat = fct_relevel(month_cat, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
      ggplot(aes(month_cat, AverageTemperature, group = 1)) +
      geom_line()+
      scale_y_continuous(limits = c(min_temp_month, max_temp_month), breaks = seq(min_temp_month, max_temp_month, by = 1)) +
      theme_bw() +
      labs(x = "Month", y = "Average Temperature (Celsius)", title = paste0("Average Temperature in ", input$variable_city, " in ", input$select_year), subtitle = "On the First of Every Month") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
            axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14), axis.text.x = element_text(angle = input$axis_angle, hjust = 0.5, vjust = 0.25))
      
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
  
  
  ###############################################################City Plots###############################################################
  output$forecast_plot <- renderPlot({
    
    #Pre-plot processing
    df2 <- env_data %>%
      filter(Country == input$variable_country & year >= 2008)%>%
      group_by(dayTime) %>% 
      mutate(pointEst = mean(AverageTemperature)) %>% 
      select(dayTime, pointEst) %>% 
      distinct(pointEst) %>% 
      as.data.frame() %>% 
      na.omit()
    
    #Get forecast
    myForecast = env_data %>%
      filter(Country ==  input$variable_country) %>% 
      group_by(dayTime) %>% 
      mutate(pointEst = mean(AverageTemperature)) %>% 
      select(pointEst) %>% 
      distinct(pointEst) %>% 
      ungroup() %>% 
      select(pointEst) %>% 
      auto.arima() %>%
      forecast(h = 5*12 )
    #Can be changed to allow person to forecast as far into the future as they want
    
    #Turn into data frame
    forecastDF = fortify(myForecast, ts.connect = TRUE) %>% 
      select("Point Forecast", "Lo 95", "Hi 95") %>% na.omit()
    rownames(forecastDF) = NULL
    
    #Need to get this to the point where it takes in the country
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
    
    combinedDF_2 <- combinedDF %>% 
      separate(dayTime, c("year_label", "month_label", "day_label"), "-")
  
    
    if(input$forecast_lowess == 1) { #Forecast plot with lowess line
      
      ggplot(data = NULL, aes(x = dayTime, y = pointEst)) +
        geom_line(data = df2) +
        geom_line(data = forecastDF)+
        geom_errorbar(data = forecastDF, 
                      aes(x = dayTime, y = pointEst,ymin = lowerBound, ymax = upperBound), 
                      color = "light blue") +
        theme_bw() +
        geom_smooth(data = combinedDF, se = FALSE, method = "loess", formula = y~x)+
        labs(x = "Date", y = "Temperature (Celsius)", title = paste0("Monthly Temperatures in ", input$variable_country, " With Predictions"), 
             subtitle = paste0("From ", min(combinedDF_2$year_label), "-", max(combinedDF_2$year_label))) +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 16), 
              axis.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 14))
      
    } else { #Forecast plot without lowess line
      
      ggplot(data = NULL, aes(x = dayTime, y = pointEst)) +
        geom_line(data = df2) +
        geom_line(data = forecastDF)+
        geom_errorbar(data = forecastDF, 
                      aes(x = dayTime, y = pointEst,ymin = lowerBound, ymax = upperBound), 
                      color = "light blue") +
        theme_bw() +
        labs(x = "Date", y = "Temperature (Celsius)", title = paste0("Monthly Temperatures in ", input$variable_country, " With Predictions"), 
             subtitle = paste0("From ", min(combinedDF_2$year_label), "-", max(combinedDF_2$year_label))) +
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
