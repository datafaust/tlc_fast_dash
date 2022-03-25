#TLC FastDash---------------------------------------------------------------------------------------------------------------------
#author:fausto Lopez
#purpose: quick simple accessible dashboard for public metrics

#load required libraries
#libs = c('data.table','shiny','shinydashboard','scales','lubridate'
#         ,'DT','Hmisc', 'fasttime','zoo','ggplot2','plotly')
#lapply(libs, require, character.only = T)

library(data.table)
library(shiny)
library(shinydashboard)
library(scales)
library(DT)
library(Hmisc)
library(zoo)
library(plotly)

#source data-----------------------------------------------------------------------------------------------------------------------------

#yellow = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_yellow.csv")
#green = fread("http://www.nyc.gov/html/tlc/downloads/csv/data_reports_monthly_indicators_shl.csv")

industry_metrics = fread("https://www1.nyc.gov/assets/tlc/downloads/csv/data_reports_monthly.csv")

pal = c("black", "maroon", "blue", "orange", "dark green", "gold")
names(industry_metrics) = tolower(gsub(" ","_", names(industry_metrics)))
industry_metrics = setDT(data.frame(lapply(industry_metrics, function(x) {
                    gsub("%","",gsub(",", "", x),x)
                })))[
                  ,percent_of_trips_paid_with_credit_card:=as.numeric(
                    as.character(
                      percent_of_trips_paid_with_credit_card
                      )
                    )/100][
                      ,month_year:=`month.year`][
                        ,`month.year`:=NULL]

#pull columns
cols = c("trips_per_day", "farebox_per_day", "trips_per_day_shared"
         , "unique_drivers", "unique_vehicles", "vehicles_per_day"
         , "percent_of_trips_paid_with_credit_card")

#turn columns numeric lapply being finicky in shiny 
#observe({
 # industry_metrics[,(cols):= lapply(.SD, as.character), .SDcols = cols]
  #})
#observe({
 # industry_metrics[,(cols):= lapply(.SD, as.numeric), .SDcols = cols]
  #})

#going old school
industry_metrics$trips_per_day=as.numeric(as.character(industry_metrics$trips_per_day))
industry_metrics$farebox_per_day=as.numeric(as.character(industry_metrics$farebox_per_day))
industry_metrics$trips_per_day_shared=as.numeric(as.character(industry_metrics$trips_per_day_shared))
industry_metrics$unique_drivers=as.numeric(as.character(industry_metrics$unique_drivers))
industry_metrics$unique_vehicles=as.numeric(as.character(industry_metrics$unique_vehicles))
industry_metrics$vehicles_per_day=as.numeric(as.character(industry_metrics$vehicles_per_day))
industry_metrics$percent_of_trips_paid_with_credit_card=as.numeric(as.character(industry_metrics$percent_of_trips_paid_with_credit_card))
industry_metrics$avg_days_vehicles_on_road = as.numeric(as.character(industry_metrics$avg_days_vehicles_on_road))
industry_metrics$avg_hours_per_day_per_vehicle = as.numeric(as.character(industry_metrics$avg_hours_per_day_per_vehicle))
industry_metrics$avg_days_drivers_on_road   = as.numeric(as.character(industry_metrics$avg_days_drivers_on_road))        
industry_metrics$avg_hours_per_day_per_driver = as.numeric(as.character(industry_metrics$avg_hours_per_day_per_driver))          
industry_metrics$avg_minutes_per_trip = as.numeric(as.character(industry_metrics$avg_minutes_per_trip))


industry_metrics[,month_year1:=as.yearmon(month_year)][
  ,days:= monthDays(as.Date(month_year1))][
    ,trips_per_month:= trips_per_day*days][
      ,month_date:= as.Date(paste(month_year,"-28",sep=""))][
        ,farebox_per_month:= farebox_per_day * days][
          ,farebox_per_month:= farebox_per_month][
            ,week:=strftime(month_date, format="%W")][
              ,week:= as.factor(week)][
                ,trips_per_week:=trips_per_day * 7][
                  ,year:=format(as.yearmon(month_year), "%Y")][
                    ,farebox_per_week:= farebox_per_day * 7][
                      ,license_class:= as.factor(license_class)][
                        ,total_trips_per_day:=trips_per_day + trips_per_day_shared][
                          ,shared_trips_per_day_percent:=round(trips_per_day_shared/total_trips_per_day,2)]


#copy for compatibility with old code
industry_metrics = industry_metrics
industry_metrics$days = NULL

setorder(industry_metrics, license_class, -month_date)
industry_metrics = as.data.frame(industry_metrics)

#ui.R-----------------------------------------------------------------------------------------------------------------------------------------
ui = dashboardPage(skin = "yellow",
                   dashboardHeader(title = "TLC FastDash"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                       menuItem("Data Bank", tabName = "databank", icon =icon("fas fa-database")),
                       menuItem("Source code", icon = icon("file-code-o"), 
                                href = "https://gitlab.com/maverick_tlc/tlc_fast_dash.git")
                     )
                   ),
                   # Body content
                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "dashboard",
                              fluidRow(
                                 #Dynamic infoBoxes
                                 valueBoxOutput("yellowtripbox", width = 3),
                                 valueBoxOutput("greentripbox", width = 3),
                                 valueBoxOutput("hvtripbox", width = 3),
                                 valueBoxOutput("bctripbox", width = 3),
                                 valueBoxOutput("lxtripbox", width = 3),
                                 valueBoxOutput("lvtripbox", width = 3),
                                 valueBoxOutput("hvsharing", width = 6),
                                 #box(textOutput("textbox2"), width = 6),
                                 #box(textOutput("textbox3")),
                                 box(textOutput("textbox4"), width = 6)
                               ),
                               fluidRow(
                                 box(background = "green", dateRangeInput("monthdate", label = h3("Choose a Date Range"),
                                                                          start = '2014-01-01',
                                                                          end = as.Date(Sys.time()))),
                                 box(background="green", selectInput(inputId = "dimension", label = strong("Choose Metric"),
                                                                     choices = c('Trips, Drivers & Vehicles'='1', 'Time & Money' = '2'), 
                                                                     multiple = FALSE, selectize = TRUE)),
                                 box(textOutput("textbox"))
                               ),
                               
                               fluidRow(
                                 box(plotlyOutput(outputId = 'trips_per_day'), width = 6),
                                 box(plotlyOutput(outputId = 'trips_year'), width = 6)),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'trips_per_month')),
                                 box(plotlyOutput(outputId = 'medallions_per_month'))
                               ),
                               #variable switch box
                               fluidRow(
                                 sidebarLayout(
                                   sidebarPanel(
                                     dateRangeInput("monthlydate", label = h3("Date Range"),start = '2016-01-01',
                                                    end = as.Date(Sys.time())),
                                     selectInput("element_id1_m", "Select Your Variable for x-axis", c("month_date", 
                                                                                                       "week",
                                                                                                       "year"), selected = "month_date"),
                                     selectInput("element_id2_m", "Select Your Variable for y-axis", c("trips_per_day",
                                                                                                       "trips_per_day_shared",
                                                                                                       "farebox_per_day",
                                                                                                       "unique_drivers",
                                                                                                       "unique_vehicles",
                                                                                                       "avg_minutes_per_trip",
                                                                                                       "avg_days_vehicles_on_road", 
                                                                                                       "avg_hours_per_day_per_vehicle",
                                                                                                       "avg_days_drivers_on_road",
                                                                                                       "avg_hours_per_day_per_driver", 
                                                                                                       "percent_of_trips_paid_with_credit_card"), selected = "trips"),
                                     selectInput("element_id3_m", "Select Your Grouping Variable", c("license_class"), selected = 'license_class')),
                                   mainPanel(h3("Outputs"),
                                             textOutput("id1_m"),
                                             textOutput("id2_m"),
                                             textOutput("id3_m"),
                                             plotlyOutput("plt_m"),
                                             plotlyOutput("plt2_m"))
                                 ))
                       ),
                       tabItem(tabName = "databank", 
                               (fluidPage(
                                 title = 'Table Access',
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     conditionalPanel(
                                       'input.dataset === "industry_metrics"',
                                       checkboxGroupInput('show_vars', 'Columns in Data Set to show:',
                                                          names(industry_metrics), selected = names(industry_metrics))
                                     ),
                                     downloadButton('downloadData1', 'Download Data Set')
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       id = 'dataset',
                                       tabPanel('industry_metrics', DT::dataTableOutput('mytable1'))
                                     )
                                   )
                                 )
                               )
                               )))
                   )
)



#server.R  -------------------------------------------------------------------------------------------------

server = function(input, output) {
  
  #trips per day----
  output$trips_per_day = renderPlotly({
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    td = subset(industry_metrics, 
                  (month_date >= start_date1 & 
                     month_date <= end_date1), c('trips_per_day', 'month_date', 'license_class'))
  
    trips = plot_ly(td, x = ~month_date
                     , y = ~trips_per_day
                     ,type = 'scatter'
                     , split = ~license_class
                     , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)   
     
     trips = layout(trips,             
                     title = "Average Trips per Day each Month", 
                     xaxis = list(           
                       title = "Month & Year",   
                       showgrid = F        
                     ),
                     yaxis = list(           
                       title = "Trips Per Day"      
                     ))
    
    #farebox per day----
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      
      td = subset(industry_metrics,
                    (month_date >= start_date1 & month_date <= end_date1), 
                  c('farebox_per_day', 'month_date', 'license_class'))
      
      trips = plot_ly(td, x = ~month_date, y = ~farebox_per_day
                      , type = 'scatter'
                      , split = ~license_class
                      , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      
      trips = layout(trips,              
                      title = "Average Farebox Per Day each Month", 
                      xaxis = list(          
                        title = "Month & Year",    
                        showgrid = F       
                      ),
                      yaxis = list(           
                        title = "Farebox Per Day"     
                      ))
    }
    trips
  })

  #trips per year----
  output$trips_year = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]

    td = subset(industry_metrics, 
                  (month_date >= start_date1 & month_date <= end_date1), 
                c('trips_per_month','trips_per_day','month_date','license_class', 'year','trips_per_week'))
    
    
    nd = aggregate(trips_per_day ~ year + license_class, data = td, FUN = sum)

    uniks = plot_ly(nd
                    , x = ~year
                    , y = ~trips_per_day
                    , split = ~license_class
                    , type = 'bar'
                    ,color = ~license_class
                    ,colors = pal)
    uniks = layout(uniks,             
                    title = "*Average Trips Per Year", 
                    xaxis = list(           
                      title = "Month & Year",    
                      showgrid = F        
                    ),
                    yaxis = list(          
                      title = "Trips"     
                    ))
    
    #farebox per year------------------------------------------
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_day','month_date','license_class', 'year','trips_per_week'))
      
      nd = aggregate(farebox_per_day ~ year + license_class, data = td, FUN = sum)

      uniks = plot_ly(nd
                      , x = ~year
                      , y = ~farebox_per_day
                      , split = ~license_class
                      , type = 'bar'
                      ,color = ~license_class
                      ,colors = pal)
      uniks = layout(uniks,              
                     title = "*Average Farebox Per Year", 
                     xaxis = list(           
                       title = "Month & Year",     
                       showgrid = F        
                     ),
                     yaxis = list(           
                       title = "Farebox"      
                     ))
    }
    uniks
  })
  
  #trips per month------------------------------------------
  output$trips_per_month = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]

    td =  subset(industry_metrics,
                   (month_date >= start_date1 & month_date <= end_date1), 
                 c('trips_per_month','month_date','license_class', 'year'))
    uniks = plot_ly(td, 
                     x = ~month_date, y = ~trips_per_month
                     , type = 'scatter'
                     , split = ~license_class
                     , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)
    
    uniks = layout(uniks,             
                    title = "*Trips Per Month Over Time", 
                    xaxis = list(          
                      title = "Month & Year",     
                      showgrid = F      
                    ),
                    yaxis = list(           
                      title = "Trips Per Month"      
                    ))
    uniks                
    
    
    #farebox per month----- 
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('farebox_per_month','month_date','license_class', 'year'))
      
      uniks = plot_ly(td, 
                       x = ~month_date, y = ~farebox_per_month
                       , type = 'scatter'
                       , split = ~license_class
                       , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      
      uniks = layout(uniks,              
                      title = "*Farebox Per Month Over Time", 
                      xaxis = list(           
                        title = "Month & Year",     
                        showgrid = F        
                      ),
                      yaxis = list(           
                        title = "Farebox Per Month"      
                      ))
    }
    uniks     
  })
  
  #vehicles per month----
  output$medallions_per_month = renderPlotly({
    
    start_date1 = input$monthdate[1]
    end_date1 = input$monthdate[2]
    td =  subset(industry_metrics, 
                   (month_date >= start_date1 & month_date <= end_date1), 
                 c('unique_vehicles','month_date','license_class'))
    uniks = plot_ly(td, 
                     x = ~month_date, y = ~unique_vehicles
                     , type = 'scatter'
                     , split = ~license_class
                     , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal)
    uniks = layout(uniks,              
                    title = "Unique Vehicles Per Month Over Time", 
                    xaxis = list(          
                      title = "Month & Year",    
                      showgrid = F        
                    ),
                    yaxis = list(           
                      title = "Unique Vehicles"      
                    ))
    uniks                
    
    
    #vehicles_per_day ----
    if (input$dimension == '2') {
      start_date1 = input$monthdate[1]
      end_date1 = input$monthdate[2]
      td =  subset(industry_metrics, 
                     (month_date >= start_date1 & month_date <= end_date1), 
                   c('vehicles_per_day','month_date','license_class'))
      uniks = plot_ly(td, 
                       x = ~month_date, y = ~vehicles_per_day
                       , type = 'scatter'
                       , split = ~license_class
                       , mode = 'lines'
                      ,color = ~license_class
                      ,colors = pal)
      uniks = layout(uniks,              
                      title = "Vehicles Per Day Over Time", 
                      xaxis = list(           
                        title = "Month & Year",    
                        showgrid = F       
                      ),
                      yaxis = list(           
                        title = "Vehicless Per Day Per Month"      
                      ))
    }
    uniks     
  })
  
  #custom tool----
  output$id1_m = renderText({
    sprintf("You have selected %s on the x-axis", input$element_id1_m)
  })
  output$id2_m = renderText({
    sprintf("You have selected %s on the y-axis", input$element_id2_m)
  })
  output$id3_m = renderText({
    sprintf("You have selected %s as your spliting variable", input$element_id3_m)
  })
  
  
  output$plt_m = renderPlotly({
    start_date = input$monthlydate[1]
    end_date = input$monthlydate[2]

    td =  subset(industry_metrics, 
                   (month_date >= start_date & 
                      month_date <= end_date))
    
    print(td)
    boots = plot_ly(x = td[,input$element_id1_m], y = td[,input$element_id2_m],# type = "bar", 
                    data = td, split = td[,input$element_id3_m]
                    , type = 'scatter'
                    , mode = 'lines'
                    ,color = ~license_class
                    ,colors = pal) 
    boots = layout(boots,             
                   title = "Monthly Industry Trends Over Time", 
                   xaxis = list(           
                     title = input$element_id1_m,     
                     showgrid = F    
                   ),
                   yaxis = list(          
                     title = input$element_id2_m      
                   ))
    
  })
  
  #data banks----
  output$mytable1 = DT::renderDataTable({
    DT::datatable(industry_metrics[, input$show_vars, drop = FALSE])
    
  })
  
  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('monthly_indicators', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(industry_metrics[, input$show_vars, drop = FALSE], con)
    }
  )
  
  output$mytable2 = DT::renderDataTable({
    DT::datatable(education, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('industry_metrics', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(industry_metrics, con)
    }
  )
  
  
  #value boxes-------------------------------------------------------------------------------------------
  output$yellowtripbox = renderValueBox({
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "*Average Yellow trips per day as of 'SAMPLE'"
    med_trips = round(mean(industry_metrics[industry_metrics$license_class=="Yellow", "trips_per_day"][1:3]))
    valueBox(
      paste0(med_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "yellow")
  }) 
  output$greentripbox = renderValueBox({
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "*Average Green trips per day as of 'SAMPLE'"
    shl_trips = round(mean(industry_metrics[industry_metrics$license_class=="Green", "trips_per_day"][1:3]))
    valueBox(
      paste0(shl_trips), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "green")
  }) 
  output$hvtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - High Volume trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - High Volume', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "maroon")
  })
  output$bctripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Black Car trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Black Car', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "purple")
  })
  output$lxtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Lux Limo trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Lux Limo', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "orange")
  })
  output$lvtripbox = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "Average FHV - Livery trips per day as of 'SAMPLE'"
    ubers_etc = round(mean(industry_metrics[industry_metrics$license_class == 'FHV - Livery', "trips_per_day"][1:3])) #simple[3,2]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "aqua")
  })
  output$hvsharing = renderValueBox({ 
    recent_date = as.character(industry_metrics[1, "month_date"])
    my_query = "% of FHV - High Volume trips per day are shared as of 'SAMPLE'"
    shared = paste(round(mean(industry_metrics[industry_metrics$license_class == 'FHV - High Volume', "shared_trips_per_day_percent"][1:3]),2) * 100, "%") #simple[3,2]
    valueBox(
      paste0(shared), sub("SAMPLE",recent_date,my_query), icon = icon("fas fa-taxi"),
      color = "red")
  })
  #choose columns to display----
  output$mytable = renderDataTable({
    industry_metrics
  })
  
  output$textbox = renderText({
    print("*Note that the * next to graphs designates these aggregations are based on daily averages going back and not on summations over selected periods")
  })
  
  #output$textbox2 = renderText({
  #  print("*Input a date range to see changes over time")
  #})
  
  output$textbox4 = renderText({
    print("*Averages above are 3 month rolling")
  })
  
  #output$textbox3 = renderText({
   # print("*Use the dropdown menu to select different metrics")
  #})
}


#Execute APP------------
shinyApp(ui, server)

