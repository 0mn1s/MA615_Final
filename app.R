library(readr)
library(tidyverse)
library(magrittr)
library(leaflet)
library(shiny)
library(tidyverse)
library(dplyr)

# data wrangling
df1<- read.csv("./stop_times/1.txt")
df2<- read.csv("./stop_times/2.txt")
df3<- read.csv("./stop_times/3.txt")
df4<- read.csv("./stop_times/4.txt")
df5<- read.csv("./stop_times/5.txt")
df6<- read.csv("./stop_times/6.txt")
df7<- read.csv("./stop_times/7.txt")
df8<- read.csv("./stop_times/8.txt")
df9<- read.csv("./stop_times/9.txt")
df10<- read.csv("./stop_times/10.txt")
df11<- read.csv("./stop_times/11.txt")
df12<- read.csv("./stop_times/12.txt")

stop_times <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

df1<- read.csv("./stops/1.txt")
df2<- read.csv("./stops/2.txt")
df3<- read.csv("./stops/3.txt")
df4<- read.csv("./stops/4.txt")
df5<- read.csv("./stops/5.txt")
df6<- read.csv("./stops/6.txt")
df7<- read.csv("./stops/7.txt")
df8<- read.csv("./stops/8.txt")
df9<- read.csv("./stops/9.txt")
df10<- read.csv("./stops/10.txt")
df11<- read.csv("./stops/11.txt")
df12<- read.csv("./stops/12.txt")


stops <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

df1<- read.csv("./trips/1.txt")
df2<- read.csv("./trips/2.txt")
df3<- read.csv("./trips/3.txt")
df4<- read.csv("./trips/4.txt")
df5<- read.csv("./trips/5.txt")
df6<- read.csv("./trips/6.txt")
df7<- read.csv("./trips/7.txt")
df8<- read.csv("./trips/8.txt")
df9<- read.csv("./trips/9.txt")
df10<- read.csv("./trips/10.txt")
df11<- read.csv("./trips/11.txt")
df12<- read.csv("./trips/12.txt")


trips <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)



stops<-stops%>%select(stop_name,stop_lat,stop_lon)
stops<-stops[!duplicated(stops[,c("stop_name")]),]
names(stops)[1]="checkpoint_name"
stops<-inner_join(stops,stop_time)%>%distinct()



lines<-c("Orange","Blue","Red","Green-B","Green-C","Green-D","Green-E")
trips<-trips%>%select(route_id,trip_id)
stop_trip<-inner_join(stops,trips)
raw<-combine_data%>%filter(route_id %in%lines)
df<-raw_data%>%select(arrival_time,
                        departure_time,
                        checkpoint_name,
                        Season,stop_lat,stop_lon,route_id)


Season <- unique(data$Season)
Transport <- unique(data$route_id)
Time <- unique(data$arrival_time)

ui <- fluidPage(
  titlePanel("MBTA Time Suggestion"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Transport", "Which transportation do you want to take?",Transport),
      br(),
      selectInput("Time", "When do you want to leave?",Time),
      br(),
      checkboxGroupInput("Season","Which season of are you traveling?",Season),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Table",tableOutput("table"))
      )
    )
  )
)

server <- function(input,output){
  newdf <- reactive({
    data %>% filter(Season%in%input$Season, route_id%in%input$Transport, arrival_time%in%input$Time)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = newdf()$stop_lat, lng = newdf()$stop_lon,
                 popup= newdf()$checkpoint_name)
  })
  output$table <- renderTable({newdf()})
}

shinyApp(ui = ui, server = server)