# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape)
library(readr)
library(tidyverse)
library(viridis)
library(hrbrthemes)

library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library("DT")
library("sf")
library('dplyr')
library('markdown')
library('tidyr')
library('shinythemes')
library('plotly')
library("ggplot2")

df <- read_csv("survey.csv")
df$Count <- 1
df$year <- format(df$Timestamp,format= "%Y")
View(df)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Mental Health in Tech Survey")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  
  
  sidebarMenu(
    menuItem("Survey Mental Health", tabName = "dashboard", icon = icon("dashboard"))
    
  ),
  
  selectInput("treatment","Treatment",
              choice=sort(unique(df$treatment)),
              multiple=T),
  
  sliderInput("year",
              label = "Years",
              step = 1,
              value = c(2014,2015),
              min =  2014,
              max = 2016
  ),
  
  
  selectInput("state","State",
              choices=sort(unique(df$state)),
              multiple = T),
  selectInput("Gender","Gender",
              choices=sort(unique(df$Gender)),
              multiple = T),
  sliderInput("count",
              label = "Count",
              step = 1,
              value = c(1,80),
              min =  1,
              max = 80),
  
  selectInput("Country","Country",
              choices=sort(unique(df$Country)),
              multiple = T)
              
  
  
)

frow2 <- fluidRow(
  
  box(
    title = "Survey",width= "700px",
    plotlyOutput("Plot3",width = "700px",height = "200px"),
    plotOutput("plot1", height = "200px",width="700px"),
    plotlyOutput("plot",width = "700px",height = "200px"),
    plotlyOutput("plotusa",width="700px",height="300px"),
    plotlyOutput("Plot",width = "700px",height = "200px")
    
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')


# create the server functions for the dashboard  
server <- function(input, output) {
  
  output$Plot3 <- renderPlotly({
    df2 <- df[df$year <= input$year,]
    df2 = df2 %>% group_by(seek_help) 
    ggplot(df2, aes(x = seek_help,fill=seek_help)) +
      geom_bar(stat="count",position = position_dodge(width = .25)) +
      labs(title=paste0("Percentage of People seek for Help: ",input$year[2]),
           x = "Seek Help", y="Survey")
    
  })
  
  #creating the plotOutput content
  output$plot1 <- renderPlot({
    df$tech_company <- as.factor(df$tech_company)
    df$mental_health_consequence<- as.factor(df$mental_health_consequence)
    
    #df2 <- melt(df[, c("tech_company", "mental_health_consequence","Count")])
    #View(df2)
    
    df8 = df %>% filter(year == input$year) %>% group_by(mental_health_consequence,tech_company) %>% 
      summarise(value = sum(Count,na.rm=T)
      )
    df8$click <- paste(df8$mental_health_consequence,df8$value,
                       df8$tech_company) 
    
    ggplot(df8, aes(mental_health_consequence, y = value, fill = c(tech_company))) +
      geom_bar(stat = "identity",position = position_dodge(width = .25))+
      labs(title=paste0("Mental Health Survey of Tech Company: ",input$year[2]),
           x = "Menatl Health Consequences", y= "Survey")+coord_flip()
    
  })
  
  
  output$plot <- renderPlotly({
    df$treatment <- as.factor(df$treatment)
    df3 <- melt(df[, c("treatment","Count")])
    
    plot_ly(df3, labels = ~treatment, values = ~value, type = 'pie') 
      
    
  })
  
  output$plotusa <- renderPlotly({
    
    df4 <- df[df$year <= input$year,]
    
    
    df4 = df4  %>% group_by(year, state,Country, mental_health_interview,phys_health_interview) %>% 
      summarise(value = sum(Count,na.rm=T)
      )

    
    df4$click <- paste(df4$state,df4$Country,
                       df4$mental_health_interview,df4$phys_health_interview, df4$state,
                       df4$values,df4$year)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo()%>% add_trace(
      z = df4$values, 
      locations = ~df4$state,
      text = df4$click,
      type = 'choropleth',
      split= ~df4$state,
      locationmode = 'USA-states') %>% 
      layout(geo = g,title = paste0("Mental and Physical Health Interview of Employee in Country and State: ",input$year[2]))
  })  
  
  output$Plot <- renderPlotly({
    
    ggplot(df, aes(x = year,fill=input$treatment)) +
      geom_bar(stat="count")+ labs(title=paste0("Treatment Trend Every Year Yes/No/All: ",input$treatment),
                                   x = "Year", y = "Survey with respect to Treatment")
    
  })
  
}

shinyApp(ui, server)

