library(tidyverse)
library(shiny)
library(rsconnect)
library(plotly)
library(viridis)


dataset <- fifa

# UI for shiny

ui <- fluidPage(
  
  #title of app and introduction
  
  titlePanel("Comparing Skill Set of Football Players"),
  helpText("This app allows the user to compare some statistics of Two Players at a time"),
  
  fluidRow(
    
    # Select Player 1
    column( width = 5,
            
            selectizeInput("player1", label = "Select Player 1 (Type Player Name)", choices = unique(fifa %>% group_by(Name) %>% select(Name))   
            )),
    
    
    # Select player 2
    column(width = 5, 
           selectizeInput("player2", label = "Select Player 2 (Type Player Name)",choices = unique(fifa %>% group_by(Name) %>% select(Name)) )  
    ) 
    
  )  ,
  
  # Create plot
  mainPanel(
    plotlyOutput("plot", height = "800px", width = "1200px")
  )
  
  
)

