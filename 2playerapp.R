library(tidyverse)
library(shiny)
library(rsconnect)
library(plotly)
library(viridis)

# Read FIFA data into R

fifa <- read_csv("data.csv")

# Group teams into various leagues

fifa <- fifa %>% 
  mutate(League = case_when(
    Club %in% c("Arsenal","Bournemouth", "Brighton", "Burnley", 
                "Cardiff City", "Chelsea", "Crystal Palace","Everton",
                "Fulham","Huddersfield","Leicester City","Liverpool",
                "Manchester City","Manchester United","Newcastle United",
                "Southampton","Tottenham Hotspur", "Watford","West Ham United","Wolverhampton Wanderers")~ "EPL",
    
    Club %in% c("Amiens SC", "Angers SCO", "AS Monaco","AS Saint-Etienne", 
                "Dijon FCO","En Avant de Guingamp","FC Nantes"
                ,"LOSC Lille","Montpellier HSC","Nîmes Olympique",
                "OGC Nice","Olympique Lyonnais","Olympique Marseille","Paris Saint-Germain", "RC Strasbourg Alsace","Stade Malherbe Caen"
                ,"Stade de Reims","Stade Rennais FC","Toulouse FC")~"Ligue 1",
    
    Club %in% c("1. FC Nürnberg","1. FSV Mainz 05","Borussia Dortmund","Fortuna Düsseldorf",
                "FC Augsburg","FC Bayern München","Eintracht Frankfurt","Hannover 96",
                "Hertha BSC","Bayer 04 Leverkusen","Borussia Mönchengladbach","RB Leipzig",
                "SC Freiburg","FC Schalke 04","TSG 1899 Hoffenheim","VfB Stuttgart",
                "VfL Wolfsburg","Werder Bremen") ~ "Bundesliga",
    
    Club %in% c("Atalanta","Bologna","Cagliari","Chievo Verona","Empoli","Fiorentina",
                "Frosinone","Genoa","Inter","Juventus","Lazio","Milan","Napoli",
                "Parma","Roma","Sampdoria","Sassuolo","SPAL","Torino","Udinese")~"Serie A",
    
    Club %in% c("Athletic Club","Atlético Madrid","CD Leganés","Deportivo Alavés","FC Barcelona",
                "Getafe CF","Girona FC","Levante UD","Real Valladolid CF","Rayo Vallecano",
                "RC Celta","RCD Espanyol","Real Betis","Real Madrid","Real Sociedad","SD Eibar",
                "SD Huesca","Sevilla FC","Valencia CF","Villarreal CF")~"La Liga"
  ))


# Group players into positions
fifa <- fifa %>% 
  mutate("General Position" = case_when(
    Position %in% c("GK")~ "Goalkeeper",
    Position %in% c("CB","LB","LCB","LWD","RB","RCB","RWB")~"Defender",
    Position %in% c("CDM","CM","LAM","LCM","LDM","LM","RAM","RCM","RDM","RM", "CAM")~"Midfielder",
    Position %in% c("CF","LF","LS","LW","RF","RS","RW","ST")~ "Forward",
    Position %in% c("NA")~ "Unknown"
  ))

# Select variables to compare
fifa <- fifa %>% 
  as_tibble() %>% 
  select(c(Name,Overall, Potential,League, Club, "General Position", "Crossing", "Finishing", "HeadingAccuracy", "ShortPassing", "Volleys", "Dribbling", "Curve", "FKAccuracy", "LongPassing", "BallControl", "Acceleration", "SprintSpeed", "Agility", "Reactions", "Balance", "ShotPower", "Jumping", "Stamina", "Strength", "LongShots", "Aggression", "Interceptions", "Positioning", "Penalties", "Composure",  "SlidingTackle", "GKDiving", "GKHandling", "GKKicking", "GKPositioning", "GKReflexes")) 

# Filter players by leagues they play in
fifa <- fifa %>% 
  filter(League %in% c("EPL", "Ligue 1", "Bundesliga", "Serie A", "La Liga")) %>% 
  gather(-c(Name, League,Club, "General Position"), key = "Skill", value = "Value")




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






# Server for app
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    fifa %>% 
      group_by(Name) %>% 
      filter(Name == input$player1 | Name == input$player2) %>% 
      select(Name, Skill, Value) %>% 
      ggplot(aes(Skill, Value))+
      geom_point(aes(color=Name, shape = Name))+
      #geom_path(linetype=2)+
      scale_fill_viridis(option = "D") + 
      theme(legend.position = "None", axis.text.x = element_text(angle=60, size=8), axis.title.x = element_blank())
    
    
  })
  
  
}






shinyApp(ui, server)

