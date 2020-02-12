



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

