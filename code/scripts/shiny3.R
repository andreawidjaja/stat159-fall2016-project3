library(shiny)

ui <- fluidPage(
  actionButton("click1", label="Conclusion"), #Creating an action button 
  textOutput("conclusion")
)



server <- function(input, output){
  observeEvent(input$click1,{output$conclusion=renderText({print("Thank you everyone! Enjoy your winter break :)")})})
  }

shinyApp(ui = ui, server = server)




