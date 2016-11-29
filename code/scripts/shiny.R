#setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3/code/scripts")
library(shiny)
clean_data=read.csv("../../data/clean_data.csv")[-1]
source("functions.R")

ui <- fluidPage(
  actionButton("click", label="Generate Relevant Variables"), #Creating an action button 
  textInput("text", label = h3("Text input"), value = "Lasso, BIC, P-Value"), #Creating an input widget
  radioButtons("Select Graduation Rate of Interest", label = h3("Radio buttons"),
               choices = list("Overall Graduation Rate" = "UGDS", "Choice 2" = 2, "Choice 3" = 3), 
               selected = 1),
  
  numericInput("var_num", label=h3("Number of Variables"), value=1:10),
  tableOutput("variables")
)



server <- function(input, output){
  observeEvent(input$click,{
    order_ind=order(table(rel_data2$obfs_bkuuid), decreasing=TRUE) #ordering unique user ids by counts of repeats in decreasing order
    ids=names(table(rel_data2$obfs_bkuuid)[order_ind]) #reordering ids by above indices
    index=sample(1:length(ids),1) #getting a random user
    output$variables=renderTable({repeatuser_mods2(ids[index],input$text)}) #plotting a random user with a specified time scale 
  })
}

shinyApp(ui = ui, server = server)
