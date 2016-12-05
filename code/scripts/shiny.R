library(shiny)
source("code/scripts/functions.R")
setwd("/Users/josephfrancia/Desktop/Fall_2016/Stats159/stat159-fall2016-project3")
scaled_data=read.csv("data/generated_data/scaled_data.csv")


ui <- fluidPage(
  actionButton("click", label="Generate Relevant Variables"), #Creating an action button 
  radioButtons("text", label=h3("Variable Selection Method"), choices=list("Lasso"="Lasso", "BIC"="BIC","P-Value"="P-Value")),
  radioButtons("radio", label = h3("Select Graduation Rate of Interest"),
               choices = list("Overall Graduation Rate" = "C150_4", "Graduation Rates of White People" = "C150_4_WHITE", "Graduation Rates of Black People" = "C150_4_BLACK",
                              "Graduation Rates of Hispanic People"="C150_4_HISP", "Graduation Rates of Asian People"="C150_4_ASIAN", "Graduation Rates of Native Americans"="C150_4_AIAN",
                              "Graduation Rates of Pacific Islanders"="C150_4_NHPI"), 
               selected = 1),
  numericInput("var_num", label=h3("Number of Variables"), value=1:10),
  tableOutput("variables")
)



server <- function(input, output){
  observeEvent(input$click,{
    df=choosing_response(scaled_data, input$radio)
    if(input$text=="Lasso"){
      output$variables=renderTable({lasso_select(df,input$radio,input$var_num)})
    }
    else if(input$text=="BIC"){
      output$variables=renderTable({bic_select(df,input$radio,input$var_num)})
    }
    else if(input$text=="P-Value"){
      output$variables=renderTable({forward_p(df, input$var_num)})
    }
  })
}

shinyApp(ui = ui, server = server)




