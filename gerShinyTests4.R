library(shiny)
library(utils)
library(gridExtra)


#s <- swiss
#snames <- colnames(s)

ui <- fluidPage(
  navbarPage(title = 'perffizienz - wrong on so many levels',
    tabPanel(
             sidebarLayout
              (
               sidebarPanel(
                  selectInput("dataset", "pick your data", choices = c("Education", "Catholic"))
              )
              ,
               
            mainPanel(
              verbatimTextOutput("summary")
              )
             ))))

  
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Eduction" = swiss$Education,
           "Catholic" = swiss$Catholic)
  })  
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  }

shinyApp(ui = ui, server = server)
