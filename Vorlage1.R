# new comment

library(shiny)
library(utils)
library(gridExtra)


#s <- swiss
#snames <- colnames(s)

ui <- fluidPage(
  navbarPage(title = 'perffizienz - wrong on so many levels',
    tabPanel('swiss',
             sidebarLayout
              (
               sidebarPanel(
                  selectInput("dataset", "pick your data", choices = c("Education", "Catholic", "Fertility"))
              )
              ,
               
            mainPanel(
              verbatimTextOutput("summary")
              )
             ))))

  
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Fertility" = swiss$Fertility
           )
  })  
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  }

shinyApp(ui = ui, server = server)
