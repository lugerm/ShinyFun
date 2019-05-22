library(shiny)
library(utils)
library(gridExtra)
snames <- list('Swiss' = 'swiss', 'Titanic' = 'titanic')

ui <- fluidPage(
  navbarPage(title = 'perffizienz - wrong on so many levels',
    tabPanel('Swiss',
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = 'dataSet', label = 'Choose wisely:',
                                         choices = snames,    #list('Swiss' = 'swiss', 'Titanic' = 'titanic'),
                                         selected = 'swiss')),
               mainPanel(
                 tableOutput('rawDataSwiss'),
                 verbatimTextOutput("summarySwiss"),
                 tableOutput("viewSwiss")
               ))),

    tabPanel('Cars',
             sidebarLayout
             (
               sidebarPanel
               (
                 radioButtons(inputId = 'dataSet', label = 'Choose:',
                                         choices = snames,    #list('Swiss' = 'swiss', 'Titanic' = 'titanic'),
                                         selected = 'swiss')
                ),
               mainPanel
               (
                 tableOutput('rawDataMtcars'),
                 
                 verbatimTextOutput("summaryCars"),
                 tableOutput("viewCars")
                
               )
             )
             )
    ))



server <- function(input, output)
  {
    output$rawDataSwiss <- renderTable(swiss)
    output$rawDataMtcars <- renderTable(mtcars)
    
    output$summaryCars <- renderPrint({
      summary(mtcars)
    })
    output$summarySwiss <- renderPrint({
      summary(swiss)
    })
  }

shinyApp(ui = ui, server = server)
