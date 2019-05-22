library(shiny)
library(utils)
library(gridExtra)

s <- swiss
snames <- colnames(s)

ui <- fluidPage(
  navbarPage(title = 'perffizienz - wrong on so many levels',
    tabPanel('Swiss',
             sidebarLayout(
               sidebarPanel(actionButton('Show Distribution', label = 'histogram'),
                            actionButton('QQ-Plot', label = 'qq-plot'),
                            actionButton('Correlation', label = 'correlation')),
               mainPanel(
                 tableOutput('rawDataSwiss')
               ))),

    
 
    tabPanel('Cars',
             sidebarLayout
             (
               sidebarPanel
               (
                 radioButtons(inputId = 'dataSet', label = 'Choose wisely:',
                                         choices = snames,
                                         selected = 'mtcars')
                ),
               mainPanel
               (
                 tableOutput('rawDataMtcars')
               )
             )
             )
    ))



server <- function(input, output)
  {
    output$rawDataSwiss <- renderTable(swiss)
    output$rawDataMtcars <- renderTable(mtcars)
  }

shinyApp(ui = ui, server = server)
