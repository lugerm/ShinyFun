library(shiny)
s <- swiss
snames <- colnames(s)

ui <- fluidPage(
  navbarPage(
    title = 'perffizienz - wrong on so many levels',
    tabPanel('Data Set',
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = 'dataSet', label = 'Choose:',
                                         choices = snames,    #list('Swiss' = 'swiss', 'Titanic' = 'titanic'),
                                         selected = 'swiss')),
               mainPanel(
                 tableOutput('rawData')
               )
             )
    ),
    tabPanel('Lagesch??tzer',
             sidebarLayout(
               sidebarPanel(actionButton('Print Histogramm', label = 'histogramm'),
                            actionButton('QQ-Plot', label = 'qq-plot')),
               mainPanel(
                 fluidRow(column(2, verbatimTextOutput('value')))
               )
             )
    )
  )
)

server <- function(input, output){
  output$rawData <- renderTable(swiss)}

shinyApp(ui = ui, server = server)