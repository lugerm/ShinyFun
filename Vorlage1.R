# new comment
a <- c(1,2,3)

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
               sidebarPanel
                (
                  selectInput("dataset", "pick your data", choices = c("Fertility", "Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality" ))
                ),
               
              mainPanel(
                verbatimTextOutput("summary"),
                plotOutput("hist"),
                plotOutput("boxplot")
                )
             ))))

  
server <- function(input, output){
  datasetInput <- reactive({
    switch(input$dataset,
           "Fertility" = swiss$Fertility,
           "Agriculture" = swiss$Agriculture,
           "Examination" = swiss$Examination,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Fertility" = swiss$Fertility,
           "Infant.Mortality" = swiss$Infant.Mortality
           )
  })  
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    hist(dataset)})
  
  output$boxplot <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)})
  
  }

shinyApp(ui = ui, server = server)
