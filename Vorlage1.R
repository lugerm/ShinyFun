library(shiny)
library(utils)
library(gridExtra)

swiss <- swiss[,-3]
#snames <- colnames(s)

ui <- fluidPage(
  navbarPage(title = 'perffizienz - wrong on so many levels',
    #tabPanel('swiss raw data', "Mein Text"),
    tabPanel('swiss', tags$h2("Overview - Head of Dataset"), #tableOutput("rawdata_swiss"),
             sidebarLayout
              (
                
                sidebarPanel
                (
                  selectInput("dataset", "Pick a variable", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality" )),
                  radioButtons("options", "Options", choices = c("Correlations", "Linear Model"))
                ),
                
              mainPanel(tags$h2("Data in more detail:"),
                  #tableOutput("rawdata_swiss"),
                  #verbatimTextOutput("summary"),
                  #plotOutput("hist"),
                  #plotOutput("boxplot"),
                  tabsetPanel(
                    tabPanel("All data", tableOutput("rawdata_swiss") ),
                    tabPanel("Summary", verbatimTextOutput("summary") ),
                    tabPanel("Histogram & Boxplot", plotOutput("hist"), plotOutput("boxplot")),
                    tabPanel("QQ-Plot", plotOutput("qqplot"))
                  )
                )
             )),
    tabPanel('XXX', "TEXT")
    ))

  
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
 
  output$rawdata_swiss <- renderTable({
    dataset <- swiss
    dataset})  # head(dataset)
   
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)})
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    hist(dataset)})
  
  output$boxplot <- renderPlot({
    dataset <- datasetInput()
    boxplot(dataset)})
  
  output$qqplot <- renderPlot({
    dataset <- datasetInput()
    qqnorm(dataset); qqline(dataset, col=2)})
  }

shinyApp(ui = ui, server = server)
