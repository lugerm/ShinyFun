library(shiny)
library(utils)
library(gridExtra)

swiss <- swiss[,-3]
#snames <- colnames(s)

##definition for the scatterplo
#------------------------------------------------------------------------------------------------
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#-------------------------------------------------------------------------------------

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
                    tabPanel("QQ-Plot", plotOutput("qqplot")),
                    tabPanel("Scatterplot", plotOutput("scatter"))
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
  
  output$scatter <- renderPlot({
    dataset <- datasetInput()
    pairs(swiss, lower.panel = panel.smooth, upper.panel = panel.cor,
          gap=0, row1attop=FALSE, main = "Scatterplot")})
}


shinyApp(ui = ui, server = server)
