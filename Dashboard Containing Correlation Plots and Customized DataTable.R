library(tidyverse)
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = 'Cor. Plot & DT'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Iris', tabName = 'Iris', icon = icon('tree')),
      menuItem('Car', tabName = 'CarsTable', icon = icon('car'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem('Iris', h1("Correlation Plot"), selectInput(inputId = 'Features', label = 'Features:',
                                  choices = c('Sepal.Width', 'Petal.Length', 'Petal.Width')),
              plotOutput('Plot')
                              
                                                                                         ),
      tabItem(tabName = 'CarsTable', fluidPage(
        h1('Cars'), dataTableOutput(outputId = 'CarsTable')
      ))))
)
    

  

server <- function(input, output) {
  output$Plot <- renderPlot({
    ggplot(iris, aes(Sepal.Length, iris[[input$Features]], color = Species)) + geom_point() + xlab(' Sepal.Length') +
      ylab('Chosen Feature')})
    output$CarsTable <- renderDataTable(mtcars)
    
  
  
}

shinyApp(ui, server)