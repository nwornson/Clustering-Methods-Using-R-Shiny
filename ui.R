library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Clustering"),
  

  sidebarPanel(
    sliderInput("c", "Number of Clusters",
                value = 2,
                min = 1,
                max = 10)

    
  ),
  
  mainPanel(
    h3(textOutput('caption')),
    fluidRow(column(5,plotOutput('pcplot')),
             column(5,plotOutput('kmplot')),
             column(5,plotOutput('scree')))
  )
))


