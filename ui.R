library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Clustering"),


  sidebarPanel(
    sliderInput("c", "Number of Clusters",
                value = 2,
                min = 1,
                max = 10)


  ),
  #h3(textOutput('caption')),
  mainPanel(
    tabsetPanel(
      tabPanel("Actual",fluid = TRUE,plotOutput('pcplot')),
      tabPanel("K-Means",fluid = TRUE,plotOutput('kmplot')),
      tabPanel("H-Clust",fluid = TRUE,plotOutput('hclust')),
      tabPanel("K-Means Elbow",fluid = TRUE,plotOutput('scree'))
    )
  )
))

# 
# tabPanel("K-Means",fluid = TRUE,
#          
#          fluidRow(column(5,plotOutput('pcplot')),
#                   column(5,plotOutput('kmplot')),
#                   column(5,plotOutput('scree')))),
# tabPanel("H-Clust",fluid = TRUE,
#          fluidRow(column(5,plotOutput('pcplot')),
#                   column(5,plotOutput('hclust'))))
# 
