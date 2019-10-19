library(shiny)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
# 

mymvrnorm <- function(n,mu,Sigma)
{
  p <- length(mu)  # p variate
  # generate z1,..,zn ~ N(0,1) and use these to generate N(mu,Sigma)
  z <- matrix(rnorm(n*p),nrow = n,ncol = p) 
  # get eigen vectors and values
  eig <- eigen(Sigma,symmetric = TRUE)
  # compute Sigma^.5
  sig.sqrt <- eig$vectors %*% diag(eig$values^.5) %*% t(eig$vectors) 
  x <- rep(1,n) %*% t(mu) + z %*%  sig.sqrt
  return(x)
}

gen_Sigma = function(p,sigma){
  Sigma = matrix(sigma,ncol=p,nrow = p)
  diag(Sigma) = 1
  return(Sigma)
}

mycorrdata = function(G,n,mu){

  p = length(mu)

  out = matrix(ncol = p)

  for(i in 1:G){
    # generate mu vector and covariance matrix
    mu_G =  mu + runif(p,max = 4,min = -4)
    rho = runif(1)
    Sigma_G = gen_Sigma(p,rho)
    # generate multivariate data for group i
    data = mymvrnorm(n,mu_G,Sigma_G)
    out = rbind(out,data)
  }
  return(out[-1,])
}

p = 3
n = 50
G = 3

test = mycorrdata(G,n,
                  mu = c(1,8,5))

# principal components, plot first two pc's
pcdata = prcomp(test,scale. = TRUE)
pcdata$group = rep(seq(1,G),times = rep(n,G))
data = cbind(pcdata$x[,1],pcdata$x[,2],pcdata$group)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  #fetch = reactiveValues()
  
  eventReactive(input$refresh,{
       mvdata = mycorrdata(G,n,
                         mu = c(1,8,5))
       
       # principal components, plot first two pc's
       pcdata = prcomp(mvdata,scale. = TRUE)
       pcdata$group = rep(seq(1,G),times = rep(n,G))
       data = cbind(pcdata$x[,1],pcdata$x[,2],pcdata$group)
    
    
  })
    
    # Compute the forumla text in a reactive expression since it is 
    # shared by the output$caption and output$mpgPlot expressions
    formulaText <- reactive({
      paste(input$n,"clusters")
    })
    
    # Return the formula text for printing as a caption
    output$caption <- renderText({
      formulaText()
    })
    
    # Generate a plot of the requested variable against mpg and only 
    # include outliers if requested
    output$pcplot <- renderPlot({
      plot(data[,1],data[,2],col = data[,3])
    })
    
    output$kmplot <- renderPlot({
      kmeans_data = kmeans(data[,1:2],input$c)
      plot(data[,1],data[,2],col = kmeans_data$cluster)
    })
    
    output$scree = renderPlot({
      ctot = input$c
      withss = numeric()
      for(i in 1:ctot){
        kmdata = kmeans(data[,1:2],i)
        withss[i] = kmdata$tot.withinss
      }
      plot(seq(1,ctot),withss,type='b')
      
    })
    
  
  
  
})