












library(shiny)

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

corr_data = mycorrdata(G,n,
                   mu = c(1,8,5))
 
# principal components, plot first two pc's
pcdata = prcomp(corr_data,scale. = TRUE)
pcdata$group = rep(seq(1,G),times = rep(n,G))
pdata = cbind(pcdata$x[,1],pcdata$x[,2],pcdata$group)


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  formulaText <- reactive({
    paste(input$n,"clusters")
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
# plot principal components
  output$pcplot <- renderPlot({
    plot(pdata[,1],pdata[,2],col = pdata[,3],
         xlab = 'PC1',
         ylab = 'PC2',
         main = 'Actual')
  })
  
  output$kmplot <- renderPlot({
    kmeans_data = kmeans(pdata[,1:2],input$c)
    plot(pdata[,1],pdata[,2],col = kmeans_data$cluster,
         xlab = 'PC1',
         ylab = 'PC2',
         main = 'K-Means Clusters')
  })
  
  output$scree = renderPlot({
    ctot = input$c
    withss = numeric()
    for(i in 1:ctot){
      kmdata = kmeans(pdata[,1:2],i)
      withss[i] = kmdata$tot.withinss
    }
    plot(seq(1,ctot),withss,type='b',
         main = 'Elbow Plot',
         xlab = 'Number of Clusters',
         ylab = 'Within Group Sum of Squares')
    
  })
  
  output$hclust <- renderPlot({
    ## heirarchical clustering
    hc = hclust(dist(pdata[,-3]))
    chc = cutree(hc,input$c)
    plot(pdata[,-3],
         col = chc,
      xlab = 'PC1',
      ylab = 'PC2',
      main = 'Heirarchical Clustering')
  })
  
  
})