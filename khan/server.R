library(shiny)
library(cluster)
load("~/khan/khan.RData")

server <- function(input, output) {
  output$plot1 <- renderPlot({
    xv = df[,input$vars]
    plot(hclust(dist(data.matrix(xv),method=input$dmeth), method=input$meth),
         xlab=paste(input$dmeth, "distance;", input$meth, "clustering"))
    abline(h=input$cutval, lty=2, col="gray")
  })
  output$pairsplot <- renderPlot({
    xv = df[,input$vars]
    pairs(data.matrix(xv))
  })
  output$silplot <- renderPlot({
    xv = df[,input$vars]
    dm = dist(data.matrix(xv),method=input$dmeth)
    hc = hclust(dist(data.matrix(xv),method=input$dmeth), method=input$meth)
    ct = cutree(hc, h=input$cutval)
    plot(silhouette(ct, dm))
  })
}
