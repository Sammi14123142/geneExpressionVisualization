library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Interactive Multivariate Visualization Using Gene Expression Data"),
  
  sidebarPanel(
    helpText("Choose distance:"),
    fluidRow(
      selectInput("dmeth",NULL, choices=dmeths,
                  selected = dmeths[1])),
    helpText("Choose cluster method:"),
    fluidRow(
      selectInput("meth", NULL, choices=cmeths,
                  selected=cmeths[1])),
    helpText("Choose height of cut:"),
    fluidRow(
      numericInput("cutval", NULL, value=40, min=0,
                   max=Inf, step=1)),
    helpText(paste("Choose variables for clustering",
                   substitute(df), ":")),
    fluidRow(checkboxGroupInput("vars", NULL, choices=nms,
                                selected=nms[1:2]))
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("tree", 
               plotOutput("Cluster")),
      tabPanel("pairs", 
               plotOutput("pairsplot")),
      tabPanel("silh", 
               plotOutput("silplot"))
    ))
))