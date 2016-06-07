library(shiny)
load("~/khan/khan.RData")

ui <- fluidPage( titlePanel(paste("hclust")),
                 sidebarPanel(
                   helpText(paste("Select distance:" )),
                   fluidRow(
                     selectInput("dmeth", NULL, choices=dmeth,
                                 selected=dmeth[1])),
                   helpText(paste("Select clustering method:" )),
                   fluidRow(
                     selectInput("meth", NULL, choices=meth,
                                 selected=meth[1])),
                   helpText(paste("Select height for cut:" )),
                   fluidRow(
                     numericInput("cutval", NULL, value=40, min=0, max=Inf, step=1)),
                   helpText(paste("Select variables for clustering from" )),
                   fluidRow(
                     checkboxGroupInput("vars", NULL, choices=nms,
                                        selected=nms[1:2]))
                 ),mainPanel(
                   tabsetPanel(
                     tabPanel("tree", 
                              plotOutput("plot1")),
                     tabPanel("pairs", 
                              plotOutput("pairsplot")),
                     tabPanel("silh", 
                              plotOutput("silplot"))
                   )
                 )
)