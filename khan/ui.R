library(shiny)
load("~/khan/khan.RData")

shinyUI(
  navbarPage("Interactive Multivariate Visualization of Khan (2012) Gene Expression Data",
             tabPanel("Data Input",
                      sidebarPanel
                      (
                        #data upload
                        fileInput("files", "Choose File", multiple=TRUE)), 
                        
                      mainPanel(
                        h3(textOutput('caption')),
                        tabsetPanel(
                          tabPanel("Data",tableOutput("filetable"))
                        ))
                      
                      ),
             
             tabPanel(
               "Clustering",
               headerPanel("Selecte Options"),
               sidebarPanel(
                 helpText(paste("Select distance:" )),
                 fluidRow(
                   selectInput("dmeth", NULL,  list("euclidean"="euclidean", "maximum"="maximum",
                                                    "manhattan"="manhattan",
                                                    "canberra"="canberra",
                                                    "binary"="binary",
                                                    "minkowski"="minkowski"),
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
                            plotOutput("pairsplot"))
                   
                 )
               )
               ),
             tabPanel("Principal Component Analysis",
                      headerPanel("Select Options"),
                      # h2("Principal Components Analysis (PCA)")
                      
                      #input
                      sidebarPanel
                      (
                        uiOutput("PCs"),
                        # tabsetPanel(id="dist",
                        # tabPanel("Data", value='norm', textInput("dist1","Xdist1", c("norm"))),
                        # tabPanel("Analyze", value='unif', textInput("dist2","Xdist2", c("unif")))),
                        checkboxInput("center","Center",TRUE),
                        selectInput("scaling","Scale", 
                                    list(none = "none", "unit variance" = "uv", pareto = "pareto"),
                                    selected="none"
                        ),	
                        
                        selectInput("method","Method", 
                                    choices=c("svd"="svd",
                                              "nipals"="nipals",
                                              "rnipals"="rnipals",
                                              "bpca"="bpca",
                                              "ppca"="ppca",
                                              "svdImpute"="svdImpute",
                                              "robustPca"="robustPca",   
                                              "nlpca"="nlpca",
                                              "llsImpute"="llsImpute",
                                              "llsImputeAll"="llsImputeAll"),
                                    selected="svd"),
                        
                        selectInput("cv","cross-validation",
                                    list (none = "none", Q2 =  "q2")
                        )
                        #helpText("Hints"),	
                      ),
                      
                      # uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
                      # uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
                      # selectInput("plot.type","Plot Type:", 
                      # list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
                      # ),
                      # checkboxInput("show.points", "show points", TRUE)
                      
                      # output				
                      mainPanel(
                      
                        tabsetPanel(
                          
                          tabPanel("Scree Plot",plotOutput("screeplot",height = 280*2, width = 250*2)),
                          tabPanel("Scores Plot",plotOutput("scores")),
                          tabPanel("Loadings Plot",plotOutput("loadings"))
                        )
                        
                      )),
             
             tabPanel("Regression Analysis",
                      headerPanel("Select Options"),
                      sidebarPanel(
                        
                        uiOutput("PCs"),
                        checkboxInput("center","Center",TRUE)
                        
                        ),
                      mainPanel(
                        h3(textOutput('caption')),
                        tabsetPanel(
                          tabPanel("Residual Plots", tableOutput("ResidualPlots")),
                          tabPanel("Summary Statistics", tableOutput("SummaryStat")),
                          tabPanel("Correlation Analysis", tableOutput("CorrAnalysis")),
                          tabPanel("Predicted Values", tableOutput("PredVal"))
                        )
                      )
                      )))

  