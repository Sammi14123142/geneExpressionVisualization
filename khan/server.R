library(shiny)
library(cluster)
load("~/khan/khan.RData")

shinyServer(function(input, output) {
  
  
  #file info  
  output$filetable <- renderTable({   
      tmp<-df
      tmp<-tmp[,seq_along(1:ncol(tmp))<=10] # show max 10 columns and binf head tail calls
      rbind(head(tmp,10),tail(tmp,10))
     # input$files     
  })
  
  #confirm load
  output$caption<-renderText({
    if (!is.null(PCA.results())) {
      "Principal Components Analysis"
    } else {
      if(is.null(input$files)) { "Load Data" }  else { "Data Loaded"}
    }
  }) 
  
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
  #number of PCs
  output$PCs<-renderUI({
    if (is.null(input$files)) { return(NULL) }
    maxPCs<-ncol(input$files)
    numericInput("PCs", "Number of Principal Components", 
                 2, min = 2, max = maxPCs)
  })
  
  PCA.results<-reactive({
    if (is.null(input$files)) { 
      return(NULL) 
    } else {
      # list(data=read.csv(input$files$datapath, header=T, stringsAsFactors =T),
      # data2=rnorm(10))
      
      # }
      #adapted from another devium
      pca.inputs<-list()
      start.data<<-read.csv(input$files$datapath, header=T, stringsAsFactors =T)
      pca.inputs$pca.data<-"start.data"
      pca.inputs$pca.algorithm<-input$method
      pca.inputs$pca.components<-input$PCs
      pca.inputs$pca.center<-input$center
      pca.inputs$pca.scaling<-input$scaling
      pca.inputs$pca.cv<-input$cv # currently not used
      devium.pca.calculate<-function(pca.inputs,args.list=TRUE,return="list", plot=TRUE)
      {
        check.get.packages("pcaMethods")	
        #port of imDEV source code optimized for GUI use
        #accepts list with the following arguments
        #pca.data<- data object (samples as rows)
        #pca.components numeric number of number of principal components
        #pca.algorithm see pcaMethods::pca for options
        #pca.center logical, mean center the data
        #pca.scaling character describing scaling method, see pcaMethods::prep for options
        
        #check for text or factor and remove (add to subset) 
        tmp<-pca.inputs
        # data.obj<-as.data.frame(get(tmp$pca.data))
        # data.obj<-data.obj[sapply(1:ncol(data.obj), function(i) {class(data.obj[,i])=="numeric"|class(data.obj[,i])=="integer"})] # has to be better way to avoid factors
        data.obj<-afixln(tmp$pca.data) # converts factors or characters to numeric
        
        if(is.null(tmp$pca.cv)){pca.cv<-"none"} else {pca.cv<-tmp$pca.cv} #avoid issues elsewhere
        
        
        #adjust PCS if > than data
        PCs<-tmp$pca.components
        if(PCs> min(dim(data.obj))){PCs<-min(dim(data.obj))} # this should be done internally in the PCa fxn
        pca.results<-pcaMethods::pca(as.matrix(data.obj), method=tmp$pca.algorithm, 
                                     nPcs=PCs, center=tmp$pca.center,scale=tmp$pca.scaling, cv = pca.cv, seed=123)
        
        #results
        scores<-as.data.frame(pca.results@scores)
        loadings<-as.data.frame(pca.results@loadings)
        eigenvalues<-data.frame(eigenvalues=pca.results@R2)
        
        
        if(pca.cv=="q2"){
          # account for unequal r2 and q2 lengths 
          q2<-tryCatch( pcaMethods:::Q2(pca.results), error=function(e) {0} )#some versions of pcaMEthods don't have this?
          q2<-c(q2,rep(q2[length(q2)],nrow(eigenvalues)-length(q2)))
          eigenvalues<-data.frame(eigenvalues,q2=q2)
        }
        
        #add leverage and dmodX
        #bind between scores and loadings
        lev<-tryCatch(as.matrix( pcaMethods:::leverage(pca.results)),error=function(e){"can not calculate"})
        dmodx<-tryCatch(as.matrix( pcaMethods:::DModX(pca.results)),error=function(e){"can not calculate"})
        diagnostics<-tryCatch(data.frame(leverage=lev,DmodX=dmodx),error=function(e){data.frame(Error="not applicable")})
        
        #scree plot
        if(plot==TRUE){ make.scree.plot.bar(eigenvalues) }
        
        
        #get the name of the data
        if(return=="list"){
          return(list(pca.scores = scores, pca.loadings =  loadings,pca.eigenvalues = eigenvalues, pca.diagnostics = diagnostics))} 
        
        if(return=="model"){return(pca.results)}
      }
      devium.pca.calculate(pca.inputs,return="list",plot=F)
      
    }
  })	
  
  
  
  #make screeplot
  output$screeplot <- renderPlot({
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      x<-PCA.results()
      x<-data.frame(x$pca.eigenvalues)
      
      # make.scree.plot(x)	
      make.scree.plot.bar(x)
    }
  })	
  
  # scores diagnostic plot
  output$scores <- renderPlot({
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      tmp<-PCA.results()
      scores<-data.frame(tmp$pca.scores)
      if(nrow(tmp$pca.diagnostics)==nrow(scores))
      {
        if(any(tmp$pca.diagnostics$DmodX=="NaN")){tmp$pca.diagnostics$DmodX<-1}
        scores<-data.frame(leverage=tmp$pca.diagnostics$leverage, dmodx=tmp$pca.diagnostics$DmodX,scores)
      } else {
        scores<-data.frame(leverage=1, dmodx=1,scores)
      }
      
      p<-ggplot(scores,mapping = aes_string(x = names(scores)[3], y = names(scores)[4],color=names(scores)[1],size=names(scores)[2])) + 
        scale_size_continuous("DmodX", range = c(4, 10)) + 
        geom_point(alpha=0.75) +.theme
      print(p)
    }
  })	
  #loadings plot
  output$loadings <- renderPlot({
    if (is.null(PCA.results())) { 
      return(NULL)
    } else {
      tmp<-PCA.results()
      loadings<-data.frame(tmp$pca.loadings,names=rownames(tmp$pca.loadings))
      
      #plot
      p<-ggplot(loadings,mapping = aes_string(x = names(loadings)[1], y = names(loadings)[2], label = "names")) + 
        geom_text(size=4,alpha=0.75) +.theme
      print(p)
    }
  })
})
