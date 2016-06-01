load("data/geneExpression.rda")

source("https://bioconductor.org/biocLite.R")
biocLite(c("Biobase","hgu133a.db"))
library(Biobase)
library(hgu133a.db)

expression <- ExpressionSet(e)
annotation(expression) <- "hgu133a.db"
pData(expression) <- tab

featureNames(expression) =  make.names(mapIds(hgu133a.db, keys=featureNames(expression),
                    keytype="PROBEID", column="SYMBOL"), unique=TRUE)
sampleNames(expression) = make.names(expression$Tissue, unique=TRUE)

nms = names(expression)
cmeths = c("ward.D", "ward.D2",
           "single", "complete", "average", "mcquitty",
           "median", "centroid")
dmeths = c("euclidean", "maximum", "manhattan", "canberra",
           "binary")