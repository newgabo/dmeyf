require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "E:/Archivo/EconFin" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/dataset_clustering.csv.gz", stringsAsFactors= TRUE)
gc()

View(dataset)