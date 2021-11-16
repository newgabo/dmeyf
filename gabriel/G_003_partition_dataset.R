require("data.table")
require("Rcpp")
require("rlist")
require("yaml")
directory.root  <- "~/buckets/b1/"  #Google Cloud
setwd( directory.root )
dataset <- fread( "./datasetsOri/paquete_premium.csv.gz")


dim(dataset)

unique(dataset[,foto_mes])


periodo = '202101'
subdataset = dataset[foto_mes == periodo]
fwrite( subdataset,
        paste0( "./datasetsOri/partitioned/dataset_epic_simple_", periodo, ".csv" ),
        logical01 = TRUE,
        sep= "," )


201801 
201802 
201803 
201804 
201805 
201806 
201807 
201808 
201809 
201810 
201811 
201812 
201901 
201902 
201903 
201904 
201905 
201906 
201907 
201908 
201909 
201910 
201911 
201912 
202001 
202002
202003 
202004 
202005 
202006 
202007 
202008 
202009 
202010 
202011 
202012 
202101