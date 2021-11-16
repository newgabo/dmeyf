#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

#defino la carpeta donde trabajo
directory.root  <- "E:/Archivo/EconFin"
setwd( directory.root )


mes_testing = 202011
mes_kaggle = 202101


#cargo el dataset donde testeo
dtest  <- fread(paste0("./datasetsOri/partitioned/datasetsOri_partitioned_dataset_epic_simple_", mes_testing, ".csv"))

# Voy armando la tabla de prediccion para cada mes
test_prediccion  <- as.data.table( list( "numero_de_cliente"= dtest[  , numero_de_cliente],
                                         "foto_mes"= dtest[  , foto_mes],
                                         "clase_ternaria"= dtest[  , clase_ternaria] ,
                                         "clase01"= dtest[  , ifelse( clase_ternaria=="CONTINUA", 0, 1 )])) #genero la salida

fwrite( test_prediccion, 
        file="./datasets/dataset_partitioned_stack.csv",
        sep=  "," )


#cargo el dataset para predecir kaggle
dkaggle  <- fread(paste0("./datasetsOri/partitioned/datasetsOri_partitioned_dataset_epic_simple_", mes_kaggle, ".csv"))

# Voy armando la tabla de prediccion para cada mes
kaggle_prediccion  <- as.data.table( list( "numero_de_cliente"= dkaggle[  , numero_de_cliente],
                                         "foto_mes"= dkaggle[  , foto_mes])) #genero la salida

fwrite( kaggle_prediccion, 
        file="./datasets/dataset_partitioned_stack_kaggle.csv",
        sep=  "," )


meses_training <- c(201801,201802,201803,201804,201805,201806,201807,201808,201809,201810,201811,201812,
201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,
202001,202002,202003,202004,202005,202006,202007,202008,202009,202010)

for (mes_training in meses_training) {
  #cargo el dataset
  dataset  <- fread(paste0("./datasetsOri/partitioned/datasetsOri_partitioned_dataset_epic_simple_", mes_training, ".csv"))
  
  #creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
  
  #Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria", "clase01") )

  #genero el formato requerido por LightGBM
  dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]), label= dataset[ , clase01]
  )
  
  
  #Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
  #Dadme un punto de apoyo y movere el mundo, Arquimedes
  modelo  <- lightgbm( data= dtrain,
                       params= list( objective= "binary",
                                     max_bin= 15,
                                     min_data_in_leaf= 4000,
                                     learning_rate= 0.051,
                                     num_iterations = 100
                                     #,feature_fraction = 0.9
                       )  )
  
  #aplico el modelo a los datos nuevos de testing
  prediccion  <- predict( modelo,  data.matrix( dtest[  , campos_buenos, with=FALSE]))
  test_prediccion[,paste0("baja_prob_",mes_training)] = prediccion

  fwrite( test_prediccion, 
          file= "./datasets/dataset_partitioned_stack.csv",
          sep=  "," )

  #aplico el modelo a los datos nuevos de kaggle_prediccion
  prediccion  <- predict( modelo,  data.matrix( dkaggle[  , campos_buenos, with=FALSE]))
  kaggle_prediccion[,paste0("baja_prob_",mes_training)] = prediccion
  
  fwrite( kaggle_prediccion, 
          file= "./datasets/dataset_partitioned_stack_kaggle.csv",
          sep=  "," )
  
}

#cargo el dataset donde aplico el modelo
#dapply  <- fread(paste0("./datasetsOri/partitioned/datasetsOri_partitioned_dataset_epic_simple_", mes_kaggle, ".csv"))

#aplico el modelo a los datos nuevos, dapply
#prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))

#la probabilidad de corte ya no es 0.025,  sino que 0.031
#entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
#                                 "Predicted"= as.numeric(prediccion > 0.0309) ) ) #genero la salida

#genero el archivo para Kaggle
#fwrite( entrega, 
#        file= paste0("./kaggle/partitioned/dataset_partitioned_", mes_training, ".csv"),
#        sep=  "," )