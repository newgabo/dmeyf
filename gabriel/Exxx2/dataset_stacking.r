#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("~/buckets/b1/" )


version  <- "v006"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_epic_simple_v005.csv.gz" )
# Esta linea es para elegir que campos tomo del dataset original. Si se comenta se toman todos

dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria",
  "mprestamos_personales","mvr_mconsumospesos",
  "mactivos_margen","mcuentas_saldo","mrentabilidad_annual","Visa_fechaalta","mv_fechaalta","mvr_mconsumototal",
  "mtransferencias_emitidas",
  "mtarjeta_master_consumo","mpasivos_margen","mtarjeta_visa_consumo","ctarjeta_visa_transacciones","Master_fechaalta",
  "Visa_mpagosdolares",
  "mrentabilidad","mforex_sell","mpayroll","cproductos","mv_Fvencimiento","mautoservicio"
      ),  with=FALSE] ) 
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

