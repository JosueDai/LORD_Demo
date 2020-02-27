setwd('/Users/lunave/documents/LORD_Demo/')


library(reshape2)   ## v 1.4.3
library(stringr)    ## v 1.4.0
library(data.table) ## v 1.12.8
library(plyr)       ## v 1.8.5
library(dplyr)      ## v 0.8.4
library(scales)     ## v 1.1.0
library(tidyr)      ## v 1.0.2

Creativo_ts <- read.csv(file="Creativo/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")


##Clean Creativo----
DataCreativo <- function(dataCre){
  #modificacion y eliminacion de tabla
  colnames(dataCre) <- as.character(unlist(dataCre[2, ]))
  dataCre<-dataCre[!((dataCre$ODT=="") | dataCre$ODT=="ODT"), ]
  
  #Limpiar datos 
  dataCre$ODT = str_replace_all(dataCre$ODT, "[^[:graph:]]" , " ") 
  dataCre$Actividad = str_replace_all(dataCre$Actividad, "[^[:graph:]]" , " ") 
  
  #********* SOMEBODY SAVE ME  *******
  #### FIX AVOID DT
  # 
  dataCre$ODTcopy<-dataCre$ODT
  dataCre<-separate(dataCre, col = "ODT", into = c("Marca", "Proyecto","Tarea"), sep = "/") #Modifica y seapara la variable ODT en tres nombres distintos.
  
  colnames(dataCre)[9] <- "ID"   #Rename ID
  
  dataCre$ODTcopy<-dataCre$ODT
  colnames(dataCre)[11] <- "ODT" #Rename ODT
  # 
  dataCre$Marca=trimws(dataCre$Marca)
  dataCre$Marca=str_replace_all(dataCre$Marca, "Rich.*", "Richs")   #Remplazo nombre de objeto
  
  dataCre$Tiempo <- as.ITime(dataCre$Tiempo, format = "%H:%M")  #Funcion para modificar tipo de dato y realizar operaciones
  tiempo <- as.POSIXlt(paste(Sys.Date(), dataCre$Tiempo))
  tiempo <- tiempo$hour*60 + tiempo$min                   #Operaciones para calcular tiempos
  dataCre$Tiempo <- tiempo
  rm(tiempo)
  
  dataCre$Fecha <- as.Date(as.character.Date(dataCre$Fecha))  #Convertir tipo de dato
  dataCre$Mes <- month(dataCre$Fecha)
  dataCre$Semana <- format(dataCre$Fecha, "%V")
  
  dataCre$Nombre<-gsub("-.*","",dataCre$Nombre) #Modifica el caracter "-" por un espacio en blanco
  dataCre$Marca<-gsub("'", "", dataCre$Marca)   #Modifica el caracter "'" por un espacio en blanco
  
  return (dataCre)
}



Creativo_ts<-DataCreativo(Creativo_ts)




TotalporODTCre <- function(dataCre){
  TotalporODTCre <-aggregate(Tiempo ~ ODT + Nombre, FUN = sum, data =dataCre)
  return(TotalporODTCre)
}


Creativo_TotalporODT<- TotalporODTCre(Creativo_ts)


write.csv(Creativo_TotalporODT, "Creativo/LORD/Creativo_TotalporODT.csv", na="")
