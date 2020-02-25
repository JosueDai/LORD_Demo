#Establecemos el directorio o carpeta de trabajo es el lugar en nuestra computadora 
#en el que se encuentran los archivos con los que estamos trabajando en R.

setwd('/Users/lunave/documents/LORD_Demo/')


library(reshape2) v ## v 1.4.3
library(stringr)    ## v 1.4.0
library(data.table) ## v 1.12.8
library(plyr)       ## v 1.8.5
library(dplyr)      ## v 0.8.4
library(scales)     ## v 1.1.0
library(tidyr)      ## v 1.0.2

#LOAD & CLEAN FILES ---------------

### LOAD ts files from area

BI_ts <-read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")     #Carga datos de la carpeta de TimeSheet
Creativo_ts <- read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")
Cuentas_ts <-read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")
PM_ts <- read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")
UX_ts <- read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")
Desarrollo_ts <- read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")
SocialCnt_ts <- read.csv(file="BI/Source/ts/lord.csv" ,head=F,sep=",", fileEncoding = "latin1")

BI_rc <-read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
Creativo_rc <-read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
Cuentas_rc <-read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
PM_rc <- read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
UX_rc <- read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
Desarrollo_rc <- read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")
SocialCnt_rc <- read.csv(file="BI/Source/rc.csv" ,head=T,sep="," , fileEncoding = "latin1")

BI_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
Creativo_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
Cuentas_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
PM_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
UX_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
Desarrollo_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")
SocialCnt_rate <- read.csv(file="BI/Source/rate.csv" ,head=T,sep=",", fileEncoding = "latin1")

#Funciones utilizadas para limpieza y modificacion de variables.
cleanData<- function(data){
   #modificacion y eliminacion de tabla
   colnames(data) <- as.character(unlist(data[2,]))
   data<-data[!((data$ODT=="") | data$ODT=="ODT"), ]
   
   #Limpiar datos 
   data$ODT = str_replace_all(data$ODT, "[^[:graph:]]" , " ") 
   data$Actividad = str_replace_all(data$Actividad, "[^[:graph:]]" , " ") 
   
   #********* SOMEBODY SAVE ME  *******
   #### FIX AVOID DT
   # 
     data$ODTcopy<-data$ODT
     data<-separate(data, col = "ODT", into = c("Marca", "Proyecto","Tarea"), sep = "/") #Modifica y seapara la variable ODT en tres nombres distintos.
     
     colnames(data)[9] <- "ID"   #Rename ID
     
     data$ODTcopy<-data$ODT
     colnames(data)[11] <- "ODT" #Rename ODT
   # 
     data$Marca=trimws(data$Marca)
     data$Marca=str_replace_all(data$Marca, "Rich.*", "Richs")   #Remplazo nombre de objeto
  
     data$Tiempo <- as.ITime(data$Tiempo, format = "%H:%M")  #Funcion para modificar tipo de dato y realizar operaciones
     tiempo <- as.POSIXlt(paste(Sys.Date(), data$Tiempo))
     tiempo <- tiempo$hour*60 + tiempo$min                   #Operaciones para calcular tiempos
     data$Tiempo <- tiempo
     rm(tiempo)
  
     data$Fecha <- as.Date(as.character.Date(data$Fecha))  #Convertir tipo de dato
     data$Mes <- month(data$Fecha)
     data$Semana <- format(data$Fecha, "%V")
  
     data$Nombre<-gsub("-.*","",data$Nombre) #Modifica el caracter "-" por un espacio en blanco
     data$Marca<-gsub("'", "", data$Marca)   #Modifica el caracter "'" por un espacio en blanco
  
   return (data)
}

cleanRC <- function (lordrc){
 #RC 
  colnames(lordrc)[1] <- "Fecha" #Cambia el nombre de la variable

 #lordrc$Fecha<-as.Date(as.character.Date(lordrc$Fecha))
 lordrc$Fecha <- as.Date(lordrc$Fecha, format ="%d/%m/%y") #Conversion de tipo de dato y formato de fecha
 
 lordrc$Mes<-month(lordrc$Fecha) #Crea la variable mes en otro apartado dsntro del dataframe
 
 #Convirte tipo de dato de las columnas
 lordrc$Puesto<-as.character(lordrc$Puesto)
 lordrc$Marca<-as.character(lordrc$Marca)
 return(lordrc)
}

cleanRate<-function(rate){
 #RATES
 colnames(rate)[1] <- "ID" #Modifica el nombre de la variable
 return(rate)
}

BI_ts<-cleanData(BI_ts)
BI_rc<-cleanRC(BI_rc)
BI_rate<-cleanRate(BI_rate)




Creativo_ts<-cleanData(Creativo_ts)
Creativo_rc<-cleanRC(Creativo_rc)
Creativo_rate<-cleanRate(Creativo_rate)

Cuentas_ts<-cleanData(Cuentas_ts)
Cuentas_rc<-cleanRC(Cuentas_rc)
Cuentas_rate<-cleanRate(Cuentas_rate)

SocialCnt_ts<-cleanData(SocialCnt_ts)
SocialCnt_rc<-cleanRC(SocialCnt_rc)
SocialCnt_rate<-cleanRate(SocialCnt_rate)

Desarrollo_ts<-cleanData(Desarrollo_ts)
Desarrollo_rc<-cleanRC(Desarrollo_rc)
Desarrollo_rate<-cleanRate(Desarrollo_rate)

UX_ts<-cleanData(UX_ts)
UX_rc<-cleanRC(UX_rc)
UX_rate<-cleanRate(UX_rate)

PM_ts<-cleanData(PM_ts)
PM_rc<-cleanRC(PM_rc)
PM_rate<-cleanRate(PM_rate)




# Total de horas por persona--------------- 
TotalporODT <- function (data){
TotalporODT <-aggregate( Tiempo ~  ODT + Nombre, FUN=sum, data=data)
return (TotalporODT)
}

BI_TotalporODT<-TotalporODT(BI_ts)
Creativo_TotalporODT<-TotalporODT(Creativo_ts)
Cuentas_TotalporODT<-TotalporODT(Cuentas_ts)
UX_TotalporODT<-TotalporODT(UX_ts)
PM_TotalporODT<-TotalporODT(PM_ts)
Desarrollo_TotalporODT<-TotalporODT(Desarrollo_ts)
SocialCnt_TotalporODT<-TotalporODT(SocialCnt_ts)

write.csv(BI_TotalporODT, "BI/LORD/BI_TotalporODT.csv", na="")   #Exportar datos 
write.csv(PM_TotalporODT, "PM/LORD/PM_TotalporODT.csv", na="")   
write.csv(Creativo_TotalporODT, "BI/LORD/Creativo/Creativo_TotalporODT.csv", na="")   
write.csv(Cuentas_TotalporODT, "Cuentas/LORD/Cuentas_TotalporODT.csv", na="")   
write.csv(Desarrollo_TotalporODT, "Desarrollo/LORD/Desarrollo_TotalporODT.csv", na="")   
write.csv(UX_TotalporODT, "UX/LORD/UX_TotalporODT.csv", na="")   
write.csv(SocialCnt_TotalporODT, "SocialCnt/LORD/SocialCnt_TotalporODT.csv", na="")   

#Total de horas por marca----
TotalporMarca <- function (data){
  TotalporMarca<-aggregate( Tiempo ~  Marca + Nombre, FUN=sum, data=data)
  return (TotalporMarca)
}


BI_TotalporMarca<-TotalporMarca(BI_ts)
Creativo_TotalporMarca<-TotalporMarca(Creativo_ts)
Cuentas_TotalporMarca<-TotalporMarca(Cuentas_ts)
UX_TotalporMarca<-TotalporMarca(UX_ts)
PM_TotalporMarca<-TotalporMarca(PM_ts)
Desarrollo_TotalporMarca<-TotalporMarca(Desarrollo_ts)
SocialCnt_TotalporMarca<-TotalporMarca(SocialCnt_ts)

write.csv(BI_TotalporMarca, "BI/LORD/BI_TotalporMarca.csv", na="")   #Exportamos datos a la carpeta almacenada
write.csv(PM_TotalporMarca, "PM/LORD/PM_TotalporMarca.csv", na="")   
write.csv(Creativo_TotalporMarca, "Creativo/LORD/Creativo_TotalporMarca.csv", na="")   
write.csv(Cuentas_TotalporMarca, "Cuentas/LORD/Cuentas_TotalporMarca.csv", na="")   
write.csv(Desarrollo_TotalporMarca, "Desarrollo/LORD/Desarrollo_TotalporMarca.csv", na="")   
write.csv(UX_TotalporMarca, "UX/LORD/UX_TotalporMarca.csv", na="")   
write.csv(SocialCnt_TotalporMarca, "SocialCnt/LORD/SocialCnt_TotalporMarca.csv", na="")   

#Funcion Horas por area 
AreaporODT <- function (data){
  AreaporODT <-aggregate( Tiempo ~  ODT, FUN=sum, data=data)
  return (AreaporODT)
}



BI_AreaporODT<-AreaporODT(BI_ts)  #Aplicamos la funcion Areapor ODT al dataframe BI_ts y lo guardamos en un nuevo llamado BI_AreaporODT
Creativo_AreaporODT<-AreaporODT(Creativo_ts)
Cuentas_AreaporODT<-AreaporODT(Cuentas_ts)
UX_AreaporODT<-AreaporODT(UX_ts)
PM_AreaporODT<-AreaporODT(PM_ts)
Desarrollo_AreaporODT<-AreaporODT(Desarrollo_ts)
SocialCnt_AreaporODT<-AreaporODT(SocialCnt_ts)

write.csv(BI_AreaporODT, "BI/LORD/BI_AreaporODT.csv", na="")   #Exportar dataframe en archivo .csv
write.csv(PM_AreaporODT, "PM/LORD/PM_AreaporODT.csv", na="")   
write.csv(Creativo_AreaporODT, "Creativo/LORD/Creativo_AreaporODT.csv", na="")   
write.csv(Cuentas_AreaporODT, "Cuentas/LORD/Cuentas_AreaporODT.csv", na="")   
write.csv(Desarrollo_AreaporODT, "Desarrollo/LORD/Desarrollo_AreaporODT.csv", na="")   
write.csv(UX_AreaporODT, "UX/LORD/UX_AreaporODT.csv", na="")   
write.csv(SocialCnt_AreaporODT, "SocialCnt/LORD/SocialCnt_AreaporODT.csv", na="")   

#Funcion Tiempo por marca
ArealporMarca <- function (data){   
  ArealporMarca<-aggregate( Tiempo ~  Marca, FUN=sum, data=data)
  return (ArealporMarca)
}


BI_ArealporMarca<-ArealporMarca(BI_ts) #Creacion de nuevo dataframe, almacena datos de la funcion aplicada
Creativo_ArealporMarca<-ArealporMarca(Creativo_ts)
Cuentas_ArealporMarca<-ArealporMarca(Cuentas_ts)
UX_ArealporMarca<-ArealporMarca(UX_ts)
PM_ArealporMarca<-ArealporMarca(PM_ts)
Desarrollo_ArealporMarca<-ArealporMarca(Desarrollo_ts)
SocialCnt_ArealporMarca<-ArealporMarca(SocialCnt_ts)

write.csv(BI_ArealporMarca, "BI/LORD/BI_ArealporMarca.csv", na="")   #Exportar datos
write.csv(PM_ArealporMarca, "PM/LORD/PM_ArealporMarca.csv", na="")   
write.csv(Creativo_ArealporMarca, "Creativo/LORD/Creativo_ArealporMarca.csv", na="")   
write.csv(Cuentas_ArealporMarca, "Cuentas/LORD/Cuentas_ArealporMarca.csv", na="")   
write.csv(Desarrollo_ArealporMarca, "Desarrollo/LORD/Desarrollo_ArealporMarca.csv", na="")   
write.csv(UX_ArealporMarca, "UX/LORD/UX_ArealporMarca.csv", na="")   
write.csv(SocialCnt_ArealporMarca, "SocialCnt/LORD/SocialCnt_ArealporMarca.csv", na="")   



# Total de horas por persona por mes--------------- 



  
  TotalporODTmes <- function (data){
  TotalporODTmes <-aggregate( Tiempo ~  ODT + Nombre + Mes, FUN=sum, data=data)
  return (TotalporODTmes)
}



BI_TotalporODTmes<-TotalporODTmes(BI_ts)
Creativo_TotalporODTmes<-TotalporODTmes(Creativo_ts)
Cuentas_TotalporODTmes<-TotalporODTmes(Cuentas_ts)
UX_TotalporODTmes<-TotalporODTmes(UX_ts)
PM_TotalporODTmes<-TotalporODTmes(PM_ts)
Desarrollo_TotalporODTmes<-TotalporODTmes(Desarrollo_ts)
SocialCnt_TotalporODTmes<-TotalporODTmes(SocialCnt_ts)

write.csv(BI_TotalporODTmes, "BI/LORD/BI_TotalporODTmes.csv", na="")   
write.csv(PM_TotalporODTmes, "PM/LORD/PM_TotalporODTmes.csv", na="")   
write.csv(Creativo_TotalporODTmes, "Creativo/LORD/Creativo_TotalporODTmes.csv", na="")   
write.csv(Cuentas_TotalporODTmes, "Cuentas/LORD/Cuentas_TotalporODTmes.csv", na="")   
write.csv(Desarrollo_TotalporODTmes, "Desarrollo/LORD/Desarrollo_TotalporODTmes.csv", na="")   
write.csv(UX_TotalporODTmes, "UX/LORD/UX_TotalporODTmes.csv", na="")   
write.csv(SocialCnt_TotalporODTmes, "SocialCnt/LORD/SocialCnt_TotalporODTmes.csv", na="")   


TotalporMarcaMes <- function (data){
  TotalporMarcaMes<-aggregate( Tiempo ~  Marca + Nombre+ ID + Mes, FUN=sum, data=data)
  TotalporMarcaMes$Horas<-TotalporMarcaMes$Tiempo/60
  return (TotalporMarcaMes)
}


BI_TotalporMarcaMes<-TotalporMarcaMes(BI_ts)
Creativo_TotalporMarcaMes<-TotalporMarcaMes(Creativo_ts)
Cuentas_TotalporMarcaMes<-TotalporMarcaMes(Cuentas_ts)
UX_TotalporMarcaMes<-TotalporMarcaMes(UX_ts)
PM_TotalporMarcaMes<-TotalporMarcaMes(PM_ts)
Desarrollo_TotalporMarcaMes<-TotalporMarcaMes(Desarrollo_ts)
SocialCnt_TotalporMarcaMes<-TotalporMarcaMes(SocialCnt_ts)

write.csv(BI_TotalporMarcaMes, "BI/LORD/BI_TotalporMarcaMes.csv", na="")   
write.csv(PM_TotalporMarcaMes, "PM/LORD/PM_TotalporMarcaMes.csv", na="")   
write.csv(Creativo_TotalporMarcaMes, "Creativo/LORD/Creativo_TotalporMarcaMes.csv", na="")   
write.csv(Cuentas_TotalporMarcaMes, "Cuentas/LORD/Cuentas_TotalporMarcaMes.csv", na="")   
write.csv(Desarrollo_TotalporMarcaMes, "Desarrollo/LORD/Desarrollo_TotalporMarcaMes.csv", na="")   
write.csv(UX_TotalporMarcaMes, "UX/LORD/UX_TotalporMarcaMes.csv", na="")   
write.csv(SocialCnt_TotalporMarcaMes, "SocialCnt/LORD/SocialCnt_TotalporMarcaMes.csv", na="")   

AreaporODTmes <- function (data){
  AreaporODTmes <-aggregate( Tiempo ~  ODT + Mes, FUN=sum, data=data)
  return (AreaporODTmes)
}



BI_AreaporODTmes<-AreaporODTmes(BI_ts)
Creativo_AreaporODTmes<-AreaporODTmes(Creativo_ts)
Cuentas_AreaporODTmes<-AreaporODTmes(Cuentas_ts)
UX_AreaporODTmes<-AreaporODTmes(UX_ts)
PM_AreaporODTmes<-AreaporODTmes(PM_ts)
Desarrollo_AreaporODTmes<-AreaporODTmes(Desarrollo_ts)
SocialCnt_AreaporODTmes<-AreaporODTmes(SocialCnt_ts)

write.csv(BI_AreaporODTmes, "BI/LORD/BI_AreaporODTmes.csv", na="")   
write.csv(PM_AreaporODTmes, "PM/LORD/PM_AreaporODTmes.csv", na="")   
write.csv(Creativo_AreaporODTmes, "Creativo/LORD/Creativo_AreaporODTmes.csv", na="")   
write.csv(Cuentas_AreaporODTmes, "Cuentas/LORD/Cuentas_AreaporODTmes.csv", na="")   
write.csv(Desarrollo_AreaporODTmes, "Desarrollo/LORD/Desarrollo_AreaporODTmes.csv", na="")   
write.csv(UX_AreaporODTmes, "UX/LORD/UX_AreaporODTmes.csv", na="")   
write.csv(SocialCnt_AreaporODTmes, "SocialCnt/LORD/SocialCnt_AreaporODTmes.csv", na="")   



ArealporMarcaMes <- function (data){
  ArealporMarcaMes<-aggregate( Tiempo ~  Marca + Mes, FUN=sum, data=data)
  return (ArealporMarcaMes)
}


BI_ArealporMarcaMes<-ArealporMarcaMes(BI_ts)
Creativo_ArealporMarcaMes<-ArealporMarcaMes(Creativo_ts)
Cuentas_ArealporMarcaMes<-ArealporMarcaMes(Cuentas_ts)
UX_ArealporMarcaMes<-ArealporMarcaMes(UX_ts)
PM_ArealporMarcaMes<-ArealporMarcaMes(PM_ts)
Desarrollo_ArealporMarcaMes<-ArealporMarcaMes(Desarrollo_ts)
SocialCnt_ArealporMarcaMes<-ArealporMarcaMes(SocialCnt_ts)

write.csv(BI_ArealporMarcaMes, "BI/LORD/BI_ArealporMarcaMes.csv", na="")   
write.csv(PM_ArealporMarcaMes, "PM/LORD/PM_ArealporMarcaMes.csv", na="")   
write.csv(Creativo_ArealporMarcaMes, "Creativo/LORD/Creativo_ArealporMarcaMes.csv", na="")   
write.csv(Cuentas_ArealporMarcaMes, "Cuentas/LORD/Cuentas_ArealporMarcaMes.csv", na="")   
write.csv(Desarrollo_ArealporMarcaMes, "Desarrollo/LORD/Desarrollo_ArealporMarcaMes.csv", na="")   
write.csv(UX_ArealporMarcaMes, "UX/LORD/UX_ArealporMarcaMes.csv", na="")   
write.csv(SocialCnt_ArealporMarcaMes, "SocialCnt/LORD/SocialCnt_ArealporMarcaMes.csv", na="")   


# Total de horas por persona por semana--------------- 


TotalporODTS <- function (data){
  TotalporODTS <-aggregate( Tiempo ~  ODT + Nombre + Semana, FUN=sum, data=data)
  return (TotalporODTS)
}

BI_TotalporODTS<-TotalporODTS(BI_ts)
Creativo_TotalporODTS<-TotalporODTS(Creativo_ts)
Cuentas_TotalporODTS<-TotalporODTS(Cuentas_ts)
UX_TotalporODTS<-TotalporODTS(UX_ts)
PM_TotalporODTS<-TotalporODTS(PM_ts)
Desarrollo_TotalporODTS<-TotalporODTS(Desarrollo_ts)
SocialCnt_TotalporODTS<-TotalporODTS(SocialCnt_ts)

write.csv(BI_TotalporODTS, "BI/LORD/BI_TotalporODTS.csv", na="")   
write.csv(PM_TotalporODTS, "PM/LORD/PM_TotalporODTS.csv", na="")   
write.csv(Creativo_TotalporODTS, "Creativo/LORD/Creativo_TotalporODTS.csv", na="")   
write.csv(Cuentas_TotalporODTS, "Cuentas/LORD/Cuentas_TotalporODTS.csv", na="")   
write.csv(Desarrollo_TotalporODTS, "Desarrollo/LORD/Desarrollo_TotalporODTS.csv", na="")   
write.csv(UX_TotalporODTS, "UX/LORD/UX_TotalporODTS.csv", na="")   
write.csv(SocialCnt_TotalporODTS, "SocialCnt/LORD/SocialCnt_TotalporODTS.csv", na="")   


TotalporMarcaS <- function (data){
  TotalporMarcaS<-aggregate( Tiempo ~  Marca + Nombre + Semana, FUN=sum, data=data)
  return (TotalporMarcaS)
}

BI_TotalporMarcaS<-TotalporMarcaS(BI_ts)
Creativo_TotalporMarcaS<-TotalporMarcaS(Creativo_ts)
Cuentas_TotalporMarcaS<-TotalporMarcaS(Cuentas_ts)
UX_TotalporMarcaS<-TotalporMarcaS(UX_ts)
PM_TotalporMarcaS<-TotalporMarcaS(PM_ts)
Desarrollo_TotalporMarcaS<-TotalporMarcaS(Desarrollo_ts)
SocialCnt_TotalporMarcaS<-TotalporMarcaS(SocialCnt_ts)

write.csv(BI_TotalporMarcaS, "BI/LORD/BI_TotalporMarcaS.csv", na="")   
write.csv(PM_TotalporMarcaS, "PM/LORD/PM_TotalporMarcaS.csv", na="")   
write.csv(Creativo_TotalporMarcaS, "Creativo/LORD/Creativo_TotalporMarcaS.csv", na="")   
write.csv(Cuentas_TotalporMarcaS, "Cuentas/LORD/Cuentas_TotalporMarcaS.csv", na="")   
write.csv(Desarrollo_TotalporMarcaS, "Desarrollo/LORD/Desarrollo_TotalporMarcaS.csv", na="")   
write.csv(UX_TotalporMarcaS, "UX/LORD/UX_TotalporMarcaS.csv", na="")   
write.csv(SocialCnt_TotalporMarcaS, "SocialCnt/LORD/SocialCnt_TotalporMarcaS.csv", na="")   



TotalporMarcaSYM <- function (data){
  TotalporMarcaSYM<-aggregate( Tiempo ~  Marca + Nombre + Semana + Mes, FUN=sum, data=data)
  return (TotalporMarcaSYM)
}

BI_TotalporMarcaSYM<-TotalporMarcaSYM(BI_ts)
Creativo_TotalporMarcaSYM<-TotalporMarcaSYM(Creativo_ts)
Cuentas_TotalporMarcaSYM<-TotalporMarcaSYM(Cuentas_ts)
UX_TotalporMarcaSYM<-TotalporMarcaSYM(UX_ts)
PM_TotalporMarcaSYM<-TotalporMarcaSYM(PM_ts)
Desarrollo_TotalporMarcaSYM<-TotalporMarcaSYM(Desarrollo_ts)
SocialCnt_TotalporMarcaSYM<-TotalporMarcaSYM(SocialCnt_ts)

write.csv(BI_TotalporMarcaSYM, "BI/LORD/BI_TotalporMarcaSYM.csv", na="")   
write.csv(PM_TotalporMarcaSYM, "PM/LORD/PM_TotalporMarcaSYM.csv", na="")   
write.csv(Creativo_TotalporMarcaSYM, "Creativo/LORD/Creativo_TotalporMarcaSYM.csv", na="")   
write.csv(Cuentas_TotalporMarcaSYM, "Cuentas/LORD/Cuentas_TotalporMarcaSYM.csv", na="")   
write.csv(Desarrollo_TotalporMarcaSYM, "Desarrollo/LORD/Desarrollo_TotalporMarcaSYM.csv", na="")   
write.csv(UX_TotalporMarcaSYM, "UX/LORD/UX_TotalporMarcaSYM.csv", na="")   
write.csv(SocialCnt_TotalporMarcaSYM, "SocialCnt/LORD/SocialCnt_TotalporMarcaSYM.csv", na="")   


AreaporODTS <- function (data){
  AreaporODTS <-aggregate( Tiempo ~  ODT + Semana, FUN=sum, data=data)
  return (AreaporODTS)
}

BI_AreaporODTS<-AreaporODTS(BI_ts)
Creativo_AreaporODTS<-AreaporODTS(Creativo_ts)
Cuentas_AreaporODTS<-AreaporODTS(Cuentas_ts)
UX_AreaporODTS<-AreaporODTS(UX_ts)
PM_AreaporODTS<-AreaporODTS(PM_ts)
Desarrollo_AreaporODTS<-AreaporODTS(Desarrollo_ts)
SocialCnt_AreaporODTS<-AreaporODTS(SocialCnt_ts)

write.csv(BI_AreaporODTS, "BI/LORD/BI_AreaporODTS.csv", na="")   
write.csv(PM_AreaporODTS, "PM/LORD/PM_AreaporODTS.csv", na="")   
write.csv(Creativo_AreaporODTS, "Creativo/LORD/Creativo_AreaporODTS.csv", na="")   
write.csv(Cuentas_AreaporODTS, "Cuentas/LORD/Cuentas_AreaporODTS.csv", na="")   
write.csv(Desarrollo_AreaporODTS, "Desarrollo/LORD/Desarrollo_AreaporODTS.csv", na="")   
write.csv(UX_AreaporODTS, "UX/LORD/UX_AreaporODTS.csv", na="")   
write.csv(SocialCnt_AreaporODTS, "SocialCnt/LORD/SocialCnt_AreaporODTS.csv", na="")   

AreaporMarcaS <- function (data){
  AreaporMarcaS<-aggregate( Tiempo ~  Marca + Semana, FUN=sum, data=data)
  return (AreaporMarcaS)
}

BI_AreaporMarcaS<-AreaporMarcaS(BI_ts)
Creativo_AreaporMarcaS<-AreaporMarcaS(Creativo_ts)
Cuentas_AreaporMarcaS<-AreaporMarcaS(Cuentas_ts)
UX_AreaporMarcaS<-AreaporMarcaS(UX_ts)
PM_AreaporMarcaS<-AreaporMarcaS(PM_ts)
Desarrollo_AreaporMarcaS<-AreaporMarcaS(Desarrollo_ts)
SocialCnt_AreaporMarcaS<-AreaporMarcaS(SocialCnt_ts)

write.csv(BI_AreaporMarcaS, "BI/LORD/BI_AreaporMarcaS.csv", na="")   
write.csv(PM_AreaporMarcaS, "PM/LORD/PM_AreaporMarcaS.csv", na="")   
write.csv(Creativo_AreaporMarcaS, "Creativo/LORD/Creativo_AreaporMarcaS.csv", na="")   
write.csv(Cuentas_AreaporMarcaS, "Cuentas/LORD/Cuentas_AreaporMarcaS.csv", na="")   
write.csv(Desarrollo_AreaporMarcaS, "Desarrollo/LORD/Desarrollo_AreaporMarcaS.csv", na="")   
write.csv(UX_AreaporMarcaS, "UX/LORD/UX_AreaporMarcaS.csv", na="")   
write.csv(SocialCnt_AreaporMarcaS, "SocialCnt/LORD/SocialCnt_AreaporMarcaS.csv", na="")   


# Total de horas por persona por semana & mes--------------- 


TotalTiempoPersonaS <- function (data){
  TotalTiempoPersonaS <-aggregate( Tiempo ~  Nombre + Semana, FUN=sum, data=data)
  TotalTiempoPersonaS$Tiempo <-TotalTiempoPersonaS$Tiempo/60
  return (TotalTiempoPersonaS)
}

TotalTiempoPersonaM <- function (data){
  TotalTiempoPersonaM <-aggregate( Tiempo ~  Nombre + Mes, FUN=sum, data=data)
  TotalTiempoPersonaM$Horas<-TotalTiempoPersonaM$Tiempo/60
  
  return (TotalTiempoPersonaM)
}


BI_TotalTiempoPersonaS<-TotalTiempoPersonaS(BI_ts)
Creativo_TotalTiempoPersonaS<-TotalTiempoPersonaS(Creativo_ts)
Cuentas_TotalTiempoPersonaS<-TotalTiempoPersonaS(Cuentas_ts)
UX_TotalTiempoPersonaS<-TotalTiempoPersonaS(UX_ts)
PM_TotalTiempoPersonaS<-TotalTiempoPersonaS(PM_ts)
Desarrollo_TotalTiempoPersonaS<-TotalTiempoPersonaS(Desarrollo_ts)
SocialCnt_TotalTiempoPersonaS<-TotalTiempoPersonaS(SocialCnt_ts)

write.csv(BI_TotalTiempoPersonaS, "BI/LORD/BI_TotalTiempoPersonaS.csv", na="")   
write.csv(PM_TotalTiempoPersonaS, "PM/LORD/PM_TotalTiempoPersonaS.csv", na="")   
write.csv(Creativo_TotalTiempoPersonaS, "Creativo/LORD/Creativo_TotalTiempoPersonaS.csv", na="")   
write.csv(Cuentas_TotalTiempoPersonaS, "Cuentas/LORD/Cuentas_TotalTiempoPersonaS.csv", na="")   
write.csv(Desarrollo_TotalTiempoPersonaS, "Desarrollo/LORD/Desarrollo_TotalTiempoPersonaS.csv", na="")   
write.csv(UX_TotalTiempoPersonaS, "UX/LORD/UX_TotalTiempoPersonaS.csv", na="")   
write.csv(SocialCnt_TotalTiempoPersonaS, "SocialCnt/LORD/SocialCnt_TotalTiempoPersonaS.csv", na="")   


BI_TotalTiempoPersonaM<-TotalTiempoPersonaM(BI_ts)
Creativo_TotalTiempoPersonaM<-TotalTiempoPersonaM(Creativo_ts)
Cuentas_TotalTiempoPersonaM<-TotalTiempoPersonaM(Cuentas_ts)
UX_TotalTiempoPersonaM<-TotalTiempoPersonaM(UX_ts)
PM_TotalTiempoPersonaM<-TotalTiempoPersonaM(PM_ts)
Desarrollo_TotalTiempoPersonaM<-TotalTiempoPersonaM(Desarrollo_ts)
SocialCnt_TotalTiempoPersonaM<-TotalTiempoPersonaM(SocialCnt_ts)

write.csv(BI_TotalTiempoPersonaM, "BI/LORD/BI_TotalTiempoPersonaM.csv", na="")   
write.csv(PM_TotalTiempoPersonaM, "PM/LORD/PM_TotalTiempoPersonaM.csv", na="")   
write.csv(Creativo_TotalTiempoPersonaM, "Creativo/LORD/Creativo_TotalTiempoPersonaM.csv", na="")   
write.csv(Cuentas_TotalTiempoPersonaM, "Cuentas/LORD/Cuentas_TotalTiempoPersonaM.csv", na="")   
write.csv(Desarrollo_TotalTiempoPersonaM, "Desarrollo/LORD/Desarrollo_TotalTiempoPersonaM.csv", na="")   
write.csv(UX_TotalTiempoPersonaM, "UX/LORD/UX_TotalTiempoPersonaM.csv", na="")   
write.csv(SocialCnt_TotalTiempoPersonaM, "SocialCnt/LORD/SocialCnt_TotalTiempoPersonaM.csv", na="")   



# TOTAL $ BI --------------- 
TotalporMarcaMesYSEM <- function (data){
  TotalporMarcaMesYSEM<-aggregate( Tiempo ~  Marca + Nombre+ ID + Mes + Semana, FUN=sum, data=data)
  return (TotalporMarcaMesYSEM)
}



Money <- function (totalPNL){
  Money<-aggregate( Money ~  Marca + Puesto + Mes, FUN=sum, data=totalPNL)
  return (Money)
}



BI_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(BI_ts)
Creativo_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(Creativo_ts)
Cuentas_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(Cuentas_ts)
UX_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(UX_ts)
PM_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(PM_ts)
Desarrollo_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(Desarrollo_ts)
SocialCnt_TotalporMarcaMesYSEM<-TotalporMarcaMesYSEM(SocialCnt_ts)



BI_totalPNL<-merge(BI_TotalporMarcaMesYSEM, subset(BI_rate, select = -Nombre), by=c("ID"))
BI_totalPNL<- merge(BI_TotalporMarcaMes, subset(BI_rate, select = -Nombre), by=c("ID")) #merge
BI_totalPNL$Money<-BI_totalPNL$Rate*BI_totalPNL$Horas

Creativo_totalPNL<-merge(Creativo_TotalporMarcaMesYSEM, subset(Creativo_rate, select = -Nombre), by=c("ID"))
Creativo_totalPNL<- merge(Creativo_TotalporMarcaMes, subset(Creativo_rate, select = -Nombre), by=c("ID")) #merge
Creativo_totalPNL$Money<-Creativo_totalPNL$Rate*Creativo_totalPNL$Horas


Cuentas_totalPNL<-merge(Cuentas_TotalporMarcaMesYSEM, subset(Cuentas_rate, select = -Nombre), by=c("ID"))
Cuentas_totalPNL<- merge(Cuentas_TotalporMarcaMes, subset(Cuentas_rate, select = -Nombre), by=c("ID")) #merge
Cuentas_totalPNL$Money<-Cuentas_totalPNL$Rate*Cuentas_totalPNL$Horas

UX_totalPNL<-merge(UX_TotalporMarcaMesYSEM, subset(UX_rate, select = -Nombre), by=c("ID"))
UX_totalPNL<- merge(UX_TotalporMarcaMes, subset(UX_rate, select = -Nombre), by=c("ID")) #merge
UX_totalPNL$Money<-UX_totalPNL$Rate*UX_totalPNL$Horas

PM_totalPNL<-merge(PM_TotalporMarcaMesYSEM, subset(PM_rate, select = -Nombre), by=c("ID"))
PM_totalPNL<- merge(PM_TotalporMarcaMes, subset(PM_rate, select = -Nombre), by=c("ID")) #merge
PM_totalPNL$Money<-PM_totalPNL$Rate*PM_totalPNL$Horas

Desarrollo_totalPNL<-merge(Desarrollo_TotalporMarcaMesYSEM, subset(Desarrollo_rate, select = -Nombre), by=c("ID"))
Desarrollo_totalPNL<- merge(Desarrollo_TotalporMarcaMes, subset(Desarrollo_rate, select = -Nombre), by=c("ID")) #merge
Desarrollo_totalPNL$Money<-Desarrollo_totalPNL$Rate*Desarrollo_totalPNL$Horas

SocialCnt_totalPNL<-merge(SocialCnt_TotalporMarcaMesYSEM, subset(SocialCnt_rate, select = -Nombre), by=c("ID"))
SocialCnt_totalPNL<- merge(SocialCnt_TotalporMarcaMes, subset(SocialCnt_rate, select = -Nombre), by=c("ID")) #merge
SocialCnt_totalPNL$Money<-SocialCnt_totalPNL$Rate*SocialCnt_totalPNL$Horas

Money <- function (totalPNL){
  Money<-aggregate( Money ~  Marca + Puesto + Mes, FUN=sum, data=totalPNL)
  return (Money)
}

BI_Money<-Money(BI_totalPNL)
SocialCnt_Money<-Money(SocialCnt_totalPNL)
Cuentas_Money<-Money(Cuentas_totalPNL)
PM_Money<-Money(PM_totalPNL)
UX_Money<-Money(UX_totalPNL)
Desarrollo_Money<-Money(Desarrollo_totalPNL)
Creativo_Money<-Money(Creativo_totalPNL)



write.csv(BI_TotalporMarcaMesYSEM, "BI/LORD/BI_TotalporMarcaMesYSEM.csv", na="")   
write.csv(PM_TotalporMarcaMesYSEM, "PM/LORD/PM_TotalporMarcaMesYSEM.csv", na="")   
write.csv(Creativo_TotalporMarcaMesYSEM, "Creativo/LORD/Creativo_TotalporMarcaMesYSEM.csv", na="")   
write.csv(Cuentas_TotalporMarcaMesYSEM, "Cuentas/LORD/Cuentas_TotalporMarcaMesYSEM.csv", na="")   
write.csv(Desarrollo_TotalporMarcaMesYSEM, "Desarrollo/LORD/Desarrollo_TotalporMarcaMesYSEM.csv", na="")   
write.csv(UX_TotalporMarcaMesYSEM, "UX/LORD/UX_TotalporMarcaMesYSEM.csv", na="")   
write.csv(SocialCnt_TotalporMarcaMesYSEM, "SocialCnt/LORD/SocialCnt_TotalporMarcaMesYSEM.csv", na="")   

write.csv(BI_totalPNL, "BI/LORD/BI_totalPNL.csv", na="")   
write.csv(PM_totalPNL, "PM/LORD/PM_totalPNL.csv", na="")   
write.csv(Creativo_totalPNL, "Creativo/LORD/Creativo_totalPNL.csv", na="")   
write.csv(Cuentas_totalPNL, "Cuentas/LORD/Cuentas_totalPNL.csv", na="")   
write.csv(Desarrollo_totalPNL, "Desarrollo/LORD/Desarrollo_totalPNL.csv", na="")   
write.csv(UX_totalPNL, "UX/LORD/UX_totalPNL.csv", na="")   
write.csv(SocialCnt_totalPNL, "SocialCnt/LORD/SocialCnt_totalPNL.csv", na="")   

# TOTAL $ RC --------------- 


totalRCMoney<- function(lordrc, rate){
  totalRCMoney<-merge(lordrc, subset(rate, select = c(-Nombre,-ID), by=c("Puesto")))
  totalRCMoney$Money<-totalRCMoney$Ratecard*totalRCMoney$Rate
  return (totalRCMoney)
}

MoneyRC<- function(totalRCMoney){
  MoneyRC<-aggregate( Money ~  Marca + Puesto + Mes, FUN=sum, data=totalRCMoney)
  return (MoneyRC)
}

Utilidad<- function(Money, MoneyRC){
  MoneyBI<-sum(MoneyRC$Money)-sum(Money$Money)
  return (MoneyBI)
}


BI_totalRCMoney<-totalRCMoney(BI_rc, BI_rate)
Creativo_totalRCMoney<-totalRCMoney(Creativo_rc, Creativo_rate)
Cuentas_totalRCMoney<-totalRCMoney(Cuentas_rc, Cuentas_rate)
UX_totalRCMoney<-totalRCMoney(UX_rc, UX_rate)
PM_totalRCMoney<-totalRCMoney(PM_rc, PM_rate)
Desarrollo_totalRCMoney<-totalRCMoney(Desarrollo_rc, Desarrollo_rate)
SocialCnt_totalRCMoney<-totalRCMoney(SocialCnt_rc, SocialCnt_rate)

BI_MoneyRC<-MoneyRC(BI_totalRCMoney)
Creativo_MoneyRC<-MoneyRC(Creativo_totalRCMoney)
Cuentas_MoneyRC<-MoneyRC(Cuentas_totalRCMoney)
UX_MoneyRC<-MoneyRC(UX_totalRCMoney)
PM_MoneyRC<-MoneyRC(PM_totalRCMoney)
Desarrollo_MoneyRC<-MoneyRC(Desarrollo_totalRCMoney)
SocialCnt_MoneyRC<-MoneyRC(SocialCnt_totalRCMoney)


BI_Utilidad<-Utilidad(BI_Money, BI_MoneyRC)
Creativo_Utilidad<-Utilidad(Creativo_Money, Creativo_MoneyRC)
Cuentas_Utilidad<-Utilidad(Cuentas_Money, Cuentas_MoneyRC)
UX_Utilidad<-Utilidad(UX_Money, UX_MoneyRC)
PM_Utilidad<-Utilidad(PM_Money, PM_MoneyRC)
Desarrollo_Utilidad<-Utilidad(Desarrollo_Money, Desarrollo_MoneyRC)
SocialCnt_Utilidad<-Utilidad(SocialCnt_Money, SocialCnt_MoneyRC)



write.csv(BI_MoneyRC, "BI/LORD/BI_MoneyRC.csv", na="")   
write.csv(PM_MoneyRC, "PM/LORD/PM_MoneyRC.csv", na="")   
write.csv(Creativo_MoneyRC, "Creativo/LORD/Creativo_MoneyRC.csv", na="")   
write.csv(Cuentas_MoneyRC, "Cuentas/LORD/Cuentas_MoneyRC.csv", na="")   
write.csv(Desarrollo_MoneyRC, "Desarrollo/LORD/Desarrollo_MoneyRC.csv", na="")   
write.csv(UX_MoneyRC, "UX/LORD/UX_MoneyRC.csv", na="")   
write.csv(SocialCnt_MoneyRC, "SocialCnt/LORD/SocialCnt_MoneyRC.csv", na="")   

write.csv(BI_Utilidad, "BI/LORD/BI_Utilidad.csv", na="")   
write.csv(PM_Utilidad, "PM/LORD/PM_Utilidad.csv", na="")   
write.csv(Creativo_Utilidad, "Creativo/LORD/Creativo_Utilidad.csv", na="")   
write.csv(Cuentas_Utilidad, "Cuentas/LORD/Cuentas_Utilidad.csv", na="")   
write.csv(Desarrollo_Utilidad, "Desarrollo/LORD/Desarrollo_Utilidad.csv", na="")   
write.csv(UX_Utilidad, "UX/LORD/UX_Utilidad.csv", na="")   
write.csv(SocialCnt_Utilidad, "SocialCnt/LORD/SocialCnt_Utilidad.csv", na="") 

            

#### Cuáles ODT son las que ocupan mas tiempo por persona--------------- 

top3ODT<- function(TotalporODT){
  top3ODT<-ddply(TotalporODT,'Nombre',function(TotalporODT)TotalporODT[order(TotalporODT$Tiempo,decreasing=TRUE)[1:3],])
  top3ODT <- na.omit(top3ODT)
  return (top3ODT)
}

BI_top3ODT<-top3ODT(BI_TotalporODT)
SocialCnt_top3ODT<-top3ODT(SocialCnt_TotalporODT)
Cuentas_top3ODT<-top3ODT(Cuentas_TotalporODT)
PM_top3ODT<-top3ODT(PM_TotalporODT)
UX_top3ODT<-top3ODT(UX_TotalporODT)
Desarrollo_top3ODT<-top3ODT(Desarrollo_TotalporODT)
Creativo_top3ODT<-top3ODT(Creativo_TotalporODT)


write.csv(BI_top3ODT, "BI/LORD/BI_top3ODT.csv", na="")   
write.csv(PM_top3ODT, "PM/LORD/PM_top3ODT.csv", na="")   
write.csv(Creativo_top3ODT, "Creativo/LORD/Creativo_top3ODT.csv", na="")   
write.csv(Cuentas_top3ODT, "Cuentas/LORD/Cuentas_top3ODT.csv", na="")   
write.csv(Desarrollo_top3ODT, "Desarrollo/LORD/Desarrollo_top3ODT.csv", na="")   
write.csv(UX_top3ODT, "UX/LORD/UX_top3ODT.csv", na="")   
write.csv(SocialCnt_top3ODT, "SocialCnt/LORD/SocialCnt_top3ODT.csv", na="")   



# KPI BI


KPI<- function( lordrc, totalPNL, rate){
  KPI<-data.frame(
    Mes = unique(totalPNL$Mes),
    TotalOcupacion = NA)
  intArea<-nrow(rate)
  KPI$TotalOcupacion<-160*intArea
  OcupacionArea<-aggregate( Horas ~ Mes, FUN=sum, data=totalPNL)
  KPIBI<- merge(KPI, OcupacionArea, by=c("Mes")) #merge
  KPIBI$dispOcp<-percent(as.numeric(KPIBI$Horas/KPIBI$TotalOcupacion))
  Asignacion<-aggregate( Ratecard ~ Mes, FUN=sum, data=lordrc)
  KPIBI<- merge(KPIBI, Asignacion, by=c("Mes")) #merge
  KPIBI$dispAsign<-percent(as.numeric(KPIBI$Ratecard/KPIBI$TotalOcupacion))
  return (KPIBI)
}

BI_KPI<-KPI(BI_rc, BI_totalPNL, BI_rate)
Creativo_KPI<-KPI(Creativo_rc, Creativo_totalPNL, Creativo_rate)
Cuentas_KPI<-KPI(Cuentas_rc, Cuentas_totalPNL, Cuentas_rate)
UX_KPI<-KPI(UX_rc, UX_totalPNL, UX_rate)
PM_KPI<-KPI(PM_rc, PM_totalPNL, PM_rate)
Desarrollo_KPI<-KPI(Desarrollo_rc, Desarrollo_totalPNL, Desarrollo_rate)
SocialCnt_KPI<-KPI(SocialCnt_rc, SocialCnt_totalPNL, SocialCnt_rate)


write.csv(BI_KPI, "BI/LORD/BI_KPI.csv", na="")   
write.csv(PM_KPI, "PM/LORD/PM_KPI.csv", na="")   
write.csv(Creativo_KPI, "Creativo/LORD/Creativo_KPI.csv", na="")   
write.csv(Cuentas_KPI, "Cuentas/LORD/Cuentas_KPI.csv", na="")   
write.csv(Desarrollo_KPI, "Desarrollo/LORD/Desarrollo_KPI.csv", na="")   
write.csv(UX_KPI, "UX/LORD/UX_KPI.csv", na="")   
write.csv(SocialCnt_KPI, "SocialCnt/LORD/SocialCnt_KPI.csv", na="") 


#ByBrand #ByUser


OcupacionUser<- function(totalPNL){
  OcupacionUser<- subset(totalPNL,select=c(-Rate, -Money))
  OcupacionUser$Puesto<- as.character(OcupacionUser$Puesto)
  OcupacionUser$ID<-as.numeric(as.character(OcupacionUser$ID))
  return (OcupacionUser)
}

BI_OcupacionUser<-OcupacionUser(BI_totalPNL)
Creativo_OcupacionUser<-OcupacionUser(Creativo_totalPNL)
Cuentas_OcupacionUser<-OcupacionUser(Cuentas_totalPNL)
UX_OcupacionUser<-OcupacionUser(UX_totalPNL)
PM_OcupacionUser<-OcupacionUser(PM_totalPNL)
Desarrollo_OcupacionUser<-OcupacionUser(Desarrollo_totalPNL)
SocialCnt_OcupacionUser<-OcupacionUser(SocialCnt_totalPNL)


BI_test<- BI_OcupacionUser
Creativo_test<- Creativo_OcupacionUser
Cuentas_test<- Cuentas_OcupacionUser
UX_test<- UX_OcupacionUser
PM_test<- PM_OcupacionUser
SocialCnt_test<- SocialCnt_OcupacionUser
Desarrollo_test<- Desarrollo_OcupacionUser



BI_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=BI_rc)
Desarrollo_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=Desarrollo_rc)
Creativo_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=Creativo_rc)
SocialCnt_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=SocialCnt_rc)
Cuentas_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=Cuentas_rc)
UX_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=UX_rc)
PM_testrc<-aggregate( Ratecard ~ Mes + Marca + ID, FUN=sum, data=PM_rc)

#### AND COUNT! 

test <-function (test, testrc){
  test<-full_join(x=test,y=testrc, by=c("Marca","ID","Mes"), all=TRUE)
  test[c("Ratecard","Tiempo","Horas")][is.na(test[c("Ratecard","Tiempo","Horas")])]<-0
  return (test)
}

BI_test<-test(BI_test, BI_testrc)
PM_test<-test(PM_test, PM_testrc)
UX_test<-test(UX_test, UX_testrc)
Creativo_test<-test(Creativo_test, Creativo_testrc)
Cuentas_test<-test(Cuentas_test, Cuentas_testrc)
SocialCnt_test<-test(SocialCnt_test, SocialCnt_testrc)
Desarrollo_test<-test(Desarrollo_test, Desarrollo_testrc)



base <-function (rate){
  base<-subset(rate, select = c(-Rate))
  base$Nombre<-trimws(as.character(base$Nombre))
  base$Puesto<-as.character(base$Puesto)
  base$ID<-as.numeric(as.character(base$ID))
  return (base)
}

BI_base<-base(BI_rate)
SocialCnt_base<-base(SocialCnt_rate)
Desarrollo_base<-base(Desarrollo_rate)
Cuentas_base<-base(Cuentas_rate)
Creativo_base<-base(Creativo_rate)
PM_base<-base(PM_rate)
UX_base<-base(UX_rate)



OcupacionUserFull <-function(testvar, base)
{
  for(i in 1:nrow(testvar)[1]) {
    if(is.na(testvar$Nombre[i])){
      testvar$Nombre[i]<-base$Nombre[testvar$ID[i]==base$ID]
    }
    if(is.na(testvar$Puesto[i])){
      testvar$Puesto[i]<-base$Puesto[testvar$ID[i]==base$ID]
    }
  }
  
  testvar$tipo<-ifelse (testvar$Ratecard==0, "NO ASIGNADA", "ASIGNADA")
  testvar$Nombre<-trimws(testvar$Nombre)
  OcupacionUser<-testvar
  
  OcupacionUser$difRC<- OcupacionUser$Horas-OcupacionUser$Ratecard
  return (OcupacionUser)
}

BI_OcupacionUserFull<-OcupacionUserFull(BI_test, BI_base)
SocialCnt_OcupacionUserFull<-OcupacionUserFull(SocialCnt_test, SocialCnt_base)
UX_OcupacionUserFull<-OcupacionUserFull(UX_test, UX_base)
PM_OcupacionUserFull<-OcupacionUserFull(PM_test, PM_base)
Cuentas_OcupacionUserFull<-OcupacionUserFull(Cuentas_test, Cuentas_base)
Creativo_OcupacionUserFull<-OcupacionUserFull(Creativo_test, Creativo_base)
Desarrollo_OcupacionUserFull<-OcupacionUserFull(Desarrollo_test, Desarrollo_base)


write.csv(BI_OcupacionUserFull, "BI/LORD/BI_OcupacionUserFull.csv", na="")   
write.csv(PM_OcupacionUserFull, "PM/LORD/PM_OcupacionUserFull.csv", na="")   
write.csv(Creativo_OcupacionUserFull, "Creativo/LORD/Creativo_OcupacionUserFull.csv", na="")   
write.csv(Cuentas_OcupacionUserFull, "Cuentas/LORD/Cuentas_OcupacionUserFull.csv", na="")   
write.csv(Desarrollo_OcupacionUserFull, "Desarrollo/LORD/Desarrollo_OcupacionUserFull.csv", na="")   
write.csv(UX_OcupacionUserFull, "UX/LORD/UX_OcupacionUserFull.csv", na="")   
write.csv(SocialCnt_OcupacionUserFull, "SocialCnt/LORD/SocialCnt_OcupacionUserFull.csv", na="") 




demo<- function(OcupacionUserFull)
{
  demo<- subset(OcupacionUserFull,select=c(Mes,Nombre, Puesto,  Horas, Ratecard))
  demo<-aggregate(. ~ Mes + Nombre + Puesto,FUN=sum, data=demo )
  demo$dispMes<-160
  demo$dispvsTrab<-demo$dispMes - demo$Horas
  demo$dispovsAsg<-demo$Ratecard- demo$Horas
  demo$porAsign<-demo$dispMes-demo$Ratecard
  return(demo)
}

BI_OcupacionUserDemo<-demo(BI_OcupacionUserFull)
Creativo_OcupacionUserDemo<-demo(Creativo_OcupacionUserFull)
Cuentas_OcupacionUserDemo<-demo(Cuentas_OcupacionUserFull)
UX_OcupacionUserDemo<-demo(UX_OcupacionUserFull)
PM_OcupacionUserDemo<-demo(PM_OcupacionUserFull)
Desarrollo_OcupacionUserDemo<-demo(Desarrollo_OcupacionUserFull)
SocialCnt_OcupacionUserDemo<-demo(SocialCnt_OcupacionUserFull)




write.csv(BI_OcupacionUserDemo, "BI/LORD/BI_OcupacionUserDemo.csv", na="")   
write.csv(PM_OcupacionUserDemo, "PM/LORD/PM_OcupacionUserDemo.csv", na="")   
write.csv(Creativo_OcupacionUserDemo, "Creativo/LORD/Creativo_OcupacionUserDemo.csv", na="")   
write.csv(Cuentas_OcupacionUserDemo, "Cuentas/LORD/Cuentas_OcupacionUserDemo.csv", na="")   
write.csv(Desarrollo_OcupacionUserDemo, "Desarrollo/LORD/Desarrollo_OcupacionUserDemo.csv", na="")   
write.csv(UX_OcupacionUserDemo, "UX/LORD/UX_OcupacionUserDemo.csv", na="")   
write.csv(SocialCnt_OcupacionUserDemo, "SocialCnt/LORD/SocialCnt_OcupacionUserDemo.csv", na="") 






# TOTAL Asignadas vs trabajadas RC --------------- 
asignadas <- function(OcupacionUserFull){
  asignadas<- subset(OcupacionUserFull,select=c(Mes, Marca, tipo, Horas, Ratecard))
  
  asignadas<-aggregate( . ~  Mes + Marca + tipo , FUN=sum, data=asignadas)
  
  asignadasUser<- subset(OcupacionUserFull,select=c(Mes, Puesto, tipo, Horas, Ratecard))
  
  asignadasUser<-aggregate( . ~  Mes + Puesto + tipo , FUN=sum, data=asignadasUser)
  
  return(asignadasUser)
}




BI_asignadas<-asignadas(BI_OcupacionUserFull)
Creativo_asignadas<-asignadas(Creativo_OcupacionUserFull)
Cuentas_asignadas<-asignadas(Cuentas_OcupacionUserFull)
UX_asignadas<-asignadas(UX_OcupacionUserFull)
PM_asignadas<-asignadas(PM_OcupacionUserFull)
Desarrollo_asignadas<-asignadas(Desarrollo_OcupacionUserFull)
SocialCnt_asignadas<-asignadas(SocialCnt_OcupacionUserFull)


write.csv(BI_asignadas, "BI/LORD/BI_asignadas.csv", na="")   
write.csv(PM_asignadas, "PM/LORD/PM_asignadas.csv", na="")   
write.csv(Creativo_asignadas, "Creativo/LORD/Creativo_asignadas.csv", na="")   
write.csv(Cuentas_asignadas, "Cuentas/LORD/Cuentas_asignadas.csv", na="")   
write.csv(Desarrollo_asignadas, "Desarrollo/LORD/Desarrollo_asignadas.csv", na="")   
write.csv(UX_asignadas, "UX/LORD/UX_asignadas.csv", na="")   
write.csv(SocialCnt_asignadas, "SocialCnt/LORD/SocialCnt_asignadas.csv", na="") 



### SEMANA COMPLETA


SemanaFull<-function(TotalTiempoPersonaS){
  
  try<-TotalTiempoPersonaS
  out<-data.frame()
  try2<-data.frame()
  
  for(i in unique(try$Nombre)){
    print(i)
    week<-try[ which(try$Nombre==i), ]
    wmin<-min(week$Semana)
    wmax<-max(week$Semana)
    csem<-length(week$Semana)
    fmin<-as.numeric(min(try$Semana))
    fmax<-as.numeric(max(try$Semana))
    cfil<-(fmax-fmin)+1
    week$Semana<-as.numeric(week$Semana)
    print(csem)
    print(cfil)
    if(csem!=cfil)
    {
      DF.NEW <- data.frame(Semana = seq(fmax)) 
      new<-data.frame(Semana = DF.NEW[DF.NEW$Semana>=fmin,])
      new<-as.numeric(new$Semana)
      new<- subset(new, !(new %in% week$Semana))
      new<-data.frame(Semana=new)
      new$Tiempo<-0
      new$Nombre<-unique(try$Nombre[try$Nombre==i])
      
      try2 <- rbind(week,new)
      
      print("dif!, TRY PRINT")
      print(try)
    } else if(csem==cfil) {
      out<-rbind(week, out)
      print("esta completo, OUT PRINT")
      
    }
    print("fuera!, OUT PRINT")
    out<-rbind(out,try2)
    
  }
  
  out<-out %>% distinct(Nombre, Semana, .keep_all = TRUE)
  TotalTiempoPersonaS<-out
  
}


BI_SemanaFull<-SemanaFull(BI_TotalTiempoPersonaS)
Creativo_SemanaFull<-SemanaFull(Creativo_TotalTiempoPersonaS)
Cuentas_SemanaFull<-SemanaFull(Cuentas_TotalTiempoPersonaS)
UX_SemanaFull<-SemanaFull(UX_TotalTiempoPersonaS)
PM_SemanaFull<-SemanaFull(PM_TotalTiempoPersonaS)
Desarrollo_SemanaFull<-SemanaFull(Desarrollo_TotalTiempoPersonaS)
SocialCnt_SemanaFull<-SemanaFull(SocialCnt_TotalTiempoPersonaS)



write.csv(BI_SemanaFull, "BI/LORD/BI_SemanaFull.csv", na="")   
write.csv(PM_SemanaFull, "PM/LORD/PM_SemanaFull.csv", na="")   
write.csv(Creativo_SemanaFull, "Creativo/LORD/Creativo_SemanaFull.csv", na="")   
write.csv(Cuentas_SemanaFull, "Cuentas/LORD/Cuentas_SemanaFull.csv", na="")   
write.csv(Desarrollo_SemanaFull, "Desarrollo/LORD/Desarrollo_SemanaFull.csv", na="")   
write.csv(UX_SemanaFull, "UX/LORD/UX_SemanaFull.csv", na="")   
write.csv(SocialCnt_SemanaFull, "SocialCnt/LORD/SocialCnt_SemanaFull.csv", na="") 


