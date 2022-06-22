#############################    MODELO PUERTO    ##############################
################################################################################
#### ARCHIVO PARA CORRER FUNCIONES DE DISTINTOS ASPECTOS DEL MODELO PUERTO
### Crecimiento/Senescencia/Ingesti?n del pasto
# Sys.setlocale("LC_NUMERIC", "Spanish_Spain.1252")  #para correr "fread"
disk<-"H:/C/PROYECTOS/Puerto2018"
# site<-"PruebaPredictia2"
# site<-"Tudanca2"
site<-"MUPs378_376"
ano1<-2010
ano2<-2016
# opcion<-2 #Procesado de tablas de inicio seg?n como se exprese el tiempo
# 1: periodos expl?citos: dia/mes inicial y dia/mes final
# 2: Se expresa con t expl?cita, pero a una escala de t=10 d?as: transformar a t=1 d?a #p.ej:Tudanca

#################################################################################
#### PREVIO PUERTO: CARGAR INFO DE RADIACI?N-CLIMA-SUELO PARA UN NUEVO SITIO ####
#################################################################################
#setwd(disk)

##Cargar la radiaci?n. S?lo para correrlo una vez:
# source(file.path(disk,"Codigo/PuertoRadiacion.R"))
# system.time(puertoradiacion(site))
## se obtienen ficheros rr1.rdata y rr25.rdata

##Cargar el clima. Solo para correrlo una vez despues de correr puertoradiacion:
source(file.path(disk,"Codigo/PuertoClima.R"))
system.time(puertoclima(site,ano1,ano2))
# se obtiene fichero clima.rdata

##Cargar el suelo. Solo para correrlo una vez:
# source(file.path(disk,"Codigo/PuertoSuelo.R"))
# system.time(puertosuelo(site))
## se obtiene fichero suelo.rdata

##################################################################################
##################################################################################

setwd(file.path(disk,"Inputs/sites",site))
tmax<-365*7      #N?mero de ts a correr el modelo
# i<-1      #Para hacer pruebas (debugging) Corresponde a las t

source(file.path(disk,"Codigo/Plant.R"))
system.time(pf<-plantf(site,tmax))


################################################################################
################################################################################
