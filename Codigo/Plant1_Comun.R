################################################################################
############################### MODELO PUERTO ##################################
################################################################################
### Para utilizar fread (no admite dec=comma)
# sessionInfo()
# Sys.localeconv()["decimal_point"]
# Sys.setlocale("LC_NUMERIC", "Spanish_Spain.1252")



########## Módulo Planta-Animal. 1ª parte:
##### Carga de ficheros y consultas que no requieren loop de tiempo

plant1comunf<-function(site){
  
###### Cargar tablas de input necesarias

#### Módulo Planta 

#Cargar la información sobre clima de las teselas (fichero cl1)
load(file.path(disk,"Inputs/sites",site,"clima.Rdata")) #Ver CorrerPuerto.R si se trata un site nuevo  
#Simular efecto de nieve sobre Temperaturas (en vez de 0 se pone 3ºC bajo nieve
  # para reducir la senescencia, que es baja bajo nieve)
  # tcor es un parámetro que corrige las Tas obtenidas por un valor fijo (añadir o 
  # quitar un valor fijo de ºC; ver fichero "Parametros.R")

cl2<-cl1[,.(IDMancha=ID_Mancha,mes,t,tmed=tmed+tcor,tmin=tmin+tcor,
            tmax=tmax+tcor,rg,prec,fecha=dia,diay=dj)][,
              tmedn:=ifelse(tmed<3,3,tmed)][,tminn:=ifelse(tmed<3,3,tmin)][,
              tmaxn:=ifelse(tmed<3,3,tmax)][]

#Genéricos de todas las sites
  setwd(file.path(disk,"Inputs/sites",site))
  #B2: campos com/com2. Tipos de pastos presentes/estratos por pasto
  B2<-fread("ZB2_Pastos.txt",sep=";",header=T)
  #B3:parámetros ecofisiológicos de los pastos
  B3<-fread("ZB3_ComFisio.txt",sep=";",header=T);setkey(B3,com)
  #Z1: textura de cada tipo de suelo
  Z1<-fread("Z1_SuelosText.txt",header=T,sep=";")

# Especificos de cada site
  #D1b: Profundidad y textura media de los suelos (+pendiente y prpm35)
  load(file.path(disk,"Inputs/sites",site,"suelo.Rdata"))
  #D2: Pastos presentes y su proporción
  D2<-fread("D2_ManchasCom.txt",sep=";",header=T)
 

### Unir lo común de input a todos los Fs
  setkey(D2,com);setkey(B2,com)
  D2B2<-B2[D2,allow.cartesian=T][,.(IDMancha,com,cobv,com2)]
###############################################################################
  
#### Módulo animal
A0<-fread("A0_Rebaños.txt",sep=";",header=T)[,.(IDRebaño,Especie,Gestor)]
A0b<-fread("A0b_Gestantes.txt",sep=";",header=T)
A1<-fread("A1_Numeros.txt",sep=";",header=T)
A1b<-fread("A1b_Inicio.txt",sep=";",header=T)
A2<-fread("A2_Alcances.txt",sep=";",header=T)
D2b<-fread("D2b_Accion.txt",sep=";",header=T)

# if(opcion==1){
# 
# #Opción 1. Puerto2018. Las tablas temporales están expresadas con el dia/mes de inicio 
# # y fin. Es necesario transformarlo a un registro por día
# 
# fechas<-unique(cl2[,.(fecha,diam=mday(fecha),mes,t,diay)]);setkey(fechas,t)
# años<-fechas[,unique(year(fecha))]
# 
# ##########################################################################################
# #Gestantes. A0b.
#   A0b<-fread("A0b_Gestantes.txt",sep=";",header=T)[,A0bID:=.I][];setkey(A0b,A0bID)
#   A0b0<-data.table(expand.grid(A0bID=A0b$A0bID,año=años))[,A0bID1:=.I][];setkey(A0b0,A0bID)
#   A0b01<-A0b[A0b0][,.(IDRebaño,Raza,Categoria,
#                       fechai=as.IDate(paste(año,mesi,diai,sep="-")),
#                       fechaf=as.IDate(paste(año,mesf,diaf,sep="-")),
#                       prgest,A0bID1)];setkey(A0b01,A0bID1)
#   A0b1<-data.table(expand.grid(t=fechas$t,A0bID1=A0b01$A0bID1));setkey(A0b1,t)
#   A0b11<-fechas[A0b1];setkey(A0b11,A0bID1)
#   A0b12<-A0b11[A0b01][fecha>=fechai & fecha<=fechaf][,
#                     .(IDRebaño,Raza,Categoria,t,diay,prgest)]
#   rm(list=c("A0b","A0b0","A0b01","A0b1","A0b11"))
#   A0b<-A0b12;rm(A0b12)
# ###########################################################################################
# # Números. A1.
#   A1<-fread("A1_Numeros.txt",sep=";",header=T)[,A1ID:=.I][];setkey(A1,A1ID)
#   A10<-data.table(expand.grid(A1ID=A1$A1ID,año=años))[,A1ID1:=.I][];setkey(A10,A1ID)
#   A101<-A1[A10][,.(IDRebaño,Especie,Raza,Categoria,
#                       fechai=as.IDate(paste(año,mesi,diai,sep="-")),
#                       fechaf=as.IDate(paste(año,mesf,diaf,sep="-")),
#                       n,A1ID1)];setkey(A101,A1ID1)
#   A11<-data.table(expand.grid(t=fechas$t,A1ID1=A101$A1ID1));setkey(A11,t)
#   A111<-fechas[A11];setkey(A111,A1ID1)
#   A112<-A111[A101][fecha>=fechai & fecha<=fechaf][,
#                     .(IDRebaño,Especie,Raza,Categoria,t,diay,n)]
#   rm(list=c("A1","A10","A101","A11","A111"))
#   A1<-A112;rm(A112)
# #####################################################################################
# #Condición corporal de inicio. A1b.
#   A1b<-fread("A1b_CCinicio.txt",sep=";",header=T)[,A1bID:=.I][];setkey(A1b,A1bID)
#   A1b0<-data.table(expand.grid(A1bID=A1b$A1bID,año=años))
#   A1b01<-A1b[A1b0][,.(IDRebaño,Especie,Raza,Categoria,
#                       fechai=as.IDate(paste(año,mesi,diai,sep="-")),
#                       cc)];setkey(A1b01,fechai)
#   setkey(fechas,fecha)
#   A1b02<-fechas[A1b01][,.(IDRebaño,Especie,Raza,Categoria,t,diay,cc)]
#   rm(list=c("A1b","A1b0","A1b01"))
#   A1b<-A1b02;rm(A1b02);setkey(fechas,t)
# ######################################################################################
# # Alcances. A2.
#   A2<-fread("A2_Alcances.txt",sep=";",header=T)[,A2ID:=.I][];setkey(A2,A2ID)
#   A20<-data.table(expand.grid(A2ID=A2$A2ID,año=años))[,A2ID1:=.I][];setkey(A20,A2ID)
#   A201<-A2[A20][,.(IDRebaño,fechai=as.IDate(paste(año,mesi,diai,sep="-")),
#                       fechaf=as.IDate(paste(año,mesf,diaf,sep="-")),
#                       UP,A2ID1)];setkey(A201,A2ID1)
#   A21<-data.table(expand.grid(t=fechas$t,A2ID1=A201$A2ID1));setkey(A21,t)
#   A211<-fechas[A21];setkey(A211,A2ID1)
#   A212<-A211[A201][fecha>=fechai & fecha<=fechaf][,.(IDRebaño,t,diay,UP)]
#   rm(list=c("A2","A20","A201","A21","A211"))
#   A2<-A212;rm(A212)
# ###########################################################################################
#   
#  #D2b: Pastos que se siegan y/o fertilizan
#   D2b<-fread("D2b_Accion.txt",sep=";",header=T)
# 
# ############################################################################################
# ############################################################################################
# } else {
# ############################################################################################
# ##Opción 2. Para los sites que se ejecutaron con t=10d. Versión Puerto2013
# ##Pasarlo a t=1d
# fechas2<-unique(cl1[,.(fecha=dia,diam=mday(dia),año,mes,t1=t,diay=dj)])[,
#                             t10:=ceiling(diay/10.15)][]
# ##########################################################
# ##Gestación A0b                                                            
# A0b<-fread("A0b_Gestantes.txt") #solo recoge registros con prgest>0
# setkey(A0b,t);setkey(fechas2,t10)
# A0b<-A0b[fechas2,allow.cartesian=T][!is.na(IDRebaño)][,
#     .(IDRebaño,Raza,Categoria,t=t1,diay,prgest)][order(IDRebaño,Raza,Categoria,t)]
# 
# A1<-fread("A1_Numeros.txt")
# setkey(A1,t);setkey(fechas2,t10)
# A1<-A1[fechas2,allow.cartesian=T][,.(IDRebaño,Especie,Raza,Categoria,t=t1,diay,n)]
# 
# A1b<-fread("A1b_CCinicio.txt")
# setkey(A1b,t);setkey(fechas2,t10)
# A1b<-A1b[fechas2,allow.cartesian=T][!is.na(IDRebaño)][,
#         diay1:=min(diay),.(IDRebaño,Especie,Raza,Categoria,cc)][diay1==diay][,
#           .(IDRebaño,Especie,Raza,Categoria,t=t1,diay,cc)][order(IDRebaño,Raza,Categoria,t)]
# 
# A2<-fread("A2_Alcances.txt")
# setkey(A2,t)
# A2<-A2[fechas2,allow.cartesian=T][,.(IDRebaño,t=t1,diay,UP)][order(IDRebaño,t,UP)]
# 
# D2b<-fread("D2b_Accion.txt")
# setkey(D2b,t);setkey(fechas2,t10)
# D2b<-D2b[fechas2,allow.cartesian=T][!is.na(IDMancha)][,
#         diay1:=min(diay),.(IDMancha,com,accion,Nestie)][diay1==diay][,
#           .(IDMancha,com,accion,t=t1,diay,Nestie)][order(IDMancha,com,accion,t)]
# }


############################################################
##########################################################################################
##########################################################################################
  
  D1<-fread("D1_Manchas.txt",sep=";",header=T)[,.(UP,IDMancha,has,cob)]
#Genérico para todas las sites
  A4<-fread("ZA4_Especies.txt",sep=";",header=T);setkey(A4,Especie)
  A6<-fread("ZA6_Necesidades.txt",sep=";",header=T)
  VP<-fread("ZBA_ValorPastoral.txt",sep=";",header=T);setkey(VP,com2)
  
################################################################################
################################################################################
  
### Para Ft.
  setkey(D2B2,com2)
  D2B2B3<-D2B2[B3,nomatch=0]
  
### Para Fh
  setkey(Z1,text); setkey(D1b,text)
  D1bZ1<-Z1[D1b][,.(IDMancha,prof,text,pwp,fc,awc)]
  setkey(D2B2B3,IDMancha);setkey(D1bZ1,IDMancha)
  
## Calcular la profundidad de raiz real a nivel de tesela, segun espesor de 
## suelo/raíz potencial.Esta ya será la tabla necesaria de las caracteristicas 
## del suelo/vegetación.
  Fh0<-D2B2B3[D1bZ1][,.(IDMancha,com,com2,ProfRc=ifelse(prof>ProfR,ProfR,
            prof),text,pwp,fc,awc,p)][,PP:=ProfRc*p][]
  
#Para com con 2 coms, calcular prop de raices para repartir la precipitacion
  Fh0[,prpr:=PP/sum(PP),by=.(IDMancha,com)]
############################################################
  
##Para t=1 y run=1
  Kc1<-D2B2B3[,.(IDMancha=as.integer(IDMancha),com,com2,
                 Kc=Kc(RemV,Ebiom,Epr0,Ek0,Epr1,Ek1))]
    
######################################################################################
######################################################################################
### Parte correspondiente del módulo animal
    
### Calcular el Indice de Matorralización por mancha. Para crear función
  setkey(D1,IDMancha);setkey(D2,IDMancha);setkey(D1b,IDMancha)
  D1D2<-D1[D2][D1b][,c("prof","text","prmen35"):=NULL][];setkey(D1D2,com)
  D1D2B3<-D1D2[B3,nomatch=0][,.(IDMancha,com,has,cob,cobv,pend,IDHL,UP,
                        Npla,prtvm)][,sup:=has*cob*cobv/10000][]
  setkey(D1D2B3,com)
  
  VP0<-VP[D1D2B3,allow.cartesian=T][,.(IM=sum(cobv*(1-Mat)/100)),
          keyby=.(IDMancha,Especie)][,Fmos:=Tri(IM,IM0,IM1,Fmos0,1)][]
  setkey(D1D2B3,IDMancha)
  VP1<-VP0[D1D2B3,allow.cartesian=T][,sup:=has*cob*cobv/10000][]
  setkey(VP1,com);setkey(B2,com)
  VP1a<-B2[VP1,allow.cartesian=T]; setkey(VP1a,Especie,com2)
  setkey(VP,Especie,com2)
  VP1b<-VP[VP1a];setkey(VP1b,Especie)
  VP2a<-A4[VP1b];setkey(VP2a,com2)
  B3a<-B3[,.(com2=com,IDHL2=IDHL)];setkey(B3a,com2)
  VP3<-B3a[VP2a][,.(IDMancha,sup,UP,com,IDHL,com2,IDHL2,Especie,Nombre,
            selectmax,selectmin,pingmax,lsup,linf,Lmx,DigV,DigM,Npla,prtvm,Fantin,
            Fmos,pend,Ipend=1-Hill(pend,kp,np))]
  setkey(VP3,IDMancha,com,com2,Especie)
  
##################################################################################
##################################################################################

  list("B3"=B3,"D1b"=D1b,"D2"=D2,"D2b"=D2b,"D2B2"=D2B2,"Z1"=Z1,"D1ct2"=cl2,
      "D2B2B3"=D2B2B3,"Fh1"=Fh0,"Kc1"=Kc1,"VP3"=VP3,"A2"=A2,"A0"=A0,"A0b"=A0b,
      "D1"=D1,"B2"=B2,"A1"=A1,"A6"=A6,"A4"=A4,"A1b"=A1b,"VP"=VP,"D1D2B3"=D1D2B3)
  
 }
  