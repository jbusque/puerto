####################################################################
####################     MODELO   PUERTO                  ##########
### VALORES MEDIOS DE VARIABLES EDÁFICAS (textura, prof)      ######
###  POR TESELA DEL MAPA DE VEGETACIÓN D1b_ManchasSuelo       ######
####################################################################

## Capas de partida:

# Teselas del mapa de vegetación donde se aplicara PUERTO: shp
# Rasteres para toda Cantabria de cada variable con pixel de 25m:
# textura, profundidad: dos rasteres. Variables categórica. ZAE2007 (ED50).

# site<-"MUP359"
# D1b_ManchasSuelo<-puertosuelo(site)

puertosuelo<-function(site){
library(data.table)
library(raster)
library(rgdal)
# setwd("D:/C/PROYECTOS/Puerto2018")


#### SUELOS: profundidad y textura

#1. Cargar el shp de teselas del site
tes<-readOGR(dsn=file.path(disk,"Inputs/sites",site,"teselas"),layer="teselas")
proj4string(tes)<-CRS("+init=epsg:25830")
tesed50<-spTransform(tes, crs("+init=epsg:23030"))

#2. Cargar los raster de Cantabria de profundidad y textura de suelos 25m
prof<-raster(file.path(disk,"CantabriaSuelos/Profundidad.tif"))
proj4string(prof)<-CRS("+init=epsg:23030")
profs<-crop(prof,tesed50)
# prof<-projectRaster(prof,crs="+init=epsg:25830",alignOnly=T) #tarda 30"
profs[profs==0]<-NA
profs[profs>5]<-NA
profs[profs==1]<-100
profs[profs==2]<-200
profs[profs==3]<-350
profs[profs==4]<-750
profs[profs==5]<-1500

text<-raster(file.path(disk,"CantabriaSuelos/Textura.tif"))
proj4string(text)<-CRS("+init=epsg:23030")
texts<-crop(text,tesed50)
# text<-projectRaster(text,crs="+init=epsg:25830",method="ngb") #tarda 30"plot(text)
#Cambiar el orden para que 1=grueso; 2=medio; 3=fino
texts[texts==0]<-NA
texts[texts==2]<-4
texts[texts==3]<-2
texts[texts==4]<-3
texts[texts>3]<-NA

######################

#3. Calcular la pendiente
mde<-raster(file.path(disk,"Rasters/mde/w001000.adf"))
proj4string(mde)<-CRS("+init=epsg:25830")
e<-extent(tes)+c(-100,100,-100,100)
mdes<-crop(mde,e)
slopes<-terrain(mdes,unit="tangent")*100 #pendiente en %

#Computar la proporción de superficie de cada tesela con pendiente<=35
slopes0<-slopes
slopes0[slopes0<=35]<-1
slopes0[slopes0>35]<-0
slopes1<-slopes
values(slopes1)<-1

#3. Extract los raster por las teselas

D1b<-data.table(IDMancha=c(tesed50@data$ID_Mancha), #su orden numérico (con "c()")
                    prof=as.vector(round(extract(profs,tesed50,mean),0)),
                    text=as.vector(round(extract(texts,tesed50,mean),0)),
                    pend=as.vector(round(extract(slopes,tes,mean),1)),
                    prmen35=as.vector(round(extract(slopes0,tes,sum)/extract(slopes1,tes,sum),2)))
save(D1b,file=file.path(disk,"Inputs/sites",site,"suelo.Rdata"))

}


# load(file.path(disk,"Inputs/sites",site,"suelo.Rdata"))
# D1b
       
