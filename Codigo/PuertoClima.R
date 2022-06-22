##################################################################################
##################################################################################
### DATOS DE CLIMA DEL PERIODO ano1-ano2 PARA LAS TESELAS DE VEGETACION DE PUERTO
##Se deben tener los datos de radiacion (ejecutar previamente "PuertoRadiacion.R")

# site<-"MUP378" #Nombre del "site" para Puerto
# ano1<-2010
# ano2<-2016

puertoclima<-function(site,ano1,ano2){


  
library(data.table)
library(raster)
library(rgdal)
# setwd("H:/C/PROYECTOS/Puerto2018")

#1. Cargar los raster de radiaci?n

load(file.path(disk,"Inputs/sites",site,"Rasters/rr25.Rdata"))
load(file.path(disk,"Inputs/sites",site,"Rasters/rr1.Rdata"))
rr25<-round(rr25/1000,2) # pasarlos a MJ/m2/d
rr1<-round(rr1/1000,2) # pasarlos a MJ/m2/d
names(rr25)<-1:365
names(rr1)<-1:365
rr25r<-raster(rr25)

#1a. Cargar el shp de teselas del site
tes<-readOGR(dsn=file.path(disk,"Inputs/sites",site,"teselas"),layer="teselas")
proj4string(tes)<-CRS("+init=epsg:25830")

#2. Reproyectar y clipear rr1 a rr25
rr1a25<-projectRaster(rr1,rr25r,method="ngb")

#3. Cargar los datos de temperaturas y precipitacion de Cantabria y las coordenadas
## de sus celdas de 1km aprox.

# load("./CantabriaClima/coorid.Rdata")
# porcion<-list.files(path="./CantabriaClima",pattern="^clima.")[1:10]
# cl<-data.table()
# for (i in 1:10){
# load(paste0("./CantabriaClima/",porcion[i]))
# cl<-rbindlist(list(cl,cl1[a?o>=a?o1 & a?o<=a?o2]))
# save(cl,coor,file=paste0("./CantabriaClima/clCantabria",a?o1,"_",a?o2,".Rdata"))
# }
load(file.path(disk,paste0("CantabriaClima/clCantabria",ano1,"_",ano2,".Rdata")))
cl<-unique(cl)


#4. Escoger solo el clima de los de los cuadrados 1km del site

# create spatial points data frame
coordinates(coor) <- ~ x + y

# coerce to SpatialPixelsDataFrame
gridded(coor) <- TRUE
# coerce to raster
rcoor<-raster(coor)
projection(rcoor)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

#Reproyectar y clipear a rr25r
rcoor25s<-projectRaster(rcoor,rr25r,method="ngb")

# plot(rcoor25s)
# plot(tes,add=T)

idsunique<-unique(values(rcoor25s))
ids25<-data.table(ids=values(rcoor25s))
ids<-unique(ids25$ids)
#Se quitan los 29/2 de los anos bisiestos para que siempre sean 365d/ano
cls<-cl[id1 %in% ids][!(diam==29 & mes==2)]
rm(cl)
#################################

#5. Anadir a rcoor25s la informacion diaria de tmed, tmin y tmax (ano a ano)
# y calcular la ecuacion 1 (puerto, p28) en formato raster

anos<-cls[,unique(año)]

#tmed

# i<-2010
tmedtes2<-data.table()
for(i in anos){
  clstmed0<-dcast.data.table(cls[año==i][,.(id1,dj,tmed)],id1~dj,value.var="tmed")
  clstmed0$id1<-as.numeric(clstmed0$id1)
  setkey(clstmed0,id1);setkey(ids25,ids)
  clstmed<-clstmed0[ids25][,id1:=NULL][]
  rtmed<-brick(rcoor25s)
  values(rtmed)<-data.matrix(clstmed)
  rtmed1<-rtmed-(0.463*(rr25-rr1a25)) #calcular la tmed corregida por la radiacion
  tmedtes<-data.table(t(extract(rtmed1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmedtes)<-c(tes@data$ID_Mancha,"diaj")
  tmedtes1<-melt.data.table(tmedtes,id.vars ="diaj",measure.vars=unique(as.character(tes@data$ID_Mancha)),
                            variable.name="ID_Mancha",value.name="tmed",variable.factor=F)[,ano:=i][]
  tmedtes2<-rbindlist(list(tmedtes2,tmedtes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
rm(list=c("rtmed","rtmed1"))

# data.table(tes@data)[,.N,keyby=ID_Mancha][N>1]
# names(tmedtes)
# tmedtes[,.N]
# as.character(tes@data$ID_Mancha)
#######
#tmin
tmintes2<-data.table()
for(i in anos){
  clstmin0<-dcast.data.table(cls[año==i][,.(id1,dj,tmin)],id1~dj,value.var="tmin")
  clstmin0$id1<-as.numeric(clstmin0$id1)
  setkey(clstmin0,id1);setkey(ids25,ids)
  clstmin<-clstmin0[ids25][,id1:=NULL][]
  rtmin<-brick(rcoor25s)
  values(rtmin)<-data.matrix(clstmin)
  rtmin1<-rtmin-(0.463*(rr25-rr1a25)) #calcular la tmin corregida por la radiacion
  tmintes<-data.table(t(extract(rtmin1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmintes)<-c(tes@data$ID_Mancha,"diaj")
  tmintes1<-melt.data.table(tmintes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="tmin",variable.factor=F)[,ano:=i][]
  tmintes2<-rbindlist(list(tmintes2,tmintes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
rm(list=c("rtmin","rtmin1"))
######
#tmax
tmaxtes2<-data.table()
for(i in anos[7:7]){
  clstmax0<-dcast.data.table(cls[año==i][,.(id1,dj,tmax)],id1~dj,value.var="tmax")
  clstmax0$id1<-as.numeric(clstmax0$id1)
  setkey(clstmax0,id1);setkey(ids25,ids)
  clstmax<-clstmax0[ids25][,id1:=NULL][]
  rtmax<-brick(rcoor25s)
  values(rtmax)<-data.matrix(clstmax)
  rtmax1<-rtmax-(0.463*(rr25-rr1a25)) #calcular la tmax corregida por la radiacion
  tmaxtes<-data.table(t(extract(rtmax1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmaxtes)<-c(tes@data$ID_Mancha,"diaj")
  tmaxtes1<-melt.data.table(tmaxtes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="tmax",variable.factor=F)[,ano:=i][]
  tmaxtes2<-rbindlist(list(tmaxtes2,tmaxtes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
rm(list=c("rtmax","rtmax1"))
#####
#precipitacion
prectes2<-data.table() 
for(i in anos){
  clsprec0<-dcast.data.table(cls[año==i][,.(id1,dj,prec)],id1~dj,value.var="prec")
  clsprec0$id1<-as.numeric(clsprec0$id1)
  setkey(clsprec0,id1);setkey(ids25,ids)
  clsprec<-clsprec0[ids25][,id1:=NULL][]
  rprec<-brick(rcoor25s)
  values(rprec)<-data.matrix(clsprec)
  prectes<-data.table(t(extract(rprec,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(prectes)<-c(tes@data$ID_Mancha,"diaj")
  prectes1<-melt.data.table(prectes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="prec",variable.factor=F)[,ano:=i][]
  prectes2<-rbindlist(list(prectes2,prectes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
#####
#Radiacion extraterrestre + orografia (salida de SAGA)
  radtes<-data.table(t(extract(rr25,tes,mean)))[,diaj:=.I][]
  names(radtes)<-c(tes@data$ID_Mancha,"diaj")
  radtes1<-melt.data.table(radtes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="radpot",variable.factor=F)[,
                            ID_Mancha:=as.integer(ID_Mancha)][]
  

  
#####  
setkey(tmedtes2,ano,diaj,ID_Mancha)
setkey(tmintes2,ano,diaj,ID_Mancha)
setkey(tmaxtes2,ano,diaj,ID_Mancha)
setkey(prectes2,ano,diaj,ID_Mancha)

ttes2<-tmedtes2[tmintes2][tmaxtes2][prectes2][,
                .(ano,diaj,ID_Mancha,tmed,tmin,tmax,prec)]
setkey(ttes2,diaj,ID_Mancha)
setkey(radtes1,diaj,ID_Mancha)
ttes3<-ttes2[radtes1][order(ano,diaj,ID_Mancha)][,
              Rs:=0.18*radpot*sqrt(tmax-tmin)][]


#Crear una tabla de fechas (quitando los dias bisiestos)
# load("./CantabriaClima/clima01.Rdata")
# fechas<-unique(cl1[,.(dia,diam,mes,est,a?o)])[!(diam==29 & mes==2)][,
#    t:=.I][,dj:=1:365][]
# save(fechas,file="./Codigo/fechas.Rdata")
load(file.path(disk,"Codigo/fechas.Rdata"))
fechas<-fechas[año>=ano1 & año<=ano2][,t:=.I][] #ajustar t al periodo a?o1-a?o2
setkey(ttes3,diaj,ano);setkey(fechas,dj,año)
cl1<-fechas[ttes3][,.(dia,diam,mes,est,año,t,dj,ID_Mancha,
                      tmed=round(tmed,1),tmin=round(tmin,1),tmax=round(tmax,1),
                      prec=round(prec,1),rg=round(Rs,1))][order(dia,ID_Mancha)]

save(cl1,file=file.path(disk,"Inputs/sites",site,"clima.Rdata"))

# load(file.path(disk,"Inputs/sites",site,"clima.Rdata"))
# ggplot(cl1[ID_Mancha==140],aes(x=dj,y=prec,colour=factor(ID_Mancha)))+geom_line()+
# #  geom_line(aes(y=radpot),colour="black")+
#     facet_wrap( ~ a?o, ncol=3)
#################################################
}








