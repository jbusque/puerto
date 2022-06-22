########################################################################################
### C?lculo de radiaci?n solar diaria en pixels 25m de toda Cantabria
########################################################################################
# La radiaci?n se calcula a partir del mde


setwd("H:/C/PROYECTOS/Puerto2018")
library(data.table)
library(raster)
library(RSAGA) # Para el c?lculo de la radiaci?n solar con SAGA

env<-rsaga.env(path="C:/SAGA-GIS/saga_2.2.3_x64")
# env$version

# C?lculo de la Radiaci?n diaria con SAGA #################################

#A?o 2015. Se asume que todos los a?os se tendr? la misma radiaci?n potencial
# (la determinada por las condiciones topogr?ficas)
# as.numeric(as.IDate("1970-01-01"))
days<-as.IDate(16436:16800,origin=as.IDate("1970-01-01"))
fechas<-data.table(days,dia=mday(days),mes=month(days),ano=year(days))

######################################

rsaga.search.modules("pj_proj4")

rsaga.geoprocessor("pj_proj4",0, list(CRS_METHOD=1,CRS_EPSG="25830"),
  GRIDS=paste0("./Inputs/sites/",site,"/Rasters/mdep1km.sgrd"),env=env)
    



#Calcular la radiaci?n total diaria del mde del site de 1km y guardar en un raster brick
setwd(paste0("./Inputs/sites/",site,"/Rasters"))
rr1<-brick()
for(i in 1:365){
rsaga.pisr2(in.dem ="mdep1km.sdat",
            out.total.grid="r",
            latitude=43,unit="kJ/m2",time.step=12,
            start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$ano[i]),
            end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$ano[i]),
            day.step=1,env=env)
rr1<-addLayer(rr1,raster(readGDAL("r.sdat")))
file.remove(list.files(pattern="^r.\\b"))
projection(rr1)<-CRS("+init=epsg:25830")
save(rr1,file="rr1.Rdata")
}
##########################################################################

#Calcular la radiaci?n total diaria del mde de 25m y guardar en data.tables
##1?tabla con las coordenadas de cada grid (centroides): xydt
rsaga.pisr2(in.dem ="./Rasters/mde25m.sdat",
           out.total.grid="r",
           latitude=43,unit="kJ/m2",time.step=12,
           start.date=list(day=fechas$dia[i],month=fechas$mes[1],year=fechas$a?o[1]),
           end.date=list(day=fechas$dia[1],month=fechas$mes[1],year=fechas$a?o[1]),
           day.step=1,env=env)
rr25<-raster(readGDAL("r.sdat"))
projection(rr25)<-CRS("+init=epsg:25830")
xydt<-data.table(xyFromCell(rr25,1:ncell(rr25)))[,id:=.I][]
######

##2?tabla: datos diarios de radiaci?n para cada grid y d?a del a?o
dt<-data.table()
for(i in 11:365){
rsaga.pisr2(in.dem ="./Rasters/mde25m.sdat",
           out.total.grid="r",
           latitude=43,unit="kJ/m2",time.step=12,
           start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a?o[i]),
           end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a?o[i]),
           day.step=1,env=env)
rr25<-raster(readGDAL("r.sdat"))

# file.remove(list.files(pattern="^r.\\b"))
projection(rr25)<-CRS("+init=epsg:25830")

#xydt<-data.table(xyFromCell(rr25,1:ncell(rr25)))[,id:=.I][]
dt0<-data.table(id=xydt$id,dj=i,rad=as.integer(getValues(rr25)))[!is.na(rad)]
dt<-rbindlist(list(dt,dt0))
save(dt,file="./Rasters/dt.Rdata")
# write.table(dt,file="./Rasters/dt.txt",row.names=F)
}

############################################################################



