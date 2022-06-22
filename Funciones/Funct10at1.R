### Para Puerto2018. Transformar tablas de partida que están a una escala temporal t=10
### a t=1 día.

library(data.table)


load("D:/C/PROYECTOS/Puerto2018/Inputs/sites/Tudanca/clima.Rdata")
cl1
fechas2<-unique(cl1[,.(fecha=dia,diam=mday(dia),año,mes,t1=t,diay=dj)])[,
                                              t10:=ceiling(diay/10.15)][]

###########################################################################
###########################################################################
setwd("D:/C/PROYECTOS/Puerto2018/Inputs/sites")

A0b<-fread("./Tudanca/A0b_Gestantes10.txt")
setkey(A0b,t);setkey(fechas2,t10)
A0b2<-A0b[fechas2,allow.cartesian=T][!is.na(IDRebaño)][,
    .(IDRebaño,Raza,Categoria,t=t1,prgest)][order(IDRebaño,Raza,Categoria,t)]
write.table(A0b2,file="./Tudanca/A0b_Gestantes.txt",sep=";",row.names=F)

A1<-fread("./Tudanca/A1_Numeros10.txt")
setkey(A1,t);setkey(fechas2,t10)
A1<-A1[fechas2,allow.cartesian=T][,.(IDRebaño,Raza,Categoria,t=t1,n)]
write.table(A1,file="./Tudanca/A1_Numeros.txt",sep=";",row.names=F)

A1b<-fread("./Tudanca/A1b_CCinicio10.txt")
#aquí t tiene que ser día del año (diay)
setkey(A1b,t);setkey(fechas2,t10)
A1b2<-unique(A1b[fechas2,allow.cartesian=T][!is.na(IDRebaño)][,
        .(t1=min(diay)),.(IDRebaño,Raza,Categoria,t,CCini)][,
          .(IDRebaño,Raza,Categoria,t=t1,CCini)][order(IDRebaño,Raza,Categoria,t)])
write.table(A1b,file="./Tudanca/A1b_CCinicio.txt",sep=";",row.names=F)


A2<-fread("./Tudanca/A2_Alcances10.txt")
setkey(A2,t)
A2<-A2[fechas2,allow.cartesian=T][,.(IDRebaño,t=t1,UP)][order(IDRebaño,t,UP)]
write.table(A2,file="./Tudanca/A2_Alcances.txt",sep=";",row.names=F)

D2b<-fread("./Tudanca/D2b_Accion10.txt")
#aquí t tiene que ser día del año (diay)
setkey(D2b,t);setkey(fechas2,t10)
D2b2<-unique(D2b[fechas2,allow.cartesian=T][!is.na(IDMancha)][,
        .(t1=min(diay)),.(IDMancha,com,accion,Nestie,t)][,
          .(IDMancha,com,accion,t=t1,Nestie)][order(IDMancha,com,accion,t)])
write.table(D2b2,file="./Tudanca/D2b_Accion.txt",sep=";",row.names=F)



#####################################################################################################
#####################################################################################################
#####################################################################################################

####Radiación solar.... en construcción

library(ggplot2)

ts<-data.table(pos=factor(rep(1:2,each=21)),dia=rep(1:21,2),Tmax=rep(10:30,2),Tmin=9,Ra=rep(c(20,25),each=21))
ts[,Rs:=0.19*sqrt(Tmax-Tmin)*Ra][]

ggplot(ts,aes(x=dia,y=Rs,colour=pos))+geom_line()


library(RSAGA) # Para el cálculo de la radiación solar con SAGA
env<-rsaga.env(path="C:/SAGA-GIS/saga_2.2.3_x64")

t11C18_1<-1.05
c12_C18_1<-0.12
C18_3_n3<-0.40
y<-0.13+ 0.16*t11C18_1-0.76*c12_C18_1+0.62*C18_3_n3
y<-1.57
sin(y)^2*100




