
#############################################################################
### Funciones utilizada en la parte planta

################################################################################

### Kc. Coeficiente que multiplica a ET0 segun la cantidad de biomasa verde que
## tenga el pasto. Por debajo de un umbral (pr0*ProdRef), se considera un Kc
## minimo (si no transpira, evapora). Por encima de ese umbral se asume todo
## transpiración, y por tanto función lineal de la biomasa verde.

Kc<-function(biov,Ebiom,Epr0,Ek0,Epr1,Ek1) {
  a<-(-(Ek1*Epr0)+(Ek0*Epr1))/(Epr1-Epr0)
  b<-(Ek1-Ek0)/((Epr1-Epr0)*Ebiom)
  ifelse(biov<Epr0*Ebiom,Ek0,ifelse((a+b*biov)>Kcmax,Kcmax,a+b*biov))
}
################################################################################
  ##############################################################################

### Coeficiente de Senescencia función de la humedad (FH) (ver drwatl Savanna)
# Sh0: proporción de hoja muriéndose diariamente a FH=0
# Sh1: proporción de hoja muriéndose diariamente a FH de senescencia mínima
# Fhs: FH a partir de la cual la senescencia es mínima (estable)
Sh<-function(FH,Sh0,Sh1,Fhs) ifelse(FH<Fhs,Sh0-(Sh0*FH/Fhs),Sh1)

################################################################################
################################################################################

## Coeficiente de Senescencia por bajas temperaturas (en Savanna "drtmpl")
# t0: temperatura extrema por debajo de la cual se produce máxima senescencia:St0
# St0: proporción de hoja muriéndose cuando t<=t0 (máxima senescencia)
# t1: temperatura por encima de la cual se produce mínima senescencia: St1
# St1: proporción de hoja muriéndose cuando t>=St1 (minima senescencia) 
St<-function(tm,t0,t1,St0,St1){
    ifelse(tm<t0,St0,ifelse(tm>t1,St1,St1+(((St0-St1)/(t1-t0))*(tm-t0))))
  }

################################################################################
################################################################################

## Coeficiente de desfronde por pastoreo: A más pastoreo más paso de muerto en 
# pie a muerto sobre el terreno.
# fmx: Tasa máxima de desfronde de la materia muerta en pie (d-1)
# fmn: Tasa mínima de desfronde de la materia muerta en pie (d-1)
# pimx: Proporción de biov0 ingerido a partir del cual se produce fmx (0,5)
# pimn: Proporción de biov0 ingerido por debajo del cual la tasa de desfronde es
# minima (0)
Dt<-function(ivgm2ift,biov0,fmn,fmx,pimx,pimn,ts){
  pi<-ifelse(biov0==0,0,ivgm2ift/biov0)
  ifelse(pi<pimn*ts,fmn,ifelse(pi>pimx*ts,fmx,fmn+(((fmx-fmn)/(ts*(pimx-pimn)))*
                                                     (pi-(ts*pimn)))))
}

################################################################################
################################################################################

## Función de tres tramos (para todos los módulos del modelo)

## Función para computar las funciones de tres tramos: constantes por debajo
## y por encima de valores de X mínimos y máximos respectivamente, y una función
## lineal entre medias
# Xmn: umbral inferior de X
# Xmx: umbral superior de X
# Ymn: valor de Y correspondiente a Xmn
# Ymx: valor de Y correspondiente a Xmx

Tri<-function(X,Xmn,Xmx,Ymn,Ymx){
  ifelse(X<Xmn,Ymn,ifelse(X>Xmx,Ymx,((Ymn*(Xmx-X))-(Ymx*(Xmn-X)))/(Xmx-Xmn)))
}

################################################################################
################################################################################
## Función sigmoidal de Hill
Hill<-function(x,k,n,b=1){b*(x^n/(x^n+k^n))}
# b: valor máximo de y (saturación)
# k: valor de x cuando y=b/2
# n: pendiente intermedia (valores 5-10)