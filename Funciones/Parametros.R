#########################################################################
#### Parámetros de valor único para el modelo PUERTO

ts<-1   #Time step del modelo (d.)

tddmin<-4 #Temperatura mínima para computar grados-día

tcor<-3 #Corrección aditiva de los valores de temperaturas de cl1 

Krs<-0.19 #Coeficiente de ajuste. 0.19=muy oceánico;0.16=muy continental
# para la Fórmula de Hargreaves, para cálculo de FR

Kcmax<-1.3 #Valor máximo del coeficiente del cultivo Kc que  multiplica a ET0

pimx<-0.05 # pimx: Proporción diaria de biov0 ingerido a partir del cual se produce un
# desfronde máximo de lo muerto en pie
pimn<-0    #Proporción de biov0 ingerido por debajo del cual la tasa de desfronde
# es minima

######################################################################
### Parámetros de la siega
biov00<-5 #g MS verde/m2 máximos que quedan en pie tras la siega
biom00<-10 #g gMS seca/m2 máximos que quedan en pie tras la siega
ksv<-0.9 #Proporción de siega de MS viva cosechada
ksm<-0.8 #Proporción de siega de MS muerta cosechada


######################################################################
### Parametros de la parte animal

lintg<-26.8   # Biomasa de pasto (g/m2) por debajo de la cual no hay ingestión

EB<-18.4                        #Energia Bruta de cualquier alimento

qm<-0.8     #Coeficiente de metabolización de la energia

Kgest<-0.154                       #Efic de uso de EM para gestación

#Valor energético de la movilización de reservas corporales en todas las categorias
# (MJ de EN por kg de peso perdido)
ENmovil<-22.4

#Eficiencia de la utilización de la EM para movilización de reservas
Kreservas<-0.84

#Parámetros para calcular el índice de matorralización (IM)
IM0<-0 # Nivel de accesibilidad por debajo de la cual la accesibilidad es mínima Fmos=Fmos0
IM1<-0.6 # Nivel de accesibilidad por encima de la cual que Fmos=1 (accesibilidad total)
Fmos0<-0 #Fmos minima

#Parámetros para estimar la producción de leche segun la Condición corporal (CC)
ccLmn<-1.75 #CC por debajo de la cual el animal no produce leche
ccLmx<-2.5 #CC por encima de la cual la producción de leche es máxima

################################################################################
prvolatN<-0.5  #Pérdidas de N de la orina por volatilización
