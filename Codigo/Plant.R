################################################################################
############################### MODELO PUERTO ##################################
################################################################################

###### Módulo Planta
#### Crear función (plantf), que corre las funciones del módulo Planta:
#### Plant1, Plant2

plantf<-function(site,tmax){
require(data.table)

#Load parameters
source(file.path(disk,"Funciones/Parametros.R"))
source(file.path(disk,"Funciones/FuncModPlanta.R"))
source(file.path(disk,"Funciones/FuncModAnimal.R"))

#Cargar y correr la parte inicial (pl1)
source(file.path(disk,"Codigo/Plant1_Comun.R"))
pl1<<-plant1comunf(site)    # Correr la función1

#Cargar y correr t veces la parte final (pl2)
source(file.path(disk,"Codigo/Plant2_Crec.R"))
for (i in 1:tmax){
  i<<-i+0
  pl2<<-plant2crecf(site)   # Correr la función2

  Fhijt<<-pl2$Fhijt
  FhijtT<<-pl2$FhijtT
  T5<<-pl2$T5
  T6<<-pl2$T6
  TT<<-pl2$TT
  Ihcif<<-pl2$Ihcif
  Balance<<-pl2$Balance
  BalanceAnimal<<-pl2$BalanceAnimal
  }

save(FhijtT,TT,Ihcif,BalanceAnimal,file=file.path(disk,"Outputs/sites",site,"Result.Rdata"))

}                       # end of function


################################################################################
################################################################################