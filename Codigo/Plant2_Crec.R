################################################################################
############################### MODELO PUERTO ##################################
################################################################################

########## Módulos Planta-Animal: 2ª parte
##### Submodulo Planta-Crecimiento. Coge inputs de Plant1
### Consultas para cada loop de tiempo


plant2crecf<-function(site){

###### 1. Cálculo del FTRhijt
  
### Meter el time step que corresponda
  
tt<-i   # i is in parent environment. Comes from time loop in "Plant.R"
setkey(pl1$D1ct2,t)
D1ctt<-pl1$D1ct2[.(tt)][,
               .(IDMancha,t,rg,prec,tmed,tmin,tmax,tmedn,tminn,tmaxn,diay)]
  
### Ligarlo con las caracteristicas de las plantas
# tmed en los pastos debajo de leñosas (com2!=com). Se corrige con una función 
# lineal.
# El pasto herbaceo debajo de leñoso recibe una radiación atenuada:k*Rs=0,2*Rs
# Las arbóreas no se cubren por la nieve: se usa tmed y no tmedn
setkey(pl1$D2B2B3,IDMancha);setkey(D1ctt,IDMancha)
FTRhijt<-pl1$D2B2B3[D1ctt][,
   tmed2:=ifelse(IDHL==4 & com==com2,tmed,ifelse(com2==com,tmedn,2+.8*tmedn)),][,
   tmin2:=ifelse(IDHL==4 & com==com2,tmin,ifelse(com2==com,tminn,2+.8*tminn))][,
   tmax2:=ifelse(IDHL==4 & com==com2,tmax,ifelse(com2==com,tmaxn,2+.8*tmaxn))][,
        FT:=ifelse(tmed2<Tmin,0,ifelse(tmed2>Tmax,0,((Tmax-tmed2)/(Tmax-Topt))*
         ((tmed2-Tmin)/(Topt-Tmin))^((Topt-Tmin)/(Tmax-Topt))))][,
           Rg2:=ifelse(com==com2,rg,rg*0.2)][,
            FR:=ifelse(Rg2>Rgmax,1,Rg2/Rgmax)][,
          .(IDMancha,com,com2,t,diay,prec,tmed2,tmin2,tmax2,Rg2,ddcrec,FT,FR)]

################################################################################

######### 3. Cálculo de FHijt. Humedad. 

##Pasar a la escala j y meter datos de temperaturas y radiaciones amortiguadas
## y calcular proporciones de precipitación correspondientes a cada estrato
setkey(FTRhijt,IDMancha,com,com2)
setkey(pl1$Fh1,IDMancha,com,com2)
  
# Cálculo de ET0. Fórmula de Hargreaves (FAO56, p.64; aplicando tb. f.50, p60)
# Cálculo de ADT (agua disponible en la zona radicular del suelo a capacidad
# de campo; ec 82 FAO56);AFA (agua fácilmente extraíble de la zona radicular;
# ec 83); ADT y AFA se multiplican por prprp para contar los casos com<>com2
# tmed3 se usa para computar la integral térmica
    
fhijt<-pl1$Fh1[FTRhijt][,
   .(IDMancha,com,com2,t,diay,ProfRc,text,pwp,fc,p,prec,
    ET0=0.0135*(tmed2+17.8)*(Rg2/2.45)*ts,ADT2=(fc-pwp)*ProfRc*prpr,
     AFA2=(fc-pwp)*ProfRc*p*prpr,FT,FR,ddcrec,
      tmed3=ifelse(tmed2<tddmin,0,tmed2*ts))]
######################
  
#### Modelo BUCKET
#t=1 es diferente por asumir: SWD anterior=0 (y por eso FHta=1: Fta=FTRN)
  
setkey(fhijt,IDMancha,com,com2);setkey(pl1$Kc1,IDMancha,com,com2)
  
## 2 opciones: t=1 y t>1

if (tt==1) {
      fhijt2<-fhijt[pl1$Kc1][,DRta:=0][,Fta:=0][,ETta:=0][,Tac:=tmed3][]
  }else{
      setkey(Fhijt,IDMancha,com,com2)
      fhijt0<-fhijt[Fhijt[,.(IDMancha,com,com2,Tac,DRta=DRf)]][,
            Tac:=ifelse(diay==1,tmed3,tmed3+Tac)][]
      setkey(fhijt0,IDMancha,com,com2);setkey(pl1$D2B2B3,IDMancha,com,com2)
      fhijt1<-fhijt0[pl1$D2B2B3[,.(IDMancha,com,com2,Epr0,Ek0,Epr1,Ek1,Ebiom)]]
      setkey(fhijt1,IDMancha,com,com2)
      fhijt2<-fhijt1[T6[,.(IDMancha,com,com2,biov)]][,
             Kc:=Kc(biov,Ebiom,Epr0,Ek0,Epr1,Ek1)][]
    }


  ## kc*ET0 (ETc) debe sumarse por IDMancha-com (agregarse cuando com<>com2)
  fhijt3<-fhijt2[,.(ETc=sum(ET0*Kc),ADT=sum(ADT2),AFA=sum(AFA2)),
                 by=.(IDMancha,com,prec)];setkey(fhijt3,IDMancha,com)
  fhijt4<-fhijt3[fhijt2][,.(IDMancha,com,com2,t,diay,prec,ET0,Kc,ETc,ADT,AFA,DRta,
        text,pwp,fc,p,FT,FR,ddcrec,tmed3,Tac)];setkey(fhijt4,IDMancha,com,com2)
  
Fhijt<-fhijt4[,
  DRi:=ETc-prec+DRta][,
   Perc:=ifelse(DRi<0,-DRi,0)][,  # Agua que se pierde por percolación
   prPerc:=Perc/(Perc+ADT)][,
    DRi:=ifelse(DRi<0,0,ifelse(DRi>ADT,ADT,DRi))][,
     Ksi:=(ADT-DRi)/(ADT-AFA)][,
      Ksi:=ifelse(Ksi>1,1,Ksi)][,
      pc:=p+0.04*(5-(Ksi*ETc/ts))][,
        pc2:=ifelse(text==1,pc-pc*0.075,ifelse(text==3,pc+pc*0.075,pc))][,
        AFAc:=ADT*pc2][,
          FH:=(ADT-DRi)/(ADT-AFAc)][,
          FH:=ifelse(FH>1,1,FH)][,
            ET:=FH*ETc][,
             DRf:=ET-prec+DRta][,                 
              DRf:=ifelse(DRf<0,0,ifelse(DRf>ADT,ADT,DRf))][,
               .(IDMancha,com,com2,t,diay,prec,ETc,DRi,prPerc,Ksi,ADT,AFAc,ET,FT,
                FR,FH,FTRH=FT*FR*FH,DRf,Tac,ddcrec, ph=ifelse(Tac<ddcrec,1,0))]
              
  # ph: efecto de la fenologia calculada a partir de la integral térmica
  ################################################################################

################################################################################
## CÁLCULO DEL VALOR DE LOS COMPONENTES DEL SISTEMA PLANTA-SUELO (planta viva ,
## muerta, desfronde, N excretas, N orgánico activo,N mineral). SE CALCULAN A 
## PARTIR DEL VALOR DEL t ANTERIOR +/- LOS PROCESOS QUE SUMEN/RESTEN:

#####
# Nombre de Procesos (T41) y Balance (T6) en manual de usuario
#####
setkey(Fhijt,com2);setkey(pl1$B3,com)
T1<-pl1$B3[Fhijt][,.(IDMancha,com=i.com,com2=com,t,diay,FT,FR,FH,FTRH,xi,ph,prPerc,
                                  crecpot=FTRH*xi*ph)]

setkey(pl1$D2B2B3,IDMancha,com,com2);setkey(T1,IDMancha,com,com2)
T2<-pl1$D2B2B3[T1][,.(IDMancha,com,com2,t,diay,RemV,RemM,RemD,FT,FR,FH,FTRH,xi,ph,
    prPerc,crecpot,Sh=Tri(FH,0,Fhs,Sh0,Sh1),t0,t1,St0,St1,TNdescd,TNdescdl,TNdesch,
    TNOrgmx,Norg0,Norgl0,Nmin0,Nres0,TNMinmx,Npla,fmx,fmn,Dr,prtvm,Rar)]

setkey(T2,IDMancha,t,diay);setkey(D1ctt,IDMancha,t,diay)                   

T3<-T2[D1ctt][,.(IDMancha,com,com2,t,diay,FT,FR,FH,FTRH,xi,ph,prPerc,crecpot,Sh,
    RemM,RemV,RemD,Norg0,Norgl0,Nmin0,Nres0,TNdescd,TNdescdl,TNdesch,TNOrgmx,TNMinmx,
    Npla,fmx,fmn,Dr,prtvm,Rar,St1=Tri(tminn,t0,t1,St0,St1))][,
    prsen:=pmax(Sh,St1)][]
        
setkey(T3,IDMancha,com,com2)

if (tt==1){
  T4<-T3[,.(IDMancha,com,com2,t,diay,FT,FR,FH,FTRH,xi,ph,prPerc,crecpot,prsen,
    biovta=RemV,biovrta=RemV*Rar/(1-Rar),biomta=RemM,biomrta=RemM,biodta=RemD,
    Nbiodta=RemD*Npla*prtvm,Nbiohta=0,Nbiodrta=RemD*Npla*prtvm,Norgta=Norg0,
    Norglta=Norgl0,Nresta=Nres0,Nminta=Nmin0,NPta=0,TNMinmx,TNdescd,TNdescdl,
    TNOrgmx,TNdesch,Npla,fmx,fmn,Dr,prtvm,Rar,ivgm2iftta=0)]
} else {
  T4<-T3[T6][,.(IDMancha,com,com2,t,diay,FT,FR,FH,FTRH,xi,ph,prPerc,crecpot,prsen,
   biovta=biov,biovrta=biovr,biomta=biom,biomrta=biomr,biodta=biod,Nbiodta=Nbiod,
   Nbiodrta=Nbiodr,Nbiohta=Nbioh,Norgta=Norg,Norglta=Norgl,Nresta=Nres,Nminta=Nmin,
   NPta=NP,TNMinmx,TNdescd,TNdescdl,TNOrgmx,TNdesch,Npla,fmx,fmn,Dr,prtvm,Rar,
   ivgm2iftta=ivgm2ift)]
  }


#### Acción de segar
##La tabla D2b señala la if a segar en el momento t
##Solo las com 37 (Cynosurion de siega) y 40 (Arrhenatherion) se siegan
#####
pl1$D2b<-pl1$D2b[,.(IDMancha=as.integer(IDMancha),com=as.integer(com),
          accion=as.integer(accion),t=as.integer(t),Nestie=as.numeric(Nestie))]
setkey(pl1$D2b,accion)
if(nrow(pl1$D2b[J(1)])==0){
  D2b<-pl1$D2b[J(1)][,.(IDMancha,com,t)][,biov00:=numeric()][,biom00:=numeric()][]
}else{
  D2b<-pl1$D2b[J(1)][,.(IDMancha,com,t)][,biov00:=biov00][,biom00:=biom00][]
}
  
setkey(D2b,IDMancha,com,t);setkey(T4,IDMancha,com,t)
T41<-D2b[T4][,
  svgm2ift:=ifelse(is.na(biov00),0,ifelse(ksv*biovta>biov00,(ksv*biovta)-biov00,0))][,
  smgm2ift:=ifelse(is.na(biom00),0,ifelse(ksm*biomta>biom00,(ksm*biomta)-biom00,0))][,
  # CRECIMIENTO (aéreo y subterráneo; compensación + crec posterior)
  # N que podría utilizarse para crecimiento
  Npos:=Nminta+Nresta][,
  # Crecimiento que se podría dar para aéreo/subterraneo
  crecpota:=FTRH*ph*xi*ts][,
  crecpotr:=FTRH*ph*xi*ts*Rar/(1-Rar)][,
  # COMPENSACIÓN DE BIOMASA AÉREA/SUBTERRÁNEA:biomasa a compensar                       
  comp:=(biovrta-(Rar/(1-Rar))*biovta)][, 
  compa:=ifelse(comp>0,comp,0)][,
  compr:=ifelse(comp<0,-comp,0)][,
  # ¿hay suficiente N para realizar toda la compensación? 
  comphechoa:=ifelse(Npos>compa*Npla,compa,Npos/Npla)][,
  comphechor:=ifelse(Npos>compr*Npla,compr,Npos/Npla)][,                                                     
  # ¿se cubre con la tasa de crecimiento existente?     
  comphechoa1:=ifelse(comphechoa>crecpota,crecpota,comphechoa)][,
  comphechor1:=ifelse(comphechor>crecpotr,crecpotr,comphechor)][,
  # CRECIMIENTO TRAS COMPENSACIÓN ¿Qué N queda?   
  Npos2:=(Npos-comphechoa1*Npla-comphechor1*Npla)][,
  # ¿Qué crecimiento queda para después de compensación?
  adic:=ifelse(Npos2>(crecpota-comphechoa1-comphechor1)*Npla,
               crecpota-comphechoa1-comphechor1,Npos2/Npla)][,
  # Crecer de acuerdo con el equilibrio aéreo-subterraneo                                                                                     
  adica1:=adic*(1-Rar)][,
  adicr1:=adic*Rar][,
  # Crecimiento total de las partes aérea y subterránea
  crec:=comphechoa1+adica1][,
  crecr:=comphechor1+adicr1][,                               
  senesc:=prsen*ts*biovta][,
  senescr:=prsen*ts*biovrta][,
  Nreabs:=(senesc+senescr)*prtvm*Npla][,
  desf:=(Dt(ivgm2iftta,biovta,fmn,fmx,pimx,pimn,ts)*ts*biomta)+((1-ksv)*svgm2ift)+
          ((1-ksm)*smgm2ift)][,
  Ndesf:=(Dt(ivgm2iftta,biovta,fmn,fmx,pimx,pimn,ts)*ts*biomta*Npla*prtvm)+
            ((1-ksv)*svgm2ift*Npla)+((1-ksm)*smgm2ift*Npla*prtvm)][,
  desfr:=Dr*ts*biomrta][,                   #desfronde de las raices muertas
  Ndesfr:=Dr*ts*biomrta*Npla*prtvm][, 
  descd:=FT*FH*TNdescd*ts*biodta][,  #descomposición a MOrap del desfronde aéreo
  Ndescd:=FT*FH*TNdescd*ts*Nbiodta][,                                
  descdl:=FT*FH*TNdescdl*ts*biodta][,#descomposición a MOlen del desfronde aéreo
  Ndescdl:=FT*FH*TNdescdl*ts*Nbiodta][,                                  
  Ndescdr:=FT*FH*1.5*TNdescd*ts*Nbiodrta][, #descomposición raices =1.5 x desc desfronde
  Ndescdrl:=FT*FH*1.5*TNdescdl*ts*Nbiodrta][,
  Norgan:=FT*FH*TNOrgmx*ts*Norglta][, #Paso de N de MO lenta a rápida
  Ndesch:=FT*FH*TNdesch*ts*Nbiohta][,
  Nminer:=FT*FH*TNMinmx*ts*Norgta][,
  Nperc:=Nminta*prPerc][,
  biovta:=ifelse(is.na(biov00),biovta,ifelse(biovta>biov00,biov00,biovta))][,
  biomta:=ifelse(is.na(biom00),biomta,ifelse(biomta>biom00,biom00,biomta))][,
  FN:=ifelse(FTRH*ph==0,0,(crec+crecr)/(FTRH*xi*ph*ts/(1-Rar)))][]

################################################################################  
################################################################################
################################################################################

#### Módulo Animal. INGESTIÓN

# j representa la primera t donde hay animales en pastoreo

if(nrow(pl1$A1)==0) {j<-1000000 # caso de ausencia de pastoreo en todas las ts
pl1$A2$t<-as.numeric(pl1$a2$t) #Para el caso de que A2 sea una tabla vacia
pl1$A2$UP<-as.numeric(pl1$a2$UP) #Para el caso de que A2 sea una tabla vacia
} else{
  setkey(pl1$A1,t)
  j<-pl1$A1[,min(t)]
  }

setkey(pl1$A2,t)
A2t<-pl1$A2[.(tt)];setkey(A2t,IDRebaño)

setkey(A2t,UP);setkey(pl1$D1,UP)
A2tD1<-A2t[pl1$D1,allow.cartesian=T,nomatch=0]

################################################################################  
##### Inciso: si no hay ganado en ese t, pasar de computar todo este módulo (ver final)
if(nrow(A2tD1)!=0) {

### Cálculo de Diashift

#Calcular VPhift
setkey(pl1$A0,IDRebaño);setkey(A2tD1,IDRebaño)
A0A2tD1<-pl1$A0[A2tD1];setkey(A0A2tD1,IDMancha);setkey(pl1$D2,IDMancha)
A0A2tD1D2<-pl1$D2[A0A2tD1,allow.cartesian=T];setkey(A0A2tD1D2,com);setkey(pl1$B2)
A0A2tD1D2B2<-pl1$B2[A0A2tD1D2,allow.cartesian=T]

### Meter en VP Forage Allowance (pasto accesible por kg PMetab de ganado)
setkey(pl1$A1,t)
FA1<-pl1$A1[.(tt)]
setkey(FA1,Especie,Raza,Categoria);setkey(pl1$A6,Especie,Raza,Categoria)
FA2<-pl1$A6[FA1][,.(PMtot=sum(n*PV^0.67)),keyby=.(Especie,IDRebaño)]
setkey(FA2,IDRebaño);setkey(A0A2tD1D2B2,IDRebaño)
FA3<-FA2[A0A2tD1D2B2][,.(IDRebaño,Especie,PMtot,UP,IDMancha,com,com2,t)]
setkey(FA3,Especie,IDMancha,UP,com,com2)
setkey(pl1$VP3,Especie,IDMancha,UP,com,com2)
FA4<-pl1$VP3[FA3]

setkey(FA4,IDMancha,com,com2);setkey(T4,IDMancha,com,com2)
VPhift<-T4[FA4][,.(IDRebaño,Especie,PMtot,UP,IDMancha,com,com2,IDHL,IDHL2,sup,
      biovta,biomta,t,Fmos,Fantin,DigV,DigM,Npla,prtvm,selectmin,selectmax,Ipend,
      NDig=((DigV*Npla*biovta)+(DigM*Npla*prtvm*biomta)),
      ch=Tri(biomta+biovta,linf,lsup,0,1))][,
      prbiovta:=ifelse(biomta+biovta==0,0,biovta/(biomta+biovta))][, 
      sel:=Tri(prbiovta,selectmin,selectmax,0,1)][,
      VP:=NDig*ch*Fantin*Fmos*sel*Ipend][,
      NhecesVhif:=(1-DigV)*Npla][,
      NhecesMhif:=(1-DigM)*Npla*prtvm][,
      NorinVhif:=DigV*(1-qm)*Npla][,
      NorinMhif:=DigM*(1-qm)*Npla*prtvm][,
      vph:=sum(VP),by=.(IDRebaño,Especie,t)][,
      diashift0:=ifelse(vph==0,0,VP/vph)][, #suman 1 para cada IDRebaño
      prcom1:=sum(diashift0),by=.(IDRebaño,com2,Fantin)][,
      corhft:=ifelse(prcom1>Fantin,Fantin/prcom1,1)][]  #posible corrección a la baja de Fantin
                                                        #según la dieta en oferta

###############################################################################################

setkey(VPhift,IDRebaño)
DIASht0<-unique(VPhift[,.(IDRebaño)]);setkey(DIASht0,IDRebaño) #rebaños presentes en t
setkey(VPhift,IDRebaño,com2,Fantin,prcom1,corhft)
DIAShft0<-unique(VPhift[,.(IDRebaño,com2,Fantin,prcom1,corhft)]) #rebaños-com presentes en t
setkey(DIAShft0,corhft)
DIASht1<-DIAShft0[!J(1)][,.(menFantin=1-sum(Fantin)),keyby=.(IDRebaño)] #casos con corrección de Fantin
DIASht2<-DIAShft0[J(1)][,.(prcom2=sum(prcom1)),keyby=.(IDRebaño)]       #casos sin corrección de Fantin
DIASht3<-DIASht2[DIASht1[DIASht0]][,.(IDRebaño,prcom3=ifelse(prcom2==0,0,   #juntar todos los casos
                      ifelse(is.na(menFantin),1/prcom2,menFantin/prcom2)))]

setkey(DIASht3,IDRebaño);setkey(DIAShft0,IDRebaño)
DIAShft1<-DIASht3[DIAShft0][,.(IDRebaño,com2,prcom4=ifelse(corhft<1,corhft,prcom3))]

setkey(DIAShft1,IDRebaño,com2);setkey(VPhift,IDRebaño,com2)
DIAShift1<-DIAShft1[VPhift][,diashift1:=diashift0*prcom4][,
                  d1:=sum(diashift1),by=.(IDRebaño)][,
                  diashift2:=ifelse(d1<.99999,0,diashift1)][]

# DIAShift1[,sum(diashift2),by=IDRebaño] #Comprobar que sum por rebaño=1 ó 0

##########################

### Cálculo de la ingestión máxima de cada rebaño en cada ts: IMaxht
setkey(pl1$A1,t)
A1t<-pl1$A1[.(tt)][,.(IDRebaño,Especie,Raza,Categoria,t,diay,n)]


###Número y cc de cada hc en cada t.  3 posibilidades:
## A diay==j (1er día de pastoreo) a los hc en pastoreo se les asigna ccRef
## En diay>i, los hc que están en diay en pastoreo pero en diay-1 no estaban en pastoreo: cc=ccRef
## En diay>i, los hc que ya estaban en pastoreo en diay-1, se les asigna el cc de Balance

setkey(pl1$A1b,t)
A1bt<-pl1$A1b[.(tt)]
setkey(A1t,IDRebaño,Especie,Raza,Categoria,t,diay)
setkey(A1bt,IDRebaño,Especie,Raza,Categoria,t,diay)
A1t2<-A1bt[A1t]

if(unique(A1t2$diay)==j){ #si un grupo hc de animales está en diay==j, su cc se pone a CCref
A1t20<-A1t2} else{
setkey(A1t2,IDRebaño,Especie,Raza,Categoria)
setkey(Balance,IDRebaño,Especie,Raza,Categoria)
A1t20<-Balance[A1t2][,.(IDRebaño,Especie,Raza,Categoria,t=i.t,diay,
                        cc=ifelse(is.na(i.cc),cc,i.cc),n=i.n)]
#Si un grupo hc de animales no está en t-1 (no existe en Balance) su cc se pone a CCref
}
#######################################################################################

## Incluir la proporción de animales en los últimos meses de gestación (A1t2)
setkey(pl1$A0b,t)
A0bt<-pl1$A0b[.(tt)]
setkey(A1t20,IDRebaño,Raza,Categoria,t)
setkey(A0bt,IDRebaño,Raza,Categoria,t)
A1t2<-A0bt[A1t20][,.(IDRebaño,Especie,Raza,Categoria,t,diay,n,cc,
                        prgest=as.numeric(prgest))]
A1t2[is.na(A1t2)]<-0

setkey(pl1$A6,Especie,Raza,Categoria)
setkey(A1t2,Especie,Raza,Categoria)
IMaxht<-pl1$A6[A1t2][,.(iMaxht=sum(n*ingmax*ts*CorccI(cc))),
                     by=.(IDRebaño,Especie)]

#######################################################
  
### Cálculo de PPhift (Prop de la ingestión de una if por cada h)
setkey(IMaxht,IDRebaño)
PPhift<-IMaxht[DIAShift1][,
  .(IDRebaño,Especie,t,UP,IDMancha,com,com2,VP,diashift2,
    ppnum=iMaxht*diashift2,NhecesVhif,NhecesMhif,NorinVhif,NorinMhif,Fantin)][,
    ppden:=sum(ppnum),by=.(IDMancha,com,com2)][,
    pphift:=ifelse(ppden<0.00001,0,ppnum/ppden)][]

### Cálculo de CIRhift
setkey(DIAShift1,IDRebaño,IDMancha,com,com2)
setkey(PPhift,IDRebaño,IDMancha,com,com2)
CIRhift01<-DIAShift1[PPhift][,.(IDRebaño,Especie,t,UP,IDMancha,com,com2,sup,
        biovta,biomta,VP,diashift2,pphift,cirnum=diashift2*pphift,NhecesVhif,
        NhecesMhif,NorinVhif,NorinMhif,Fantin)][,
          cirden:=sum(cirnum),by=IDRebaño][,
          cirhift01:=ifelse(cirden==0,0,cirnum/cirden)][]



########################################################
  
### Cálculo de la corrección de la ingestión segun el pasto en oferta: Chif. 
# Simplemente calculando MSU para cada hif
## En el caso en que chif sea<1, hay que ver si ese menor consumo se compensa con
## el mayor consumo de otras if accesibles y cuyas ch==1 y estén por debajo de 
## su Fantin. Es un proceso recursivo, hasta que no se producen cambios.

setkey(pl1$A4,Especie);setkey(CIRhift01,Especie)

CIRhift02<-pl1$A4[CIRhift01][,msu:=biovta+biomta][,
    ch:=Tri(msu,linf,lsup,0,1)][,cirhift02:=ifelse(cirhift01<=ch,cirhift01,ch)][,
        mxcir2:=ifelse(ch<1,0,ifelse(cirhift02==Fantin,0,1))][,
          .(IDRebaño,t,UP,IDMancha,com,com2,ch,Fantin,cirhift02,mxcir2,
            biovta,biomta,NhecesVhif,NhecesMhif,NorinVhif,NorinMhif)]

## Los valores de cirhift02 que quedan ya fijos: mxcir2=0
## Los valores de cirhift02 que pueden cambiar: mxcir2=1

##Condiciones que debe cumplir la función while:
# cond1: que todos los cirs de cada rebaño sumen 1; cond1==0 (No tiene porqué:
# si todas las ch son<1, no sumará 1)
# cond2: que no haya ningun cir por encima de Fantin; cond2==0
#cond1<-sum(CIRhift02[,list(uno=sum(cirhift05)),by=list(IDRebaño)][,round(uno,1)<1])
#cond2<-sum(CIRhift02[,cirhift02>Fantin])

# De momento no lo hacemos con "while". Loop "for" que se repita 4 veces.
for (m in 1:4){
  CIRht03<-CIRhift02[,.(tod=sum(cirhift02)),keyby=.(IDRebaño,t,mxcir2)]
  setkey(CIRht03,mxcir2)
  CIRht030<-CIRht03[J(0)][,.(IDRebaño,t,tod0=tod)]
  CIRht031<-CIRht03[J(1)][,.(IDRebaño,t,tod1=tod)]
  CIRht032<-CIRht03[,length(mxcir2),.(IDRebaño,t)]
  setkey(CIRht032,IDRebaño,t);setkey(CIRht030,IDRebaño,t)
  CIRht040<-CIRht030[CIRht032]
  setkey(CIRht040,IDRebaño,t);setkey(CIRht031,IDRebaño,t)
  CIRht04<-CIRht031[CIRht040][,tod1:=ifelse(is.na(tod1),0,tod1)][,
          tod0:=ifelse(is.na(tod0),0,tod0)]
  setkey(CIRht04,IDRebaño,t);setkey(CIRhift02,IDRebaño,t)
  CIRhift02<-CIRhift02[CIRht04][,
      cirhift02:=ifelse(mxcir2==0,cirhift02,ifelse(tod1==0,cirhift02,
      cirhift02*(1-tod0)/tod1))][,
        cirhift02:=ifelse(cirhift02>Fantin,Fantin,cirhift02)][,
          mxcir2:=ifelse(cirhift02==Fantin,0,mxcir2)][,
            .(IDRebaño,t,UP,IDMancha,com,com2,ch,Fantin,cirhift02,mxcir2,
            biovta,biomta,NhecesVhif,NhecesMhif,NorinVhif,NorinMhif)]
}
# Comprobar que suman 1
# CIRhift02[,sum(cirhift02),by=.(IDRebaño)]

# setkey(CIRhift02,IDRebaño,IDMancha,com,com2)
# setkey(PPhift,IDRebaño,IDMancha,com,com2)
# AB<-CIRhift02[PPhift][,.(IDRebaño,IDMancha,com,com2,pphift,cirhift02)]
# AB[,sum(pphift),keyby=.(IDMancha,com,com2)]
################################################################################

### Cálculo de la ingestión real por rebaño h y raza-categoría de animal c
  
## Ingestión potencial (máxima; Kg por t (10d) por cada rebaño-raza-categoria)
## Corrección por condición corporal
setkey(A1t2,Especie,Raza,Categoria)
IMaxhct<-A1t2[pl1$A6,nomatch=0][,.(IDRebaño,Especie,Raza,Categoria,n,PV,cc,ingmax,
  iMaxhct=n*ingmax*ts*CorccI(cc))]

setkey(IMaxhct,IDRebaño);setkey(CIRhift02,IDRebaño)
IMaxhcift0<-IMaxhct[CIRhift02,allow.cartesian=T][,.(IDRebaño,Especie,Raza,
      Categoria,n,UP,IDMancha,com,com2,t,biovta,biomta,iMaxhct,
      ihcift=iMaxhct*cirhift02,NhecesVhif,NhecesMhif,NorinVhif,NorinMhif)]

setkey(IMaxhcift0,Especie)
IMaxhcift<-pl1$A4[IMaxhcift0][,.(Especie,IDRebaño,Raza,Categoria,n,UP,IDMancha,
  com,com2,t,biovta,biomta,iMaxhct,ihcift,selectmin,selectmax,NhecesVhif,
  NhecesMhif,NorinVhif,NorinMhif)][,
  ivhcift:=ifelse((biomta+biovta)==0,0,
          ihcift*Tri((biovta/(biomta+biovta)),selectmin,selectmax,0,1))][,
  imhcift:=ihcift-ivhcift][]

##Repartición de excretas entre if (para cada hc) (según parámetro probH)
setkey(pl1$D2B2B3,IDMancha,com,com2);setkey(IMaxhcift,IDMancha,com,com2)

PRHhift<-pl1$D2B2B3[IMaxhcift][,list(hnum=sum(ihcift*probH)),
          by=list(IDRebaño,t,IDMancha,com,com2)][,
                hden:=sum(hnum),by=list(IDRebaño,t)][,
                    prH:=ifelse(hden==0,0,hnum/hden)][]

# PRHhift[,round(sum(prH),2),by=list(IDRebaño,com2)][IDRebaño==18]
########################

setkey(IMaxhcift,IDRebaño,IDMancha,com,com2,t)
setkey(PRHhift,IDRebaño,IDMancha,com,com2,t)
Ihcift00<-IMaxhcift[PRHhift][,list(IDRebaño,Especie,Raza,Categoria,n,UP,IDMancha,
          com,com2,t,ihcift,ivhcift,imhcift,NHvhcift=ivhcift*NhecesVhif,
          NHmhcift=imhcift*NhecesMhif,NOvhcift=ivhcift*NorinVhif,
          NOmhcift=imhcift*NorinMhif,prH)]

## Se suma todo lo excretado por hc
Ihct000<-Ihcift00[,list(NHhct=sum(NHvhcift+NHmhcift),
                        NOhct=sum(NOvhcift+NOmhcift)),
                  keyby=list(IDRebaño,Especie,Raza,Categoria,t)]
## Se multiplica el total de lo excretado por prH
setkey(Ihcift00,IDRebaño,Especie,Raza,Categoria,t)
Ihcift0000<-Ihcift00[Ihct000][,list(IDRebaño,Especie,Raza,Categoria,n,UP,
          IDMancha,com,com2,t,ihcift,ivhcift,imhcift,prH,NHhcift=NHhct*prH,
          NOhcift=NOhct*prH)]

##Iif(v-m)t, que es lo que ha de ir al balance de crec-senesc-ingestión
Iift0<-Ihcift0000[,list(ivift=sum(ivhcift),imift=sum(imhcift),
                  NHift=sum(NHhcift),NOift=sum(NOhcift)),
               keyby=list(IDMancha,com,com2,t)][,
                  privift:=ifelse(ivift+imift==0,0,ivift/(ivift+imift))][,
                  primift:=ifelse(ivift+imift==0,0,imift/(ivift+imift))]

#La división /10 es para pasar de kg/ha a g/m2
setkey(Iift0,IDMancha,com);setkey(pl1$D1D2B3,IDMancha,com)
Iift00<-pl1$D1D2B3[Iift0][,ivgm2ift:=ivift/sup/10][,imgm2ift:=imift/sup/10][,
                      prNHift:=ifelse((ivift+imift)==0,0,NHift/(ivift+imift))][,
                      prNOift:=ifelse((ivift+imift)==0,0,NOift/(ivift+imift))]

setkey(Iift00,IDMancha,com,com2,UP,t)

##Poner la proporción de lo ingerido en cada if por cada hc (por si después hay que 
## usarlo tras ajustar la ingestión Iift)
setkey(Ihcift00,IDMancha,com,com2,UP,t)
Ihcift01<-Iift00[Ihcift00][,prIv:=ifelse(ivift==0,0,ivhcift/ivift)][,
                            prIm:=ifelse(imift==0,0,imhcift/imift)]
Ihcift01[is.na(Ihcift01)]<-0

# for (k in c("ivift","imift","ivgm2ift","imgm2ift","privift",
#            "primift")) {Ihcift01[is.na(get(k)),k:=0,with=FALSE]}

###Si biota-iiftha es menor que lintg (26.8g/m2), habrá que ajustar iift: iift1
Iift<-T4[Iift00][,
 ivgm2ift:=ifelse(ivgm2ift+imgm2ift>0,ifelse(biovta+biomta-ivgm2ift-imgm2ift<lintg,
    (biovta+biomta-lintg)*privift,ivgm2ift),0)][,
 imgm2ift:=ifelse(ivgm2ift+imgm2ift>0,ifelse(biovta+biomta-ivgm2ift-imgm2ift<lintg,
    (biovta+biomta-lintg)*primift,imgm2ift),0)][,
 NHgm2ift:=(ivgm2ift+imgm2ift)*prNHift][,
 NOgm2ift:=(ivgm2ift+imgm2ift)*prNOift][,
    list(IDMancha,com,com2,t,ivift,imift,ivgm2ift,imgm2ift,NHgm2ift,NOgm2ift)]


setkey(Ihcift01,IDMancha,com,com2);setkey(Iift,IDMancha,com,com2)
Ihcift<-Iift[Ihcift01][,list(IDRebaño,Especie,Raza,Categoria,n,UP,IDMancha,com,
  com2,sup,t,ivgm2hcift=ivgm2ift*prIv,imgm2hcift=imgm2ift*prIm,
  ivhcift=ivgm2ift*prIv*sup*10/ts,imhcift=imgm2ift*prIm*sup*10/ts)]
# ihcif(v-m)t se expresa en kg MS ingeridos por ha en 1 día


###### Tabla global con la ingestión por hcift, para todas las ts
#En la primera t con ganado (i==j); else...
if(i==j | nrow(pl1$A2[t==tt-1])==0 ){Ihcif<-Ihcift
}else {Ihcif<-rbindlist(list(Ihcif,Ihcift))}

##########################################################################

###### Cuando no hay animales en pastoreo (viene de 156)
} else {
Iift<-data.table(IDMancha=1,com=1,com2=1,t=1,ivift=0,imift=0,ivgm2ift=0,
                 imgm2ift=0,NHgm2ift=0,NOgm2ift=0)[0]
        }

################################################################################
################################################################################

setkey(Iift,IDMancha,com,com2,t);setkey(T41,IDMancha,com,com2,t)
T5<-Iift[T41]
T5[is.na(T5)]<-0

# for (k in c("ivift","imift","ivgm2ift","imgm2ift","NHgm2ift","NOgm2ift")) {
#  T5[is.na(get(k)),k:=0,with=FALSE]}

###Balance de los componentes de suelo-planta

# Fertilización
if(nrow(pl1$D2b[J(2)])==0){
  D2c<-pl1$D2b[J(2)][,.(IDMancha=as.integer(IDMancha),com=as.integer(com),
                 com2=as.integer(com),t=as.integer(t),Nestie=as.integer(Nestie))]
}else{
  D2c<-pl1$D2b[J(2)][,.(IDMancha,com,com2=com,t,Nestie)]
}

setkey(D2c,IDMancha,com,com2,t)
setkey(T5,IDMancha,com,com2,t)

T60<-D2c[T5][,
  biov:=ifelse(biovta+crec-senesc-ivgm2ift-svgm2ift<0,0,
                      biovta+crec-senesc-ivgm2ift-svgm2ift)][,
  biovr:=biovrta+crecr-senescr][,
  biom:=ifelse(biomta+(senesc*(1-prtvm))-desf-imgm2ift-smgm2ift<0,0,
                       biomta+(senesc*(1-prtvm))-desf-imgm2ift-smgm2ift)][,
  biomr:=biomrta+(senescr*(1-prtvm))-desfr][,
  biod:=biodta+desf-descd][,
  Nbiod:=Nbiodta+Ndesf-Ndescd-Ndescdl][,
  Nbiodr:=Nbiodrta+Ndesfr-Ndescdr-Ndescdrl][,
  Nbioh:=Nbiohta+NHgm2ift-Ndesch][,
  Norg:=ifelse(is.na(Nestie),Norgta+Ndescd+Ndescdr+Norgan+Ndesch-Nminer,
               Norgta+Ndescd+Ndescdr+Norgan+Ndesch-Nminer+Nestie)][,
  Norgl:=Norglta+Ndescdl+Ndescdrl-Norgan][,
  Nvolat:=NOgm2ift*prvolatN][,
  den:=Nresta+Nreabs+Nminta+Nminer+(NOgm2ift*(1-prvolatN))-Nperc][,
  # Las pérdidas de Nmin y Nres por crto, se reparten equitativamente
  Nmin0:=Nminta+Nminer+(NOgm2ift*(1-prvolatN))-Nperc-
    (((Nminta+Nminer+(NOgm2ift*(1-prvolatN))-Nperc)/den)*(crec+crecr)*Npla)][,
  Nmin:=ifelse(Nmin0<0,0,Nmin0)][,
  NP:=NPta+Nperc][,
  Nres0:=Nresta+Nreabs-(((Nresta+Nreabs)/den)*(crec+crecr)*Npla)][,
  Nres:=ifelse(Nres0<0,0,Nres0)][]
                                                                     
##################################################################################
##################################################################################
T6<-T60[,.(IDMancha,com,com2,t,FT,FR,FH,FN,FTRH,crec,crecr,senesc,senescr,desf,
  Ndesf,NHgm2ift,Ndesfr,Ndesch,descd,Ndescd,Ndescdr,descdl,Ndescdl,Ndescdrl,Nreabs,
  Norgan,Nminer,Nperc,Nvolat,ivgm2ift,imgm2ift,svgm2ift,smgm2ift,biov,biovr,biom,
  biomr,biod,Nbiod,Nbiodr,Nbioh,Norg,Norgl,Nres,Nmin,NP,ph)]
#############################################
if(tt==1){TT<-T6
         FhijtT<-Fhijt
} else { TT<-rbindlist(list(TT,T6))
         FhijtT<-rbindlist(list(FhijtT,Fhijt))}
    

##################################################################################
##################################################################################
##################################################################################

##### Vuelta al Módulo Animal
  
##### Si no hay ganado, pasar de computar todo este módulo
  if(nrow(A2tD1)!=0) {

#### Balance energético y determinación de los rendimientos animales
## Necesidades energeticas (EN) por cada hc
setkey(pl1$A6,Especie,Raza,Categoria)
setkey(A1t2,Especie,Raza,Categoria)
Nec0<-pl1$A6[A1t2]; setkey(Nec0,Especie)
Nec<-Nec0[pl1$A4,nomatch=0][,
      .(IDRebaño,Especie,Raza,Categoria,t,n,fcr,fcs,
      Nmd2=(Nm+Nd)*n*CorCC(cc),Ngest2=prgest*Ngest*n*CorCC(cc),
      Nl2=Nl*n*Kccl(cc,Lmx,ccLmx,ccLmn),EGKg,PV,PVr=PV*CorCC(cc),
      Kml1,Km2,Kl2,KG1,KG2)]
  
## Energia ingerida (EN) por cada hc
setkey(Ihcift,com2,Especie);setkey(pl1$VP,com2,Especie)
ENIhc0<-pl1$VP[Ihcift];setkey(ENIhc0,Especie,Raza,Categoria)
setkey(pl1$A6,Especie,Raza,Categoria)
ENIhc<-pl1$A6[ENIhc0][,
  EMI:=(ivhcift*EM(DigV))+(imhcift*EM(DigM))][,
  ENImd:=(ivhcift*EM(DigV)*((Kml1*DigV)+Km2))+(imhcift*EM(DigM)*((Kml1*DigM)+Km2))][,
  ENIgest:=(ivhcift*EM(DigV)*Kgest)+(imhcift*EM(DigM)*Kgest)][,
  ENIl:=(ivhcift*EM(DigV)*((Kml1*DigV)+Kl2))+(imhcift*EM(DigM)*((Kml1*DigM)+Kl2))][,
  ENIgan:=(ivhcift*EM(DigV)*((KG1*DigV)+KG2))+(imhcift*EM(DigM)*((KG1*DigM)+KG2))][,
      .(EMIhc=sum(EMI),ENIhmd=sum(ENImd),
        ENIhgest=sum(ENIgest),ENIhl=sum(ENIl),ENIhgan=sum(ENIgan)),
         by=.(IDRebaño,Especie,Raza,Categoria)][,
             KmProm:=ifelse(EMIhc==0,1,ENIhmd/EMIhc)][,
             KgestProm:=ifelse(EMIhc==0,1,ENIhgest/EMIhc)][,
             KlProm:=ifelse(EMIhc==0,1,ENIhl/EMIhc)][,
             KganProm:=ifelse(EMIhc==0,1,ENIhgan/EMIhc)][]
  
setkey(Nec,IDRebaño,Especie,Raza,Categoria)
Balance<-Nec[ENIhc][,.(IDRebaño,Especie,Raza,Categoria,t,n,PV,PVr,KganProm,
            EGKg,EMAPeso=EMIhc-(Nmd2/KmProm)-(Ngest2/KgestProm)-(Nl2/KlProm))][,
            VarPesoInd:=ifelse(EMAPeso<0,EMAPeso*Kreservas/ENmovil,
            EMAPeso*KganProm/EGKg)*1000/n][,
            PVr:=PVr+(VarPesoInd*ts/1000)][,cc:=3+((PVr-PV)*5/PV)][]

if(i==j){BalanceAnimal<-Balance
  } else { BalanceAnimal<-rbindlist(list(BalanceAnimal,Balance))}
################################################################################

##### Si no hay ganado pastando (viene de 324)
} else {if(i<j) {
Ihcif<-0
Balance<-0
BalanceAnimal<-0
                 } else{
Ihcif<-Ihcif
Balance<-Balance
BalanceAnimal<-BalanceAnimal
                        }
        }


##################################################################################
##################################################################################

list("Fhijt"=Fhijt,"FhijtT"=FhijtT,"T1"=T1,"T4"=T4,"T5"=T5,"T6"=T6,"TT"=TT,
     "Ihcif"=Ihcif,"Balance"=Balance,"BalanceAnimal"=BalanceAnimal)

}
