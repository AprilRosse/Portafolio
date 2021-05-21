############Preliminar############
library(tidyverse) #conjunto de paquetes manejo bases de datos
library(quantmod) #para extraer info fuentes externas (ej, yahoo)
library(fBasics) #estadisticas basicas
library(plotly) #para graficar
library(fPortfolio) #calculo frontera eficiente

############# Parametros ##################

ruta = "G:/Mi unidad/.." #directorio de trabajo
empresasfile = "EmpresasIBEX.txt" #file con los tickets de las empresas a buscar

inicio="2020-04-30"
fin="2021-05-01"
periodicidad="weekly"

file1 = "base_serie_precios_IBEX.csv" #salida base de precios extraeida
file2 = "base_serie_rendiminetos_IBEX.csv" #salida base de rendimientos
file3 = "base_estadisticas_IBEX.csv" #estadisticas, rentabilidad anualizada, volatilidad etc
file4 = "MatrizCovarianzasx2Activos.csv" #matriz de covarianzas
file5 = "Carterax2Activos.csv" # base combinatoria de carteras de 2 activos

graf1 = "Rent_Vol_Componentes.png" #Grafico rentabilidad y volatilidades individuales
graf2 = "Indicadores cartera por activos.png" #Grafico rentabilidad y volatidades combinatorias portafolio de 2 activos
graf3 = "PortafolioOptimo.png" #participacion de cada activo en el portalio optimo
graf4 = "FronteraEficineteOptimizacion.png" #grafica frontera eficiente

################# 1. Basico #####################

getwd() #directorio por defecto
setwd(ruta)
#recordar cambiar la barra invertida

#30 princiipales empresas del IBEX35
empresas <- read.delim(empresasfile,colClasses = "character")
empresas = as.character(unlist(empresas))

################ Extraccion informacion ################

empresas_list=getSymbols(empresas, src='yahoo', from=inicio, to=fin,periodicity = periodicidad,auto.assign=TRUE)
list <- lapply(empresas, function(x) Ad(get(x)))
serie_precios <- do.call(merge,list) #base consolidada
names(serie_precios) = str_replace(names(serie_precios),".MC.Adjusted","")

salida=as.data.frame(serie_precios)
write.csv2(salida,file = file1)
rm(list = empresas,salida)

############### RENDIMIENTOS #########################

retornos <- data.frame(apply(serie_precios, 2, function(x) Delt(x, type = "log")),
                       fecha = index(serie_precios)) %>%
  na.omit()

empresas_list = str_replace(empresas_list,".MC","")
rownames(retornos)=retornos$fecha
retornos$fecha <- NULL

salida2=as.data.frame(retornos)
write.csv2(salida2,file = file2)
rm(empresas,salida2)#Eliminar no deseados
length(empresas_list) #validacion cantidad de empresas

############### VALORES GLOBALES ESTADISTICAS ######################

#estadisticas <-  basicStats(retornos[1:4])[c("Mean", "Stdev", "Median", "Minimum", "Variance","Maximum", "nobs","Skewness","Kurtosis"),]
estadisticas <-  basicStats(retornos)[c("Mean", "Stdev","Variance"),]
estadisticas <- data.frame(t(estadisticas))
names(estadisticas) <- c("Rent_Semanal_Media","Volatilidad_semanal","Varianza") #se agrega la varianza para validar la matriz de covarianzas

#Rentabilidad Anualizada = rentabilidad semanal * 52
estadisticas["Rent_Anualizada"]=estadisticas["Rent_Semanal_Media"]*52
#Volatididad Anualizada = volatilidad semanal * raiz de 52
estadisticas["Volatilidad_Anualizada"]=estadisticas["Volatilidad_semanal"]*sqrt(52)
#Exportar resultados
salida=as.data.frame(estadisticas)
write.table(salida,file = file3,sep=";")
rm(salida) #eliminar archivo temporal

##############  GRAFICOS RENTAVILIDAD Y VOLATIDIDADES INDIVIDUALES ################
png(graf1)
ggplot(estadisticas, aes(Volatilidad_Anualizada, Rent_Anualizada, label= rownames(estadisticas),
                         colour = rownames(estadisticas), fill=rownames(estadisticas))) +
  geom_point(size=4,shape=21,stat = "identity") +
  geom_text(vjust = 0, nudge_y = 0.05, angle = 45) +
  labs(title = "30 Principales Componentes IBEX")+
  theme_minimal()
dev.off()


############ CONSTRUCCION DE CARTERAS - MANUAL X2#################
matriz_covarianza_semanal = cov(retornos)
matriz_covarianza_semanal = as.data.frame(matriz_covarianza_semanal)

salida=as.data.frame(matriz_covarianza_semanal)
write.table(salida,file = file4,sep=";")
rm(salida)


n=1
cartera=data.frame(rent_cartera=double(),var_cartera=double())
pond_act_1=0.5
pond_act_2=1-pond_act_1

for (x in empresas_list){
  
  r_m_semanal1 = estadisticas[x,"Rent_Semanal_Media"]
  var_sem1 = estadisticas[x,"Varianza"]
  resto=empresas_list[-c(n)]
  
  
  for(y in resto){
    
    r_m_semanal2=estadisticas[y,"Rent_Semanal_Media"]
    var_sem2 = estadisticas[y,"Varianza"]
    combinacion=str_c(x,y,sep = "_")
    cartera[combinacion,"rent_cartera"]=pond_act_1*r_m_semanal1+pond_act_2*r_m_semanal2
    cartera[combinacion,"var_cartera"]=(pond_act_1^2)*var_sem1+(pond_act_2^2)*var_sem2+2*pond_act_1*pond_act_2*matriz_covarianza_semanal[x,y]
    }
  }


cartera["Rent_Anualizada"]=cartera["rent_cartera"]*52
cartera["Volatilidad_Anualizada"]=sqrt(cartera["var_cartera"])*sqrt(52)

salida=as.data.frame(cartera)
write.table(salida,file = file5,sep=";")
rm(salida)

############ GRAFICO DE CARTERAS - MANUAL*2 #################
png(graf2)  
ggplot(cartera, aes(Volatilidad_Anualizada, Rent_Anualizada))+
  geom_point()+
  labs(title = "Indicadores Cartera x2 Activos")
dev.off()

######### AUTOMATIZACION PORTAFOLIO DE MENOR RIESGO - FRONTERA EFICIENTE ###########
#https://finance-r.netlify.app/quantmod.html
#https://rpubs.com/EsmeraldaAN/em21

Spec <- portfolioSpec()
setRiskFreeRate(Spec)<- 0.0001 #Tasa libre de riesgo
setNFrontierPoints(Spec) <- 30 #Cantidad de carteras en frontera
setType(Spec) <- "MV" #Markowitz
setEstimator(Spec) <- "covEstimator"
setOptimize(Spec) <- "minRisk"
setOptimize(Spec) <- "solveRquadprog"
constraints <- "LongOnly" # Que los pesos varien de 0 a 1(posicion larga)

ts_retornos <- as.timeSeries(retornos)
efPortfolio<- efficientPortfolio(ts_retornos,Spec);efPortfolio

png(graf3) 
summary(efPortfolio)
dev.off()

portafolio=as.data.frame(efPortfolio@portfolio@portfolio[["weights"]])
colnames(portafolio)="Peso"

portafolio = portafolio %>%
  filter(Peso>0) %>%
  arrange(-Peso)
portafolio 

FronteraEficiente <- portfolioFrontier(ts_retornos,Spec,constraints)

png(graf4)
frontierPlot(FronteraEficiente) #Graficando
dev.off()
