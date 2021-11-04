############# SCRIPT DO PROJETO DE MODELAGEM DE RODOLITOS #############

# 1. Teste VIF com as variáveis
# 2. Modelagem teste de Phymatolithon calcareum

##########################################################################################

# Carregar as bibliotecas necessarias 

if (!require(dplyr)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(grDevices)) install.packages('grDevices')
if (!require(sf)) install.packages('sf')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')
if (!require(ggspatial)) install.packages('ggspatial')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')
if (!require(ggsn)) install.packages('ggsn')
if (!require(sdmpredictors)) install.packages('sdmpredictors')
if (!require(leaflet)) install.packages('leaflet')
if (!require(RStoolbox)) install.packages('RStoolbox')
if (!require(usdm)) install.packages('usdm')
if (!require(sp)) install.packages('sp')
if (!require(corrplot)) install.packages('corrplot')


##########################################################################################

### CARREGAR A PLANILHA
rod <- read.csv('Banco_de_dados_Rodolitos.csv', sep=';')

# Mudar a separação decimal das colunas de lat e long de virgula para ponto
rod$Lat <- scan(text=rod$Lat, dec=",", sep=".")
rod$Long <- scan(text=rod$Long, dec=",", sep=".")

# Mudar a classe das colunas long e lat para numericas
rod["Long"] <- sapply(rod["Long"],as.numeric)
rod["Lat"] <- sapply(rod["Lat"],as.numeric)

# Retirar pontos com as mesmas coordenadas, permanecendo apenas um
rod <- rod %>%
    distinct(Lat, Long, .keep_all=TRUE)
length(rod$Lat)

##########################################################################################

# Carregar dados de ocorrencia de Phymatolithon calcareum
calc <- rod %>%
    filter(rod$Spp == "Phymatolithon calcareum") %>%
    dplyr::select(Lat, Long) %>%
    tidyr::drop_na()

head(calc)

##########################################################################################

# Explorar as camadas no dataset
list_layers()


# Carregar as camadas ambientais

### Camadas presente (superficie)
cams_pres <- list.files(path='./Camadas/Presente/', 
                        pattern='.asc', full.names=TRUE) 

cams_pres <- raster::stack(cams_pres)

# Aplicar projecao
projection(cams_pres) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Verificar
plot(cams_pres[[1]])  # velocidade da corrente na superfície


### Camadas RCP 2.6 (superficie)
cams_26 <- list.files(path='./Camadas/2040-2050/RCP26_superficie/', 
                      pattern='.asc', full.names=TRUE) 

cams_26 <- raster::stack(cams_26)

# Aplicar projecao
projection(cams_26) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# verificar 
plot(cams_26[[1]])  # velocidade da corrente na superfície


##########################################################################################

#------- Teste VIF com as variáveis ambientais do presente -------#

# Converter os pontos para SpatialPoints com referenciamento georgrafico
calc <- calc[,c(1,2)]
calc <- SpatialPointsDataFrame(coords = calc, data = calc,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Verificacao dos dados
calc


# Extrair os valores das camadas nos pontos geograficos
ex <- raster::extract(cams_pres, calc, na.rm=TRUE)# existem pontos que podem estar fora
head(ex)                                    # dos rasters, pois foram extraídos valores NA
                                                    
head(na.omit(ex))  # Omitir valores NA exraidos

# omitir NAs
ex <- na.omit(ex)

# Teste VIF
vif <- usdm::vifstep(ex)
cor(ex)

# Matriz de correlacao
vif@corMatrix

### Grafico da matriz de correlacao entre as variaveis 
corr <- corrplot::corrplot(cor(ex), tl.cex=0.8)

##########################################################################################


#--------- 2. RODAGEM DO MODELO PREVIO E SELECAO DAS VARIAVEIS   ---------#

# MODELO CHEIO

calc$species <- 1

# Adicionar os dados previo para fazer um modelo com todas as variaveis
dados_mod_cheio <- sdm::sdmData(species~., spg, predictors = bioCams, 
                   bg=list(method='gRandom', n=10000))
dados_mod_cheio


# Nome dos algoritmos
getmethodNames()

# Ajustar e criar os modelos
modeloC <- sdm::sdm(species~., dados_mod_cheio, methods = c('maxent','rf','glm','maxlike',
                                                            'gam','bioclim','domain.dismo'), replication=c('sub', 'boot'),
               test.p=30, n=25, parallelSettings=list(ncore=5, method='parallel'))

# NOTAS: 1) Para o MaxEnt funcionar o Java do computador deve estar atualizado. 
# 2) O parametro ncore e a quantidade de cores de processamento utilizados para
# a modelagem, altere conforme a capacidade do computador

modeloC

# Plot da importancia das variaveis
plot(getVarImp(modeloC), 'AUC', main="Importância relativa das biovariáveis", 
     ylab='Variáveis', xlab="Importância relativa da variável") # Biovars: 6, 17, 19, 14
getVarImp(modeloC)

# Para abrir uma interface de exploracao do modelo
sdm::gui(modeloC)






