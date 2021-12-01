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
if (!require(usdm)) install.packages('sdm')
if (!require(sp)) install.packages('sp')
if (!require(corrplot)) install.packages('corrplot')
if (!require(dismo)) install.packages('dismo')

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
    dplyr::select(Long, Lat) %>%
    tidyr::drop_na()

head(calc)

##########################################################################################

# Explorar as camadas no dataset
list_layers()


# Carregar as camadas ambientais

### Camadas presente (superficie)
cams_pres <- list.files(path='./Camadas/Presente/superficie/', 
                        pattern='.asc', full.names=TRUE) 

cams_pres <- raster::stack(cams_pres)

# Aplicar projecao
projection(cams_pres) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Verificar
plot(cams_pres[[1]])  # velocidade min da corrente na superfície



### Camadas presente (bentonica min)
cams_ben_min <- list.files(path='./Camadas/Presente/bentonica_profund_minima', 
                        pattern='.asc', full.names=TRUE) 

cams_ben_min <- raster::stack(cams_ben_min)

# Aplicar projecao
projection(cams_ben_min) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Verificar
plot(cams_ben_min[[1]])  # velocidade min da corrente na profundidade min



### Camadas RCP 2.6 (superficie)
cams_26 <- list.files(path='./Camadas/2040-2050/RCP26/superficie', 
                      pattern='.asc', full.names=TRUE) 

cams_26 <- raster::stack(cams_26)

# Aplicar projecao
projection(cams_26) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# verificar 
plot(cams_26[[1]])  # velocidade min da corrente na superfície



### Camadas RCP 2.6 (bentonica min)
cams_ben_min_26 <- list.files(path='./Camadas/2040-2050/RCP26/bentonica_profund_minima/', 
                           pattern='.asc', full.names=TRUE) 

cams_ben_min_26 <- raster::stack(cams_ben_min_26)

# Aplicar projecao
projection(cams_ben_min_26) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Verificar
plot(cams_ben_min_26[[1]])  # velocidade min da corrente na profundidade min


##########################################################################################

#------- Teste VIF com as variáveis ambientais do presente -------#

# Remover pontos de localidade com valores NA dos rasters
e <- extract(cams_ben_min, calc)

# dataframe com os valores extraídos de cada célula
x <- data.frame(calc, e)

head(x)

# Remover células com valores NA nos rasters
calc_ajust <- na.omit(x)

# Verificar
calc_ajust

calc_ajust$especie <- 1
calc_ajust <- calc_ajust[, c('Long', 'Lat')]

coordinates(calc_ajust) <- c('Long', 'Lat')

# Verificacao dos dados
calc_ajust

# omitir NAs obtidos do extract
ex <- na.omit(e)

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

# Adicionar os dados previo para fazer um modelo com todas as variaveis
dados_mod_cheio <- sdm::sdmData(species~., train=calc, predictors = cams_ben_min, 
                   bg=list(method='gRandom', n=10000))

dados_mod_cheio


# Nome dos algoritmos
getmethodNames()

# Ajustar e criar os modelos
modeloC <- sdm::sdm(species~., dados_mod_cheio, methods = c('maxent','rf','glm','maxlike',
                                                            'gam','bioclim','domain.dismo'), replication=c('sub', 'boot'),
               test.p=30, n=25, parallelSettings=list(ncore=5, method='parallel'))

modeloC

# Plot da importancia das variaveis
plot(getVarImp(modeloC), 'AUC', main="Importância relativa das biovariáveis", 
     ylab='Variáveis', xlab="Importância relativa da variável") 
getVarImp(modeloC)

# Para abrir uma interface de exploracao do modelo
sdm::gui(modeloC)



##########################################################################################