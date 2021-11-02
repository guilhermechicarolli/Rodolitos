############# SCRIPT DO PROJETO DE MODELAGEM DE RODOLITOS #############

# 1. Modelagem teste de Phymatolithon calcareum

##########################################################################################

# Carregar as bibliotecas necessarias 

if (!require(dplyr)) install.packages('dplyr')
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


##########################################################################################

### CARREGAR A PLANILHA
rod <- read.csv('Banco_de_dados_Rodolitos.csv', sep=';')
rod <-as_tibble(rod)

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
    select(Lat, Long)


##########################################################################################

# Explorar as camadas no dataset
list_layers()


# Carregar as camadas ambientais

### Camadas presente (superficie)
cams_pres <- list.files(path='./Camadas/Presente/', 
                        pattern='.asc', full.names=TRUE) 

cams_pres <- raster::stack(cams_pres)

# Verificar
plot(cams_pres[[1]])  # velocidade da corrente na superfície


### Camadas RCP 2.6 (superficie)
cams_26 <- list.files(path='./Camadas/2040-2050/RCP26_superficie/', 
                      pattern='.asc', full.names=TRUE) 

cams_26 <- raster::stack(cams_26)

# verificar 
plot(cams_26[[1]])  # velocidade da corrente na superfície









