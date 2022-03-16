############# SCRIPT DO PROJETO DE MODELAGEM DE RODOLITOS #############

# 1. Teste VIF com as variáveis
# 2. Modelagem teste de Phymatolithon calcareum

################################################################################

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
if (!require(ggnewscale)) install.packages('ggnewscale')
if (!require(patchwork)) install.packages('patchwork')
if (!require(ggpolypath)) install.packages('ggpolypath')
library('sdm')

################################################################################

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

################################################################################

# Carregar dados de ocorrencia de Phymatolithon calcareum
calc <- rod %>%
    filter(rod$Spp == "Phymatolithon calcareum") %>%
    dplyr::select(Long, Lat) %>%
    tidyr::drop_na()

calc$species <- 1

head(calc)

################################################################################

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


################################################################################

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

calc_ajust <- calc_ajust[, c('Long', 'Lat')]
calc_ajust$species <- 1

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

################################################################################

#--------- 2. RODAGEM DO MODELO PREVIO E SELECAO DAS VARIAVEIS   ---------#


# MODELO CHEIO

# Adicionar os dados previo para fazer um modelo com todas as variaveis
dados_mod_cheio <- sdm::sdmData(species~., calc_ajust, predictors = cams_ben_min, 
                   bg=list(method='gRandom', n=20000))

dados_mod_cheio


# Nome dos algoritmos
getmethodNames()

# Ajustar e criar os modelos
modeloC <- sdm::sdm(species~., dados_mod_cheio, methods = c('maxent','rf','glm','maxlike',
                                                            'gam','domain.dismo'), replication=c('sub', 'boot'),
               test.p=30, n=5, parallelSettings=list(ncore=6, method='parallel'))

modeloC

# Plot da importancia das variaveis
plot(getVarImp(modeloC), 'AUC', main="Importância relativa das biovariáveis", 
     ylab='Variáveis', xlab="Importância relativa da variável") 
getVarImp(modeloC)

# Para abrir uma interface de exploracao do modelo
sdm::gui(modeloC)


#----------
# TESTE VIF COM AS VARIAVEIS COM MAIOR IMPORTANCIA

camadas_selec <- raster::subset(cams_ben_min, c('Benthic.Min.Depth.Current.Velocity.Range.asc.BOv2_1',
                                       'Benthic.Min.Depth.Temperature.Max',
                                       'Benthic.Min.Depth.Salinity.Min',
                                       'Benthic.Min.Depth.Current.Velocity.Min.asc.BOv2_1',
                                       'Benthic.Min.Depth.Temperature.Mean',
                                       'Benthic.Min.Depth.Salinity.Range'))
vif(camadas_selec)


teste <- raster::extract(camadas_selec, calc)
head(teste)

h <- usdm::vifstep(teste)

#vif
h   # Nenhuma das 6 variáveis com problema de correlação segundo o teste vif

#Deixar apenas as vars sem problema de colinearidade
camadas_selec <- usdm::exclude(camadas_selec)

# Verificar
camadas_selec

# omitir NAs obtidos do extract
teste1 <- na.omit(teste)

### Grafico da matriz de correlacao entre as variaveis 
corrplot::corrplot(cor(teste1), tl.cex=0.8)


################################################################################

#--------- MODELAGEM COM AS CAMADAS SELECIONADAS  
#                      NA SECAO ANTERIOR  ---------#

# Adicionar os dados previos: ocorrencias, camadas e pontos de background
d <- sdm::sdmData(species~., calc_ajust, predictors = camadas_selec
                  , bg=list(method='gRandom', n=20000))
d

# Ajustar os modelos, 6 replicacoes, 3 por Subsampling e 3 por Bootstrap para
# cada um dos 6 métodos
m <- sdm::sdm(species~., d, methods = c('maxent','rf','glm',
                                        'maxlike','gam','domain.dismo'), 
              replication=c('sub', 'boot'), test.p=30, n=6, 
              parallelSettings=list(ncore=6, method='parallel'))

# NOTAS: o parametro ncore e a quantidade de cores de processamento
# utilizados para a modelagem, altere conforme a configuracao do computador

m

# Plot das contribuicoes das variaveis
plot(getVarImp(m), 'AUC')
sdm::getVarImp((m))

# Para abrir uma interface de exploracao do modelo
sdm::gui(m)


################################################################################

#--------- PROJECAO DO MODELO PARA O PRESENTE   ---------#

# Projecao dos modelos criados na seção anterior para o presente
# p1 <- predict(m, camadas_selec, filename='./Resultados_Phymatolithon_calcareum/presente.img',
#              overwrite=TRUE)
# p1

# Criar um modelo consenso dentre os criados para o presente por meio da 
# mediana ponderada
en <- sdm::ensemble(m, camadas_selec, filename = 
                        './Resultados_Phymatolithon_calcareum/ensemble_presente.img', 
                    setting =list(method='weighted', stat='tss', opt=2), 
                    overwrite=TRUE)

# Verificacao
plot(p1)
plot(en)


################################################################################

#--------- PROJECAO DO MODELO PARA O FUTURO (RCP26)   ---------#

# Selecionar apenas as 6 biovariaveis 
proj_rcp26 <- raster::subset(cams_ben_min_26, c('Present.Benthic.Min.Depth.Current.Velocity.Range.asc.BOv2_1',
                                            'Present.Benthic.Min.Depth.Temperature.Max',
                                            'Present.Benthic.Min.Depth.Salinity.Min',
                                            'Present.Benthic.Min.Depth.Current.Velocity.Min.asc.BOv2_1',
                                            'Present.Benthic.Min.Depth.Temperature.Mean',
                                            'Present.Benthic.Min.Depth.Salinity.Min'))
plot(proj_rcp26)

# Predicao utilizando o modelo criado para as camadas de RCP26
# p2 <- predict(m, proj_rcp26, filename='./Resultados_Phymatolithon_calcareum/modelos_RCP26.img',
#              overwrite=TRUE)
# p2

# Criar um modelo consenso dentre os criados para o futuro RCP26 por meio da 
# mediana ponderada
en26 <- sdm::ensemble(m, proj_rcp26, filename='./Resultados_Phymatolithon_calcareum/ensemble_futuro_RCP26.img',
                      setting =list(method='weighted', stat='tss', opt=2), 
                      overwrite=TRUE)
en26

# Verificacao
plot(p2)
plot(en26)


################################################################################
#---------   CONSTRUCAO DE MAPAS BINARIOS E
#             DE DENSIDADE DE PROBABILIDADE  ---------#


### OBTER MEDIDAS DE THRESHOLD PARA A CONSTRUCAO DOS mapaP45 BINARIOS
df <- as.data.frame(d)
df <- data.frame(species=df$species, coordinates(d))
xy = as.matrix(df[,c('Long', 'Lat')])
head(xy)

# Extrair do raster da predicao do presente os valores das biovariaveis nos 
# pontos de ocorrencias das especies
p<-raster::extract(en,xy)

# Avaliacao do modelo
ev <- evaluates(df$species,p)
ev@statistics

# Medidadas de threshold
ev@threshold_based

th <- ev@threshold_based$threshold[2] #Threshold pelo metodo SSS: max(espec+sens)
th

#----------
### MAPA BINARIO DO PRESENTE UTILIZANDO O VALOR DE THRESHOLD th
pa1 <- raster(en)
pa1[] <- ifelse(en[] >= th, 1,0)
plot(pa1)

### MAPA BINARIO DO FUTURO (RCP26)
pa2 <- raster(en26)
pa2[] <- ifelse(en26[] >= th, 1,0)
plot(pa2)

### MAPA BINARIO DE ALTERACAO DE ADEQUABILIDADE (Futuro RCP45 - Presente)
chp26 <- pa2 - pa1 
plot(chp26, col=c('red','gray','blue'))

plot(pa2, col=c('gray','blue'))

################################################################################
#--------- 7. CLASSIFICACAO DAS ALTERACOES DE AREA   ---------#

# Obter os tamanhos das celulas
cel_tam<- raster::area(pa1, na.rm=TRUE, weights=FALSE)
cel_tam<-cel_tam[!is.na(cel_tam)]
ctam <- median(cel_tam)
ctam

#----------
### AREA PRESENTE  (KM^2)
mapa <- pa1$layer@data@values==1
tamanho <- sum(mapa[!is.na(mapa)])
area <- tamanho*ctam
area

#----------
###  AREA FUTURA (RCP26)  (KM^2)
mapaF26 <- pa2$layer@data@values==1
tamanhoF26 <- sum(mapaF26[!is.na(mapaF26)])
areaF26 <- tamanhoF26*ctam
areaF26


#----------
###  AREA ALTERADA ENTRE O PRESENTE E O FUTURO RCP26 (KM^2)
# area de perda < 0
# area de ganho > 0

mapaP26 <- chp26$layer@data@values == -1
tamanhoP26 <- sum(mapaP26[!is.na(mapaP26)])
areaP26 <- tamanhoP26*ctam
areaP26        # Area perdida

# Porcentagem de perda
(areaP26/area)*100


mapaG26 <- chp26$layer@data@values == 1
tamanhoG26 <- sum(mapaG26[!is.na(mapaG26)])
areaG26 <- tamanhoG26*ctam
areaG26        # Area ganha

# Porcentagem de ganho
(areaG26/area)*100

# Área mantida
area - areaP26

# Porcentagem mantida
((area - areaP26)/area)*100


################################################################################

# Mapa presente
png('./Resultados_Phymatolithon_calcareum/mapas/presente_Phymatolithon_calcareum.png', 
    height=nrow(en), width=ncol(en))
plot(en, maxpixels=ncell(en))
dev.off()

# Mapa RCP26
png('./Resultados_Phymatolithon_calcareum/mapas/RCP26_Phymatolithon_calcareum.png', 
    height=nrow(en26), width=ncol(en26))
plot(en26, maxpixels=ncell(en26))
dev.off()

# Mapa presente binário
png('./Resultados_Phymatolithon_calcareum/mapas/presente_binario_Phymatolithon_calcareum.png', 
    height=nrow(pa1), width=ncol(pa1))
plot(pa1, maxpixels=ncell(pa1), col=c('lightgray','darkgreen'))
dev.off()

# Mapa RCP26 binário
png('./Resultados_Phymatolithon_calcareum/mapas/RCP26_binario_Phymatolithon_calcareum.png', 
    height=nrow(pa2), width=ncol(pa2))
plot(pa2, maxpixels=ncell(pa2), col=c('lightgray','darkred'))
dev.off()

# Mapa de alteracao RCP26 binario
png('./Resultados_Phymatolithon_calcareum/mapas/alteracao_Phymatolithon_calcareum.png', 
    height=nrow(chp26), width=ncol(chp26))
plot(chp26, col=c('red','gray','blue'), maxpixels=ncell(chp26))
dev.off()

################################################################################

# Transformar os rasters dos modelos em poligonos
# Mapa binario do presente
enPol <- raster::rasterToPolygons(pa1, dissolve = TRUE, fun=function(x){x!=0})
plot(enPol)


# Mapa binario do futuro RCP26
enPol2 <- raster::rasterToPolygons(pa2, dissolve = TRUE, fun=function(x){x!=0})
plot(enPol2)


# Mapa binario de alteracao de adequabilidade do futuro RCP26
enPol4 <- raster::rasterToPolygons(chp26, dissolve = TRUE, fun=function(x){x!=0})
plot(enPol4)


################################################################################
#--------- 8. CONSTRUCAO DOS MAPAS FINAIS   ---------#

#### MAPA BINÁRIO PRESENTE
t1 <- ggplot2::fortify(enPol)
unique(t1$id)


presenteP <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray', size=0.07) +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    
    # Adicionar a distribuição
    ggpolypath::geom_polypath(data = t1, aes(x = long, y = lat, group = group),
                              fill = '#FBFE00')+

    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title='Plot para o presente da espécie Phymatolithon calcareum')

presenteP

ggsave(file = "Presente.pdf",
       plot = presenteP,
       width = 5000, 
       height = 3000, 
       unit = "px",
       dpi = 400)

dev.off()


#### MAPA BINÁRIO RCP 26
t2 <- ggplot2::fortify(enPol2)
unique(t2$id)


rcp26_bin <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray', size=0.07) +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    
    # Adicionar a distribuição
    ggpolypath::geom_polypath(data = t2, aes(x = long, y = lat, group = group),
                              fill = '#F58900')+
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title='Plot para o RCP 2.6 da espécie Phymatolithon calcareum')

rcp26_bin

ggsave(file = "RCP26.pdf",
       plot = rcp26_bin,
       width = 5000, 
       height = 3000, 
       unit = "px",
       dpi = 400)

dev.off()


#### MAPA BINÁRIO DE ALTERACAO
t3 <- ggplot2::fortify(enPol4)
unique(t3$id)


alterac_bin <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray', size=0.07) +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    
    new_scale_fill() +
    ggpolypath::geom_polypath(data = t2, aes(x = long, y = lat, group = group),
                              fill = '#F58900') +
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = t3, aes(x = long, y = lat, group = group,
                                             fill = id)) +
    
    scale_fill_manual(name = " ",
                      values = c("#FFE32D", "#1320BF", '#F58900'),
                      labels = c("Perda de área", "Ganho de área", "Área mantida")) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title='Plot para alteração na distribuição da espécie Phymatolithon calcareum\n entre o presente e o RCP 2.6')

alterac_bin

ggsave(file = "Alteracao.pdf",
       plot = alterac_bin,
       width = 5000, 
       height = 3000, 
       unit = "px",
       dpi = 400)

dev.off()
