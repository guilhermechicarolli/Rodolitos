############# SCRIPT DO PROJETO DE MODELAGEM DE RODOLITOS #############

# 1. Plot da distribuicao de todas as especies do banco de dados

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

# Verificacao
View(rod)

# Verificar as especies do banco de dados
unique(rod$Spp)

# Quantidade de especies
length(unique(rod$Spp))   # 97


# Espécies com mais pontos de ocorrência
View(rod %>%
         count(Spp))

# Phymatolithon calcareum: 1863
# Lithothamnion glaciale: 1346
# Lithophyllum incrustans: 862

# Carregar o mapa dos paises através do pacote Mapdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world


############################### PLOTS ###########################################

#1. PLOT COM TODAS AS ESPÉCIES

# Contrucao do mapa
g1 <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = rod, aes(x = Long, y = Lat), colour = "#998A53",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title='Plot de todas as espécies')

g1

# Exportar o mapa como uma imagem PNG
png("./Mapas/Todas.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g1
dev.off()

################################################################################

# 2. Phymatolithon calcareum

# Separar dados da especie
calcareum <- rod %>%
    filter(Spp=="Phymatolithon calcareum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Phymatolithon calcareum")))
    
# Contrucao do mapa
g2 <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = calcareum, aes(x = Long, y = Lat), colour = "#1D7800",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)

g2

# Exportar o mapa como uma imagem PNG
png("./Mapas/Phymatolithon_calcareum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g2

dev.off()

################################################################################

# Lithothamnion glaciale

# Separar dados da especie
glaciale <- rod %>%
    filter(Spp=="Lithothamnion glaciale") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo2 <- expression(paste("Plot de ", italic("Lithothamnion glaciale")))


# Contrucao do mapa
g3 <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = glaciale, aes(x = Long, y = Lat), colour = "#34008D",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo2)
g3

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithothamnion_glaciale.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g3

dev.off()

################################################################################

# Lithophyllum incrustans

# Separar dados da especie
incrustans <- rod %>%
    filter(Spp=="Lithophyllum incrustans") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo3 <- expression(paste("Plot de ", italic("Lithophyllum incrustans")))

# Contrucao do mapa
g4 <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = incrustans, aes(x = Long, y = Lat), colour = "#CF8800",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo3)


g4

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum_incrustans.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g4

dev.off()

################################################################################

# Mesophyllum lichenoides

# Separar dados da especie
lichenoides <- rod %>%
    filter(Spp=="Mesophyllum lichenoides") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Mesophyllum lichenoides")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = lichenoides, aes(x = Long, y = Lat), colour = "#801515",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Mesophyllum lichenoides.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithothamnion corallioides

# Separar dados da especie
corallioides <- rod %>%
    filter(Spp=="Lithothamnion corallioides") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithothamnion corallioides")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = corallioides, aes(x = Long, y = Lat), colour = "#81B651",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithothamnion corallioides.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Pneophyllum fragile

# Separar dados da especie
fragile <- rod %>%
    filter(Spp=="Pneophyllum fragile") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Pneophyllum fragile")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = fragile, aes(x = Long, y = Lat), colour = "#162672",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Pneophyllum fragile.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Mesophyllum expansum

# Separar dados da especie
expansum <- rod %>%
    filter(Spp=="Mesophyllum expansum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Mesophyllum expansum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = expansum, aes(x = Long, y = Lat), colour = "#4A6F59",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Mesophyllum expansum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Titanoderma pustulatum

# Separar dados da especie
pustulatum <- rod %>%
    filter(Spp=="Titanoderma pustulatum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Titanoderma pustulatum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = pustulatum, aes(x = Long, y = Lat), colour = "#FFA834",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Titanoderma pustulatum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Porolithon onkodes

# Separar dados da especie
onkodes <- rod %>%
    filter(Spp=="Porolithon onkodes") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Porolithon onkodes")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = onkodes, aes(x = Long, y = Lat), colour = "#FE5252",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Porolithon onkodes.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Phymatolithon purpureum

# Separar dados da especie
purpureum <- rod %>%
    filter(Spp=="Phymatolithon purpureum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Phymatolithon purpureum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = purpureum, aes(x = Long, y = Lat), colour = "#322D33",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Phymatolithon purpureum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Hydrolithon boergesenii

# Separar dados da especie
boergesenii <- rod %>%
    filter(Spp=="Hydrolithon boergesenii") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Hydrolithon boergesenii")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = boergesenii, aes(x = Long, y = Lat), colour = "#345926",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Hydrolithon boergesenii.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Melyvonnea erubescens

# Separar dados da especie
erubescens <- rod %>%
    filter(Spp=="Melyvonnea erubescens") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Melyvonnea erubescens")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = erubescens, aes(x = Long, y = Lat), colour = "#EFFF00",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Melyvonnea erubescens.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Mastophora rosea

# Separar dados da especie
rosea <- rod %>%
    filter(Spp=="Mastophora rosea") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Mastophora rosea")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = rosea, aes(x = Long, y = Lat), colour = "#FF4D28",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Mastophora rosea.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithophyllum stictiforme

# Separar dados da especie
stictiforme <- rod %>%
    filter(Spp=="Lithophyllum stictiforme") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithophyllum stictiforme")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = stictiforme, aes(x = Long, y = Lat), colour = "#513947",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum stictiforme.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithothamnion phymatodeum

# Separar dados da especie
phymatodeum <- rod %>%
    filter(Spp=="Lithothamnion phymatodeum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithothamnion phymatodeum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = phymatodeum, aes(x = Long, y = Lat), colour = "#925D28",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithothamnion phymatodeum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Neogoniolithon brassica-florida

# Separar dados da especie
brassica <- rod %>%
    filter(Spp=="Neogoniolithon brassica-florida") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Neogoniolithon brassica-florida")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = brassica, aes(x = Long, y = Lat), colour = "#6E172B",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Neogoniolithon brassica-florida.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithophyllum prototypum

# Separar dados da especie
prototypum <- rod %>%
    filter(Spp=="Lithophyllum prototypum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithophyllum prototypum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = prototypum, aes(x = Long, y = Lat), colour = "#61D61B",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum prototypum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Phymatolithon repandum

# Separar dados da especie
repandum <- rod %>%
    filter(Spp=="Phymatolithon repandum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Phymatolithon repandum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = repandum, aes(x = Long, y = Lat), colour = "#75005F",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Phymatolithon repandum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithophyllum racemus

# Separar dados da especie
racemus <- rod %>%
    filter(Spp=="Lithophyllum racemus") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithophyllum racemus")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = racemus, aes(x = Long, y = Lat), colour = "#EEB71D",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum racemus.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Sporolithon durum

# Separar dados da especie
durum <- rod %>%
    filter(Spp=="Sporolithon durum") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Sporolithon durum")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = durum, aes(x = Long, y = Lat), colour = "#792A8C",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Sporolithon durum.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithophyllum margaritae

# Separar dados da especie
margaritae <- rod %>%
    filter(Spp=="Lithophyllum margaritae") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithophyllum margaritae")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = margaritae, aes(x = Long, y = Lat), colour = "#2B3321",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum margaritae.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

# Lithophyllum imitans

# Separar dados da especie
imitans <- rod %>%
    filter(Spp=="Lithophyllum imitans") %>%
    select(Lat, Long) %>%
    as.data.frame()


# Titulo do plot
titulo <- expression(paste("Plot de ", italic("Lithophyllum imitans")))

# Contrucao do mapa
g <- ggplot() +
    geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                 color = "#323334", fill='gray') +
    scale_x_continuous(breaks = (-9:9)*20) +
    scale_y_continuous(breaks = (-9:9)*10) +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                        linetype = "dashed", size = 0.5),
          panel.background = element_rect(fill = 'lightblue'))+
    
    geom_point(data = imitans, aes(x = Long, y = Lat), colour = "#FF0D1F",
               alpha = 0.6, size = 2.2) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    labs(title=titulo)


g

# Exportar o mapa como uma imagem PNG
png("./Mapas/Lithophyllum imitans.png", res = 300,
    width = 2400, height = 1600, unit = "px")
g

dev.off()

################################################################################

cor = c('#FF0D1F', '#E0A126')

for (i in c('Lithophyllum hibernicum', 'Lithophyllum imitans')) {
    
    for (j in cor) {
        # Separar dados da especie
        sp <- rod %>%
            filter(Spp==i) %>%
            select(Lat, Long) %>%
            as.data.frame()
        
        
        # Titulo do plot
        titulo <- expression(paste("Plot de ", italic(i)))
        
        # Contrucao do mapa
        g <- ggplot() +
            geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                         color = "#323334", fill='gray') +
            scale_x_continuous(breaks = (-9:9)*20) +
            scale_y_continuous(breaks = (-9:9)*10) +
            theme_bw() +
            theme(panel.grid = element_blank(), panel.grid.major = element_line(color = gray(.5), 
                                                                                linetype = "dashed", size = 0.5),
                  panel.background = element_rect(fill = 'lightblue'))+
            
            geom_point(data = sp, aes(x = Long, y = Lat), colour = j,
                       alpha = 0.6, size = 2.2) +
            
            # Configurar a descrição dos eixos X e Y
            labs(x = "Longitude", y = "Latitude") +
            labs(title=titulo)
        
        
        # Exportar o mapa como uma imagem PNG
        file = paste0('./teste/', paste(i, "png", sep='.'))
        png(file, res = 300,
            width = 2400, height = 1600, unit = "px")
        g
        
        dev.off()
        
    }
    
    
}
