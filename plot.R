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

cores <- c('#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44',
           '#F9F42F', '#FB6274', '#276560', '#FF6E0F', '#007500', '#870003', 
           '#FF6D33', '#423224', '#191329', '#0F0900', '#01C901', '#6C226C', '#A78C44')

names <- c(unique(rod$Spp))

# Loop for pra criar os 92 plots das espécies

for (num in 1:92) {

    nome_sp = names[num]
    
    # Separar dados da especie
    sp <- rod %>%
        filter(Spp==nome_sp) %>%
        select(Lat, Long) %>%
        as.data.frame()
    
    # Titulo do plot
    # titulo <- expression(paste("Plot de ", (italic(nome_sp))))     # não está funcionando
    
    # Contrucao do mapa
    g <- ggplot() +
        geom_polygon(data = map_data("world"), aes(long, lat, group = group), 
                     color = "#323334", fill='gray') +
        scale_x_continuous(breaks = (-9:9)*20) +
        scale_y_continuous(breaks = (-9:9)*10) +
        theme_bw() +
        theme(panel.grid = element_blank(), 
              panel.grid.major = element_line(color = gray(.5),
                                              linetype = "dashed", size = 0.5),
              panel.background = element_rect(fill = 'lightblue'))+
        
        geom_point(data = sp, aes(x = Long, y = Lat), colour = cores[num],
                   alpha = 0.6, size = 2.2) +
        
        # Configurar a descrição dos eixos X e Y
        labs(x = "Longitude", y = "Latitude") 
        
        # labs(title=expression(paste("Plot de ", italic(nome_sp))))     # ñ funcionou
    
    
    ggsave(g, file=paste0("./teste/", nome_sp,".png"), 
           width = 2400, height = 1600, units = "px")

}

################################################################################

# O SCRIPT A SEGUIR FOI CRIADO ANTERIORMENTE AO LOOP FOR DOS PLOTS,
# NÃO APAGUEI ELE PARA LEMBRAR DOS CÓDIGOS

'''

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

'''