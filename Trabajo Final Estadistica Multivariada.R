# Proyecto final Muestreo 

#Trabajo
# 1. Averiguar acerca del Índice de Prosperidad para dar contexto a los datos
# 2. Realizar un análisis de componentes principales para explicar la relación entre las variables y el comportamiento de los individuos
# 3. Construir un indicador Prosperidad
# 4. Realizar un agrupamiento no jerárquico de los individuos explicando las características relevantes de los grupos resultantes

#Legatum Prosperity Index

#El Índice de Prosperidad Legatum es un ranking anual elaborado por el Instituto Legatum
#Año de la medición 2019


#--------------------------------- Descargamos Los Paquetes--------------------#
library(clValid)
require(FactoMineR)
require(factoextra)
import.package(readxl)
require(dplyr)
require(fpc)
require(tidyverse)
#---------------------------- We Download The Data-----------------------------# 
 
data <- read_excel("C:/Users/Ryzen 5/Downloads/Trabajo final Estadistica Multivariada/6. Legatum Prosperity Index 2019.xlsx")

#------------- We make the first changes to the data so we can use it ---------#

# We change the data to numeric 
data[, - c(1, 2)] <- lapply(data[, -c(1, 2)], function(x) as.numeric(as.character(x)))


# we select and transform the data  on the DF 
datos <- data%>% 
  filter(area_name != "Grand Total")%>%
  tibble::column_to_rownames(var = "area_code")%>%
  select(-c("area_name","area_group"))
#----------------------------------------------PCA-----------------------------#
# Create the ACP 
acp=PCA(datos,graph=F,);acp

# Eigen values and eigen vectors of the DF 
acp$eig

# plot the variance for each principal component 
fviz_screeplot(acp)

# check the correlations of each variable with each princial component 
acp$var$coord # how much each component exaplines each variable 


# we graph principal componen interactions with variables 


fviz_pca_var(acp,repel=T, axes=c(1,2),col.var="cos2")+
  scale_color_gradient2(low="red",mid="blue",
                        high="black",midpoint=0.5)
fviz_pca_var(acp,repel=T, axes=c(1,3),col.var="cos2")+
  scale_color_gradient2(low="red",mid="blue",
                        high="black",midpoint=0.5) 
fviz_pca_var(acp,repel=T, axes=c(2,3),col.var="cos2")+
  scale_color_gradient2(low="red",mid="blue",
                        high="black",midpoint=0.5)

# check cos2 for each variable witch component explain it best 

acp$var$cos2 # we use this one to check whitch  are represented on each PC 




# check the contributions of each variable to each component 
# with this we can say what each principal component represents 

acp$var$contrib

fviz_contrib(acp,choice="var", axes=1)
fviz_contrib(acp,choice="var", axes=2)
fviz_contrib(acp,choice="var", axes=3)
fviz_contrib(acp,choice="var", axes=c(1,2))

# coordenates for each individual for each component and component pair 
acp$ind$coord


fviz_pca_ind(acp,axes=c(1,2),repel=T)
fviz_pca_ind(acp,axes=c(1,3),repel=T)

fviz_pca_ind(acp,axes=c(2,3),repel=T)

# graph for variables and countr in this case useless too many countr's
#fviz_pca_biplot(acp,axes=c(1,2),repel=T)

acp$svd# single value dicomposition, 
# sqrt of eigen values 
#$ on v we have the eigenvlaues to create the ecuations for the PC 

#--------------------We create the indicator-----------------------------------

# 1ra C.P.
round(acp$svd$V[,1:4], 2)
colnames(data)


Ind = acp$ind$coord[,1] ; Ind # Indic. de cond. socio-econom.

sort(Ind)[c(1:3,165:167)]

hist(Ind)

Ind = (Ind-min(Ind))/(max(Ind)-min(Ind))*100 ; Ind

sort(Ind)[c(1:3,165:167)]


Ind = acp$ind$coord[,1] ; 
a = mean(Ind)-3.5*sd(Ind)
b = mean(Ind)+ 3.5*sd(Ind)
Ind = (Ind-a)/(b-a)*100;Ind
sort(Ind)[c(1:3,165:167)]


sort(Ind)

hist(Ind, 
     breaks=30, 
     col="skyblue", 
     border="black", 
     main="Distribution of Variable Ind",
     xlab="Value",
     ylab="Frequency")
grid(nx = NA, ny = NULL)
d<-density(Ind)
datos$ind<-Ind

#----------------- create clusters and select the best one --------------------#

# Algoritmo no jerárquico
X=acp$ind$coord[,1:3]
set.seed(123)
km = kmeans(X, centers = 5) # constructed by a k-means metodology
#km2 = pamk(4) # used the metodology k medioids  and selects automaticallly yhe number of clusters 
km2 = pam(X,5,metric="euclidean",stand = FALSE) # used the metodology k medioids  but this object output we can represent on a graph

g1 = km$cluster ; g1 # name of the country and cluster number 

g2 = km2$cluster ; g2 # name of the country and clouster number 

# first possible graph
fviz_cluster(list(data=datos,cluster=km$cluster),stand=T,axes=c(1,2),repel=T)  # we can pick this one beacouse it represents better the 1 PCA


fviz_cluster(list(data=datos,cluster=km2$clustering),
             repel = T, stand = T,axes=c(1,2))


# Conteos ("comparación") de grupos jer. y no jer.
table(g1, g2)

# ¿por qué dos grupos?
# Determinar el número de grupos
# Jerárquico
val = clValid(scale(X), nClust = 2:5,
              clMethods = "kmeans",
              validation = c("internal","stability"),
              maxitems = nrow(X), metric = "euclidean",
              method = "ward")

sval = summary(val) ; sval

# No jerárquicos
val2 = clValid(scale(X), nClust = 2:5,
               clMethods = "pam",
               validation = c("internal","stability"),
               maxitems = nrow(X), metric = "euclidean")

sval2 = summary(val2) ; sval2

# no usar el metodo de la silhueta usar el metodo 
# dunn on conectividad 
# dunn mide la varianz adentro de los grupos y devuelve una medida
# de homogenidad que debe ser maximisada
# conectividad es cuandots vecinoms mas cercanos tengo yo que estan 
# por fuera de mi grupo 
# cuando una medida de muchos grupos no esta sirviendo para clasificar 
# es mejor quedarse con lo que es mas frecuente 
# nunca promediar eso 


# Seleccionamos las medidas para cada 
# método y número óptimo de grupos

cbind(measures(val)[,1,],
      measures(val2)[,1,]) # it seems that the second method it's better it minim 
# what should be minim and max what shloud be max 

# estamos revisando gerarquicos con no gerarquicos 
# para todos menos dunn y silhueta tiene que se minimisado 


# El mejor resultado es k-means con 2 grupos

# Caracterización de los grupos
by(X, g2, colMeans)
t.test(datos$`Economic Quality` ~ g2)# this wont really work beacouse it is 
# more the 2 

 # lest try a anova 

 # there is significan changes based on the clusters for the 
#variable Economic Quality   lest repad this for all significan valirbale sor clusters 


)
by(M, k.g, summary)

# hacemos test para revisar como se diferencias en promedios
# medias y medianas 
plot(density(Ind))


# lest verify the amount of countrys by cluster 

km$size

# check the centers by cluster
km$centers

datos$cluster <-g2 

# creating the mean of each variable by clusters with the original data:
table_means<-aggregate(datos , by=list(cluster=km2$cluster), mean)

table_means[order(table_means$`Economic Quality`),] # clusters agregados y ordenados por su media 
# Now the sd

aggregate(datos , by=list(cluster=km2$cluster), var)
# we give names to each cluster based on the characteristics 



datos <- datos %>%
  mutate(NombreCluster = case_when(
    cluster == 1 ~ "Cluster de Economía Emergente 1",
    cluster == 2 ~ "Cluster de Economía en Desarrollo 2",
    cluster == 4 ~ "Cluster de Economía en Transición 3",
    cluster == 5 ~ "Cluster de Economía Avanzada 4",
    cluster == 3 ~ "Cluster de Economía Líder 5",
    TRUE ~ "Desconocido"
  ))

#---------------lest do a simple barplot for the indicator---------------------#
datos$ind<-Ind

mean_values <- datos %>%
  group_by(NombreCluster) %>%
  summarize(mean_value = mean(ind))


# Plotting the barplot
barplot(mean_values$mean_value,
        names.arg = mean_values$NombreCluster,
        col = "steelblue",
        main = "Mean Values by Cluster",
        xlab = "Cluster",
        ylab = "Mean Value",
        border = "white",
        las = 1)  # Orienting axis labels horizontally

# Adding grid lines for better visibility
grid(nx = NA, ny = NULL, lty = "dotted", col = "gray")


#-------------- lest represent this on a global map ---------------------------# 

mapdata <- map_data("world")
View(mapdata)

# lest add the indicator, the indicator number and the cluster name to the data 

datos$region<-data%>% # add country name 
  filter(area_name != "Grand Total")%>%
  select(c("area_name"))


datos$region <- datos$region$area_name


datos$ind <- Ind # add indicator number 



# we create the join 

map_data<-full_join(mapdata,datos,by="region")
View(map_data)

# countries that are not maching to the map list 
map_data %>%
  filter(is.na(long))%>%
  distinct(region) 


# posible names 
map_data %>%
  filter(is.na(ind))%>%
  distinct(region)
# names changes 
datos1 <- datos %>%
  mutate(region = case_when(
    region == "Cabo Verde" ~ "Cape Verde",
    region == "Congo" ~ "Republic of Congo",
    region == "Côte d'Ivoire" ~ "Ivory Coast",
    region == "Czechia" ~ "Czech Republic",
    region == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    region == "Eswatini" ~ "Swaziland",
    region == "Hong Kong" ~ "Hong Kong",  # Remaining as "Hong Kong"
    region == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
    region == "Taiwan, China" ~ "Taiwan",
    region == "The Gambia" ~ "Gambia",
    region == "Trinidad and Tobago" ~ "Trinidad",
    region == "United Kingdom" ~ "UK",
    region == "United States" ~ "USA",
    TRUE ~ region
  ))

# reacreate the merge but with the correct values 
map_data<-full_join(mapdata,datos1,by="region")

# countries that are not maching to the map list 
map_data %>%
  filter(is.na(long))%>%
  distinct(region) 


# posible names 
map_data %>%
  filter(is.na(ind))%>%
  distinct(region)


#remove hongknog 
mapdata1<-map_data%>%filter(!is.na(mapdata$ind))
View(mapdata1)

# create the maps with the full info 

map1<-ggplot(map_data,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=ind),color="black")+
  scale_fill_gradient2(low = "red", mid = "blue", high = "green", 
                       midpoint = median(map_data$ind, na.rm = TRUE)) +
  labs(fill = "Indicator")
map1

# if we whant to tansformit into a interactive map 
library(plotly)

map1 <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = ind), color = "black") +
  scale_fill_gradient2(low = "red", mid = "blue", high = "green", 
                       midpoint = median(map_data$ind, na.rm = TRUE)) +
  labs(fill = "Indicator")

interactive_map <- ggplotly(map1)

interactive_map

# change the interactive plot to show more information

info <- paste0(
  "<br>Country: ", map_data$region,
  "<br>Cluster: ", map_data$cluster,
  "Economic Quality",map_data$`Economic Quality`,
  "Education",map_data$)
info[1:2]

map_data$info<-info
map1 <- ggplot(map_data, aes(x = long, y = lat, group = group,label=info)) +
  geom_polygon(aes(fill = ind), color = "black") +
  scale_fill_gradient2(low = "red", mid = "blue", high = "green", 
                       midpoint = median(map_data$ind, na.rm = TRUE)) +
  labs(fill = "Indicator")

interactive_map <- ggplotly(map1)

interactive_map
# second try 

#------------------------Final map---------------------------------------------#

map_data$tooltip <- with(map_data, paste("Pais: ", region,
                                         "<br> Indicador",ind,
                                         "<br>Cluster: ", NombreCluster,
                                         "<br>Investment Environment ", `Investment Environment`,
                                         "<br>Market Access and Infrastructure ", `Market Access and Infrastructure`,
                                         "<br>Governance ", Governance,
                                         "<br>Natural Environment ", `Natural Environment`,
                                         "<br>Social Capital ", `Social Capital`))

map1 <- ggplot(map_data, aes(x = long, y = lat, group = group, text = tooltip)) +
  geom_polygon(aes(fill = ind), color = "black") +
  scale_fill_gradient2(low = "red", mid = "blue", high = "green", 
                       midpoint = median(map_data$ind, na.rm = TRUE))+
  labs(title = "Mapa Interactivo por Indicador",
       subtitle = "Información detallada disponible al pasar el cursor sobre cada país",
       fill = "Cluster",
       caption = "Fuente: Datos Proporcionados") +
  theme_minimal() +
  theme(legend.position = "right")
  labs(fill = "Indicator")

interactive_map <- ggplotly(map1, tooltip = "text")

interactive_map

#-------------------------map by cluster--------------------------------------#

# Set the order of the clusters for the legend
cluster_order <- c("Cluster de Economía Líder 5", "Cluster de Economía Avanzada 4",
                   "Cluster de Economía en Transición 3", "Cluster de Economía en Desarrollo 2", 
                   "Cluster de Economía Emergente 1")

# Factor the 'NombreCluster' with the specified order
map_data$NombreCluster <- factor(map_data$NombreCluster, levels = cluster_order)

# Now generate the plot with actual color codes
map1 <- ggplot(map_data, aes(x = long, y = lat, group = group, text = tooltip)) +
  geom_polygon(aes(fill = NombreCluster), color = "black", size = 0.2) +
  scale_fill_manual(values = c("Cluster de Economía Líder 5" = "#1f77b4", 
                               "Cluster de Economía Avanzada 4" = "#ff7f0e",
                               "Cluster de Economía en Transición 3" = "#2ca02c",
                               "Cluster de Economía en Desarrollo 2" = "#d62728",
                               "Cluster de Economía Emergente 1" = "#9467bd"),
                    labels = cluster_order) +
  labs(title = "Mapa Interactivo por Cluster",
       subtitle = "Información detallada disponible al pasar el cursor sobre cada país",
       fill = "Cluster",
       caption = "Fuente: Datos Proporcionados") +
  theme_minimal() +
  theme(legend.position = "right")

# Convert to an interactive plot with plotly
interactive_map <- ggplotly(map1, tooltip = "text")

# Display the interactive map
interactive_map

saveWidget(interactive_map, file="map_cluster1.html")



#simple map two 

map2<-ggplot(map_data,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=cluster),color="black")
map2

# same map for clusters characteristics 














#--------------------------more interactive map------------didnt work --------------------# 

library(leaflet)

# Assuming 'map_data' has columns 'long', 'lat', 'ind', 'region', 'group', and 'info'

# Create a color palette for the 'ind' variable
color_pal <- colorNumeric(
  palette = c("red", "blue", "green"),
  domain = map_data$ind
)

# Create the leaflet map
leaflet_map <- leaflet(map_data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(
    lng = ~long, 
    lat = ~lat, 
    fillColor = ~color_pal(ind), 
    weight = 1, 
    color = "black",
    fillOpacity = 0.7,
    popup = ~paste("Region: ", region, 
                   "<br>Cluster: ", cluster,
                   "<br>Indicator: ", round(ind, 2), 
                   "<br>Info: ", info)
  ) %>%
  addLegend("bottomright", 
            pal = color_pal, 
            values = ~ind,
            title = "Indicator",
            opacity = 1)

# Print the map
leaflet_map

leaflet::mapshot(leaflet_map)

install.packages("mapview")
library(mapview)

mapshot(leaflet_map, file = "map.html")
