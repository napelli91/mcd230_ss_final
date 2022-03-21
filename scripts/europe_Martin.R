#Para que funcione el mapa

library(dplyr)
library(geoR)
library(ggplot2)
library(gstat)
library(mapview)
library(rgdal)
library(readr)
library(sf)
library(sp)
library(spdep)

#####EDA#####
## Leemos el dataset inicial y separamos las lat y lon
lon_range = c(-10.5 , 1.9)
lat_range = c(49.0 , 59.0)

csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"
data <- read.csv(csv_loc)

data <- data %>% dplyr::select(-X)
names(data) <- c('lat', 'lon', 'temp' )

data_clean <- filter(data, lat>49&lat<59,lon>-10.5&lon<1.9)
table(data_clean$temp == -999.99) ["TRUE"]
data_clean <- filter(data_clean,temp != -999.99)

datos_sf <- sf::st_as_sf(data_clean, coords = c("lon", "lat"), crs = "4267")
View(data_clean)
plot(data_clean)
plot(datos_sf)
mapview(datos_sf,
        zcol = "temp",
        alpha.regions = 0.5,
        col.regions = terrain.colors,
        native.crs=TRUE)

data(world)
uk_map <- world %>% filter(name_long == 'United Kingdom') %>% dplyr::select(geom)

ggplot() +
    geom_sf(data = uk_map$geom) +
    geom_point(data = data_clean,
               aes(x = lon,
                   y = lat,
                   color = temp
               ),
               alpha = .5) +
    coord_sf(datum=st_crs(4326))

old_par = mfrow=c(1,1)
par(mfrow=c(1,2))
# Histograma de los datos
hist(data_clean$temp,col='red',nclass=15,main="Histograma",ylab='Frecuencia Relativa',xlab='Temperatura')
#box-plot de los datos
boxplot(data_clean$temp,col='green',ylab='Temperatura',main="Box-Plot")

par(mfrow=c(1,1))
#first approach
data_geo <-cbind(data_clean$lon,data_clean$lat,data_clean$temp)
vg<-as.geodata(data_geo)
plot(vg)
#Se ve cierta dependencia de Y, latitud lo cual tiene sentido. Parecería ser una distrución normal

pixel <-coordinates(vg[1])
grilla <- dnearneigh(pixel,0,2)
plot(grilla ,pixel)
pesos <- nb2listw(grilla, style = "W")

#Con Geary y Moran se rechaza la hipótesis nula, hay dependencia espacial
moran_test <- moran.test(vg$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
geary_test <- geary.test(vg$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
# Generamos un gr?fico que eval?a cuan similar es
# cada dato con respecto a los datos de sus vecinos
M<-moran.plot(vg$data,pesos,zero.policy=F,col=3, quiet=T,labels=T,xlab="Temperatura", ylab="lag(Temperatura)")
View(M)
#Calculamos el ?ndice de Moran local y
#mostramos los resultados
ML <- localmoran(vg$data,pesos,alternative ="less")
IML<-printCoefmat(data.frame(ML,row.names=data_clean$Casos),check.names=FALSE)

data_clean_sp <- data_clean
coordinates(data_clean_sp) = ~lon+lat

nube_clasica <- variog(vg, option = "cloud")
plot(nube_clasica, main = "classical estimator")

vg.map <- variogram(temp~1, data_clean_sp, cutoff = 8, width = 1, map = T)
plot(vg.map)

bin_clasico <- variog(vg, uvec=seq(0,10,l=12))
plot(bin_clasico)

v <- variogram(temp~1, data_clean_sp, cutoff=3, width=0.5, map=T)
plot(v)
v <- variogram(temp~1, data_clean_sp)
plot(v)

# eyefit para ver como da
res1.v.ef=eyefit(bin_clasico)

v4_sel = fit.variogram(v, vgm(5, c("Exp", "Sph" ,"Gau" ,"Mat"), 8, 0))
v4_sel
plot(v , v4_sel)
attr(v4_sel, 'SSErr')


