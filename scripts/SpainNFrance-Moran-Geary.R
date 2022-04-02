library(udunits2)
library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(raster)
library(geoR)
library(sf)
library(spData)
library(spdep)
library(lattice)
library(rgdal)

load(file = "spain_geodata_temperature_1989_spring.RData")
load(file = "france_geodata_temperature_1989_spring.RData")

###SCRIPT DE TRABAJO PARA INDICES DE AUTOCORRELACIÓN ESPACIAL###

#Tanto Moran cómo Geary representan medidas de auto-correlación lineal
#Asume media y varianza constante, cosa que posiblemente no se cumpla con respecto a la media.


###EMPEZAMOS POR ESPAÑA###

#Tanto para Moran cómo para Geary
sp_geo_data <- as.geodata(cbind(sp_data_final$longitude,sp_data_final$latitude,sp_data_final$temp))
pixel <-coordinates(sp_geo_data[1])
grilla <- dnearneigh(pixel,0,2)
plot(grilla ,pixel)
pesos <- nb2listw(grilla, style = "W")

sum_neigthbours <- 0
vector <- seq(1,length(grilla))
for (i in vector) {
    print(i)
    print(length(grilla[[i]]))
    sum_neigthbours <- sum_neigthbours + length(grilla[[i]])}

vecinos_prom <- sum_neigthbours/163

#Con Geary y Moran se rechaza la hipótesis nula, hay dependencia espacial
moran_test <- moran.test(sp_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
geary_test <- geary.test(sp_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)

#Entendemos que hay auto-correlación, nos gustaría hacer un pequeño examen de estabilidad
#en función a la distancia de los vecinos.

try_dist <- seq(1.5,8,0.25)
stat_moran <- NULL
stat_geary <- NULL

for (i in try_dist){
    grilla <- dnearneigh(pixel,0,i)
    pesos <- nb2listw(grilla, style = "W")
    moran_test_try <- moran.test(sp_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
    geary_test_try <- geary.test(sp_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
    stat_moran <- append(stat_moran, moran_test_try$statistic)
    stat_geary <- append(stat_geary, geary_test_try$statistic)
}

dat_gg_mg <- data.frame(try_dist, stat_moran, stat_geary)

g1 = ggplot(dat_gg_mg) +
    geom_line(aes(x = try_dist, y = stat_moran),size = 1, col = "turquoise3") +
    geom_line(aes(x = try_dist, y = stat_geary), size = 1, col = "orange") +
    geom_hline(yintercept = 20, colour = "purple") +
    ylab("Estadístico") +
    xlab("Máxima distancia de vecinos")+
    theme(legend.position = "bottom", legend.justification = c("right", "top"))+
    ggtitle("Estadísticos de Moran y Geary según distancia de vecinos")

g1

grilla1 <- dnearneigh(pixel,0,2)
grilla2 <- dnearneigh(pixel,0,3)
par(mfrow=c(1,2))
plot(grilla1 ,pixel)
plot(grilla2 ,pixel,col=alpha("red",0.9))

# Generamos un gr?fico que eval?a cuan similar es
# cada dato con respecto a los datos de sus vecinos
M<-moran.plot(sp_geo_data$data,pesos,zero.policy=F,col=3, quiet=T,labels=T,xlab="Temperatura", ylab="lag(Temperatura)")
View(M)

#Calculamos el ?ndice de Moran local y
#mostramos los resultados
ML <- localmoran(sp_geo_data$data,pesos,alternative ="less")
IML<-printCoefmat(data.frame(ML,row.names=sp_geo_data$Casos),check.names=FALSE)

#IML
#Se eliminan datos dísimiles positivos y negativos
#FALSE: dato similar a su entorno.
Elim=M$is_inf
Elim

#Moran Local
neg_Moran_Local <- subset(IML, Pr.z...E.Ii.. < 0.05 & Z.Ii < 0)
neg_IML <- as.numeric(rownames(neg_Moran_Local))

sp_data_final_clean <- sp_data_final[-c(neg_IML), ]
sp_geo_data_clean <- as.geodata(cbind(sp_data_final_clean$longitude,sp_data_final_clean$latitude,sp_data_final_clean$temp))

pixel_c <-coordinates(sp_geo_data_clean[1])
grilla_c <- dnearneigh(pixel_c,0,3)
plot(grilla_c ,pixel_c)
pesos_c <- nb2listw(grilla_c, style = "W")

#Con Geary y Moran se rechaza la hipótesis nula, hay dependencia espacial
moran_test_clean <- moran.test(sp_geo_data_clean$data, pesos_c,randomisation=FALSE)
geary_test_clean <- geary.test(sp_geo_data_clean$data, pesos_c,randomisation=FALSE)


###AHORA VAMOS POR FRANCIA###

#Tanto para Moran cómo para Geary
fr_geo_data <- as.geodata(cbind(fr_data_final$longitude,fr_data_final$latitude,fr_data_final$temp))

pixel <-coordinates(fr_geo_data[1])
grilla <- dnearneigh(pixel,0,3)
plot(grilla ,pixel)
pesos <- nb2listw(grilla, style = "W")

#Con Geary y Moran se rechaza la hipótesis nula, hay dependencia efracial
fr_moran_test <- moran.test(fr_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
fr_geary_test <- geary.test(fr_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)

#Entendemos que hay auto-correlación, nos gustaría hacer un pequeño examen de estabilidad
#en función a la distancia de los vecinos.

try_dist <- seq(1.5,8,0.25)
stat_moran <- NULL
stat_geary <- NULL

for (i in try_dist){
    grilla <- dnearneigh(pixel,0,i)
    pesos <- nb2listw(grilla, style = "W")
    moran_test_try <- moran.test(fr_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
    geary_test_try <- geary.test(fr_geo_data$data, nb2listw(grilla, style = "W"),randomisation=FALSE)
    stat_moran <- append(stat_moran, moran_test_try$statistic)
    stat_geary <- append(stat_geary, geary_test_try$statistic)
}

fr_dat_gg_mg <- data.frame(try_dist, stat_moran, stat_geary)

g2 = ggplot(fr_dat_gg_mg) +
    geom_line(aes(x = try_dist, y = stat_moran),size = 1, col = "turquoise3") +
    geom_line(aes(x = try_dist, y = stat_geary), size = 1, col = "orange") +
    geom_hline(yintercept = 10, colour = "purple") +
    ylab("Estadístico") +
    xlab("Máxima distancia de vecinos")+
    theme(legend.position = "bottom", legend.justification = c("right", "top"))+
    ggtitle("Estadísticos de Moran y Geary según distancia de vecinos")

g2

grilla1 <- dnearneigh(pixel,0,2)
grilla2 <- dnearneigh(pixel,0,3)
par(mfrow=c(1,2))
plot(grilla1 ,pixel)
plot(grilla2 ,pixel,col=alpha("red",0.9))

# Generamos un gr?fico que eval?a cuan similar es
# cada dato con refrecto a los datos de sus vecinos
fr_M<-moran.plot(fr_geo_data$data,pesos,zero.policy=F,col=3, quiet=T,labels=T,xlab="Temperatura", ylab="lag(Temperatura)")
View(M)
#Calculamos el ?ndice de Moran local y
#mostramos los resultados
fr_ML <- localmoran(fr_geo_data$data,pesos,alternative ="less")
fr_IML<-printCoefmat(data.frame(ML,row.names=fr_geo_data$Casos),check.names=FALSE)

#IML
#Se eliminan datos dísimiles positivos y negativos
#FALSE: dato similar a su entorno.
Elim=M$is_inf
Elim

#Moran Local
neg_Moran_Local <- subset(IML, Pr.z...E.Ii.. < 0.05 & Z.Ii < 0)
neg_IML <- as.numeric(rownames(neg_Moran_Local))

fr_data_final_clean <- fr_data_final[-c(neg_IML), ]
fr_geo_data_clean <- as.geodata(cbind(fr_data_final_clean$longitude,fr_data_final_clean$latitude,fr_data_final_clean$temp))

pixel_c <-coordinates(fr_geo_data_clean[1])
grilla_c <- dnearneigh(pixel_c,0,3)
plot(grilla_c ,pixel_c)
pesos_c <- nb2listw(grilla_c, style = "W")

#Con Geary y Moran se rechaza la hipótesis nula, hay dependencia efracial
moran_test_clean <- moran.test(fr_geo_data_clean$data, pesos_c,randomisation=FALSE)
geary_test_clean <- geary.test(fr_geo_data_clean$data, pesos_c,randomisation=FALSE)



