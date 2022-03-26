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


###SCRIPT DE TRABAJO PARA INDICES DE AUTOCORRELACIÓN ESPACIAL###

#Tanto Moran cómo Geary representan medidas de auto-correlación lineal
#Asume media y varianza constante, cosa que posiblemente no se cumpla con respecto a la media.


###EMPEZAMOS POR ESPAÑA###

#Tanto para Moran cómo para Geary

pixel <-coordinates(sp_geo_data[1])
grilla <- dnearneigh(pixel,0,2)
plot(grilla ,pixel)
pesos <- nb2listw(grilla, style = "W")

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
    geom_hline(yintercept = 9.92, colour = "purple") +
    ylab("Estadístico") +
    xlab("Máxima distancia de vecinos")+
    theme(legend.position = "bottom", legend.justification = c("right", "top"))+
    ggtitle("Estadísticos de Moran y Geary según distancia de vecinos")

grilla1 <- dnearneigh(pixel,0,2)
grilla2 <- dnearneigh(pixel,0,4)
grilla3 <- dnearneigh(pixel,0,7)
par(mfrow=c(1,3))
plot(grilla1 ,pixel, main="Vecinos Max dist 2",col=alpha("blue",0.9))
plot(grilla2 ,pixel, main="Vecinos Max dist 4",col=alpha("red",0.9))
plot(grilla3 ,pixel, main="Vecinos Max dist 7",col=alpha("green",0.9))

