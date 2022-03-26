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

plot(try_dist,stat_moran)
plot(try_dist,stat_geary)

