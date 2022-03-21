print("esto es una prueba")

library(geoR)
library(mapview)
library(leaflet)
library(RColorBrewer)
library(lattice)
library(spdep)
library(rgdal)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(readr)
library(PerformanceAnalytics)
library(ggmap)
library(tibble)
library(caret)

## Leemos el dataset inicial y separamos las lat y lon
lon_range = c(-10.5 , 1.9)
lat_range = c(49.0 , 59.0)

csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"
data <- read.csv(csv_loc)
data_clean <- filter(data, Latitude>49&Latitude<59,Longitude>-10.5&Longitude<1.9)

data_geo <- st_as_sf()



# Histograma de los datos
hist(elevation$data,col='red',nclass=15,main="Histograma",ylab='Frecuencia Relativa',xlab='elevation')

#box-plot de los datos
boxplot(elevation$data,col='green',ylab='elevation',main="Box-Plot")

# en pixel estar?n las coordenadas x-y de todos los puntos
pixel <-coordinates(elevation[1])
# En grilla, se define un vecindario; los vecinos de cada punto,
# son todas las posiciones que est?n a una distancia
# mayor a 0 y menor que 25, del punto
grilla <- dnearneigh(pixel,0,25)
card(grilla)
# Ahora son todas las posiciones que est?n a una distancia
# mayor a 0 y menor que 1.5, del punto
grilla <- dnearneigh(pixel,0,1.5)
card(grilla)

# Generamos un gr?fico que eval?a cuan similar es
# cada dato con respecto a los datos de sus vecinos
M<-moran.plot(elevation$data,pesos,zero.policy=F,col=3, quiet=T,labels=T,xlab="elevation", ylab="lag(elevation)")
View(M)

#Calculamos el ?ndice de Moran local y
#mostramos los resultados
ML <- localmoran(elevation$data,pesos,alternative ="less")
IML<-printCoefmat(data.frame(ML,row.names=elevation$Casos),check.names=FALSE)

# INDICE DE MOR?N GLOBAL (IMG) e ?NDICE DE GEARY(C)
data(package="spdep")
data(oldcol)
View(COL.OLD)
class(COL.OLD)
help(COL.OLD)
x<-COL.OLD$X
y<-COL.OLD$Y
data<-COL.OLD$CRIME
v<-cbind(x,y,data)
vg<-as.geodata(v)
plot(vg)
moran.test(vg$data, nb2listw(COL.nb, style="W"))
geary.test(vg$data, nb2listw(COL.nb, style="W"))

#summary(grilla) da informaci?n similar a card(grilla)
summary(grilla)

pesos <- nb2listw(grilla, style = "W")
pesos <- nb2listw(grilla, style = "S")
moran.test(elevation$data, nb2listw(grilla, style = "S"))
moran.test(elevation$data, nb2listw(grilla, style = "S"),randomisation=FALSE)
geary.test(elevation$data, nb2listw(grilla, style = "S"),randomisation=FALSE)

# Generamos un variograma emp?rico sin tendencia
v <- variogram(cadmium~1, d)

# Ahora con el modelo esf?rico, sin tendencia
vt_sph = fit.variogram(v, vgm(16, "Sph", 1500, 4))
vt_sph
plot(v , vt_sph)

# Comparo los modelos. Calculo la Suma de cuadrado del error
# para cada uno de los modelos ajustados
attr(vt_exp, 'SSErr')
attr(vt_sph, 'SSErr')

# C?mo fijar los valores de cutoff y width?
# Respuesta: Usar el variograma nube
# para ello se usa la funci?n variogram
nube_clasica <- variog(d, option = "cloud")
nube_clasica <- variog(d$cadmium, option = "cloud")

# El problema es que variog es de la familia geodata
class(d)
# transformemos d en un objeto gd de la clase geodata
gd<-as.geodata(d)

class(gd)
nube_clasica <- variog(gd, option = "cloud")
plot(nube_clasica, main = "classical estimator")
