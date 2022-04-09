library(gstat)
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

######

d = sp_data_final_clean
#latitude, longitude y temp
class(d)

coordinates(d) = ~longitude+latitude
summary(d)
#quisiera tener el distance summary

#vemos si tiene tendencia
gd<-as.geodata(d)
class(gd)
plot(gd, lowess=TRUE)
#tendencia negativa en y (latitude)

#variograma sin tendencia
v_nube_sintend <- variogram(temp~1, d, cloud=T)
plot(v_nube_sintend)
v_map_sintend <- variogram(temp~1, d, cutoff=4, width = 1, map=T)
plot(v_map_sintend)

#variograma empirico
nube_clasica <- variog(gd, option = "cloud")
plot(nube_clasica)

nube_CH <- variog(gd, option = "cloud", estimator.type = "modulus")
plot(nube_CH)

bin_clasico <- variog(gd, uvec=seq(0,8,l=18))
plot(bin_clasico)

bin_CH <- variog(gd, uvec=seq(0,8,l=18), estimator.type= "modulus")
plot(bin_CH)

bin1 <- variog(gd, uvec = seq(0,8,l=18), bin.cloud = T)
bin2 <- variog(gd, uvec = seq(0,8,l=18), estimator.type = "modulus", bin.cloud = T)

plot(bin1, bin.cloud = T, main = "classical estimator")
plot(bin2, bin.cloud = T, main = "modulus estimator")

vario.4 <- variog4(gd, max.dist = 8)
plot(vario.4, lwd = 2)
#vario.2 <- variog(elevation, uvec = seq(0,5,l=11), dir=0)
#vario.3 <- variog(elevation, uvec = seq(0,5,l=11), dir=pi/2)
#vario.4 <- variog(elevation, uvec=seq(0,5,l=11), dir=pi/4)

#plot(bin_clasico, type="l")
#lines(vario.2, lty = 2, col = 2)
#lines(vario.3, lty = 3, col = 3)
#lines(vario.4, lty = 4, col = 4)
#legend("topleft", c("omnidireccional", "0", "90", "45"), col=c(1,2,3,4), lty=c(1,2,3,4))


#variograma con tendencia en y
v_nube_tend_y <- variogram(temp~latitude+longitude, d, cloud=T)
plot(v_nube_tend_y)
v_map_tend_y <- variogram(temp~latitude+longitude, d, cutoff = 4, width = 1, map=T)
plot(v_map_tend_y)
#ver más parámetros para variogram?
v_tend_y <- variogram(temp~latitude+longitude, d)
plot(v_tend_y)



## variograma empirico
vg_trend <- variog(gd, trend = "1st", uvec = seq(0,7, l = 20))
plot(vg_trend)
#cortamos en 7

# intervalos de simulacion por permutacion aleatoria de los residuos
s1 = variog.mc.env(gd, obj = vg_trend)
plot(vg_trend, env = s1)

#ajusto variograma teórico
v = variogram(temp~latitude, d)
plot(v)

vgm(3, "Exp", 8,1)

vt_exp = fit.variogram(v_tend_y, vgm(2.19, "Exp", 0.78, 0), fit.method = 6)
vt_exp
plot(v_tend_y, vt_exp)

vt_mat = fit.variogram(v_tend_y, vgm(2.2, "Mat", 1, 0, kappa = 0.1), fit.kappa = T)
vt_mat
plot(v_tend_y, vt_mat)

vt_esf = fit.variogram(v_tend_y, vgm(3, "Sph", 1.14, 0.03))
vt_esf
plot(v_tend_y, vt_esf)

#6 = OLS

#gaussian sigma 2.21 phi 1.14 tausq 0.03 practical range 1.97
#exponential sigma 2.14 phi 1.14 tausq 0.03 practical range 3.41
attr(vt_exp, 'SSErr')
attr(vt_esf, 'SSErr')
attr(vt_mat, 'SSErr')
#tiene menos error también

vt_exp

patos = eyefit(vg_trend)
patos
