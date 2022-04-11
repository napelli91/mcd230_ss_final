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

bin_clasico <- variog(gd, uvec=seq(0,8,l=18))
plot(bin_clasico)

bin1 <- variog(gd, uvec = seq(0,8,l=18), bin.cloud = T)
plot(bin1, bin.cloud = T, main = "classical estimator")

vario.4 <- variog4(gd, max.dist = 8)
plot(vario.4, lwd = 2)

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

attr(vt_exp, 'SSErr')
attr(vt_esf, 'SSErr')
attr(vt_mat, 'SSErr')

patos = eyefit(vg_trend)
patos
