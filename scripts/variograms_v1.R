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

#variograma con tendencia en x e y
v_nube_tend <- variogram(temp~latitude+longitude, d, cloud=T)
plot(v_nube_tend)

v_map_tend <- variogram(temp~latitude+longitude, d, cutoff = 4, width = 1, map=T)
plot(v_map_tend)

v_tend <- variogram(temp~latitude+longitude, d, cutoff = 7)
plot(v_tend)

## variograma empirico
vg_trend <- variog(gd, trend = "1st", uvec = seq(0,7, l = 20))
plot(vg_trend)

# intervalos de simulacion por permutacion aleatoria de los residuos
s1 = variog.mc.env(gd, obj = vg_trend)
plot(vg_trend, env = s1)

#ajusto variograma teórico
vt_exp = fit.variogram(v_tend, vgm(2.2, "Exp", 0.78, 0))#, fit.method = 6)
vt_exp
plot(v_tend, vt_exp)

vt_mat = fit.variogram(v_tend, vgm(2.05, "Mat", 0.28, 0, kappa = 1.8)
                       , fit.kappa = T)
vt_mat
plot(v_tend, vt_mat)

vt_esf = fit.variogram(v_tend, vgm(2.02, "Sph", 1.50, 0))
vt_esf
plot(v_tend, vt_esf)

vt_bes = fit.variogram(v_tend, vgm(2.1, "Bes", 0.42, 0))
vt_bes #Besel
plot(v_tend, vt_bes)

vt_pen = fit.variogram(v_tend, vgm(2.03, "Pen", 1.87, 0))
vt_pen #Pentaspherical
plot(v_tend, vt_pen)

#graficos
vt_mat_2 = variogramLine(vt_mat, maxdist = max(v_tend$dist))
vt_bes_2 = variogramLine(vt_bes, maxdist = max(v_tend$dist))
vt_pen_2 = variogramLine(vt_pen, maxdist = max(v_tend$dist))

vg.mat.plot <- v_tend %>% ggplot(aes(x = dist, y = gamma)) +
    geom_point(shape = 6,
               size = 2,
               colour = 'white') +
    geom_line(data = vt_mat_2,
              colour = 'blue',
              size = 1) +
    ylim(0,3) +
    labs( x = 'Distance',
          y = 'Semivarianza') +
    theme_dark()
vg.mat.plot

vg.bes.plot <- v_tend %>% ggplot(aes(x = dist, y = gamma)) +
    geom_point(shape = 6,
               size = 2,
               colour = 'white') +
    geom_line(data = vt_bes_2,
              colour = 'yellow',
              size = 1) +
    ylim(0,3) +
    labs( x = 'Distance',
          y = 'Semivarianza') +
    theme_dark()
vg.bes.plot

vg.pen.plot <- v_tend %>% ggplot(aes(x = dist, y = gamma)) +
    geom_point(shape = 6,
               size = 2,
               colour = 'white') +
    geom_line(data = vt_pen_2,
              colour = 'green',
              size = 1) +
    ylim(0,3) +
    labs( x = 'Distance',
          y = 'Semivarianza') +
    theme_dark()
vg.pen.plot

#errores
attr(vt_exp, 'SSErr')
attr(vt_esf, 'SSErr')
attr(vt_mat, 'SSErr')
attr(vt_bes, 'SSErr')
attr(vt_pen, 'SSErr')
