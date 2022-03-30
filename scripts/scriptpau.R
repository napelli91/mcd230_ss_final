library(gstat)
library(geoR)
library(spdep)

d = sp_data_final
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

vario.4 <- variog4(gd, max.dist = 5)

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
v_nube_tend_y <- variogram(temp~latitude, d, cloud=T)
plot(v_nube_tend_y)
v_map_tend_y <- variogram(temp~latitude, d, cutoff=1, width = 1, map=T)
plot(v_map_tend_y)
#aun agregando la tendencia no queda lindo
#ver más parámetros para variogram?

## variograma empirico
bin_clasico <- variog(temp~latitude, uvec=seq(0,8,l=18))
plot(bin_clasico)


#data_clean <- filter(df, Latitude>-83.98&Latitude<-79.51,Longitude>24.98&Longitude<31.55)

#ajusto variograma teórico
v = variogram(temp~latitude, d)
plot(v)

vt_exp = fit.variogram(v, vgm(2.5, "Exp", 4, 1))
vt_exp
plot(v, vt_exp)

vt_sph = fit.variogram(v, vgm(2.5, "Sph", 4, 1))
vt_sph
plot(v , vt_sph)
#creo que este me gusta mas

attr(vt_exp, 'SSErr')
attr(vt_sph, 'SSErr')
#tiene menos error también
