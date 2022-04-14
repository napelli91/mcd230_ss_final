if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    'gstat',
    'ggplot2',
    'ggpubr',
    'geoR',
    'spdep',
    'sp',
    'dplyr',
    'automap',
    'Metrics',
    'viridis'
)

## librerias utilizadas
library(gstat)
library(geoR)
library(spdep)
library(sp)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(viridis)

library(automap)
library(Metrics)
library(FNN)

## Cargando datos limpios

load('data/spain_geodata_temperature_1989_spring.RData')
load('data/spain_preprocessed_data.RData')

validation.data <- sp_data_bin
sp_data_final_clean = sp_data_final_clean
sp_geo_data_clean = sp_geo_data_clean

sp_data_final_clean = sp_data_final_clean %>% select(longitude,latitude,temp)

rm(sp_data, sp_data_bin, sp_data_final)

## Calculamos el summary de los datos

summary(sp_data_final_clean)
summary(sp_geo_data_clean)

## vemos los datos geo

plot(sp_geo_data_clean)

## convertimos a SpatialDataFrame

coordinates(sp_data_final_clean) <- ~ longitude + latitude

## Calculo del variograma con tendencia

vg.emp <- variogram(temp ~ longitude + latitude, sp_data_final_clean, cutoff = 7)

vg.emp.plot <- vg.emp %>% ggplot() +
               geom_point(aes(x = dist, y = gamma),
                          shape = 6,
                          size = 1) +
               ylim(0,3) +
               labs( x = 'Distance',
                     y = 'Semivarianza')


vg.emp.plot


## calculamos los parametros del variograma fitteado

vg.fit = fit.variogram(vg.emp,
                       vgm(1, c("Exp", "Sph" ,"Gau" ,"Mat"), 2.15, 1))

vg.fit

preds = variogramLine(vg.fit, maxdist = max(vg.emp$dist))

vg.fit.plot <- vg.emp %>% ggplot(aes(x = dist, y = gamma)) +
    geom_point(shape = 6,
               size = 2,
               colour = 'yellow') +
    geom_line(data = preds,
             colour = 'blue',
             size = 1) +
    ylim(0,3) +
    labs( x = 'Distance',
          y = 'Semivarianza') +
    theme_dark()
vg.fit.plot


## Alternativa

# Choosing the best variogram
vg.fit.alternative = autofitVariogram(temp~longitude+latitude,
                             sp_data_final_clean,
                             model = c("Exp", "Sph" ,"Gau" ,"Mat"),
                             kappa = seq(0,2,0.05),
                             fix.values = c(NA),
                             start_vals = c(0,1,2.5),
                             verbose = T)
vg.best.fit <- vg.fit.alternative$var_model

vg.best.fit

altpreds = variogramLine(vg.best.fit, maxdist = max(vg.emp$dist))

vg.altfit.plot <- vg.emp %>% ggplot(aes(x = dist, y = gamma)) +
    geom_point(shape = 6,
               size = 2,
               colour = 'white') +
    geom_line(data = altpreds,
              colour = 'blue',
              size = 1) +
    geom_line(data = preds,
              colour = 'yellow',
              size = 1) +
    ylim(0,3) +
    labs( x = 'Distance',
          y = 'Semivarianza') +
    theme_dark()
vg.altfit.plot


attr(vg.best.fit, 'SSErr')
attr(vg.fit, 'SSErr')


## Una vez seleccionados los modelos vamos a crear una grilla de predicción
## para ello vamos a tomar un sector interno del mapa de datos y vamos a analizar

# los límites totales los podemos sacar de summary:
#
#             Coordinates:
#             min  max
# latitude   36 43.5
# longitude  -9  2.5
#
# pero podemos ver de graficar el mapa que estaríamos considerando datos fuera de españa,
# con lo que vamos a definir de 1 punto decimal en cada coordenada.

g.lon.min = -8
g.lon.max = 1
g.lat.min = 37
g.lat.max = 42


grid.to.pred <- expand.grid(longitude = seq(g.lon.min, g.lon.max, by = 0.2),
                            latitude = seq(g.lat.min, g.lat.max, by	 = 0.2))
#View(grid.to.pred)
#class(grid.to.pred)

gridded(grid.to.pred) <-  ~ longitude + latitude
plot(grid.to.pred)



## Ahora que generamos la grilla vamos a hacer el cokrirgging

krig.fit <- krige(temp ~ longitude + latitude,
                  locations = sp_data_final_clean,
                  newdata = grid.to.pred,
                  model = vg.fit)

krig.alt.fit <- krige(temp ~ longitude + latitude,
                      locations = sp_data_final_clean,
                      newdata = grid.to.pred,
                      model = vg.best.fit)

## Ploteamos nuestras predicciones sobre la grilla

spplot(krig.fit["var1.pred"],
       main = "Kriging universal: Valores Predichos (Exp)",
       col.regions = terrain.colors)
spplot(krig.fit["var1.var"],
       main = "Kriging universal: Varianza de las Predicciones (Exp)",
       col.regions = terrain.colors)

spplot(krig.alt.fit["var1.pred"],
       main = "Kriging ordinario: Valores Predichos (Mat)",
       col.regions = terrain.colors)
spplot(krig.alt.fit["var1.var"],
       main = "Kriging ordinario: Varianza de las Predicciones (Mat)",
       col.regions = terrain.colors)


krig.fit %>% as.data.frame %>%
    ggplot(aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "yellow", high="red") +
    #scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    theme_bw()

krig.alt.fit %>% as.data.frame %>%
    ggplot(aes(x = longitude, y = latitude)) +
    geom_tile(aes(fill = var1.pred)) + coord_equal() +
    scale_fill_gradient(low = "yellow", high="red") +
    #scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    theme_bw()


## Tomamos los datos iniciales que recortamos y vamos a testear

validation.coords <- validation.data[,c(1,2)]

coordinates(validation.coords) <- ~ longitude + latitude

krig.val.data <- krige(temp ~ longitude + latitude,
                       locations = sp_data_final_clean,
                       newdata = validation.coords,
                       model = vg.fit)

krig.val.data.alt <- krige(temp ~ longitude + latitude,
                       locations = sp_data_final_clean,
                       newdata = validation.coords,
                       model = vg.best.fit)


kriging_error = sqrt(mean((validation.data$temp - krig.val.data$var1.pred)^2))

# K-nearest neighbours
k_vector = seq(1, 20)
k_error_vector = c()
for (k in k_vector){
    knn_model = knn.reg(sp_data_final_clean@coords, test = NULL,
                        sp_data_final_clean$temp, k = k)
    k_error = sqrt(mean((sp_data_final_clean$temp - knn_model$pred)^2))
    k_error_vector = c(k_error_vector, k_error)

}

plot(k_vector, k_error_vector, type="l")



## a partir de los modelos obtenidos vamos a crossvalidar las regresiones creadas

tend = formula(temp ~ longitude + latitude)

valcruz1 <- krige.cv(formula = tend,
                     locations = sp_data_final_clean,
                     model = vg.fit,
                     nfold = 153,
                     verbose = T,
                     debug.level = 10)
valcruz2 <- krige.cv(formula = tend,
                     locations = sp_data_final_clean,
                     model = vg.best.fit,
                     nfold = 153,
                     verbose = T,
                     debug.level = 10)

valcruz1
valcruz2


# Error medio de predicción.
# Se espera que sea lo mas proximo a cero posible.
mean(valcruz1$residual)
mean(valcruz2$residual)

# Error cuadratico medio de predicción.
# Se espera que sea lo mas chico posible.
mean(valcruz1$residual^2)
mean(valcruz2$residual^2)

# Error cuadrático medio normalizado.
# Se espera que sea lo mas proximo a 1 posible.
mean(valcruz1$zscore^2)
mean(valcruz2$zscore^2)

# Correlación lineal entre valores observados y predichos
cor(valcruz1$observed, valcruz1$observed - valcruz1$residual)
cor(valcruz2$observed, valcruz2$observed - valcruz2$residual)

# Correlaci?n lineal entre valores observados y predichos
par(mfrow = c(1,2))
plot(valcruz1$observed,valcruz1$observed - valcruz1$residual,
     xlab="Observados (Exp)",
     ylab="Predichos (Exp)")
plot(valcruz2$observed,valcruz2$observed - valcruz2$residual,
     xlab="Observados (Mat)",
     ylab="Predichos (Mat)")

r1 <- valcruz1$observed - valcruz1$residual
regresion1 <- lm(valcruz1$observed ~ r1, data = valcruz1)
summary(regresion1)


r2 <- valcruz2$observed - valcruz2$residual
regresion2 <- lm(valcruz2$observed ~ r2, data = valcruz2)
summary(regresion2)
##################################


krig.cv.plot1 <- valcruz1@data %>% ggplot(aes(x = observed, y = observed - residual)) +
    geom_point(colour = "turquoise3",
               shape = 15) +
    labs( x = "Observados (Exp)",
          y = "Predichos (Exp)") +
    geom_smooth(formula = y ~ x,
                method = lm,
                aes(x = observed,
                    y = observed - residual),
                show.legend = T,
                colour = "#cd0800",
                fill = "#eb9c99")

krig.cv.plot2 <- valcruz2@data %>% ggplot(aes(x = observed, y = observed - residual)) +
    geom_point(colour = "turquoise3",
               shape = 15) +
    labs( x = "Observados (Mat)",
          y = "Predichos (Mat)") +
    geom_smooth(formula = y ~ x,
                method = lm,
                aes(x = observed,
                    y = observed - residual),
                show.legend = T,
                colour = "#cd0800",
                fill = "#eb9c99")

ggarrange(krig.cv.plot1,
          krig.cv.plot2,
          ncol = 2,
          nrow = 1)


# data(world)
# spain_shp <- world %>% filter(name_long == 'Spain') %>% dplyr::select(geom)
#
# ggplot() +
#     geom_sf(data = spain_shp$geom) +
#     geom_point(data = sp_data,
#                aes(x = longitude,
#                    y = latitude,
#                    color = temp
#                ),
#                alpha = .5) +
#     coord_sf(datum=st_crs(4326))


polys = as(krig.fit,"SpatialPolygonsDataFrame")
polys_sf = as(polys, "sf")
points_sf = as(krig.fit, "sf")


# Plot in original  projection (note that in this case the cells are squared):
my_theme <- theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())

dens.plot.1 <- ggplot(polys_sf) +
    geom_sf(aes(fill = var1.pred)) +
    ggtitle("Valores predichos [Exp]") +
    coord_sf(datum=st_crs(4326)) +
    scale_fill_viridis(option = "magma", direction = -1)
    # + my_theme

polys = as(krig.best.fit,"SpatialPolygonsDataFrame")
polys_sf = as(polys, "sf")
points_sf = as(krig.fit, "sf")
dens.plot.2 <- ggplot(polys_sf) +
    geom_sf(aes(fill = var1.pred)) +
    ggtitle("Valores predichos [Mat]") +
    coord_sf(datum=st_crs(4326)) +
    scale_fill_viridis(option = "magma", direction = -1)
    # + my_theme

dens.plot.1
dens.plot.2
