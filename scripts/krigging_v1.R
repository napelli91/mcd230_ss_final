if(!require(pacman)) install.packages("pacman")
pacman::p_load(
    'gstat',
    'geoR',
    'spdep',
    'sp',
    'dplyr',
    'automap',
    'metrics'
)

## librerias utilizadas
library(gstat)
library(geoR)
library(spdep)
library(sp)
library(dplyr)
library(ggplot2)

library(automap)
library(Metrics)
library(FNN)

## Cargando datos limpios

load('data/spain_geodata_temperature_1989_spring.RData.RData')
load('data/spain_preprocessed_data.RData')

validation.data <- sp_data_bin
sp_data_final_clean = sp_data_final_clean
sp_geo_data_clean = sp_geo_data_clean


rm(sp_data, sp_data_bin, sp_data_final)

## Calculamos el summary de los datos

summary(sp_data_final_clean)
summary(sp_geo_data_clean)

## vemos los datos geo

plot(sp_geo_data_clean)

## convertimos a SpatialDataFrame

coordinates(sp_data_final_clean) <- ~ latitude + longitude

## Calculo del variograma con tendencia

vg.emp <- variogram(temp~latitude+longitude, sp_data_final_clean, cutoff = 7)

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
vg.fit.alternative = autofitVariogram(temp~latitude+longitude,
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
View(grid.to.pred)
class(grid.to.pred)
gridded(grid.to.pred) <-  ~ latitude + longitude
plot(grid.to.pred)



## Ahora que generamos la grilla vamos a hacer el cokrirgging

krig.fit <- krige(temp~latitude+longitude,
                  locations = sp_data_final_clean,
                  newdata = grid.to.pred,
                  model = vg.fit)

krig.alt.fit <- krige(temp~latitude+longitude,
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

coordinates(validation.coords) <- ~ latitude + longitude

krig.val.data <- krige(temp~latitude+longitude,
                       locations = sp_data_final_clean,
                       newdata = validation.coords,
                       model = vg.fit)

krig.val.data.alt <- krige(temp~latitude+longitude,
                       locations = sp_data_final_clean,
                       newdata = validation.coords,
                       model = vg.best.fit)


kriging_error = sqrt(mean((validation.data$temp - exp(krig.val.data$var1.pred))^2))

# K-nearest neighbours
k_vector = seq(1, 20)
k_error_vector = c()
for (k in k_vector){
    knn_model = knn.reg(sp_data_final_clean@coords, test = NULL,
                        sp_data_final_clean$temp, k = k)
    k_error = sqrt(mean((sp_data_final_clean$temp - exp(knn_model$pred))^2))
    k_error_vector = c(k_error_vector, k_error)

}

plot(k_vector, k_error_vector, type="l")
