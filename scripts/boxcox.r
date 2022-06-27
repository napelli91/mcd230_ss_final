load(file = "./data/spain_geodata_temperature_1989_spring.RData")

## librerias básicas
library(dplyr)
library(ggplot2)
library(ggpubr)
library(readr)
library(reshape2)

## Librerias de geo estadística
library(geoR)
library(gstat)
library(rgdal)
library(sp)
library(spdep)

library(MASS)


powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {

    boxcoxTrans <- function(x, lam1, lam2 = NULL) {

        # if we set lambda2 to zero, it becomes the one parameter transformation
        lam2 <- ifelse(is.null(lam2), 0, lam2)

        if (lam1 == 0L) {
            log(y + lam2)
        } else {
            (((y + lam2)^lam1) - 1) / lam1
        }
    }

    switch(method
           , boxcox = boxcoxTrans(y, lambda1, lambda2)
           , tukey = y^lambda1
    )
}


bc2 <- boxcoxfit(sp_data_final$temp, lambda2 = F)



bcfit = boxcox(lm(sp_data_final$temp ~ 1),lambda = seq(-3,5,0.05))
bcfit

lambda <- bcfit$x[which.max(bcfit$y)]


temp_sqrt <- powerTransform(sp_data_final$temp, 0.5)
temp_quad <- powerTransform(sp_data_final$temp, 2)
temp_opti <- powerTransform(sp_data_final$temp, lambda)
temp_log <- powerTransform(sp_data_final$temp, 0)

sp_data_final$temp_sqrt = temp_sqrt
sp_data_final$temp_quad = temp_quad
sp_data_final$temp_opti = temp_opti
sp_data_final$temp_log = temp_log

sp_data_final

d = sp_data_final %>% dplyr::select(longitude,latitude,temp_5) # guardamos el set de datos recortado en un objeto

coordinates(d) = ~longitude+latitude
gd <- as.geodata(d) # lo pasamos a un objetivo geodata para poder graficarlo

plot(gd, lowess=TRUE)


temp_sqrt <- powerTransform(sp_data_bin$temp, 0.5)
temp_quad <- powerTransform(sp_data_bin$temp, 2)
temp_opti <- powerTransform(sp_data_bin$temp, lambda)
temp_log <- powerTransform(sp_data_bin$temp, 0)

sp_data_bin$temp_sqrt = temp_sqrt
sp_data_bin$temp_quad = temp_quad
sp_data_bin$temp_opti = temp_opti
sp_data_bin$temp_log = temp_log

write.csv(sp_data_final,'./data/spain_temp_preprocessed_BoxCox_training_data.csv')
write.csv(sp_data_bin,'./data/spain_temp_preprocessed_BoxCox_validation_data.csv')
