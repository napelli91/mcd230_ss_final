library(dplyr)
library(ggplot2)
library(geoR)
library(readr)

## Leemos el dataset inicial

lon_range = c(-10.5 , 1.9)
lat_range = c(49.0 , 59.0)


csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"

data <- read.csv(csv_loc)

