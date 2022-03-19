# mcd230_ss_final
práctica de estadística espacial y geoestadística

## Como usar el repositorio

el orden del repositorio es por carpetas

```
 /
 |-> data/ # datasets como RDS object para cargar en el entorno.
 | 	|-> Europa/
 |	|	|...
 |	|-> Pajaritos/
 |	|	|...
 |-> scripts/ #Scripts de an?lisis
 |	|...
 |-> content/ #Sectores del informe
 |	|...
 |-> main.rmd #markdown notebook central
 |-> requirements.txt #Librerias que se van a usar
```

Para instalar las librerias lo que hay que correr la primera vez desde la terminal es:

```
scripts/install.R requirements.txt
```

en requirements.txt hay que listar los paquetes que vamos a utilizar

## Como cargar los datasets

los datsets estan guardados como objetos RDS, para poder cargarlos o guardarlos se pueden
usar las funciones `saveRDS` y `readRDS` para guardar y leer respectivamente. Ejemplito:

```r

z = runif(10)
#> z
# [1] 0.93781501 0.04437402 0.04995689 0.01317236 0.83926320 0.72379189
# [7] 0.07266764 0.12712897 0.55884653 0.26702832
#that's some random numbers - save them in a file:

saveRDS(z, file="/path/to/file/zrandom.rds")
#now remove the object and read the file back into another object:
rm(z)


z2 = readRDS(file="/path/to/file/zrandom.rds")
#z2
# [1] 0.93781501 0.04437402 0.04995689 0.01317236 0.83926320 0.72379189
# [7] 0.07266764 0.12712897 0.55884653 0.26702832
#and I've got the same numbers back.
```

Otra manera es utilizar directamente RData que es simplemente pedirle a R que guarde los 
datos, y se hace el commit de eso

## Markdown

se puede trabajar de a pedazos de notebook y en el archivo `main.rmd` se hace un knitting 
de las partes. O podemos trabajar todo un solo conjunto.
