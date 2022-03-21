##Script PAJARITOS
library(gstat)
library(geoR)
library(spdep)

AKN.pntXY@coords = (AKN.pntXY@coords)/1e4
#unique(AKN.pntXY@data$Observation.Date)

AKN.pntXY2 = AKN.pntXY[AKN.pntXY@data$Observation.Date == "2006-05-09",]
plot(AKN.pntXY2)

v_nube <- variogram(Dens~1, AKN.pntXY2, cloud=T)
plot(v_nube)

v_map <- variogram(Dens~1, AKN.pntXY2, cutoff=80, width = 10, map=T)
plot(v_map)


#df = as.data.frame(AKN.pntXY)
#class(df)
#colnames(df)

#df$Latitude = df$Latitude/1e4
#df$Longitude = df$Longitude/1e4

#min(df$Longitude)
#min(AKN.pntXY@coords[,1])

#datosg<-as.geodata(AKN.pntXY2)
#plot(datosg)

#4267

#data_clean <- filter(df, Latitude>-83.98&Latitude<-79.51,Longitude>24.98&Longitude<31.55)
#esto me dice que no encuentra longitude nidea
