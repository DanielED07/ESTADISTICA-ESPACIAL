rm(list=ls())
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(sp)
library(geoR)
library(gstat)
library(rgdal)
library(raster)
setwd("./2da mitad/parcial2/ESTADISTICA-ESPACIAL/")
#brazil_clima <- read.csv2("./2da mitad/parcial2/ESTADISTICA-ESPACIAL/brazil/conventional_weather_stations_inmet_brazil_1961_2019.csv")
#save(brazil_clima,file="brazil.RData")

#sample_brazil <- brazil_clima[brazil_clima$Data=="23/10/2012",]

#datos_brazil <- sample_brazil[,c("Estacao","Data","Hora","VelocidadeVento","Velocidade.do.Vento.Media")]
#save(datos_brazil,file="datos_brazil.RData")
#load("datos_brazil.RData")
# La velocidad esta medida en m/s
#estaciones_ubicaciones <- read.csv2("./2da mitad/parcial2/ESTADISTICA-ESPACIAL/brazil/weather_stations_codes.csv")[,c(2,3,4)]
#names(estaciones_ubicaciones) <- c("Codigo","Latitude","Longitude")

# Union de los datos (Relacionalmente)
#datos_bzl <- merge(x=datos_brazil,y=estaciones_ubicaciones,by.x="Estacao",by.y="Codigo")

viento <- datos_bzl %>% 
  group_by(Estacao,Latitude,Longitude) %>% 
  summarise(Value = max(VelocidadeVento))
#save(viento,file="viento.RData")
load("viento.RData")
# Primera visualizacion de las ubicaciones
plot(viento[,c(2,3)])  


cont_bzl <- shapefile("./brazil/brazil_administrative_boundaries_national_polygon.shp")
#plot(cont_bzl)
cont <- cont_bzl@polygons[[2]]@Polygons[[1]]@coords
cont_b <- list("sp.polygons", Polygon(cont))
plot(cont,type="l")

#----------
viento <- na.omit(data.frame(apply(viento, 2, as.numeric)))
names(viento) <- c("Codigo","Latitude","Longitude", "Viento")
viento <- viento[,c(2,3,4)]
viento_geo <- as.geodata(viento,coords=2:1,var=3,borders=T)
viento_geo$borders <- cont
coordinates(viento) <- ~Longitude+Latitude

# Mapa con ubicaciones y covariable
my.palette <- viridis(100, option = "A")[100:1]
spplot(viento, "Viento",sp.layout =cont_b , colorkey = TRUE, col.regions = my.palette,main = "Velocidad Viento",
       ylim=c(min(cont[,2]),max(cont[,2])),
       xlim=c(min(cont[,1]),max(cont[,1])))


# E.D.A

x_eda <- ggplot()+geom_point(aes(x=viento_geo$coords[,1],y=viento_geo$data))+xlab("X Coord")+ylab("Data")+
  geom_smooth(aes(x=viento_geo$coords[,1],y=viento_geo$data))
y_eda <- ggplot()+geom_point(aes(y=viento_geo$coords[,2],x=viento_geo$data))+ylab("Y Coord")+xlab("Data")+
  geom_smooth(aes(y=viento_geo$coords[,2],x=viento_geo$data))
xy_eda <- spplot(viento, "Viento",sp.layout =cont_b , colorkey = TRUE, col.regions = my.palette,main = "Velocidad Viento",
                 ylim=c(min(cont[,2]),max(cont[,2])),
                 xlim=c(min(cont[,1]),max(cont[,1])))
dist_eda <- ggplot()+geom_histogram(aes(x=viento_geo$data,y=..density..),bins=5,color="black", fill="lightblue")+ geom_density(aes(x=viento_geo$data),col="red") +ylab("Frecuencia")+xlab("Data")
grid.arrange(xy_eda,x_eda,y_eda,dist_eda,ncol=2,nrow=2)


# Normalidad

# Prueba de Shapiro-Wilk para normalidad
shapiro.test(viento_geo$data)

# Q-Q plot
ggplot()+stat_qq(aes(sample = viento_geo$data)) + stat_qq_line(aes(sample =viento_geo$data))


# Variogramas

# max(dist(coordinates(viento)))
# 37.80331

## Cloud

cloud_a <- variogram(Viento  ~ 1, locations = coordinates(viento),viento,cutoff=37.80331/2,cloud=T) # Aleatorio
cloud_d <- variogram(Viento  ~ Longitude+Latitude, locations = coordinates(viento),viento,cutoff=37.80331/2,cloud=T) # Dependencia
c_a <- ggplot(cloud_a,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Cloud (Aleatorio)")
c_d <- ggplot(cloud_d,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Cloud (Dependecia)")
grid.arrange(c_a,c_d,ncol=2)

# Bin

bin_a <- variogram(Viento ~ 1, locations = coordinates(viento),viento,cutoff=20) %>% mutate(Modelo = "Aleatorio")
bin_d <- variogram(Viento  ~ Longitude+Latitude, locations = coordinates(viento),viento,cutoff=20) %>% mutate(Modelo = "Dependiente")

b_a <- ggplot(bin_a,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Bin (Aleatorio)")
b_d <-  ggplot(bin_d,aes(x=dist,y=gamma))+geom_point(shape=21,fill="red")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Bin (Dependiente)")

grid.arrange(b_a,b_d,ncol=2)

# Fit Bin
bin_a.fit <- fit.variogram(bin_a,vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
bin_d.fit <- fit.variogram(bin_d,vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

plot(bin_a,bin_a.fit,main="Ajuste Semivariograma Bin(Aleatorio)")
plot(bin_d,bin_d.fit,main="Ajuste Semivariograma Bin(Dependiente)")

# Ventana para prediccion

#---------------------------------------------
my.palette <- viridis(16, option = "A")[16:1]
#---------------------------------------------
pg <- pred_grid(cont,by=0.5)
gr0 <- pg[.geoR_inout(pg, cont),]
par(mfrow=c(1,1))
points(viento_geo)
points(pg, col=2, pch=19, cex=0.3)
points(gr0, col=3, pch=19, cex=0.3)

#---------------------------------------------

datos_pred.grid <- gr0
colnames(datos_pred.grid) <- c("Longitude","Latitude")
coords_pred <- as.data.frame(coordinates(datos_pred.grid))
gridded(coords_pred) = ~Longitude+Latitude

#---------------------------------------------

# Kriging

viento_a.kriged = krige(Viento~1,viento,coords_pred, model = bin_a.fit)
viento_d.kriged = krige(Viento~Longitude+Latitude,viento,coords_pred, model = bin_d.fit)

# Graficos

## Prediccion
plot_a_pred <- spplot(viento_a.kriged["var1.pred"],col.regions = my.palette,main="Kriging Ordinario Predicciones" ,contour=T,cuts=15)
plot_d_pred <- spplot(viento_d.kriged["var1.pred"],col.regions = my.palette,main="Kriging Universal Predicciones" ,contour=T,cuts=15)

## Varianza
plot_a_var <- spplot(viento_a.kriged["var1.var"],col.regions = my.palette,main="Kriging Ordinario Varianza" ,contour=T,cuts=15)
plot_d_var <- spplot(viento_d.kriged["var1.var"],col.regions = my.palette,main="Kriging Universal Varianza" ,contour=T,cuts=15)

# Predicciones vs. Varianza
grid.arrange(plot_a_pred,plot_a_var,ncol=2)
grid.arrange(plot_d_pred,plot_d_var,ncol=2)

# Comparacion Predicciones
grid.arrange(plot_a_pred,plot_d_pred,ncol=2)

# Comparacion Predicciones y Observado
grid.arrange(plot_a_pred,xy_eda,plot_d_pred,nrow=3)


# Validacion Cruzada

validacion.a <- krige.cv(Viento~1,viento,model=bin_a.fit,nfold=nrow(viento))
validacion.d <- krige.cv(Viento~Longitude+Latitude,viento,model=bin_d.fit,nfold=nrow(viento))

plot_val_a <- bubble(validacion.a, "residual", main = "Residuales Viento (Aleatorio)")
plot_val_d <- bubble(validacion.d, "residual", main = "Residuales Viento (Dependiente)",na.rm=T)

# Comparacion Validacion
mean(validacion.a$residual)
mean(validacion.d$residual)
grid.arrange(plot_val_a,plot_val_d,ncol=2)


# Metodo IDW

idw_vel <- gstat::idw(Viento~1,viento,coords_pred)

plot_idw_vel <- spplot(idw_vel,"var1.pred",main = "Predicciones Velocidad IDW",col.regions = my.palette,contour=T)
grid.arrange(plot_idw_vel,plot_d_pred,ncol=2)






