# proyecciones

# DATOS PROYECTADOS
proy_nj <- project(as.matrix(coords_nj),"+proj=utm +zone=18N ellps=WGS84")
proy_dat <- project(as.matrix(datos@coords),"+proj=utm +zone=18N ellps=WGS84")

# GRAFICO DATOS PROYECTADOS
plot(proy_nj,type="l")
points(proy_dat,add=T)

# DATOS PROYECTADOS JUNTADOS
datos_proy <- cbind(datos@data,proy_dat)
coordinates(datos_proy) <- ~Longitude+Latitude

# DISTANCIAS
max(dist(datos_proy@coords))
#244505.4

# VARIOGRAMAS

# CLOUD
p_cloud_a <- variogram(WindSpeed  ~ 1, locations = coordinates(datos_proy),datos_proy,cutoff=244505.4/2,cloud=T) # Aleatorio
p_cloud_d <- variogram(WindSpeed  ~ Longitude+Latitude, locations = coordinates(datos_proy),datos_proy,cutoff=244505.4/2,cloud=T) # Dependencia
p_c_a <- ggplot(p_cloud_a,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Cloud (Aleatorio)")
p_c_d <- ggplot(p_cloud_d,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Cloud (Dependecia)")
grid.arrange(p_c_a,p_c_d,ncol=2)

# BIN

p_bin_a <- variogram(WindSpeed  ~ 1, locations = coordinates(datos_proy),datos_proy,cutoff=100000) %>% mutate(Modelo = "Aleatorio")
p_bin_d <- variogram(WindSpeed  ~ Longitude+Latitude, locations = coordinates(datos_proy),datos_proy,cutoff=100000) %>% mutate(Modelo = "Dependiente")

p_b_a <- ggplot(p_bin_a,aes(x=dist,y=gamma))+geom_point(shape=21,fill="lightblue")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Bin (Aleatorio)")
p_b_d <-  ggplot(p_bin_d,aes(x=dist,y=gamma))+geom_point(shape=21,fill="red")+xlab("Distancia")+ylab("Semivarianza")+labs(title = "Variograma Bin (Dependiente)")


grid.arrange(p_b_a,p_b_d,ncol=2)

# FIT BIN
p_bin_a.fit <- fit.variogram(p_bin_a,vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
p_bin_d.fit <- fit.variogram(p_bin_d,vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

plot(p_bin_a,p_bin_a.fit)
plot(p_bin_d,p_bin_d.fit)
