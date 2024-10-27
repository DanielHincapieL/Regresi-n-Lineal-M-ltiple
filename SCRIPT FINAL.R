#FACTORES CLAVE EN LA VARIACIÓN DE PRECIOS DE 
#COMPUTADORES PORTÁTILES: UN ESTUDIO PREDICTIVO (SCRIPT ÚNICAMENTE DE CÓDIGO)

####BASE####
url<-'base.csv'
base<- read.csv(url , sep = "," , dec = ".", quote = "")

base

####PAQUETES Y LIBRERIAS####

install.packages("GGally")
install.packages('ggplot2')
install.packages('gplots')
install.packages('hrbrthemes')
install.packages ('ggplot')
install.packages('corrplot')
install.packages('nortest')
install.packages('lmtest')
install.packages('car')
install.packages('tseries')

library(readr)
library(corrplot)
library(MASS)
library(graphics)
library(ggplot2)
library(GGally)
library(gplots)
library(lattice)
library(hrbrthemes)
library(nortest)
library(lmtest)
library(car)
library(tseries)
library(patchwork)

####Ajustes previos####

base$tam <- as.factor(base$tam) # Tamaño pantalla
base$gar <- as.factor(base$gar) # Garantía
base$ram <- as.factor(base$ram) # RAM
base$alm <- as.factor(base$alm) # Almacenamiento

varct <- subset(base, select = c("peso", "vib", "precio"))

varcl <- subset(base, select = c('mar', 'pro', 'ram', 'alm',
                                 'tam', 'tag', 'sop', 'gar'))

####ANÁLISIS EXPLORATORIO####

##Marca vs Precio
pm <- ggplot(base, aes(x = mar, y = precio)) + 
  geom_boxplot(color = "black", fill = "mediumpurple4", alpha = 0.6, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot marca vs precio")

pm1 <- ggplot(base, aes(x = mar, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pm + pm1

##Procesador vs Precio
pp<- ggplot(base, aes(x = pro, y = precio)) + 
  geom_boxplot( color="black", fill="deeppink3", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot procesador vs precio")

pp1<-ggplot(base, aes(x = pro, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pp + pp1

##RAM vs Precio
pr<- ggplot(base, aes(x = ram, y = precio)) + 
  geom_boxplot( color="black", fill="mediumpurple4", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot RAM vs precio")

pr1<-ggplot(base, aes(x = ram, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pr + pr1

##Almacenamiento vs Precio
pa<- ggplot(base, aes(x = alm, y = precio)) + 
  geom_boxplot( color="black", fill="deeppink3", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot almacenamiento vs precio")

pa1<-ggplot(base, aes(x = alm, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pa + pa1

##Tamaño pantalla vs Precio
pta<-ggplot(base, aes(x = tam, y = precio)) + 
  geom_boxplot( color="black", fill="mediumpurple4", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot tamaño de pantalla vs precio")

pta1<-ggplot(base, aes(x = tam, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pta + pta1

##Tarjeta gráfica vs Precio
pt<-ggplot(base, aes(x = tag, y = precio)) + 
  geom_boxplot( color="black", fill="deeppink3", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot tarjeta gráfica vs precio")

pt1<-ggplot(base, aes(x = tag, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pt + pt1

##Sistema operativo vs Precio
ps<-ggplot(base, aes(x = sop, y = precio)) + 
  geom_boxplot( color="black", fill="mediumpurple4", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot sistema operativo vs precio")

ps1<-ggplot(base, aes(x = sop, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

ps + ps1

##Garantía vs Precio
pg<-ggplot(base, aes(x = gar, y = precio)) + 
  geom_boxplot( color="black", fill="deeppink3", alpha = 0.25, notch = TRUE, notchwidth = 0.8) +
  ggtitle("Boxplot garantía vs precio")

pg1<-ggplot(base, aes(x = gar, y = precio)) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +  
  
  ggtitle("Gráfico de medias") +
  theme_minimal()

pg + pg1

##Peso vs Precio
ggplot(base, aes(x = peso, y = precio)) +
  geom_bin2d(bins = 25) +  
  scale_fill_gradient(low = "white", high = "mediumpurple4") +
  labs(title = "Gráfico peso vs precio",
       x = "Peso", 
       y = "Precio") +
  theme_minimal()

##Vida de la batería vs Precio
ggplot(base, aes(x = vib, y = precio)) +
  geom_bin2d(bins = 10) +  
  scale_fill_gradient(low = "white", high = "mediumpurple4") +
  labs(title = "Gráfico vida de la batería vs precio",
       x = "Vida de la batería del computador", 
       y = "Precio") +
  theme_minimal() 
####MODELOS DE REGRESIÓN LINEAL####
####Modelo 1####

m1<- lm(precio ~ pro + ram + tag + vib,  data= base)

###Multicolinealidad
vif(m1)

###Puntos de influencia..

qqPlot(base$precio, xlab="Cuantiles de distribución normal",
       ylab="Cuantiles de residuales", pch= 16, col= "mediumpurple4",
       col.lines="deeppink3")

influence.measures(m1)


dffits(m1)

dffits1<-dffits(m1)

base$DFFITS<-dffits(m1)

nuevo_m1<- subset(base, abs(DFFITS) <= 0.07303 ) #Depuración

#comparativa
q1 <- ggplot(data.frame(sample = base$precio), aes(sample = sample)) +
  stat_qq(color = "mediumpurple4") +
  stat_qq_line(color = "deeppink3") +
  labs(x = "Cuantiles de distribución normal", y = "Cuantiles de residuales") +
  ggtitle("Modelo 1")

q2 <- ggplot(data.frame(sample = nuevo_m1$precio), aes(sample = sample)) +
  stat_qq(color = "mediumpurple4") +
  stat_qq_line(color = "deeppink3") +
  labs(x = "Cuantiles de distribución normal", y = "Cuantiles de residuales") +
  ggtitle("Nuevo modelo 1")

grid.arrange(q1, q2, ncol = 2)

###Validación

m1n<- lm(precio ~ pro + ram + tag + vib,  
         data= nuevo_m1)

##Normalidad
shapiro.test(m1n$residuals) 

ad.test(m1n$residuals)

jarque.bera.test(m1n$residuals)

##Homocedasticidad
bptest(m1n)

##Independencia

bgtest(m1n)
dwtest(m1n)

####Modelo 2####
m2<- lm(precio~ tam + vib + peso, data= base)

###Multicolinealidad y puntos de influencia

influence_measures <- influence.measures(m2)
summary(influence_measures)

###Validación 

##Normalidad

shapiro.test(m2$residuals) 

ad.test(m2$residuals)

jarque.bera.test(m2$residuals)

##Homocedasticidad

bptest(m2)

##Independencia

bgtest(m2)

dwtest(m2)

###Predicciones

predicciones <- predict(m2, data=base)
valores_ajustados <- fitted(m2)

rmse(valores_ajustados, predicciones)

####Modelo 3####

m3<- lm( precio ~ vib + mar + pro + ram + 
           alm + tam + tag + sop, data = base)

###Multicolinealidad y puntos de influencia

vif(m3)

###Validación

##Normalidad

shapiro.test(m3$residuals) 

ad.test(m3$residuals)

jarque.bera.test(m3$residuals)

##Homocedasticidad

bptest(m3)

##Independencia

bgtest(m3)
dwtest(m3)

####Modelo 4####

m4<- lm( precio ~ vib + peso, data = base)

###Multicolinealidad y puntos de influencia

vif(m4)

###Validación

##Normalidad

shapiro.test(m4$residuals) 

ad.test(m4$residuals)

jarque.bera.test(m4$residuals)

##Homocedasticidad

bptest(m4)

##Independencia

bgtest(m4)
dwtest(m4)

#




