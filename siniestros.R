library(readr)
library(dplyr)
siniestros1<- read.table("C:/Users/HP/Downloads/gen re/Sini_2016.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestros2<- read.table("C:/Users/HP/Downloads/gen re/Sini_2017.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestros3<- read.table("C:/Users/HP/Downloads/gen re/Sini_2018.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestros4<- read.table("C:/Users/HP/Downloads/gen re/Sini_2019.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestros5<- read.table("C:/Users/HP/Downloads/gen re/Sini_2020.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestros6<- read.table("C:/Users/HP/Downloads/gen re/Sini_2021.txt", header = TRUE, sep = "\t", fill = TRUE)
siniestrosacumulado<- rbind(siniestros1, siniestros2, siniestros3, siniestros4, siniestros5, siniestros6)
dim(siniestrosacumulado)[1:1]
glimpse(siniestrosacumulado)
--siniestros1$Fecha.de.Ocurrido<-as.Date(siniestros1$Fecha.de.Ocurrido,format = "%d/%m/%Y")
--siniestros1$añoocurrido<-format(siniestros1$Fecha.de.Ocurrido,"%Y")

head(siniestrosacumulado)
siniestrosacumulado<-subset(siniestrosacumulado, select = -X)
--siniestrosacumulado
glimpse(siniestrosacumulado)
siniestrosacumulado$Fecha.de.Ocurrido<-as.Date(siniestrosacumulado$Fecha.de.Ocurrido,format = "%d/%m/%Y")
siniestrosacumulado$Fecha.de.Nacimiento<-as.Date(siniestrosacumulado$Fecha.de.Nacimiento,format = "%d/%m/%Y")
siniestrosacumulado$Fecha.de.Emision<-as.Date(siniestrosacumulado$Fecha.de.Emision,format = "%d/%m/%Y")
siniestrosacumulado$Inicio.de.Vigencia<-as.Date(siniestrosacumulado$Inicio.de.Vigencia,format = "%d/%m/%Y")
siniestrosacumulado$Sexo<-as.factor(siniestrosacumulado$Sexo)
class(siniestrosacumulado$Fecha.de.Ocurrido)
class(siniestrosacumulado$Pagos)
siniestrosacumulado$añoocurrido<-format(siniestrosacumulado$Fecha.de.Ocurrido,"%Y")
siniestrosacumulado$añoocurrido<-as.factor(siniestrosacumulado$añoocurrido)
levels(siniestrosacumulado$añoocurrido) #hay info de 2022 pero no tiene fecha de emisión
filter(siniestrosacumulado, añoocurrido==1998)

#veamos si existen NAs en pagos y añoocurrido

siniestrosacumulado[is.na(siniestrosacumulado$añoocurrido), ]
#eliminemos estas filas lascuales sólo muestran el nombre y sexo de los individuos pero el resto de información la omiten
siniestrosacumulado <- siniestrosacumulado[!is.na(siniestrosacumulado$Pagos),]
siniestrosacumulado <- siniestrosacumulado[!is.na(siniestrosacumulado$añoocurrido),]

glimpse(siniestrosacumulado)
#Analizando la información por año de ocurrido vs pagos

resumenxaño = siniestrosacumulado %>% group_by(añoocurrido)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), número_pagos=n(),
            pago_mínimo=min(Pagos),pago_máximo=max(Pagos), )
print(resumenxaño, n=40)

  
#analizando la información por año de ocurrido y sexo
resumenxaño1 = siniestrosacumulado %>% group_by(añoocurrido, Sexo)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), número_pagos=n(),
            pago_mínimo=min(Pagos),pago_máximo=max(Pagos), )
resumenxaño1

library(ggplot2)

#total de pagos por año
ggplot(resumenxaño,aes(x=añoocurrido, y=pago_total))+
geom_bar(stat="identity")  +
  labs(title = "Gráfico de Pagos Totales por Año")


# de pagos por año

ggplot(resumenxaño,aes(x=añoocurrido, y=número_pagos))+
  geom_bar(stat="identity")

#pago promedio por año

ggplot(resumenxaño,aes(x=añoocurrido, y=pago_promedio))+
  geom_bar(stat="identity")


#número de siniestros x fecha

resumen3=siniestrosacumulado %>% group_by(Fecha.de.Ocurrido)  %>%
  summarise(pago_total = sum(Pagos), número_pagos=n())
resumen3
ggplot(resumen3,aes(x=Fecha.de.Ocurrido,y=pago_total))+
  geom_point()

#mapa de calor x sexo y año  y pago total

ggplot(resumenxaño1,aes(x=Sexo,y=añoocurrido,fill=pago_total))+
  geom_tile()

ggplot(resumenxaño1,aes(x=Sexo,y=añoocurrido,fill=número_pagos))+
  geom_tile()


#agrupando por asegurado

resumenxañoxaseg = siniestrosacumulado %>% group_by(añoocurrido,Nombre)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), número_pagos=n())
resumenxañoxaseg

#asegurados x año con más pago de siniestros

resumenxañoxaseg%>%
  group_by (añoocurrido)%>% 
  filter (pago_total == max (pago_total, na.rm = TRUE ))

#inflación del 4%
#crearemos una base idéntica que nos servirá de soporte para no modificar la original
base_soporte<-siniestrosacumulado
glimpse(base_soporte)
base_soporte$añoocurrido<-as.numeric(as.character(base_soporte$añoocurrido))
base_soporte$Difvs_2022<-2022 - base_soporte$añoocurrido
base_soporte$pagos_a2022<-base_soporte$Pagos*(1.04)^base_soporte$Difvs_2022  
#para el número de pagos usemos el promedio del número de pagos realizados
#en los últimos 5 años (2017 a 2021)
resumenxaño$número_pagos[19:23]
resumenxaño
promedio<- mean(resumenxaño$número_pagos[19:23])
promedio
#generemos una muestra aleatoria de 211522 montos de pago de siniestro provenientes de la columna pagos_a2022
muestra_pagos2022 <- sample(base_soporte$pagos_a2022, 211522)
sum(muestra_pagos2022)
mean(muestra_pagos2022)
boxplot(muestra_pagos2022,main = "Boxplot del monto de pagos para 2022",
        xlab = "Pagos en 2022", ylab="monto")
