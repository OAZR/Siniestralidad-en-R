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
--siniestros1$a�oocurrido<-format(siniestros1$Fecha.de.Ocurrido,"%Y")

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
siniestrosacumulado$a�oocurrido<-format(siniestrosacumulado$Fecha.de.Ocurrido,"%Y")
siniestrosacumulado$a�oocurrido<-as.factor(siniestrosacumulado$a�oocurrido)
levels(siniestrosacumulado$a�oocurrido) #hay info de 2022 pero no tiene fecha de emisi�n
filter(siniestrosacumulado, a�oocurrido==1998)

#veamos si existen NAs en pagos y a�oocurrido

siniestrosacumulado[is.na(siniestrosacumulado$a�oocurrido), ]
#eliminemos estas filas lascuales s�lo muestran el nombre y sexo de los individuos pero el resto de informaci�n la omiten
siniestrosacumulado <- siniestrosacumulado[!is.na(siniestrosacumulado$Pagos),]
siniestrosacumulado <- siniestrosacumulado[!is.na(siniestrosacumulado$a�oocurrido),]

glimpse(siniestrosacumulado)
#Analizando la informaci�n por a�o de ocurrido vs pagos

resumenxa�o = siniestrosacumulado %>% group_by(a�oocurrido)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), n�mero_pagos=n(),
            pago_m�nimo=min(Pagos),pago_m�ximo=max(Pagos), )
print(resumenxa�o, n=40)

  
#analizando la informaci�n por a�o de ocurrido y sexo
resumenxa�o1 = siniestrosacumulado %>% group_by(a�oocurrido, Sexo)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), n�mero_pagos=n(),
            pago_m�nimo=min(Pagos),pago_m�ximo=max(Pagos), )
resumenxa�o1

library(ggplot2)

#total de pagos por a�o
ggplot(resumenxa�o,aes(x=a�oocurrido, y=pago_total))+
geom_bar(stat="identity")  +
  labs(title = "Gr�fico de Pagos Totales por A�o")


# de pagos por a�o

ggplot(resumenxa�o,aes(x=a�oocurrido, y=n�mero_pagos))+
  geom_bar(stat="identity")

#pago promedio por a�o

ggplot(resumenxa�o,aes(x=a�oocurrido, y=pago_promedio))+
  geom_bar(stat="identity")


#n�mero de siniestros x fecha

resumen3=siniestrosacumulado %>% group_by(Fecha.de.Ocurrido)  %>%
  summarise(pago_total = sum(Pagos), n�mero_pagos=n())
resumen3
ggplot(resumen3,aes(x=Fecha.de.Ocurrido,y=pago_total))+
  geom_point()

#mapa de calor x sexo y a�o  y pago total

ggplot(resumenxa�o1,aes(x=Sexo,y=a�oocurrido,fill=pago_total))+
  geom_tile()

ggplot(resumenxa�o1,aes(x=Sexo,y=a�oocurrido,fill=n�mero_pagos))+
  geom_tile()


#agrupando por asegurado

resumenxa�oxaseg = siniestrosacumulado %>% group_by(a�oocurrido,Nombre)  %>%
  summarise(pago_total = sum(Pagos),pago_promedio=mean(Pagos), n�mero_pagos=n())
resumenxa�oxaseg

#asegurados x a�o con m�s pago de siniestros

resumenxa�oxaseg%>%
  group_by (a�oocurrido)%>% 
  filter (pago_total == max (pago_total, na.rm = TRUE ))

#inflaci�n del 4%
#crearemos una base id�ntica que nos servir� de soporte para no modificar la original
base_soporte<-siniestrosacumulado
glimpse(base_soporte)
base_soporte$a�oocurrido<-as.numeric(as.character(base_soporte$a�oocurrido))
base_soporte$Difvs_2022<-2022 - base_soporte$a�oocurrido
base_soporte$pagos_a2022<-base_soporte$Pagos*(1.04)^base_soporte$Difvs_2022  
#para el n�mero de pagos usemos el promedio del n�mero de pagos realizados
#en los �ltimos 5 a�os (2017 a 2021)
resumenxa�o$n�mero_pagos[19:23]
resumenxa�o
promedio<- mean(resumenxa�o$n�mero_pagos[19:23])
promedio
#generemos una muestra aleatoria de 211522 montos de pago de siniestro provenientes de la columna pagos_a2022
muestra_pagos2022 <- sample(base_soporte$pagos_a2022, 211522)
sum(muestra_pagos2022)
mean(muestra_pagos2022)
boxplot(muestra_pagos2022,main = "Boxplot del monto de pagos para 2022",
        xlab = "Pagos en 2022", ylab="monto")
