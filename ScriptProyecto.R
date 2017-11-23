#Hola
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)

#Juntar Datasets
Parte1 <- read_csv(paste(getwd(), "/Proyecto final parte 1.csv", sep = ""))
Parte2 <- read_csv(paste(getwd(), "/Proyecto final parte 1.csv", sep = ""))

datasetfinal <- rbind(Parte1, Parte2)
rm(Parte1)
rm(Parte2)

##Convertir en factores
datasetfinal$Formato <- as.factor(datasetfinal$Formato)
datasetfinal$Proveedor <- as.factor(datasetfinal$Proveedor)
datasetfinal$`Brand Desc` <- as.factor(datasetfinal$`Brand Desc`)
datasetfinal$`Dept Desc` <- as.factor(datasetfinal$`Dept Desc`)

#Cambiar y agrupar nombres
colnames(datasetfinal)[1:4] <- c("Tienda", "Proveedor", "Marca", "Departamento")

datasetfinal$Marca <- as.character(datasetfinal$Marca)
datasetfinal$Marca[datasetfinal$Marca == "M & M"] <- "M&M'S"
datasetfinal$Marca[datasetfinal$Marca == "M&MS"] <- "M&M'S"
datasetfinal$Marca[datasetfinal$Marca == "M&M"] <- "M&M'S"
datasetfinal$Marca[datasetfinal$Marca == "M&M¾S_MARS"] <- "M&M'S"
datasetfinal$Marca[datasetfinal$Marca == "M&M_S MARS"] <- "M&M'S"
datasetfinal$Marca[datasetfinal$Marca == "SNIKERS MARS"] <- "SNICKERS"
datasetfinal$Marca[datasetfinal$Marca == "& CAF<e5>"] <- "&CAFE"
datasetfinal$Marca <- as.factor(datasetfinal$Marca)

##Intento de agrupar segun las fechas
#nombres<- data.frame(1:370, names(datasetfinal))
#colnames(nombres) <- c("No", "Fechas")

#nombres <- filter(nombres, No>4)
#nombres$Fechas<- dmy(nombres$Fechas)
#nombres <- nombres %>% 
 #mutate(Mes=month(Fechas, label = F), Dia.Semana = wday(Fechas, label = F), No.Dia = yday(Fechas), No.Semana = week(Fechas))

#Hacerlo Vertical
datasetfinal<- gather(datasetfinal, Fecha, Ventas, 5:370, na.rm = TRUE)
#Hacerlo tipo fecha
datasetfinal$Fecha <- dmy(datasetfinal$Fecha)

#Crear columnas para agrupar
datasetfinal <- datasetfinal %>% 
  mutate(Mes=month(Fecha, label = T, abbr=T ), Dia.Semana = wday(Fecha, label = T, abbr=T), No.Dia.Mes= mday(Fecha), No.Dia.Year = yday(Fecha), No.Semana = isoweek(Fecha))

#resumenes
resumen <- datasetfinal %>% 
  group_by(Tienda, Proveedor, Marca, Departamento, Mes) %>%
  summarise(Ventas=sum(Ventas))

resumendept <- aggregate(Ventas~Departamento, data=resumen, sum)

resumenbrand <- datasetfinal %>% 
  group_by(Proveedor,Marca) %>% 
  summarise(Ventas=sum(Ventas))

resumenprov <- datasetfinal %>% 
  group_by(Proveedor) %>% 
  summarise(Ventas=sum(Ventas))%>%
  arrange(desc(Ventas))

resumentienda <- aggregate(Ventas~Tienda, data=resumen, sum) 

#Pie de Tiendas
rt <- plot_ly(resumentienda, labels = ~Tienda, values = ~Ventas, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              showlegend = FALSE) %>%
  layout(title = 'Ventas por Tienda',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
rt
##a partir de esta grafica nos enfocaremos en la distribucion unicamente para Despensa Familiar


#Pie de Marcas, Error por UTF8
rm <- plot_ly(resumenbrand, labels = ~Marca, values = ~Ventas, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              showlegend = FALSE) %>%
  layout(title = 'Ventas por Marca',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

rm

#Pie de Proveedores, Error por UTF8
rp <- plot_ly(resumenprov, labels = ~Proveedor, values = ~Ventas, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              showlegend = FALSE) %>%
  layout(title = 'Ventas por Marca',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

rp

#Ventas por dia en la Despensa Familiar
p<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar") %>%
  group_by(Fecha) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Fecha, y = ~Ventas, colors = "Dark2", mode = 'lines', type = 'scatter')%>%
  layout(title = 'Ventas de Despensa Familiar')

p


#Ventas por dia del Mes de SCA por dia del mes
a<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor=="SCA") %>%
  group_by(Marca, No.Dia.Mes) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~No.Dia.Mes, y = ~Ventas, color= ~Marca, colors = "Paired", type = 'bar')%>%
  layout(title = 'Ventas de SCA por dia del mes')
a

#Ventas por dia del Mes de DEspensa por dia del mes
c<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor!="&Caf<U+008E>") %>%
  group_by(Marca, No.Dia.Mes) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~No.Dia.Mes, y = ~Ventas, color= ~Marca, colors = "Paired", type = 'bar')%>%
  layout(title = 'Ventas de Despensa por dia del mes')
c


#Ventas por dia del Mes de DEspensa por dia del mes
d<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor!="&Caf<U+008E>") %>%
  group_by(Marca, Dia.Semana) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Dia.Semana, y = ~Ventas, color= ~Marca, colors = "Paired", type = 'bar')%>%
  layout(title = 'Ventas de Despensa por dia de la Semana')
d

#Ventas por dia del Mes Mars por dia del mes
b<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor=="Mars") %>%
  group_by(Marca, No.Dia.Mes) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~No.Dia.Mes, y = ~Ventas, color= ~Marca, colors = "Paired", type = 'bar')%>%
  layout(title = 'Ventas de Mars por dia del mes')
b

#A partir de las graficas anteriores decidimos enfocarnos en el Proveedor Mars para la Tienda Despensa Familiar
#Tanto por su importancia en cantidades vendidas como en su diversidad

#Dataset especifico de Mars 
datasetMars<-datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor=="Mars", Marca!="MUNCHKIN", Marca!="MARS")

e<- datasetMars %>% 
  group_by(Marca, Dia.Semana) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Dia.Semana, y = ~Ventas, color= ~Marca, colors = "Paired", type = 'bar')%>%
  layout(title = 'Ventas de Mars por dia de la semana')
e


#Pie de la distribucion del proveedor Mars en los departamentos de tienda
pm <-datasetMars %>% 
  group_by(Departamento) %>% 
  summarise(Ventas=sum(Ventas))%>% 
  plot_ly( labels = ~Departamento, values = ~Ventas, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              showlegend = TRUE) %>%
  layout(title = 'Ventas de Mars por Departamento',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pm

#Ventas por dia en la Despensa Familiar del proveedor Mars
l<- datasetfinal %>% 
  filter(Tienda== "Despensa Familiar", Proveedor=="Mars") %>%
  group_by(Fecha) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Fecha, y = ~Ventas, colors = "Dark2", mode = 'lines', type = 'scatter')%>%
  layout(title = 'Ventas de Mars')

l

#Pie de la distribucion de las Ventas de Mars de PETS AND SUPPLIES por Dia de la Semana
pds <-datasetMars %>% 
  filter(Departamento=="PETS AND SUPPLIES")%>%
  group_by(Dia.Semana) %>% 
  summarise(Ventas=sum(Ventas))%>% 
  arrange(Dia.Semana)%>%
  plot_ly( labels = ~Dia.Semana, values = ~Ventas, colors = "Accent" ,type = 'pie',
           textposition = 'inside',
           textinfo = 'label+percent',
           insidetextfont = list(color = '#FFFFFF'),
           showlegend = FALSE) %>%
  layout(title = 'Ventas de Mars de PETS AND SUPPLIES por Dia de la Semana',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pds

#Pie de la distribucion de las Ventas de Mars de Dulces por Dia de la Semana
pds <-datasetMars %>% 
  filter(Departamento!="PETS AND SUPPLIES")%>%
  group_by(Dia.Semana) %>% 
  summarise(Ventas=sum(Ventas))%>% 
  arrange(Dia.Semana)%>%
  plot_ly( labels = ~Dia.Semana, values = ~Ventas, colors = "Accent" ,type = 'pie',
           textposition = 'inside',
           textinfo = 'label+percent',
           insidetextfont = list(color = '#FFFFFF'),
           showlegend = FALSE) %>%
  layout(title = 'Ventas de Mars de Golosinas por Dia de la Semana',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pds


##Queremos analizar lo que es comparable, las golosinas
datasetGolosinas <- datasetMars %>% 
  filter(Departamento!="PETS AND SUPPLIES")

##Historico de Golosinas en anio
f<- datasetGolosinas %>% 
  group_by(Marca, Fecha) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Fecha, y = ~Ventas, color= ~Marca, colors = "Accent", mode = 'lines', type = 'scatter')%>%
  layout(title = 'Ventas de Golosinas en el Anio') 
f

#Ventas de Golosinas Por Mes
g<- datasetGolosinas %>% 
  filter(Marca!="TWIX")%>%
  group_by(Marca, Mes) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~Mes, y = ~Ventas, color= ~Marca, barmode = 'group', type = 'bar')%>%
  layout(title = 'Ventas de Golosinas Por Mes') 
g

#Ventas de Golosinas Por Dia del Mes
g<- datasetGolosinas %>% 
  filter(Marca!="TWIX")%>%
  group_by(Marca, No.Dia.Mes) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~No.Dia.Mes, y = ~Ventas, color= ~Marca, barmode = 'group', type = 'bar')%>%
  layout(title = 'Ventas de Golosinas Por dia del Mes') 
g


#Ventas de Golosinas Por Semana
h<- datasetGolosinas %>% 
  filter(Marca!="TWIX")%>%
  group_by(Marca, No.Semana) %>% 
  summarise(Ventas=sum(Ventas))%>%
  plot_ly(x = ~No.Semana, y = ~Ventas, color= ~Marca, barmode = 'group', type = 'bar')%>%
  layout(title = 'Ventas de Golosinas Por Semana') 
h

#promedios golosina por meses
prom <-datasetGolosinas %>% 
  filter(Marca!="TWIX")%>%
  plot_ly( x = ~Mes, y = ~Ventas, color = ~Marca, type = "box", boxpoints = 'suspectedoutliers') %>%
  layout(boxmode = "group", title = 'Promedios Mensuales')
prom
