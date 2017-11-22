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
datasetfinal$Marca[datasetfinal$Marca == "M&M¾S MARS"] <- "M&M'S"
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
  mutate(Mes=month(Fecha, label = F), Dia.Semana = wday(Fecha, label = F), No.Dia = yday(Fecha), No.Semana = week(Fecha))

#resumenes
resumen <- datasetfinal %>% 
  group_by(Tienda, Proveedor, Marca, Departamento, Mes) %>%
  summarise(datos=sum(Ventas))

resumendept <- aggregate(datos~Departamento, data=resumen, sum) 
resumenbrand <- aggregate(datos~Marca, data=resumen, sum) 
resumentienda <- aggregate(datos~Tienda, data=resumen, sum) 

resumen <- aggregate(datos~Departamento+Marca+Tienda, data=resumen, sum)





dir.create(paste(getwd(), "/Graficas", sep = ""))
dir.create(paste(getwd(), "/Graficas/Pie", sep = ""))

plot_ly(resumenbrand, labels = ~Tipo, values = ~Cantidad, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste( Tipo, ": ", Cantidad, 'Unidades'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Unidades Producidas en 2016',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




#Pie Charts
for(mesActual in Meses$Mes){ #Se repite a lo largo de los elementos de la variable
  forBar <- formato %>% 
    filter(Meses==mesActual) 
  
  #Graficar Barra
  ggplot(data=forBar, aes(x=, y=`Ventas`, fill=Formato  )) + 
    ggtitle(paste("Ventas ", mesActual, sep="")) + 
    geom_bar(position = "fill")  +
    geom_text(aes(label = Formato),size=5, position=position_dodge(0.9)) +
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
    
    #Guardar la ultima grafica
    ggsave(filename = paste("Barra", mesActual, ".png", sep = ""), plot = last_plot(), device = "png",
           path = paste(getwd(), "/Graficas/Pie", sep= ""), scale = 1, width = 11, height = 6, units = c("in", "cm", "mm"), dpi = 300)
  
}
