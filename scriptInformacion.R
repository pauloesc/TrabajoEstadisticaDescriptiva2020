#importo el conjunto de datos
libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")

#dimensiones del dataset
dim(libertad)

#informacion de cada una de las variables 
summary(libertad)

#importo libreria para graficar
library(ggplot2)


#-------------------------------------------------

#Histograma de libertades
lh<-ggplot(libertad,aes(x=libertad_humana_puntaje))+geom_histogram()
lh
lp<-ggplot(libertad,aes(x=libertad_personal_puntaje))+geom_histogram()
lp
le<-ggplot(libertad,aes(x=libertad_economica_puntaje))+geom_histogram()
le

#---------------------------------------------------

#grafico de dispersion libertad humana y libertad personal y anio
#con recta de regresion lineal
#para los anios 2010 2012 2014 2016
ggplot(subset(libertad, libertad$anio == c("2010","2012","2014","2016") ), aes(x = libertad_personal_puntaje, y = libertad_humana_puntaje)) + geom_point(aes(color=region),size = 1.7) + theme(legend.position = "bottom") + ggtitle("Relación entre libertad personal y libertad humana según región y año") + xlab("libertad personal puntaje [0,10]") + ylab("libertad humana puntaje [0,10]") + geom_smooth(size = 0.5, se=FALSE, method = lm, color = "black") + facet_wrap(~anio)

#grafico de dispersion libertad humana y libertad personal y anio
#con recta de regresion lineal
#las grafica estan en paneles diferentes, tantos paneles como regiones, esto lo logramos con la funcion facet_wrap
ggplot(subset(libertad, libertad$anio == c("2016") ), aes(x = libertad_personal_puntaje, y = libertad_humana_puntaje)) + geom_point(aes(color=region),size=1.7) + ggtitle("Relación entre libertad personal y libertad humana según región y año(2016)")+ geom_smooth(aes(group = region, colour = region), size = 0.5, se=FALSE, method = lm, color = "black") + theme(legend.position = "none") + facet_wrap(~region) + xlab("libertad personal puntaje [0,10]") + ylab("libertad humana puntaje [0,10]")

#-------------------------------------------------
  
#grafico dispersion libertad humana y libertad economica y anio
#con recta de regresion lineal
#para los anios 2010 2012 2014 2016
ggplot(subset(libertad, libertad$anio == c("2010","2012","2014","2016") ), aes(x = libertad_economica_puntaje, y = libertad_humana_puntaje)) + geom_point(aes(color=region),size=1.7) + theme(legend.position = "bottom") + ggtitle("Relación entre libertad económica y libertad humana según región y año") + xlab("libertad economica puntaje [0,10]") + ylab("libertad humana puntaje [0,10]") + geom_smooth(size = 0.5, se=FALSE, method = lm, color = "black") + facet_wrap(~anio)

#grafica de tendencia entre libertad humana y libertad economica y anio
#con recta de regresion lineal
#las grafica estan en paneles diferentes, tantos paneles como regiones, esto lo logramos con la funcion facet_wrap
ggplot(subset(libertad, libertad$anio == c("2016") ), aes(x = libertad_economica_puntaje, y = libertad_humana_puntaje)) + geom_point( aes(color=region), size=1.7  ) + ggtitle("Relación entre libertad económica y libertad humana según región y año(2016)") +geom_smooth( aes(group = region), size = 0.5, se=FALSE, method = lm, color = "black") +  theme(legend.position = "none") + facet_wrap(~region) + xlab("libertad economica puntaje [0,10]") + ylab("libertad humana puntaje [0,10]")

#-------------------------------------------------

install.packages("tidyverse")
library("tidyverse")
install.packages("tidytext")
library("tidytext")

#diagramas de cajas de todas las regiones segun anio
#filtro la informacion y saco los null
a1 <- filter( libertad, anio %in% c(2010, 2012, 2014, 2016), !is.na(libertad_humana_puntaje) )
#modifico la ordenacion de los datos
b2 <- mutate(a1, anio=as.factor(anio), region = reorder_within(region, libertad_humana_puntaje, anio) )
ggplot(b2, aes(region, libertad_humana_puntaje, fill = anio)) + geom_boxplot(show.legend = FALSE) + coord_flip() +  scale_x_reordered() + scale_y_continuous(expand = c(0,0)) + facet_wrap(~anio, scales = "free_y") + labs(x = "Region",  y = "Libertad Humana (Puntaje)", title = "Evolución en el tiempo, libertad humana")


#-------------------------------------------------

#ordenamos el dataset por la variable libertad humana ranking
anio2016<-subset(libertad,subset=grepl("2016",libertad$anio))
puntaje<-anio2016[order(anio2016$libertad_humana_ranking),]

#nos quedamos con los primeros 5 datos y los ultimos 5
extremos<-puntaje[c(1:5,158:162),]

#grafica de columnas
#hacemos reorden en x para ordenar las barras de la grafica de mayor a menor altura
#con theme le damos formato a la apariencia de los nombres de las variables, en este caso dimos un angulo de 25 grados
ggplot(extremos,aes( x=reorder(pais, libertad_humana_puntaje),y=libertad_humana_puntaje,fill=region )) + geom_col( ) + theme(legend.position="right", axis.text.x = element_text(angle = 25, hjust = 1)) + xlab("Paises") + ylab("libertad humana puntaje")

#-------------------------------------------------

install.packages("GGally")
library("GGally")
#correlacion entre libertades
libertades<-subset(libertad,libertad$anio=="2016",select=c("pais","region","libertad_humana_puntaje","libertad_personal_puntaje","libertad_economica_puntaje"))
correlacion<-cor(libertades[,-2][,-1])
round(correlacion,2)
ggpairs(libertades[,c(-1,-2)])  + ggtitle("Año 2016")

#-------------------------------------------------

install.packages("data.table")
library(data.table)

#creamos un dataframe con las columnas: libertad_economica_puntaje, libertad_humana_puntaje, libertad_personal_puntaje
indices_promedios<- as.data.table(aggregate(cbind(libertad_economica_puntaje,libertad_humana_puntaje, libertad_personal_puntaje)~pais,libertad, FUN=mean, na.action=na.omit))
colnames(indices_promedios)<- c("pais","Libertad economica", "Libertad humana", "Libertad personal")
indices_promedios$`Libertad economica`<- round(indices_promedios$`Libertad economica`,2)
indices_promedios$`Libertad humana`<- round(indices_promedios$`Libertad humana`,2)
indices_promedios$`Libertad personal`<- round(indices_promedios$`Libertad personal`,2)

#ranking de los 5 mejores y los 5 peores indices de libertad humana
indices_promediosH<- indices_promedios[order(indices_promedios$`Libertad humana`, decreasing = TRUE),]


#ranking de los 5 mejores y los 5 peores indices de libertad economica
indices_promediosE<- indices_promedios[order(indices_promedios$`Libertad economica`, decreasing = TRUE),]


#ranking de los 5 mejores y los 5 peores indices de libertad personal
indices_promediosP<- indices_promedios[order(indices_promedios$`Libertad personal`, decreasing = TRUE),]

#-------------------------------------------------  
