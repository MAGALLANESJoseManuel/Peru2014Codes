# PARTE 1 MAS CAMBIOS Y DIFERENCIAS
# Ejemplo tomado del libro "VISUALIZE THIS" de Nathan Yau 
# Las anotaciones y modificaciones son del Prof. José Manuel Magallanes


### Paso 1: Cargando la data
### La data está en el website de Nathan, pero como tenemos el link no necesitamos descargarla en la computadora
linkDeData = "http://datasets.flowingdata.com/crimeRatesByState-formatted.csv"
datosDeTrabajo<-read.csv(linkDeData, sep=",", header=TRUE)

### Paso 2: Familiarizandose con lo que se tiene
### recomiendo darle na mirada breve a la cabecera y cola del archivo, asi como ver  el nombre de las variables
head (datosDeTrabajo)
tail (datosDeTrabajo)
names (datosDeTrabajo)

### Paso 3: Alternativas para multivariables

#### A) Estrellas 

stars(datosDeTrabajo) #muy basico
# Para saber donde
row.names(datosDeTrabajo) <- datosDeTrabajo$state 
# Eliminamos primera fila (promedio USA) y primera columna (nombre Estado)
# (si se desea se puede eliminar mas columnas)
datosDeTrabajo <- datosDeTrabajo[c(-1),c(-1)] 

#stars con dimensiones y leyendas
stars(datosDeTrabajo, flip.labels=FALSE, key.loc = c(15, 1.5))
#stars con dimensiones y leyendas (modificada)
stars(datosDeTrabajo, flip.labels=FALSE, key.loc = c(15, 1.5), full=FALSE)
#stars con dimensiones (como segmentos -areas) y leyendas (modificada)
stars(datosDeTrabajo, flip.labels=FALSE, key.loc = c(15, 1.5), draw.segments=TRUE)


#### B) Heatmap

heatmap(datosDeTrabajoMX)
heatmap(datosDeTrabajoMX, margins=c(12,10),scale="column")

heatmap(datosDeTrabajoMX, Rowv=NA,Colv=NA,scale="column", margins=c(12,10))

heatmap(datosDeTrabajoMX, Rowv=NA,Colv=NA,scale="column", margins=c(12,10),col = cm.colors(256))

heatmap(datosDeTrabajoMX, Rowv=NA,Colv=NA,scale="column", margins=c(12,10),col = heat.colors(256))

#### colores personalizados
misRojos <- c("#ffd3cd", "#ffc4bc", "#ffb5ab", "#ffa69a", "#ff9789", "#ff8978", "#ff7a67", "#ff6b56", "#ff5c45", "#ff4d34")
heatmap(datosDeTrabajoMX, Rowv=NA, Colv=NA, scale="column", margins=c(12,10), col =misRojos)

library(RColorBrewer)
heatmap(datosDeTrabajoMX, Rowv=NA, Colv=NA, scale="column", margins=c(12,10), col = brewer.pal(9, "Blues"))


#### C) Paralelas
datosDeTrabajo<-read.csv(linkDeData, sep=",", header=TRUE)

library(lattice)
parallelplot(datosDeTrabajo)
parallelplot(datosDeTrabajo, horizontal.axis=FALSE)
parallelplot(datosDeTrabajo[,c(-1)], horizontal.axis=FALSE)
parallelplot(datosDeTrabajo[,c(-1)], horizontal.axis=FALSE, col="black")

# tercer cuartil?

limite=quantile(datosDeTrabajo$murder,0.75)[[1]]

# Color by reading SAT
misColores <- c()
for (i in 1:nrow(datosDeTrabajo)) {
	
	if (datosDeTrabajo$murder[i] > limite) {
		color <- "red"	
	} else {
		color <- "green"	
	}
	misColores <- c(misColores, color)
}
parallelplot(datosDeTrabajo[,c(-1)], horizontal.axis=FALSE, col=misColores)


### Multidimensional scaling
datosDeTrabajoDist <- dist(datosDeTrabajo[,c(-1)], method="euclidean")
datosDeTrabajoMDS <- cmdscale(datosDeTrabajoDist)
plot(datosDeTrabajoMDS[,1],datosDeTrabajoMDS[,2])
plot(datosDeTrabajoMDS[,1],datosDeTrabajoMDS[,2], type="n")
text(datosDeTrabajoMDS[,1],datosDeTrabajoMDS[,2], labels=datosDeTrabajo$state )
text(datosDeTrabajoMDS[,1],datosDeTrabajoMDS[,2], labels=datosDeTrabajo$state , col=misColores)