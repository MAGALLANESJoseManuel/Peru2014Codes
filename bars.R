
# PARTE 1 CAMBIOS EN EL TIEMPO
# Ejemplo tomado del libro "VISUALIZE THIS" de Nathan Yau 
# Las anotaciones y modificaciones son del Prof. José Manuel Magallanes


### Paso 1: Cargando la data
### La data está en el website de Nathan, pero como tenemos el link no necesitamos descargarla en la computadora

linkDeData = "http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
datosDeTrabajo<-read.csv(linkDeData, sep=",", header=TRUE)

### Paso 2: Familiarizandose con lo que se tiene
### recomiendo darle na mirada breve a la cabecera y cola del archivo, asi como ver  el nombre de las variables
head (datosDeTrabajo)
tail (datosDeTrabajo)
names (datosDeTrabajo)

### Paso 3: Visualización Básica
### uso mas simple del comando, lo que se obtenga muestra la gráfica con los valores por defecto
variableDeInteres = datosDeTrabajo$Dogs.eaten
barplot(variableDeInteres)

### Paso 4: Añadiendo detalles a la Visualización Básica
# los años:
fechas = datosDeTrabajo$Year
barplot(variableDeInteres, names.arg=fechas)

# etiquetas de los ejes
etiquetaX = "Año"
etiquetaY = "Cantidad"
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY)

# Color y borde de la barra

# primero le ponemos color a la barra
colorBarra = 'red' 
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY,col=colorBarra)

#le quitamos el borde a la barra
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY,col=colorBarra, border=NA)

### Paso 5: Resaltando datos

# mostrar cuando el ganador fue de Estados Unidos
# aqui vemos como se escribe:
levels(datosDeTrabajo$Country)
#aqui lo guardamos
valorQueResaltaremos=levels(datosDeTrabajo$Country)[4]

listaDeColores <- c()  #lista vacia
# para cada fila del archivo 
for ( i in 1:nrow(datosDeTrabajo) ) {
  #si el pais de esa fila es igual al valor que queremos resaltar:
	if (datosDeTrabajo$Country[i] ==valorQueResaltaremos) {
    #se anade este color (rojo) a la lista de colores
	  listaDeColores <- c(listaDeColores, colorBarra)
	} else { # sino, se añade gris a la lista de colores
	  listaDeColores <- c(listaDeColores, "gray")
	}
}

titulo= "Años que USA ganó"
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY,col=listaDeColores, border=NA, main = titulo)


# resaltar los años cuando hay nuevo record

# aqui vemos como se escribe:
summary(datosDeTrabajo$New.record)
#aqui lo guardamos
valorQueResaltaremos=max(datosDeTrabajo$New.record)

listaDeColores <- c()  #lista vacia
# para cada fila del archivo 
for ( i in 1:nrow(datosDeTrabajo) ) {
  if (datosDeTrabajo$New.record[i] ==valorQueResaltaremos) {
    listaDeColores <- c(listaDeColores, colorBarra)
  } else { 
    listaDeColores <- c(listaDeColores, "gray")
  }
}

titulo= "Años que se batió el Record"
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY,col=listaDeColores, border=NA, main = titulo, las=3)

# ULTIMOS RETOQUES
titulo = "Competencia de Glotones de 1980 a 2010"
subtitulo= "Años que se batió el Record"
encabezado=paste(titulo,"\n",subtitulo)
# y anadimos "las" para la posición de las etiquetas
barplot(variableDeInteres, names.arg=fechas, xlab=etiquetaX, ylab=etiquetaY,col=listaDeColores, border=NA, main = encabezado, las=3) 