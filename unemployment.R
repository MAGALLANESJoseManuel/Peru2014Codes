# PARTE 1 CAMBIOS EN EL TIEMPO
# Ejemplo tomado del libro "VISUALIZE THIS" de Nathan Yau 
# Las anotaciones y modificaciones son del Prof. José Manuel Magallanes

### Paso 1: Cargando la data
### La data está en el website de Nathan, pero como tenemos el link no necesitamos descargarla en la computadora

## otra alternativa
# library(RCurl)
# linkDeData <- getURL("https://raw.githubusercontent.com/MAGALLANESJoseManuel/Peru2014Codes/master/unemploymentData.csv")
# datosDeTrabajo<-read.csv(text = linkDeData, header=TRUE)

linkDeData = "http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv"
datosDeTrabajo<-read.csv(linkDeData, sep=",", header=TRUE)


### Paso 2: Familiarizandose con lo que se tiene
### recomiendo darle na mirada breve a la cabecera y cola del archivo, asi como ver  el nombre de las variables
head (datosDeTrabajo)
tail (datosDeTrabajo)
names (datosDeTrabajo)


### Paso 3: Visualización Básica
### uso mas simple del comando, lo que se obtenga muestra la gráfica con los valores por defecto

rango=(1:nrow(datosDeTrabajo))
plot(rango, datosDeTrabajo$Value)

### Paso 3: Visualización de Tendencia

### Anadiendo linea de tendencia
abline(lm(datosDeTrabajo$Value~rango),col='blue')

# Anadiendo linea loess
lines(lowess(datosDeTrabajo$Value~rango), col="red") # lowess line (x,y)

### Paso 4: Retoques

etiquetaX = "Año"
etiquetaY = "Tasa de Desempleo"
titulo = "Tasa de desempleo en EUA, 1948 - 2010"
subtitulo= "detectando tendencias"
fechas = datosDeTrabajo$Year

plot(rango, datosDeTrabajo$Value, xaxt="n",col = "darkgray", main=titulo,ylim=c(0,11),ylab=etiquetaY,xlab=etiquetaX,type="l")
valsX=seq(1,746,12)
valsyear=seq(1948,2010)
axis(1, at=valsX, labels=valsyear,tck=0,las=2,lwd=0,cex.axis=0.5)
abline(lm(datosDeTrabajo$Value~rango),col='blue',lwd=3,lty=3)
lines(lowess(datosDeTrabajo$Value~rango), col="red",lwd=4) 
legend("topleft",inset=0.8,c('observado','lineal','loess'),lty=c(1,1,1),col=c('gray','blue',"red"),cex=0.8,lwd=2)

## otra alternativa
encabezado=paste(titulo,"\n",subtitulo)
plot(rango, datosDeTrabajo$Value,xaxt="n",col = "gray", ylim=c(0,11),type="l",ylab="",xlab="")
valsX=seq(1,746,12)
valsyear=seq(1948,2010)
axis(1, at=valsX, labels=valsyear,tck=0,las=2,lwd=0,cex.axis=0.5)
abline(lm(datosDeTrabajo$Value~rango),col='blue',lwd=3,lty=3)
lines(lowess(datosDeTrabajo$Value~rango), col="red",lwd=4) 
title(main=encabezado,xlab=etiquetaX,ylab=etiquetaY)
legend("topleft",inset=0.8,c('observado','lineal','loess'),lty=c(1,1,1),col=c('gray','blue',"red"),cex=0.8,lwd=2)



# quieres trabajar sobre una serie de tiempo?
tasa <- ts(datosDeTrabajo$Value, start=c(1948, 1), frequency=12) 
plot.ts(tasa,main=titulo)



