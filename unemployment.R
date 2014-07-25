# PARTE 1 CAMBIOS EN EL TIEMPO
# Ejemplo tomado del libro "VISUALIZE THIS" de Nathan Yau 
# Las anotaciones y modificaciones son del Prof. José Manuel Magallanes

### Paso 1: Cargando la data
### La data está en el website de Nathan, pero como tenemos el link no necesitamos descargarla en la computadora

linkDeData = "http://datasets.flowingdata.com/datosDeTrabajo-rate-1948-2010.csv"
datosDeTrabajo<-read.csv(linkDeData, sep=",", header=TRUE)

### Paso 2: Familiarizandose con lo que se tiene
### recomiendo darle na mirada breve a la cabecera y cola del archivo, asi como ver  el nombre de las variables
head (datosDeTrabajo)
tail (datosDeTrabajo)
names (datosDeTrabajo)


# Simple
rango=(1:nrow(datosDeTrabajo))
plot(rango, datosDeTrabajo$Value, col="gray")

# Anadiendo linea de tendencia
abline(lm(datosDeTrabajo$Value~rango),col='blue',lwd = 3,lty=3)

# Anadiendo linea loess
lines(lowess(datosDeTrabajo$Value~rango), col="red",lwd = 3) # lowess line (x,y)



