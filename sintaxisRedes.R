rm(list=ls(all=TRUE))

# SUBIENDO LOS DATOS
require(xlsx)
data=read.xlsx("casoFigueroa.xls", sheetIndex = 1,startRow=2,header=F)
data[,1] = gsub(" ", "_", data[,1]) #ELIMINO ESPACIOS ENTRE APELLIDOS

values=data[,c(-1,-ncol(data))] #matriz de adjacencia
nombres=data[,1] #apellidos

atributo=data[,c(1,ncol(data))] # es o no multinacional
names(atributo)=c("nombre","multinacional") #nombre al atributo

str(values)
v=as.matrix(values,37,37)
rownames(v)=colnames(v)=nombres

library(igraph)
g <- graph.adjacency(v)
g= as.undirected(g, mode = c("collapse"))

V(g)
V(g)$multinacional=as.character(atributo$multinacional[match(V(g)$name,atributo$nombre)]) 
V(g)$color=V(g)$multinacional
V(g)$color=gsub("0","red",V(g)$color) 
V(g)$color=gsub("1","blue",V(g)$color)

plot(g,layout=layout.kamada.kawai)
plot(g,layout=layout.fruchterman.reingold)
plot(g,layout=layout.fruchterman.reingold,vertex.size=as.numeric(betweenness(g)))

# para gephi
write.graph(g, "casoFigueroa.gml", format = "gml")



# otro ejemplo (data de UCINET)
# library(RCurl)
# library(igraph)
# REDES1 <- getURL("https://docs.google.com/spreadsheet/pub?key=0AhVqDdZgThPldFBiT09pMHZZSHJhd2ZRaTEtaGpaMVE&single=true&gid=0&output=csv")
# ORGANIZACIONES <- getURL("https://docs.google.com/spreadsheet/pub?key=0AhVqDdZgThPldG4waE9SRERtcWd1MnVmQVBSR2p4V3c&single=true&gid=0&output=csv")
# ORGANIZACIONESAT <- getURL("https://docs.google.com/spreadsheet/pub?key=0AhVqDdZgThPldG4waE9SRERtcWd1MnVmQVBSR2p4V3c&single=true&gid=1&output=csv")
# 
# 
# 
# data=as.matrix(read.csv(textConnection(ORGANIZACIONES),header=TRUE,row.names=1,check.names=FALSE))
# network <- graph.adjacency(data)
# plot(network, layout=layout.fruchterman.reingold, edge.arrow.size=0.2,edge.curved=TRUE)
# 
# 
# at=read.csv(textConnection(ORGANIZACIONESAT))
# V(network)$especialista=as.character(at$ESPECIALISTA[match(V(network)$name,at$Node)]) 
# V(network)$color=V(network)$especialista
# V(network)$color=gsub("0","red",V(network)$color) 
# V(network)$color=gsub("1","blue",V(network)$color) 
# 
# V(network)$sector=as.character(at$SECTOR[match(V(network)$name,at$Node)]) 
# V(network)$shape=V(network)$sector
# V(network)$shape=gsub("0","circle",V(network)$shape) 
# V(network)$shape=gsub("1","square",V(network)$shape) 
# 
# plot.igraph(network,vertex.label.cex=0.8,vertex.label.color="white",edge.arrow.size=0.2,layout=layout.fruchterman.reingold)
# 
# 
# plot.igraph(network,vertex.size=as.numeric(graph.coreness(network)+3)^2,vertex.label.cex=0.8,vertex.label.color="white",edge.arrow.size=0.2,layout=layout.fruchterman.reingold)
# 
