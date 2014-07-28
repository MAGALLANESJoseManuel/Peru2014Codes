rm(list=ls(all=TRUE))

require(xlsx)
data=read.xlsx("casoFigueroa.xls", sheetIndex = 1,startRow=2,header=F)
data[,1] = gsub(" ", "_", data[,1])


values=data[,c(-1,-ncol(data))]

ei=c()

for (i in 1:nrow(data)){
  Unos=apply(values[i,],1,sum)[[1]]  
  Ceros=ncol(values)-Unos
  if (atributo[i,2] == 0){
    ei[i]=(Ceros-Unos)/ncol(values)
  } else{
    ei[i]=(Unos-Ceros)/ncol(values)
  }
}
ei
nombres=data[,1]
atributo=data[,c(1,ncol(data))]
names(atributo)=c("nombre","multinacional")
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

minC <- rep(-Inf, vcount(g))
maxC <- rep(Inf, vcount(g))
minC[1] <- maxC[1] <- 0
co <- layout.fruchterman.reingold(g, minx=minC, maxx=maxC,
                                  miny=minC, maxy=maxC,coolexp=5)
plot(g,layout=co,vertex.size=as.numeric(betweenness(g)/2))
write.graph(g, "casoFigueroa.gml", format = "gml")


r <- graph.ring(10)
r <- add.edges(r, c(1,2, 2,3, 1,3))
r= as.undirected(r, mode = c("collapse"))
graph.coreness(r) 
get.edgelist(r)
betweenness(r)
edge.betweenness(r)
plot(r,layout=layout.fruchterman.reingold,vertex.size=as.numeric(betweenness(r)*5))


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
