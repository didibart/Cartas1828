DatosCarta1828 <- read.csv("Cartas1828/1828red2.csv"
                       ,header = TRUE
                       ,sep = ",", stringsAsFactors = TRUE
                       ,encoding = "UTF-8")
#,encoding = "iso-8859-1" (bajo la hoja del drive y la abro como UTF-8)
View(DatosCarta1828)
library(sqldf)
library(igraph)
library(ggplot2)

RedCartas <- graph.data.frame(DatosCarta1828,directed=FALSE)
tkplot(RedCartas)
#xlim=c(0,5), ylim=c(0,5), asp = 0,
coord_cartesian(xlim = c(0,5), ylim = c(0,5),expand = TRUE)
#-4 -3 -1 3
plot(RedCartas, xlim=c(-0.7,0.7), ylim=c(0.7,-0.7), asp = 0, edge.arrow.size=.2,
     edge.witdh=1,
     vertex.color="gold", 
     vertex.size= 0.25, 
     vertex.frame.color="gray", 
     #vertex.label=NA,
     vertex.label.color="black", 
     vertex.label.cex=0.4, 
     vertex.label.dist=0.2, 
     vertex.label.size=1,
     edge.curved=0.2)
#margin=c(0.1,0.1,0.1,0.1),
#grado
deg <- degree(RedCartas, mode="all")
View(deg)
degsorted <- sort.int(deg,decreasing=TRUE,index.return=FALSE)
View(degsorted)
write.csv(degsorted,"Cartas1828/degsorted.csv")
#ibtermediacion
degbet <-betweenness(RedCartas)
degbetsorted <- sort.int(degbet,decreasing = TRUE)
View(degbet)
View(degbetsorted)
write.csv(degbetsorted,"Cartas1828/degbetsorted.csv")
#componentes
degcomponents <- components(RedCartas)
degcomponetsgr <- groups(degcomponents)
View(degcomponetsgr)
write.csv(degcomponetsgr,"Cartas1828/degcomponetsgr.csv")
print(degcomponents)
count_components(RedCartas)
component_distribution(RedCartas)
#cercanía
degcloseness <- closeness(RedCartas)
View(degcloseness)
degclosenesssorted <- sort.int(degcloseness,decreasing = TRUE)
write.csv(degclosenesssorted,"Cartas1828/degclosenesssorted.csv")
#densidad
degdensidad <- edge_density(RedCartas)
# puntos de corte
degcutpoint <- articulation_points(RedCartas)
cp <- as.list(degcutpoint)
Cut.Points <- names(cp)
cuts <- as.data.frame(Cut.Points)
View(cuts)
write.csv(cuts,"Cartas1828/puntosdecorte.csv")
V(RedCartas)$color = ifelse(V(RedCartas) %in%
                      articulation_points(RedCartas),   
                    "salmon", "lightblue")
typeof(as_ids(articulation_points(RedCartas)))
typeof(V(RedCartas)$name)
p<-V(RedCartas)$name %in% 
  as_ids(articulation_points(RedCartas))
plot(RedCartas, 
     vertex.label = ifelse( V(RedCartas)$name %in% 
                              as_ids(articulation_points(RedCartas)), 
                            V(RedCartas)$name, 
                            NA),
     xlim=c(-0.7,0.7), 
     ylim=c(0.7,-0.7), 
     asp = 0, edge.arrow.size=.2,
     edge.witdh=1,
     vertex.size= 3, 
     vertex.label.cex=0.9, 
     vertex.label.dist=0.2, 
     vertex.label.size=1,
     edge.curved=0.2)
#clustering
degcluster <- cluster_optimal(RedCartas)
#cliques
degclique<- cliques(RedCartas, min = 3, max = 3)
#1er clique
degclique1 <- degclique[[1]]
print(degclique)
V(RedCartas)$label <- V(RedCartas)$name
g.lc <- subgraph(RedCartas, degclique[[1]])
plot(g.lc, layout=layout.fruchterman.reingold, vertex.color="gray60", vertex.size = 0, edge.arrow.size = 0.5, edge.color = "gray80")
clique_num(RedCartas)
#nuclearidad
degnuc <- coreness(RedCartas)
V(RedCartas)$core <- degnuc      # Add the cores as a vertex attribute
plot.igraph(RedCartas, 
            vertex.color=V(RedCartas)$core, 
            vertex.label=ifelse(degree(RedCartas.pos) > 50, V(RedCartas.pos)$name, NA),
            xlim=c(-0.7,0.7), ylim=c(0.7,-0.7), asp = 0, edge.arrow.size=.2,
            edge.witdh=1,
            vertex.size= 0.25, 
            vertex.label.cex=0.8, 
            vertex.label.dist=0.2, 
            vertex.label.size=1,
            edge.curved=0.2)

degclikaNum <- clique_num(RedCartas)
degclikaNum
degclika <- cliques(RedCartas, max = 3)
View(degclika)
degclikasorted <- sort.int(degclika,decreasing = TRUE)
View(degclikasorted)
largest_cliques(RedCartas)
max_cliques(RedCartas)
lclika <- largest.cliques(RedCartas) 
lclika1 <- lclika[[4]]
induced.subgraph(graph = RedCartas, vids = lclika1)

RedCartas.pos <- delete.edges(RedCartas,which(E(RedCartas)$weight<0))
#Remove unconnected vertices
RedCartas.pos <- delete.vertices(RedCartas.pos,which(degree(RedCartas.pos)<1))
#paquete de paleta
install.packages("viridis")
library(viridis)
RedCartas.pos_deg<-degree(RedCartas.pos, v=V(RedCartas.pos), mode="all")
fine <- 500
graphCol = viridis(fine)[as.numeric(cut(RedCartas.pos_deg,breaks = fine))]
plot(RedCartas.pos, xlim=c(-0.69,0.69), ylim=c(0.69,-0.69), asp = 0, vertex.color=graphCol,
     edge.color="black",
     vertex.size= 4,
     vertex.label.cex=0.9,
     #vertex.label.size=10,
     vertex.label.dist=0.188,
     vertez.label.color="red",
     vertex.label = ifelse(degree(RedCartas.pos) > 50, V(RedCartas.pos)$name, NA),
     layout=layout_with_fr(RedCartas.pos))
RedCartas.pos
