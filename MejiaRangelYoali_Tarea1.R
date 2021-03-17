##########TAREA 1. Yoali Mejia Rangel, GenÛmica Funcional############

##EJERCICIO 1
#1. A partir de las redes de la figura 1. Calcula con igraph, las siguientes
#propiedades: a) Vecinos.b) La distribuci??on de conectividades,c) El nodo m·s conectado, d)el diametro,
#e)matriz de distancias y heatmap.

library (igraph) #primero se carga la librerÌa que se va a usar

######FIGURA/GR¡FICA 1######
fig1 <- make_empty_graph(n=10, directed = FALSE ) #red 1, es directa, sin unir
V(fig1)$color="darkgoldenrod" 
plot(fig1) #correr red realizada, no se observan conexiones
fig1 <- add.edges(fig1,c(1,10, 1,2, 1,3, 1,4, 1,5, 1,6, 1,7, 1,8, 1,9)) 
plot(fig1) #se especifican conexiones
#a) Vecinos.
vecinos1 <- degree(fig1) #se utuliza la funciÛn de degree
vecinos1
#b) La distribucion de conectividades
degree.distribution(fig1)
#c) El nodo m¬¥as conectado.
sort(degree(fig1))
head(sort(degree(fig1),decreasing=TRUE),1)
#d) El di¬¥ametro
diameter(fig1)
#e) La matriz de distancias y el heatmap asociado.
distMatrix1 <- shortest.paths(fig1) #shortest para el camino m·s corto de los nodos
distMatrix1
heatmap(distMatrix1) #heatmap asociado

##FIGURA 2
fig2<-make_empty_graph(n=10, directed = FALSE) #red indirecta
+V(fig2)$color="palevioletred3"
plot(fig2)
fig2<-add.edges(fig2,c(10,7, 10,9, 10,2, 10,1, 1,8, 1,2, 1,9, 1,4,
                       9,8, 9,2, 9,4, 7,3, 7,5, 7,4, 7,2, 
                       7,9, 3,5, 3,8, 8,4, 8,6, 6,2, 6,5, 6,4, 5,8,
                       5,4, 9,6)) #conectar los puntos
plot(fig2) #observar red 
#a) Vecinos.
vecinos2<-degree(fig2)
vecinos2
#b) La distribucion de conectividades
degree.distribution(fig2)
#c) El nodo mas conectado.
sort(degree(fig2))
head(sort(degree(fig2),decreasing=TRUE),1)
#d) El diametro
diameter(fig2)
#e) La matriz de distancias y el heatmap asociado.
distMatrix2 <- shortest.paths(fig2) #conocer la distancia m·s corta
distMatrix2
heatmap(distMatrix2) #heatmap asociado

##FIGURA 3
fig3<- make_empty_graph(n = 10, directed = FALSE) #red directa
V(fig3)$color = "aquamarine3"
plot(fig3)
fig3 <- add.edges(fig3,c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,1))
plot(fig3)
#a) Vecinos.
vecinos3<-degree(fig3)
vecinos3
#b) La distribucion de conectividades
degree.distribution(fig3)
#c) El nodo mas conectado.
sort(degree(fig3))
head(sort(degree(fig3),decreasing=TRUE),1)
#d) El diametro
diameter(fig3)
#e) La matriz de distancias y el heatmap asociado.
distMatrix3 <- shortest.paths(fig3) #distancia m·s corta de los nodos
distMatrix3
heatmap(distMatrix3) #heatmap asociado

##FIGURA 4
fig4 <- make_empty_graph(n = 10, directed = FALSE)
V(fig4)$color = "salmon"
plot(fig4)
fig4 <- add.edges(fig4,c(1,2, 2,9, 1,8, 1,3, 3,4, 1,6, 1,5, 5,7, 5,10)) #agregar conexiones
plot(fig4)
##a) Vecinos
vecinos4<- degree(fig4)
vecinos4
##b) La distribuci¬¥on de conectividades
degree.distribution(fig4)
##c) El nodo m¬¥as conectado.
sort(degree(fig4))
head(sort(degree(fig4),decreasing=TRUE),1)
##d) El di¬¥ametro
diameter(fig4)
##e) La matriz de distancias y el heatmap asociado.
distMatrix4 <- shortest.paths(fig4) #distancia mas corta
distMatrix4
heatmap(distMatrix4) #heatmap asociado


######EJERCICIO 2######
#Elabora un programa en R que utilice un ciclo for para a partir del vector siguiente
#v <-sample(100) imprima los cuadrados de los n??umeros impares.
v <- sample(100) #este es el vector dado 
v
#este ciclo for muestra los n˙meros impares del vector 
v<-sample(100)
for (i in v) {
  if((i%%2==1)){
    print(i)
  }
}
#ciclo for para realizar los cuadrados de los numeros impares 
for (i in v) {
  if ( i%% 2 ==1) 
    print(i^2)
} #se le asiganron los n˙meros divisibles entre 2 e impares con el 1


######EJERCICIO 3######
#Elabora un programa en R que a partir del archivo de amistades del grupo.
#a) Cargue el archivo, b) Genere el vector de nombres de todos tus amigos (los tuyos), c) Genere el vector de nombres 
#de todos los que se consideren tus amigos, d) Imprima el texto: "Hola amigo1", en donde amigo1 es el nombre de
#cada uno de tus amigos
yoali <- read.csv("6to semestre/GenÛmica/red_amigos.csv")
View(yoali) ##este es el archivo que se generÛ de las redes de amistades 
#hacer la red con el archivo ya precargado
row.names (yoali) <- yoali[,1]
yoali <- yoali[,-1]
yoali<- as.matrix(yoali) #matriz necesaria para gr·fico de la red 
redamistades<- graph_from_adjacency_matrix(yoali,mode ="directed") #red directa
plot(redamistades) #mostrar red
#b) Genere el vector de nombres de todos tus amigos (los tuyos)
amixes<-which(redamistades["YOALI",]>=1) #which ayuda a extraer solo a los considero mis amigos, por eso debe ser >=1
amixes
names(amixes) #mostrar solo los nombres
#c) Genere el vector de nombres de todos los que se consideren tus amigos.
amixes2<-as.matrix(redamistades)
amixes2 <-which(redamistades[,"YOALI"]>=1) #which para seleccionar a los que me consideran su amiga, condiciÛn: >=1
amixes2
names(amixes2) #mostrar solo los nombres de las personas
#d) Imprima el texto: ‚ÄúHola amigo1‚Äù, en donde amigo1 es el nombre de cada uno de tus amigos.
amixes3 <-which(redamistades["YOALI",]>=1) #se extraen a los sujetos que considero mis amigos, condiciÛn: >=1
amixes3
for (i in 1:length(amixes3)) {
  print(paste("Hola amig@", names(amixes3)[i]))
} #con el ciclo for se le indica que extraiga el nombre del amigo y se le aÒada el mensaje a cada uno

######EJERCICIO 4######
#Utiliza la red del club de Karate de Zachary (investiga cmo puedes generarla en igraph)
#a) Calcula los nodos ms conectados, b) Calcula el dimetro, c) Encuentra la distribucin de conectividades.
#d) Genera la matriz de adyacencia, e) Dibuja la red con los nodos proporcionales al degree de cada uno de ellos.

zach<-graph("Zachary") #precargar base de datos
plot(zach)

#a) Calcula los nodos mas cercanos
sort(degree(zach), decreasing = T)[1:3] #arroja a 17,16,12 
#b)calcula el di·metro
diameter(zach) #di·metro de 5
#c)Calcula la distribuciÛn de conectividades
degree.distribution (zach)
#d)Genera la matriz de adyacencia
adj<- get.adjacency(zach) #matriz de adyacencia con funciÛn especÌfica
adj
#e)Dibuja la red con los nodos proporcionales al degree de cada uno de ellos.
plot(zach, layout=layout_nicely, vertex.size=degree(zach, V(zach), "in")*2+2,
     vertex.label.dist=0.8, edge.arrow.size=0.8) #se ordenan, dandole tamaÒo a cada nodo


