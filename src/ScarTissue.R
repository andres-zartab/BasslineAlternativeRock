#Notas melodia de la cancion
melEle<-c("F","A","F","A","F","A","F","A","C","C","D","D","D","D","F","D","F","D","F","E","C")

#Tiempos melodia de la cancion
tmelEle<-c(4,8,16,16,16,16,8,8,8,16,8,16,8,16,16,16,16,8,16,16,8)

#Bajo Original
bassEle<-cbind(c("F","F","F","A","A","A","A","C","C","D","D","D","D","D","D","F","D","C"),c(4,8,16,8,16,8,8,8,16,8,16,8,16,8,16,8,8,8))

#Percusion (bombo)
kdEle<-c("K","S","K","S","K","K","S","K","S","K","S","K","K","S")
ktEle<-c(8,16,16,4,8,8,4,8,16,16,4,8,8,4)

#Valor del criterio a optimizar 1 con el bajo original
buenBass(bassEle[,1],1/as.numeric(bassEle[,2]),c("F","Dm"),"F",1)

#Valor del criterio a optimizar 2 con el bajo original
goodFigures(melbassFigures(melEle,1/tmelEle,bassEle[,1],1/as.numeric(bassEle[,2])))

#Busca un "buen" bajo, utilizando una prob = 0.7 y 15 mineros
#se utilizan los acordes originales de la cancion y la clave es F
algoST<-solve(0.7,15,c("F","Dm"),c(1,1),kdEle,ktEle,melEle,tmelEle,"F")

#Imprime la matriz de resultados
#Col 1: Indice
#Col 2: Soluciones encontradas
#Col 3: Valor criterio 1
#Col 4: Valor criterio 2
#Col 5: Pertenece a la frontera? 1 si si y 0 dlc
algoST

#Imprime una solucion particular, en este caso la 4
algoST[[4,2]]

#En este caso la 10
algoST[[10,2]]
