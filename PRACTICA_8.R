# Cargar datos
datos <- read.csv("C:/Users/alici/Desktop/UNI/ANALISIS ESTADISTICO AVANZADO/R/SocioeconomicDataset.csv", sep = ";")
row.names(datos) <- datos$Country
datosnum <- datos[, -1]

# Ver datos
head(datosnum)
str(datosnum)

# Clustering jerárquico 1 (Manhattan / average)
distancia1 <- dist(datosnum, method = "manhattan") 
clust1 <- hclust(distancia1, method = "average")
plot(clust1, labels = rownames(datosnum), main = "DENDOGRAMA 1 (Manhattan / Average)", cex = 0.7)
clusters1 <- cutree(clust1, k = 4)
datosnum$Clusters_average <- clusters1

# Clustering jerárquico 2 (Maximum / complete)
# Elimina las columnas de clusters antes de calcular distancias
distancia2 <- dist(datosnum[, -ncol(datosnum)], method = "maximum")
clust2 <- hclust(distancia2, method = "complete")
plot(clust2, labels = rownames(datosnum), main = "DENDOGRAMA 2 (Maximum / Complete)", cex = 0.7)
clusters2 <- cutree(clust2, k = 4)
datosnum$Clusters_complete <- clusters2

# Comparar los dendrogramas lado a lado
par(mfrow = c(1, 2))
plot(clust1, main = "DENDOGRAMA 1 (Manhattan / Average)", cex = 0.7)
plot(clust2, main = "DENDOGRAMA 2 (Maximum / Complete)", cex = 0.7)

# Calcular el Índice Rand Ajustado
if (!require("mclust")) install.packages("mclust")
library(mclust)
ari <- adjustedRandIndex(datosnum$Clusters_average, datosnum$Clusters_complete)
print(paste("Índice Rand Ajustado:", ari))
