################### a.csv ###################
entrada_a <- read.csv("a.csv", header = FALSE)
colnames(entrada_a)[colnames(entrada_a) == "V3"] <- "CLASS"
entrada_a$CLASS <- entrada_a$CLASS + 1
plot(entrada_a$V1, entrada_a$V2, col = entrada_a$CLASS, main = "Dataset a.csv")

#K medias 
model_kmeans_a <- kmeans(entrada_a[ ,1:2], centers = 3)
plot(entrada_a$V1, entrada_a$V2, col = model_kmeans_a$cluster, main = "Dataset a.csv - K vecinos")
points(model_kmeans_a$centers[, c("V1","V2")], col = 1:3, pch = 19, cex = 5)
matrizConfusionKmeans_a <- table(entrada_a$CLASS, model_kmeans_a$cluster)
matrizConfusionKmeans_a
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_a)){
  aux = aux + matrizConfusionKmeans_a[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_a))*100
tasa_aciertos_1


#Cluster jerarquicos
entrada_a_cluster = entrada_a
entrada_a_cluster$CLASS <- NULL
entrada_a_cluster= as.matrix(entrada_a_cluster)
distancia = dist(entrada_a_cluster)


#single method 
model_cluster_single_a <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_a, k=3)
plot(entrada_a$V1, entrada_a$V2, col = ct_cs)
matrizConfusionClusterSingle_a <- table(entrada_a$CLASS,ct_cs)
matrizConfusionClusterSingle_a
aux = 0
for (i in 1:ncol(matrizConfusionClusterSingle_a)){
  aux = aux + matrizConfusionClusterSingle_a[i,i]
  aux
}
tasa_aciertos_2 <- (aux / nrow(entrada_a))*100
tasa_aciertos_2


#complete method 
model_cluster_complete_a <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_a, k=3)
plot(entrada_a$V1, entrada_a$V2, col = ct_cs)
matrizConfusionClusterComplete_a <- table(entrada_a$CLASS,ct_cs)
matrizConfusionClusterComplete_a
aux = 0
for (i in 1:ncol(matrizConfusionClusterComplete_a)){
  aux = aux + matrizConfusionClusterComplete_a[i,i]
  aux
}
tasa_aciertos_3 <- (aux / nrow(entrada_a))*100
tasa_aciertos_3



################### a_big.csv ###################
entrada_a_big <- read.csv("a_big.csv", header = FALSE)
colnames(entrada_a_big)[colnames(entrada_a_big) == "V3"] <- "CLASS"
entrada_a_big$CLASS <- entrada_a_big$CLASS + 1
plot(entrada_a_big$V1, entrada_a_big$V2, col = entrada_a_big$CLASS, main = "Dataset a_big.csv")

#K medias 
model_kmeans_a_big <- kmeans(entrada_a_big[ ,1:2], centers = 3)
plot(entrada_a_big$V1, entrada_a_big$V2, col = model_kmeans_a_big$cluster, main = "Dataset a_big.csv - K vecinos")
points(model_kmeans_a_big$centers[, c("V1","V2")], col = 1:3, pch = 19, cex = 5)
matrizConfusionKmeans_a_big <- table(entrada_a_big$CLASS, model_kmeans_a_big$cluster)
matrizConfusionKmeans_a_big
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_a_big)){
  aux = aux + matrizConfusionKmeans_a_big[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_a_big))*100
tasa_aciertos_1


################### good_luck.csv ###################
entrada_good_luck <- read.csv("good_luck.csv", header = FALSE)
colnames(entrada_good_luck)[colnames(entrada_good_luck) == "V11"] <- "CLASS"
entrada_good_luck$CLASS <- entrada_good_luck$CLASS + 1
plot(entrada_good_luck[,1:10], col = entrada_good_luck$CLASS, main = "Dataset good_luck.csv")

#K medias 
model_kmeans_good_luck <- kmeans(entrada_good_luck[ ,1:10], centers = 2)
plot(entrada_good_luck[,1:10], col = model_kmeans_good_luck$cluster, main = "Dataset good_luck.csv - K vecinos")
matrizConfusionKmeans_good_luck <- table(entrada_good_luck$CLASS, model_kmeans_good_luck$cluster)
matrizConfusionKmeans_good_luck
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_good_luck)){
  aux = aux + matrizConfusionKmeans_good_luck[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_good_luck))*100
tasa_aciertos_1


#Cluster jerarquicos
entrada_good_luck_cluster = entrada_good_luck
entrada_good_luck_cluster$CLASS <- NULL
entrada_good_luck_cluster= as.matrix(entrada_good_luck_cluster)
distancia = dist(entrada_good_luck_cluster)


#single method 
model_cluster_single_good_luck <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_good_luck, k=2)
plot(entrada_good_luck[ ,1:10], col = ct_cs)
matrizConfusionClusterSingle_good_luck <- table(entrada_good_luck$CLASS,ct_cs)
matrizConfusionClusterSingle_good_luck
aux = 0
for (i in 1:ncol(matrizConfusionClusterSingle_good_luck)){
  aux = aux + matrizConfusionClusterSingle_good_luck[i,i]
  aux
}
tasa_aciertos_2 <- (aux / nrow(entrada_good_luck))*100
tasa_aciertos_2


#complete method 
model_cluster_complete_good_luck <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_good_luck, k=2)
plot(entrada_good_luck[ ,1:10], col = ct_cs)
matrizConfusionClusterComplete_good_luck <- table(entrada_good_luck$CLASS,ct_cs)
matrizConfusionClusterComplete_good_luck
aux = 0
for (i in 1:ncol(matrizConfusionClusterComplete_good_luck)){
  aux = aux + matrizConfusionClusterComplete_good_luck[i,i]
  aux
}
tasa_aciertos_3 <- (aux / nrow(entrada_good_luck))*100
tasa_aciertos_3



################### moon.csv ###################
entrada_moon <- read.csv("moon.csv", header = FALSE)
colnames(entrada_moon)[colnames(entrada_moon) == "V3"] <- "CLASS"
entrada_moon$CLASS <- entrada_moon$CLASS + 1
plot(entrada_moon$V1, entrada_moon$V2, col = entrada_moon$CLASS, main = "Dataset moon.csv")

#K medias 
model_kmeans_moon <- kmeans(entrada_moon[ ,1:2], centers = 2)
plot(entrada_moon$V1, entrada_moon$V2, col = model_kmeans_moon$cluster, main = "Dataset moon.csv - K vecinos")
points(model_kmeans_moon$centers[, c("V1","V2")], col = 1:2, pch = 19, cex = 5)
matrizConfusionKmeans_moon <- table(entrada_moon$CLASS, model_kmeans_moon$cluster)
matrizConfusionKmeans_moon
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_moon)){
  aux = aux + matrizConfusionKmeans_moon[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_good_luck))*100
tasa_aciertos_1


#Cluster jerarquicos
entrada_moon_cluster = entrada_moon
entrada_moon_cluster$CLASS <- NULL
entrada_moon_cluster= as.matrix(entrada_moon_cluster)
distancia = dist(entrada_moon_cluster)


#single method 
model_cluster_single_moon <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_moon, k=2)
plot(entrada_moon[ ,1:2], col = ct_cs)
matrizConfusionClusterSingle_moon <- table(entrada_moon$CLASS,ct_cs)
matrizConfusionClusterSingle_moon
aux = 0
for (i in 1:ncol(matrizConfusionClusterSingle_moon)){
  aux = aux + matrizConfusionClusterSingle_moon[i,i]
  aux
}
tasa_aciertos_2 <- (aux / nrow(entrada_moon))*100
tasa_aciertos_2


#completed method 
model_cluster_complete_moon <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_moon, k=2)
plot(entrada_moon[ ,1:2], col = ct_cs)
matrizConfusionClusterComplete_moon <- table(entrada_moon$CLASS,ct_cs)
matrizConfusionClusterComplete_moon
aux = 0
for (i in 1:ncol(matrizConfusionClusterComplete_moon)){
  aux = aux + matrizConfusionClusterComplete_moon[i,i]
  aux
}
tasa_aciertos_3 <- (aux / nrow(entrada_moon))*100
tasa_aciertos_3


################### Dataset con reglas para asignacion de clases ###################

################### h.csv ################### 
 #install.packages("scatterplot3d")
library("scatterplot3d")
entrada_h <- read.csv("h.csv", header = FALSE)
entrada_h$CLASS = floor(entrada_h$V4) - 3
plot(entrada_h[1:3], col= entrada_h$CLASS)
scatterplot3d(entrada_h$V1, entrada_h$V2, entrada_h$V3, color = entrada_h$CLASS)
entrada_h$V4 <- NULL

#K medias 
model_kmeans_h <- kmeans(entrada_h[ ,1:3], centers = 11)
matrizConfusionKmeans_h <- table(entrada_h$CLASS, model_kmeans_h$cluster)
matrizConfusionKmeans_h
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_h)){
  aux = aux + matrizConfusionKmeans_h[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_h))*100
tasa_aciertos_1


#Cluster jerarquicos
entrada_h_cluster = entrada_h
entrada_h_cluster$CLASS <- NULL
entrada_h_cluster= as.matrix(entrada_h_cluster)
distancia = dist(entrada_h_cluster)


#single method 
model_cluster_single_h <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_h, k=11)
scatterplot3d(entrada_h$V1, entrada_h$V2, entrada_h$V3, color = ct_cs)

matrizConfusionClusterSingle_h <- table(entrada_h$CLASS,ct_cs)
matrizConfusionClusterSingle_h
aux = 0
for (i in 1:ncol(matrizConfusionClusterSingle_h)){
  aux = aux + matrizConfusionClusterSingle_h[i,i]
  aux
}
tasa_aciertos_2 <- (aux / nrow(entrada_h))*100
tasa_aciertos_2


#complete method 
model_cluster_complete_h <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_h, k=11)
scatterplot3d(entrada_h$V1, entrada_h$V2, entrada_h$V3, color = ct_cs)

matrizConfusionClusterComplete_h <- table(entrada_h$CLASS,ct_cs)
matrizConfusionClusterComplete_h
aux = 0
for (i in 1:ncol(matrizConfusionClusterComplete_h)){
  aux = aux + matrizConfusionClusterComplete_h[i,i]
  aux
}
tasa_aciertos_3 <- (aux / nrow(entrada_h))*100
tasa_aciertos_3


################### s.csv ################### 
entrada_s <- read.csv("s.csv", header = FALSE) #"CLASS" -4.707243 - 4.710585
entrada_s$CLASS = floor(entrada_s$V4) + 6
scatterplot3d(entrada_s$V1, entrada_s$V2, entrada_s$V3, color = entrada_s$CLASS)
plot(entrada_s[1:3], col= entrada_s$CLASS)
entrada_s$V4 <- NULL

#K medias 
model_kmeans_s <- kmeans(entrada_s[ ,1:3], centers = 10)
matrizConfusionKmeans_s <- table(entrada_s$CLASS, model_kmeans_s$cluster)
matrizConfusionKmeans_s
aux = 0
for (i in 1:ncol(matrizConfusionKmeans_s)){
  aux = aux + matrizConfusionKmeans_s[i,i]
  aux
}
tasa_aciertos_1 <- (aux / nrow(entrada_s))*100
tasa_aciertos_1


#Cluster jerarquicos
entrada_s_cluster = entrada_s
entrada_s_cluster$CLASS <- NULL
entrada_s_cluster= as.matrix(entrada_s_cluster)
distancia = dist(entrada_s_cluster)


#single method 
model_cluster_single_s <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_s, k=10)
scatterplot3d(entrada_s$V1, entrada_s$V2, entrada_s$V3, color = ct_cs)

matrizConfusionClusterSingle_s <- table(entrada_s$CLASS,ct_cs)
matrizConfusionClusterSingle_s
aux = 0
for (i in 1:ncol(matrizConfusionClusterSingle_s)){
  aux = aux + matrizConfusionClusterSingle_s[i,i]
  aux
}
tasa_aciertos_2 <- (aux / nrow(entrada_s))*100
tasa_aciertos_2

#complete method
model_cluster_complete_s <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_s, k=10)
scatterplot3d(entrada_s$V1, entrada_s$V2, entrada_s$V3, color = ct_cs)

matrizConfusionClusterComplete_s <- table(entrada_s$CLASS,ct_cs)
matrizConfusionClusterComplete_s
aux = 0
for (i in 1:ncol(matrizConfusionClusterComplete_s)){
  aux = aux + matrizConfusionClusterComplete_s[i,i]
  aux
}
tasa_aciertos_3 <- (aux / nrow(entrada_s))*100
tasa_aciertos_3




################### guess.csv ################### 
entrada_guess <- read.csv("guess.csv", header = FALSE)
plot(entrada_s[1:2])
jambu = rep(0,30)
for (k in 1:30)
{
  grupos = kmeans(entrada_guess, k)
  jambu[k] = grupos$tot.withinss
}
plot(jambu, col = "blue", type = "b", main = "Codo de Jambu")


# Se escoge K = 5 ya que apartir de este punto la curva empieza a estabilizarse

#K medias 
model_kmeans_guess <- kmeans(entrada_guess[ ,1:2], centers = 5)
plot(entrada_guess$V1, entrada_guess$V2)

plot(entrada_guess$V1, entrada_guess$V2, col = model_kmeans_guess$cluster, main = "Dataset guess.csv - K vecinos")


#Cluster jerarquicos
entrada_guess_cluster = entrada_guess
entrada_guess_cluster= as.matrix(entrada_guess_cluster)
distancia = dist(entrada_guess_cluster)


#single method 
model_cluster_single_guess <- hclust(distancia, method="single")
ct_cs <- cutree(model_cluster_single_guess, k=5)
plot(entrada_guess$V1, entrada_guess$V2, col = ct_cs)


#complete method
model_cluster_complete_guess <- hclust(distancia, method="complete")
ct_cs <- cutree(model_cluster_complete_guess, k=5)
plot(entrada_guess$V1, entrada_guess$V2, col = ct_cs)


################### help.csv ################### 
entrada_help <- read.csv("help.csv", header = FALSE) 
entrada_help$CLASS = floor(entrada_help$V4) + 6
scatterplot3d(entrada_help$V1, entrada_help$V2, entrada_help$V3, color = entrada_help$CLASS)

#K medias 
model_kmeans_help <- kmeans(entrada_help[ ,1:2], centers = 2)
matrizConfusionKmeans_help <- table(entrada_help$CLASS, model_kmeans_help$cluster)
matrizConfusionKmeans_help