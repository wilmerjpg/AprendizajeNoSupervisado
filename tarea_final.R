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
#La matriz me la da desordenada, como se hace en este caso??

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
#La matriz me la da desordenada, como se hace en este caso??

################### good_luck.csv ###################
entrada_good_luck <- read.csv("good_luck.csv", header = FALSE)
colnames(entrada_good_luck)[colnames(entrada_good_luck) == "V11"] <- "CLASS"
entrada_good_luck$CLASS <- entrada_good_luck$CLASS + 1
plot(entrada_good_luck[,1:10], col = entrada_good_luck$CLASS, main = "Dataset good_luck.csv")

#K medias 
model_kmeans_good_luck <- kmeans(entrada_good_luck[ ,1:10], centers = 2)
plot(entrada_good_luck[,1:10], col = model_kmeans_good_luck$cluster, main = "Dataset good_luck.csv - K vecinos")
#points(model_kmeans_good_luck$centers[, c("V1","V2")], col = 1:3, pch = 19, cex = 5)
matrizConfusionKmeans_good_luck <- table(entrada_good_luck$CLASS, model_kmeans_good_luck$cluster)
matrizConfusionKmeans_good_luck
#La matriz me la da desordenada, como se hace en este caso??



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



################### Dataset con reglas para asignacion de clases ###################

################### h.csv ################### 
entrada_h <- read.csv("h.csv", header = FALSE)
entrada_h$CLASS = floor(entrada_h$V4) - 3
plot(entrada_h[1:3], col= entrada_h$CLASS)



#K medias 
model_kmeans_h <- kmeans(entrada_h[ ,1:2], centers = 2)
matrizConfusionKmeans_h <- table(entrada_h$CLASS, model_kmeans_h$cluster)
matrizConfusionKmeans_h


################### help.csv ################### 
entrada_help <- read.csv("help.csv", header = FALSE) #"CLASS" -4.711871 - 14.13536
entrada_help$CLASS = floor(entrada_h$V4) - 3
plot(entrada_help[1:3], col= entrada_help$CLASS)

#K medias 
model_kmeans_help <- kmeans(entrada_help[ ,1:2], centers = 2)
matrizConfusionKmeans_help <- table(entrada_help$CLASS, model_kmeans_help$cluster)
matrizConfusionKmeans_help

################### s.csv ################### 
entrada_s <- read.csv("s.csv", header = FALSE) #"CLASS" -4.707243 - 4.710585
entrada_s$CLASS = floor(entrada_s$V4) + 6
plot(entrada_s[1:3], col= entrada_s$CLASS)


#K medias 
model_kmeans_s <- kmeans(entrada_s[ ,1:2], centers = 2)
matrizConfusionKmeans_s <- table(entrada_s$CLASS, model_kmeans_s$cluster)
matrizConfusionKmeans_s
