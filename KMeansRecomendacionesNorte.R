# K-means recomendaciones por sector de codigo_scian denue region norte

setwd("D:/BDAvanzadas")
getwd()
all.data.raw <- read.csv("datos_resultados.csv", stringsAsFactors = F)
str(all.data.raw)
nrow(all.data.raw)
summary(all.data.raw)

all.data.selected <- all.data.raw
colnames(all.data.selected)
rownames(all.data.selected) <- all.data.selected$id
head(all.data.selected)
#all.data.selected["6170742","codigo_scian"]

#Eliminando columnas
all.data.selected <- subset(all.data.selected, select = c(id,
                                                           codigo_scian,
                                                           estrato_min,
                                                           estrato_max,
                                                           nombre_clase_actividad,
                                                           total_min,
                                                           total_max,
                                                           total_of_sector,
                                                           suma_min_max,
                                                           estrato_avg))
#str(all.data.selected)

all.data.transformed <- transform(all.data.selected,
                                  codigo_scian = as.numeric(codigo_scian),
                                  estrato_min = as.numeric(estrato_min),
                                  estrato_max = as.numeric(estrato_max),
                                  total_min = as.numeric(total_min),
                                  total_max = as.numeric(total_max),
                                  total_of_sector = as.numeric(total_of_sector),
                                  suma_min_max = as.numeric(suma_min_max),
                                  estrato_avg = as.numeric(estrato_avg)
                                  )

#str(all.data.transformed)
all.data.selected <- subset(all.data.transformed, select = -c(id, nombre_clase_actividad))

#str(all.data.transformed)
#nrow(all.data.transformed)
# K-means
wss <- vector()
wss
for (i in 1:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(all.data.selected, 
                       centers=i)$withinss)
}
wss
# PLOT 1
plot(1:15, wss, type="b", xlab="Numero de clusters",
     ylab="Error Standard")

# K = 4
#?kmeans
#nrow(all.data.selected)
set.seed(1234)
kmeans.4 <- kmeans(x=all.data.selected, centers=4)
#attributes(kmeans.4)
#kmeans.4$withinss
#kmeans.4$cluster
#kmeans.4$size
#kmeans.4$centers



all.data.enriched <- all.data.selected
all.data.enriched$cluster <- kmeans.4$cluster
#head(all.data.enriched)
all.data.enriched$recomendation <- kmeans.4$cluster

#install.packages("fpc")
library(fpc)
plotcluster(all.data.enriched[,-c(11,12)],kmeans.4$cluster)

# grupo1 --> norecomendable
# grupo2 --> pocorecomendable
# grupo3 --> recomendable
# grupo4 --> muyrecomendable

all.data.enriched[all.data.enriched$cluster==1,]$recomendation <- "no recomendable"
all.data.enriched[all.data.enriched$cluster==2, ]$recomendation <- "poco recomendable"
all.data.enriched[all.data.enriched$cluster==3, ]$recomendation <- "recomendable"
all.data.enriched[all.data.enriched$cluster==4, ]$recomendation <- "muy recomendable"
#all.data.enriched
#str(all.data.enriched)
all.data.enriched$recomendation <- factor(all.data.enriched$recomendation)


#head(all.data.enriched)

plot(all.data.enriched[,c("codigo_scian","total_min")],
     col = kmeans.4$cluster)

points(kmeans.4$centers[,c("codigo_scian","total_min")],
       col=4:6,
       pch =4, cex=6)

plot(all.data.enriched[,c("codigo_scian","total_max")],
     col = kmeans.4$cluster)

points(kmeans.4$centers[,c("codigo_scian","total_max")],
       col=4:6,
       pch =4, cex=6)

plot(all.data.enriched[,c("codigo_scian","estrato_avg")],
     col = kmeans.4$cluster)

points(kmeans.4$centers[,c("codigo_scian","estrato_avg")],
       col=4:6,
       pch =4, cex=6)
