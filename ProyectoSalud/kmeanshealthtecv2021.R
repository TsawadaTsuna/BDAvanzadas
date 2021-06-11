# K-means health data

setwd("D:/BDAvanzadas/ProyectoSalud")
getwd()
all.data.raw <- read.csv("allhealthdata.csv", stringsAsFactors = F)
str(all.data.raw)
nrow(all.data.raw)
summary(all.data.raw)

all.data.selected <- all.data.raw
colnames(all.data.selected)
rownames(all.data.selected) <- all.data.selected$patient_int
head(all.data.selected)
all.data.selected["54","diastolic_lastest_nbr"]


head(all.data.selected)


#Eliminando columnas
#all.data.selected$patient_int <- NULL
#all.data.selected <- all.data.raw[,-1] # bad practice
all.data.selected <- subset(all.data.selected, select = -c(patient_int, date_lastest_bp_dt,
                                                      date_lastest_wh_dt,
                                                      date_max_glucose_dt,
                                                      date_min_glucose_dt,
                                                      date_min_spo2_dt))
str(all.data.selected)

all.data.transformed <- transform(all.data.selected,
                               avbloodpressure_lastest_nbr= as.numeric(avbloodpressure_lastest_nbr),
                               diastolic_lastest_nbr     = as.numeric(diastolic_lastest_nbr),
                               glucose_max_dt= as.numeric(glucose_max_dt),
                               glucose_min_dt  = as.numeric(glucose_min_dt),
                               heartrate_lastest_nbr       = as.numeric(heartrate_lastest_nbr),
                               height_lastest_nbr          = as.numeric(height_lastest_nbr),
                               imc_lastest_nbr             = as.numeric(imc_lastest_nbr),
                               spo2_min_dt          = as.numeric(spo2_min_dt),
                               systolic_lastest_nbr        = as.numeric(systolic_lastest_nbr),
                               weight_lastest_nbr        = as.numeric(weight_lastest_nbr)
)
str(all.data.transformed)
all.data.selected <- subset(all.data.transformed, select = -c(patientid, measurementdate))
colnames(all.data.transformed)
colnames(all.data.transformed) <- c("avgpress","diast","gluc.max","gluc.min",
                                    "heartrate","height","imc","spo2","systo","weight")

str(all.data.transformed)

nrow(all.data.transformed)
# K-means
wss <- vector()
wss
for (i in 1:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(all.data.transformed, 
                                      centers=i)$withinss)
}
wss
# PLOT 1
plot(1:15, wss, type="b", xlab="Numero de clusters",
     ylab="Error Standard")

# K = 3
?kmeans
nrow(all.data.selected)
set.seed(1234)
kmeans.3 <- kmeans(x=all.data.transformed, centers=3)
attributes(kmeans.3)
kmeans.3$withinss
kmeans.3$cluster
kmeans.3$size
kmeans.3$centers

# grupo1 --> diabeticosobrepeso
# grupo2 --> diabetico
# grupo3 --> menospeores

all.data.enriched <- all.data.transformed
all.data.enriched$cluster <- kmeans.3$cluster
head(all.data.enriched)
all.data.enriched$classification <- kmeans.3$cluster
all.data.enriched[all.data.enriched$cluster==1,]$classification <- "diabeticosobrepeso"
all.data.enriched[all.data.enriched$cluster==2, ]$classification <- "diabetico"
all.data.enriched[all.data.enriched$cluster==3, ]$classification <- "menospeores"
all.data.enriched
str(all.data.enriched)
all.data.enriched$classification <- factor(all.data.enriched$classification)


#install.packages("fpc")
library(fpc)
plotcluster(all.data.enriched[,-c(11,12)],kmeans.3$cluster)

head(all.data.enriched)

plot(all.data.enriched[,c("gluc.max","imc")],
     col = kmeans.3$cluster)

points(kmeans.3$centers[,c("gluc.max","imc")],
       col=4:6,
       pch =4, cex=6)

plot(all.data.enriched[,c("heartrate","avgpress")],
     col = kmeans.3$cluster)

points(kmeans.3$centers[,c("heartrate","avgpress")],
       col=4:6,
       pch =4, cex=6)

plot(all.data.enriched[,c("weight","imc")],
     col = kmeans.3$cluster)

points(kmeans.3$centers[,c("weight","imc")],
       col=4:6,
       pch =4, cex=6)
# implement CART
library(party)

set.seed(1234)
ind <- sample(x=c(1,2), size=nrow(all.data.enriched), replace=TRUE, prob=c(0.8, 0.2))
ind

training.set <- all.data.enriched[ind==1,]
test.set <- all.data.enriched[ind==2,]
summary(training.set)
summary(test.set)

formula.1 <- classification ~ spo2 + glucose + avgbloodpressure + diastolic + heartrate + height + imc + weight + systolic

formula.1 <- classification ~ spo2 + glucose + avgbloodpressure + diastolic + heartrate + imc + systolic



class(formula.1)
healthcare.ctree <- ctree(formula = formula.1, data = training.set)
plot(healthcare.ctree)
getwd()
setwd("/Users/adeobeso/Downloads/")
png(filename = 'ctree_healthcare2.png', width = 6000, height = 1200)
plot(healthcare.ctree)
dev.off()
?predict
prediction <- predict(healthcare.ctree, newdata=test.set)
nrow(test.set)
prediction
result <- table(test.set$classification, prediction)
sum(result)
accuracy <- (72 + 41 + 71) / sum(result)
accuracy <- (77 + 48 + 81) / sum(result)
healthcare.ctree
names(training.set) 
patient.x <- data.frame(spo2=100, glucose=80, avgbloodpressure=100, diastolic=100,
                        heartrate=100, height=1.80, imc=23, weight=75, systolic=100)
patient.x.prediction <- predict(healthcare.ctree, newdata=patient.x)
save(healthcare.ctree, file="healthcare.ctree.R")

patient.y <- data.frame(spo2=60, glucose=139, avgbloodpressure=140, diastolic=100,
                        heartrate=100, height=1.80, imc=40.4, weight=210, systolic=100)
patient.y.prediction <- predict(healthcare.ctree, newdata=patient.y)
