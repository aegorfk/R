Sys.setenv(LANG = "en")
#install.packages("devtools")
library("devtools")
#install_github("rCharts","ramnathv")
library("rCharts")
#install.packages("rgl")
library("rgl")
#install.packages("e1071", dependencies = TRUE)
library("e1071")
#install.packages("class", dependencies = TRUE)
library("class")
install.packages("clue")
library("clue")


bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE,  header=TRUE, sep=";")
bankruptcy <- bankruptcy[ which(bankruptcy$Автономность < 30 ), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Рентабельность.активов > -6), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Доходность.активов > -6), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Оборачиваемость.активов < 10), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Доходность.активов > -6), ]
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))

for (i in 2:7) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))
for (i in 2:6) bankruptcy[,i] <- 1/(1+(exp(-1*bankruptcy[,i])))
#pairs(~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Автономность + Оборачиваемость.активов,data=bankruptcy,  main="Scatterplot Matrix")
#plot3d(x = bankruptcy$Ликвидность.активов, y = bankruptcy$Рентабельность.активов, z = bankruptcy$Доходность.активов)

bankruptcy$Банкрот <- factor(bankruptcy$Банкрот)
#сбалансированно бьем выборку на тестовую и проверочную
ind1 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==1, select=ID: Банкрот)
ind0 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==0, select=ID: Банкрот)
sampind1 <- ind1[sample(1:nrow(ind1), 53, replace=FALSE),]
sampind0 <- ind0[sample(1:nrow(ind0), 158, replace=FALSE),]
testing_data<-NULL
training_data<-NULL
training_data$values <- rbind(sampind0, sampind1)
testing_data$values <- bankruptcy[!(bankruptcy$ID %in% training_data$values$ID),]
rownames(training_data$values)<-NULL
rownames(testing_data$values)<-NULL
rm(ind0, ind1, sampind0, sampind1, i)

testing_data$values$ID<-NULL
training_data$values$ID<-NULL
testing_data$Банкрот<-testing_data$values$Банкрот
training_data$Банкрот<-training_data$values$Банкрот
testing_data$values$Банкрот<-NULL
training_data$values$Банкрот<-NULL

bank <- (nrow(training_data$values) -1)*sum(apply(training_data$values, 2,var))
for (i in 2:20) bank[i]  <- sum(kmeans(training_data$values, center = i)$withinss)
plot(1:20, bank, type="b")
axis(side=1, at=c(0:20))


nr <- NROW(training_data$values)
#ind <- sample(nr, 0.9 * nr, replace = FALSE)
party <- kmeans(training_data$values, 7)

#lets see what we got
plot(training_data$values, col=party$cluster)
points(party$centers, pch=16)

kmeans_test <- cl_predict(party, training_data$values)
kmeans_test_res <- table(cluster = kmeans_test, bankrot = training_data$Банкрот)
kmeans_test_res

kmeans_predict<- cl_predict(party, testing_data$values)
kmeans_res <- table(cluster = kmeans_predict, bankrot = testing_data$Банкрот)
kmeans_res





#bankruptcy2$clusterid[-ind]<-kmeans_predict






bank_kmeans <- kmeans(bankruptcy, centers=2, iter.max=10, nstart = 210)
bankruptcy$cluster <- factor(bank_kmeans$cluster)






options(
  rcharts.mode = 'iframesrc', 
  rcharts.cdn = TRUE,
  RCHART_WIDTH = 600,
  RCHART_HEIGHT = 400
)
library(knitr)
opts_chunk$set(tidy = F, results = 'asis', comment = NA)
training_data$cluster<-party$cluster
plot_frame = data.frame(
    t = party$cluster,
    var = training_data$Банкрот,
    val<-training_data$values
  )
p1 <- nPlot(Ликвидность.активов ~ t, group =  'var', data = plot_frame, 
            type = 'scatterChart', id = 'chart', color = 'var'
)
p1$xAxis(axisLabel = 'Кластер')
p1$yAxis(axisLabel = 'Ликвидность активов')
p1$chart(color = c('blue', 'red') )
p1

p2 <- nPlot(Рентабельность.активов ~ t, group =  'var', data = plot_frame, 
            type = 'scatterChart', id = 'chart', color = 'var'
)
p2$xAxis(axisLabel = 'Кластер')
p2$yAxis(axisLabel = 'Рентабельность активов')
p2$chart(color = c('blue', 'red') )
p2

p3 <- nPlot(Доходность.активов ~ t, group =  'var', data = plot_frame, 
            type = 'scatterChart', id = 'chart', color = 'var'
)
p3$xAxis(axisLabel = 'Кластер')
p3$yAxis(axisLabel = 'Доходность активов')
p3$chart(color = c('blue', 'red') )
p3

p4 <- nPlot(Автономность ~ t, group =  'var', data = plot_frame, 
            type = 'scatterChart', id = 'chart', color = 'var'
)
p4$xAxis(axisLabel = 'Кластер')
p4$yAxis(axisLabel = 'Автономность')
p4$chart(color = c('blue', 'red') )
p4


p5 <- nPlot(Оборачиваемость.активов ~ t, group =  'var', data = plot_frame, 
            type = 'scatterChart', id = 'chart', color = 'var'
)
p5$xAxis(axisLabel = 'Кластер')
p5$yAxis(axisLabel = 'ДОборачиваемость активов')
p5$chart(color = c('blue', 'red') )
p5



gplot <- nPlot(values$Ликвидность.активов ~ Банкрот, group = 'cluster', data = training_data, type = 'scatterChart')
gplot
dat <- data.frame(
  t = rep(0:23, each = 4), 
  var = rep(LETTERS[1:4], 4), 
  val = round(runif(4*24,0,50))
)
p8 <- nPlot(val ~ t, group =  'var', data = dat, 
            type = 'stackedAreaChart', id = 'chart'
)
p8
#Наивный классификатор
classifier<-naiveBayes(as.factor(Банкрот) ~ ., data = bankruptcy)
predicted = predict(classifier, bankruptcy[,-c(6)]);
predicted<-as.numeric(as.character(predicted))
bankruptcy$naiveBayes <- predicted

#Точность модели
i<-0;
for (j in 1:nrow(bankruptcy)){
  if (bankruptcy$naiveBayes[j] == bankruptcy$Банкрот[j]) {
    i<-i+1;
    }
}
i<-i/nrow(bankruptcy)*100

hist(bankruptcy$naiveBayes, col='blue', ylim=c(0,200), xlab = "Банкротство", ylab = 'Количество', main='Наивный Байесовский классификатор')
hist(bankruptcy$Банкрот, col=rgb(1,0.5,0.1,alpha=0.8), add=T)
legend("topright", c("предсказанное", "реальное"), col=c("blue", "orange"), lwd=10)
