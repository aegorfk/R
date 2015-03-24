Sys.setenv(LANG = "en")
#install.packages("devtools")
library("devtools")
#install_github("rCharts","ramnathv")
library("rCharts")
#install.packages("rgl")
library("rgl")


bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE,  header=TRUE, sep=";")
bankruptcy2 <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE,  header=TRUE, sep=";")
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))

for (i in 2:6) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))
for (i in 2:6) bankruptcy[,i] <- 1/(1+(exp(-1*bankruptcy[,i])))
pairs(~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Автономность + Оборачиваемость.активов,data=bankruptcy,  main="Scatterplot Matrix")
plot3d(x = bankruptcy$Ликвидность.активов, y = bankruptcy$Рентабельность.активов, z = bankruptcy$Доходность.активов)

bankruptcy$Банкрот <- NULL
bankruptcy2$ID  <- NULL
bankruptcy2$Ликвидность.активов  <- NULL
bankruptcy2$Рентабельность.активов  <- NULL
bankruptcy2$Доходность.активов  <- NULL
bankruptcy2$Автономность  <- NULL
bankruptcy2$Оборачиваемость.активов  <- NULL


bank <- (nrow(bankruptcy) -1)*sum(apply(bankruptcy, 2,var))
for (i in 2:20) bank[i]  <- sum(kmeans(bankruptcy, center = i)$withinss)
plot(2:20, bank, type="b")

bank_kmeans <- kmeans(bankruptcy, centers=2, iter.max=10, nstart = 210)
bankruptcy$cluster <- factor(bank_kmeans$cluster)


gplot <- nPlot(bankruptcy$cluster ~  bankruptcy2$Банкрот, group = 'cluster', data = bankruptcy_85, type = 'scatterChart', tooltip = "function(item) {return 'hi'}")
gplot

#Наивный классификатор
install.packages('e1071', dependencies = TRUE)
library(class)
library(e1071)
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

hist(bankruptcy$naiveBayes, col='blue', ylim=c(0,200))
hist(bankruptcy$Банкрот, col=rgb(1,0.5,0.1,alpha=0.8), add=T)

