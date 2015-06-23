# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK


Sys.setenv(LANG = "en")
#install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
#install.packages("MASS", dependencies=TRUE)
library("MASS")
#install.packages("epicalc", dependencies=TRUE)
library("epicalc")
#install.packages("outliers", dependencies = TRUE)
library("outliers")
#install.packages("rpart", dependencies = TRUE)
library("rpart")
#install.packages("randomForest", dependencies = TRUE)
library("randomForest")
#install.packages("C50", dependencies = TRUE)
library("C50")
#install.packages("neuralnet", dependencies = TRUE)
library("neuralnet")
#install.packages("knitr", dependencies = TRUE)
library("knitr")
#install.packages("ROCR", dependencies = TRUE)
library("ROCR")
#install.packages("Hmisc", dependencies = TRUE)
library("Hmisc")
#install.packages("png", dependencies=TRUE)
library("png")
#install.packages("grid", dependencies=TRUE)
library("grid")
#install.packages("data.table", dependencies=TRUE)
library("data.table")




avto <- read.csv(file="avto.csv",stringsAsFactors = FALSE,sep=",", header=TRUE, dec= ".")
avto[,4] <- as.numeric(avto[,4])
avto[,2] <- as.character(avto[,2])
avto <- na.omit(avto)
library(plyr)
names(avto)[1] <- "risk"
names(avto)[2] <- "avtomark"
names(avto)[3] <- "compensation"
names(avto)[4] <- "score"
names(avto)[5] <- "number"
avto$avto <- as.character(lapply(strsplit(as.character(avto$avtomark), split=" "), "[", 1))
avto$mark <- as.character(lapply(strsplit(as.character(avto$avtomark), split=" "), "[", 2))
avto$compensation <- lapply(as.numeric(avto$compensation), function(x) ifelse(x == 1, 0, 1))
avto$region <- lapply(avto$number, function(x) substr(x, 7, 16)) 
data.set <- data.frame(avto)



avto$Возмещено <- ifelse(avto$Возмещено == 1, 0, 1)
avto$Штрафы2 <- lapply(avto$Штрафы, function(x) (x-min(avto$Штрафы))/(max(avto$Штрафы)-min(avto$Штрафы)))
avto$Штрафы2 <- as.numeric(avto$Штрафы2)

summary(avto)
hist(avto$Штрафы,breaks=20)
hist(avto$Возмещено,breaks=20)


avto$Регион <- lapply(avto$НомерАвто, function(x) substr(x, 7, 16))
avto$Регион <-  as.character(avto$Регион)
avto$Авто = as.character(lapply(strsplit(as.character(avto$МаркаАвто), split=" "), "[", 1))
avto$Mарка = as.character(lapply(strsplit(as.character(avto$МаркаАвто), split=" "), "[", 2))

sub <- sample(nrow(avto), floor(nrow(avto) * 0.9))
training_data <- avto[sub, ]
testing_data <- avto[-sub, ]
training_data <- na.omit(training_data)
testing_data <- na.omit(testing_data)

#строим логистическую регрессию, оказалось, что Автономность мало влияет на Банкротство
glm.out <- step(glm(Возмещено ~ Авто + Марка + Штрафы2 + Регион, family=binomial, data=training_data))
summary(glm.out)
exp(confint(glm.out))



#Строим регрессионное дерево
reg_tree <- rpart(Возмещено ~ Авто + Mарка + Штрафы2 + Регион, data = training_data, method = "anova")
plot(reg_tree, uniform=TRUE, main="Дерево регрессии")
text(reg_tree, use.n=TRUE, all=TRUE, cex=.8)
#Получим прогноз
testing_data$predicted_value_regtree <- predict(reg_tree,  testing_data, type = c("vector", "prob", "class", "matrix"), na.action = na.pass)
preds <- prediction(as.numeric(testing_data$predicted_value_regtree), as.numeric(testing_data$Возмещено)) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc")

auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_regtree <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_regtree,sep="")

#Округлим полученные значения
correct <- function(data){if(data >= 0.1)return (1) else return (0)}
testing_data$predicted_value_regtree <- lapply(testing_data$predicted_value_regtree, correct)

#ROC-анализ
plot(perf, main="ROC-кривая random forests для тестовых данных ", lwd=2, col="pink")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.5,0.4,c(maxauct,"\n"),border="white",cex=1.1,box.col = "white")







levels(avto$Возмещено) <- levels(avto$Возмещено)
#Метод random forests
fit <- randomForest(Возмещено ~ Авто + Mарка + Штрафы2 + Регион, data=training_data)
print(fit) # view results 
importance(fit) # importance of each predictor
plot(fit)

#Получим прогноз
testing_data$predicted_value_random <-  predict(fit, newdata = clear_test, type = "response")
preds <- prediction(as.numeric(testing_data$predicted_value_random), as.numeric(testing_data$Банкрот)) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc")

auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_random <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_random,sep="")

#Округлим полученные значения
testing_data$predicted_value_random <- lapply(testing_data$predicted_value_random, correct)

#ROC-анализ
plot(perf, main="ROC-кривая random forests для тестовых данных ", lwd=2, col="pink")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.5,0.4,c(maxauct,"\n"),border="white",cex=1.1,box.col = "white")



