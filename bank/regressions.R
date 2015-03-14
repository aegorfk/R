Sys.setenv(LANG = "en")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("MASS")
library("MASS")
#install.packages("epicalc", dependencies=TRUE)
library("epicalc")

#считываем данные из файла
bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE, fileEncoding = "UTF8", header=TRUE, sep=";")
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))
for (i in 2:6) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))

#сбалансированно бьем выборку на тестовую и проверочную
ind1 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==1, select=ID : Банкрот)
ind0 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==0, select=ID : Банкрот)
sampind1 <- ind1[sample(1:nrow(ind1), 53, replace=FALSE),]
sampind0 <- ind0[sample(1:nrow(ind0), 158, replace=FALSE),]

training_data <- rbind(sampind0, sampind1)
testing_data <- bankruptcy[!(bankruptcy$ID %in% training_data$ID),]
rownames(training_data)<-NULL
rownames(testing_data)<-NULL
rm(ind0, ind1, sampind0, sampind1, i)



summary(bankruptcy$Ликвидность.активов)
boxplot(Ликвидность.активов ~ Банкрот , data = bankruptcy, xlab = "Ликвидность активов", ylab = "Банкрот", main = "Зависимость банкротства от ликвидности активов")
boxplot(Рентабельность.активов ~ Банкрот , data = bankruptcy, xlab = "Рентабельность активов", ylab = "Банкрот", main = "Зависимость банкротства от рентабельности активов")
range(bankruptcy$Рентабельность.активов)
boxplot(Доходность.активов ~ Банкрот , data = bankruptcy, xlab = "Доходность активов", ylab = "Банкрот", main = "Зависимость банкротства от доходности активов")
boxplot(Автономность ~ Банкрот , data = bankruptcy, xlab = "Автономность", ylab = "Банкрот", main = "Зависимость банкротства от автономности активов")
boxplot(Оборачиваемость.активов ~ Банкрот , data = bankruptcy, xlab = "Оборачиваемость.активов", ylab = "Банкрот", main = "Зависимость банкротства от оборачиваемости активов")

#строим логистическую регрессию
glm.out = glm(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Автономность + Оборачиваемость.активов, family = "binomial", data=training_data)
summary(glm.out)

logistic.display(glm(training_data$Банкрот ~ training_data$Ликвидность.активов + training_data$Рентабельность.активов + training_data$Доходность.активов + training_data$Автономность + training_data$Оборачиваемость.активов, family=binomial), decimal=1)
testing_data$predicted_value <- predict(glm.out, newdata = testing_data, type = "response")

