# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK


Sys.setenv(LANG = "en")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("MASS")
library("MASS")
#install.packages("epicalc", dependencies=TRUE)
library("epicalc")
#install.packages("outliers", dependencies = TRUE)
library("outliers")



#считываем данные из файла
bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE, header=TRUE, sep=";")
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))
for (i in 2:6) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))

#Посмотрим на наши данные
summary(bankruptcy)
boxplot(Ликвидность.активов ~ Банкрот , data = bankruptcy, xlab = "Ликвидность активов", ylab = "Банкрот", main = "Зависимость банкротства от ликвидности активов")
boxplot(Рентабельность.активов ~ Банкрот , data = bankruptcy, xlab = "Рентабельность активов", ylab = "Банкрот", main = "Зависимость банкротства от рентабельности активов")
boxplot(Доходность.активов ~ Банкрот , data = bankruptcy, xlab = "Доходность активов", ylab = "Банкрот", main = "Зависимость банкротства от доходности активов")
boxplot(Автономность ~ Банкрот , data = bankruptcy, xlab = "Автономность", ylab = "Банкрот", main = "Зависимость банкротства от автономности активов")
boxplot(Оборачиваемость.активов ~ Банкрот , data = bankruptcy, xlab = "Оборачиваемость.активов", ylab = "Банкрот", main = "Зависимость банкротства от оборачиваемости активов")

bankruptcy <- bankruptcy[ which(bankruptcy$Автономность < 30 ), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Рентабельность.активов > -6), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Доходность.активов > -6), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Оборачиваемость.активов < 10), ]
bankruptcy <- bankruptcy[ which(bankruptcy$Доходность.активов > -6), ]

#Смотрим итоговую выборку
summary(bankruptcy)
boxplot(Рентабельность.активов ~ Банкрот , data = bankruptcy, xlab = "Рентабельность активов", ylab = "Банкрот", main = "Зависимость банкротства от рентабельности активов")
boxplot(Доходность.активов ~ Банкрот , data = bankruptcy, xlab = "Доходность активов", ylab = "Банкрот", main = "Зависимость банкротства от доходности активов")
boxplot(Автономность ~ Банкрот , data = bankruptcy, xlab = "Автономность", ylab = "Банкрот", main = "Зависимость банкротства от автономности активов")
boxplot(Оборачиваемость.активов ~ Банкрот , data = bankruptcy, xlab = "Оборачиваемость.активов", ylab = "Банкрот", main = "Зависимость банкротства от оборачиваемости активов")



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


#строим логистическую регрессию
glm.out = step(glm(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Автономность + Оборачиваемость.активов, family=binomial, data=training_data))
summary(glm.out)

anova(glm.out, test="Chisq")
testing_data$predicted_value <- predict(glm.out, newdata = testing_data, type = "response")


