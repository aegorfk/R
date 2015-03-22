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
install.packages("pander", dependencies = TRUE)
library("pander")


#считываем данные из файла
bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE, header=TRUE, sep=";", dec= ".")
for (i in 2:7) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))



#Посмотрим на наши данные
summary(bankruptcy)
pairs(Банкрот ~ Ликвидность.активов + Рентабельность.активов	+ Доходность.активов	+ Автономность +	Оборачиваемость.активов,data = bankruptcy, main = "Диаграммы рассеивания для всех переменных")
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
ind1 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==1, select=ID: Банкрот)
ind0 <- subset(bankruptcy, bankruptcy[,"Банкрот"]==0, select=ID: Банкрот)
sampind1 <- ind1[sample(1:nrow(ind1), 53, replace=FALSE),]
sampind0 <- ind0[sample(1:nrow(ind0), 158, replace=FALSE),]

training_data <- rbind(sampind0, sampind1)
testing_data <- bankruptcy[!(bankruptcy$ID %in% training_data$ID),]
rownames(training_data)<-NULL
rownames(testing_data)<-NULL
rm(ind0, ind1, sampind0, sampind1, i)
clear_test <- subset(testing_data, select=Ликвидность.активов:Банкрот)

#строим логистическую регрессию, оказалось, что Автономность мало влияет на Банкротство
glm.out <- step(glm(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, family=binomial, data=training_data))
summary(glm.out)
confint(glm.out)
exp(glm.out$coefficients)
exp(confint(glm.out))

#Округлим полученные значения
testing_data$predicted_value_log <-  predict(glm.out, newdata = clear_test, type = "response")
convert <- function(data){if(data >= 0.5)return (1) else return (0)}
testing_data$predicted_value_log <- lapply(testing_data$predicted_value_log, convert)

#Строим регрессионное дерево
reg_tree <- rpart(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, data = training_data, method = "anova")
printcp(reg_tree)
plotcp(reg_tree) # покажем график кросс-валидации
summary(reg_tree) 

rsq.rpart(reg_tree) # visualize cross-validation results    
plot(reg_tree, uniform=TRUE, main="Дерево регрессии")
text(reg_tree, use.n=TRUE, all=TRUE, cex=.8)

#Тестим дерево
testing_data$predicted_value_regtree <- predict(reg_tree,  clear_test, type = c("vector", "prob", "class", "matrix"), na.action = na.pass)
correct <- function(data){if(data >= 0.5)return (1) else return (0)}
testing_data$predicted_value_regtree <- lapply(testing_data$predicted_value_regtree, correct)


#Метод random forests
fit <- randomForest(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, data=training_data)
print(fit) # view results 
importance(fit) # importance of each predictor

#Тестим дерево
testing_data$predicted_value_random <-predict(fit, clear_test, type="response" )
testing_data$predicted_value_random <- lapply(testing_data$predicted_value_random, correct)
plot(fit)


#Алгоритм C.5.0
reg_tree_c50 <- C5.0(x = clear_test, y = clear_test$Банкрот)
plot(reg_tree_c50)
testing_data$predicted_value_regtreeс50 <- predict(reg_tree_c50,  clear_test)
summary(reg_tree_c50)


#Нейронная сеть
clear_test$Банкрот <- as.integer(clear_test$Банкрот)
training_data$Банкрот <- as.integer(training_data$Банкрот)
clear_test <- as.data.frame(clear_test)
nn <- neuralnet(Банкрот ~ Ликвидность.активов  + Рентабельность.активов	+ Доходность.активов	+ Автономность +	Оборачиваемость.активов, data = training_data, hidden = 6, stepmax = 2e05, lifesign = "minimal",linear.output=F) 
plot(nn, rep = "best")
print(nn)
clear_test <- subset(clear_test, select = c("Ликвидность.активов", "Рентабельность.активов", "Доходность.активов", "Автономность", "Оборачиваемость.активов"))
bankruptcynet.results <- compute(nn,  clear_test)
testing_data$prediction_nn <- round(bankruptcynet.results$net.result)


rmarkdown::render("reg_tree_neural.Rmd")
