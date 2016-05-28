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
sampind1 <- ind1[sample(1:nrow(ind1), 0.8*nrow(ind1), replace=FALSE),]
sampind0 <- ind0[sample(1:nrow(ind0), 0.8*nrow(ind0), replace=FALSE),]

training_data <- rbind(sampind0, sampind1)
testing_data <- bankruptcy[!(bankruptcy$ID %in% training_data$ID),]
training_data <- bankruptcy[!(bankruptcy$ID %in% testing_data$ID),]
rownames(training_data)<-NULL
rownames(testing_data)<-NULL
rm(ind0, ind1, sampind0, sampind1, i)
clear_test <- subset(testing_data, select=Ликвидность.активов:Банкрот)


#Функция построения матрицы Пирсона с уровнями значимости:
corstarsl <- function(x)
{ 
  #Исходная матрица
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  #задание уровней значимости. 
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  #Округление значений матрицы до 2 знаков после запятой
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  #Построение матрицы со значениями корреляции и уровнями значимости
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  #Удаление верхнего треугольника матрицы
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  #Удаление последней колонки ( так как она пуста ) и возврат готовой матрицы
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

#Выборка из 7 переменных для анализа взаимосвязей показателей с помощью матрицы Пирсона
x <- subset(training_data, select = c("Ликвидность.активов", "Рентабельность.активов", "Доходность.активов",  "Автономность", "Оборачиваемость.активов",	"Банкрот")); 

#Вызов функции построения матрицы
corstarsl(data.matrix(x))




#строим логистическую регрессию, оказалось, что Автономность мало влияет на Банкротство
glm.out <- step(glm(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, family=binomial, data=training_data))
summary(glm.out)
exp(confint(glm.out))

#Получим прогноз
testing_data$predicted_value_log <-  predict(glm.out, newdata = clear_test, type = "response")
preds <- prediction(as.numeric(testing_data$predicted_value_log), as.numeric(testing_data$Банкрот)) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc") 
auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_reg <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_reg,sep="")

#Округлим полученные значения
convert <- function(data){if(data >= 0.4)return (1) else return (0)}
testing_data$predicted_value_log <- lapply(testing_data$predicted_value_log, convert)

plot(perf, main="ROC-кривая регрессии для тестовых данных", lwd=2, col="pink")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.5,0.4,c(maxauct,"\n"),border="white",cex=1.1,box.col = "white")



#Строим регрессионное дерево
reg_tree <- rpart(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, data = training_data, method = "anova")
plot(reg_tree, uniform=TRUE, main="Дерево регрессии")
text(reg_tree, use.n=TRUE, all=TRUE, cex=.8)
#Получим прогноз
testing_data$predicted_value_regtree <- predict(reg_tree,  clear_test, type = c("vector", "prob", "class", "matrix"), na.action = na.pass)
preds <- prediction(as.numeric(testing_data$predicted_value_regtree), as.numeric(testing_data$Банкрот)) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc")

auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_regtree <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_regtree,sep="")

#Округлим полученные значения
correct <- function(data){if(data >= 0.1)return (1) else return (0)}
testing_data$predicted_value_regtree <- lapply(testing_data$predicted_value_regtree, correct)





#Метод random forests
fit <- randomForest(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, data=training_data)
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




#Алгоритм C.5.0
training_data$Банкрот<-as.factor(training_data$Банкрот)
testing_data$Банкрот<-as.factor(testing_data$Банкрот)
clear_test$Банкрот<-as.factor(clear_test$Банкрот)

reg_tree_c50 <- C5.0(x = clear_test, y = clear_test$Банкрот)
summary(reg_tree_c50)

#Получим прогноз
testing_data$predicted_value_regtreeс50 <-  predict(reg_tree_c50, newdata = clear_test)
preds <- prediction(as.numeric(testing_data$predicted_value_regtreeс50), as.numeric(testing_data$Банкрот)) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc")

auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_c50 <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_c50,sep="")

#Кривая ROC
plot(perf, main="ROC-кривая однослойного персептрона для тестовых данных ", lwd=2, col="pink")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.5,0.4,c(maxauct,"\n"),border="white",cex=1.1,box.col = "white")


#Нейронная сеть
clear_test$Банкрот <- as.integer(clear_test$Банкрот)
training_data$Банкрот <- as.integer(training_data$Банкрот)
clear_test <- as.data.frame(clear_test)

nn <- neuralnet(Банкрот ~ Ликвидность.активов  + Рентабельность.активов  + Доходность.активов	+ Автономность +	Оборачиваемость.активов, data = training_data, hidden = 6, stepmax = 2e05, lifesign = "minimal",linear.output=F) 
clear_test <- subset(clear_test, select = c("Ликвидность.активов", "Рентабельность.активов", "Доходность.активов", "Автономность", "Оборачиваемость.активов"))

bankruptcynet.results <- compute(nn,  clear_test)


#Получим прогноз
testing_data$prediction_nn <-  bankruptcynet.results$net.result
nn_res <- list(predictions = testing_data$prediction_nn, labels = testing_data$Банкрот)
preds <- prediction(nn_res$predictions, nn_res$labels) 
perf <- performance(preds, "tpr", "fpr") 
perf2 <- performance(preds, "auc")

auc <- unlist(slot(performance(preds, "auc"), "y.values"))
maxauc<-max(round(auc, digits = 2))
maxauc_nn <- maxauc
maxauct <- paste(c("max(AUC) = "),maxauc_nn,sep="")

#ROC кривая
plot(perf, main="ROC-кривая нейронной сети для тестовых данных ", lwd=2, col="pink")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
legend(0.5,0.4,c(maxauct,"\n"),border="white",cex=1.1,box.col = "white")

rmarkdown::render("reg_tree_neural.Rmd")
 

