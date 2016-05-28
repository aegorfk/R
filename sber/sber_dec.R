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

data <- read.csv(file="Xtrain.csv",stringsAsFactors = TRUE, header=TRUE, sep=",", dec= ".")
data2 <- read.csv(file="Ytrain.csv",stringsAsFactors = TRUE, header=TRUE, sep=",", dec= ".")
data <- cbind(data, data2)
rm(data2)
control <- read.csv(file="Xtest.csv",stringsAsFactors = TRUE, header=TRUE, sep=",", dec= ".")

 data$product_id <- as.factor(data$product_id)
data$ts_create <- as.factor(data$ts_create)
data$ts_update <- as.factor(data$ts_update)
data$bank_id <- as.factor(data$bank_id)
data$status <- as.factor(data$status)
data$address_id <- as.factor(data$address_id)
data$company_id <- as.factor(data$company_id)
data$agent_id <- as.factor(data$agent_id)
 data$email_agent_id <- as.factor(data$email_agent_id)
data$phone_agent_id <- as.factor(data$phone_agent_id)
data$direction_id <- as.factor(data$direction_id)
 data$partner_company_id <- as.factor(data$partner_company_id)
data$online_id <- as.factor(data$online_id)
data$channel_id <- as.factor(data$channel_id)
 data$source_id <- as.factor(data$source_id)
data$system_id <- as.factor(data$system_id)
data$target <- as.factor(data$target)

fit <- princomp(data, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

#сбалансированно бьем выборку на тестовую и проверочную
data$ID <- rownames(data)
ind1 <- subset(data, data[,"target"]==1, select=ID: target)
ind0 <- subset(data, data[,"target"]==0, select=ID: target)
sampind1 <- ind1[sample(1:nrow(ind1), 0.8*nrow(ind1), replace=FALSE),]
sampind0 <- ind0[sample(1:nrow(ind0), 0.8*nrow(ind0), replace=FALSE),]                        

training_data <- rbind(sampind0, sampind1)
testing_data <- data[!(data$ID %in% training_data$ID),]
training_data <- data[!(data$ID %in% testing_data$ID),]
rownames(training_data)<-NULL
rownames(testing_data)<-NULL
testing_data$ID <- NULL
training_data$ID <- NULL
rm(ind0, ind1, sampind0, sampind1, i)

                        
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

#Вызов функции построения матрицы
corstarsl(training_data)


#строим логистическую регрессию, оказалось, что Автономность мало влияет на Банкротство
glm.out <- glm(target ~ product_id + bank_id + status + address_id + company_id + agent_id + email_agent_id + 
                      phone_agent_id + direction_id + partner_company_id + online_id + channel_id + system_id, family=binomial, data=training_data)
summary(glm.out)
exp(confint(glm.out))