Sys.setenv(LANG = "en")
install.packages("ISLR")
library("ISLR")
install.packages("tree")
library("tree")




bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE, fileEncoding = "UTF8", header=TRUE, sep=";")
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))
for (i in 2:6) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))

Банкротство <- ifelse(bankruptcy$Банкрот == 1, "Да", "Нет" )
bankruptcy <- data.frame(bankruptcy, Банкротство)
bankruptcy$Банкрот <- NULL

range(bankruptcy$Ликвидность.активов)
Ликвидность <- ifelse(bankruptcy$Ликвидность.активов >=0.5, "Высокая", "Низкая " )
bankruptcy <- data.frame(bankruptcy,Ликвидность)
bankruptcy$Ликвидность.активов <- NULL

range(bankruptcy$Рентабельность.активов)
test_func <- function(rent) { if (rent <= -3.5) "Низкая" else if (rent <= -2) "Средняя" else if (rent >= -2) "Высокая"}
lapply(bankruptcy$Рентабельность.активов, function(bankruptcy$Рентабельность.активов))

bankruptcy <- data.frame(bankruptcy,Рентабельность)
bankruptcy$Рентабельность.активов <- NULL

range(bankruptcy$Доходность.активов)
range(bankruptcy$Автономность)
range(bankruptcy$Оборачиваемость.активов)


set.seed(2)
train = sample(1:nrow(bankruptcy), nrow(bankruptcy)/3)
test = - train
bankruptcy_training_data = bankruptcy[train,]
bankruptcy_testing_data = bankruptcy[test,]
bankruptcy_testing_Bankruptcy = Bankruptcy[test]

#training

tree_model_bankruptcy = tree(Bankruptcy~.,bankruptcy_training_data)
plot(tree_model_bankruptcy)
text(tree_model_bankruptcy, pretty=0)

tree_pred_bankruptcy = predict(tree_model_bankruptcy, testing_data_bankruptcy, type="class")
mean(tree_pred_bankruptcy != bankruptcy_testing_Bankruptcy)

set.seed(3)
cv_tree <- cv.tree(tree_model_bankruptcy, FUN = prune.misclass)
plot(cv_tree$size,cv_tree$dev, type = "b")
pruned_model_bankruptcy <- prune.misclass(tree_model_bankruptcy, best = 4) #взяли с графика 9
plot(pruned_model_bankruptcy)
text(pruned_model_bankruptcy, pretty=0)
tree_pred_bankruptcy = predict(pruned_model_bankruptcy, testing_data_bankruptcy, type="class")
mean(tree_pred_bankruptcy != bankruptcy_testing_Bankruptcy)


