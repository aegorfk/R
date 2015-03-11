Sys.setenv(LANG = "en")
install.packages("ISLR")
library("ISLR")
install.packages("tree")
library("tree")

attach(Carseats)
head(Carseats)
Hight <- ifelse(Carseats$Sales >=8, "Yes", "No" )
length(Hight)
Carseats <- data.frame(Carseats, Hight)
Carseats$Sales <- NULL

set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/3)
test = - train
training_data = Carseats[train,]
testing_data = Carseats[test,]
testing_Hight = Hight[test]

#training
tree_model = tree(Hight ~ ., training_data)
plot(tree_model)
text(tree_model, pretty=0)
tree_pred = predict(tree_model, testing_data, type="class")
mean(tree_pred != testing_Hight)

set.seed(3)
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)
plot(cv_tree$size,cv_tree$dev, type = "b")
pruned_model <- prune.misclass(tree_model, best = 4) #взяли с графика 9
plot(pruned_model)
text(pruned_model, pretty=0)
tree_pred = predict(pruned_model, testing_data, type="class")
mean(tree_pred != testing_Hight)



