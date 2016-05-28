#install.packages("recommenderlab")
library(recommenderlab)
library(reshape2)
library(ggplot2)

#Загружаем данные в формат, удобный для пакета
data <- read.csv(file="Data.csv")
data$X <- NULL
data[,1]<-NULL
for (i in 1:ncol(data)) names(data)[i] <- i
#у нас 1257 строк, 285 колонок
data<-as.matrix(data)
m <- matrix(sample(c(0,1), 358245, replace=TRUE), nrow=nrow(data), ncol=ncol(data), dimnames=list(users=paste("u", 1:nrow(data), sep=''),  items=paste("i", 1:ncol(data), sep='')))

matrix <- as(m, "binaryRatingMatrix")
matrix

scheme <- evaluationScheme(matrix[1:1257,], method="split", train=.9, k=1, given=110)
scheme <- evaluationScheme(matrix[1:1257], method="cross", k=4, given=110, goodRating=1)

#algorithms <- list( "random items" = list(name="RANDOM", param=NULL), "popular items" = list(name="POPULAR", param=NULL),"user-based CF" = list(name="UBCF", param=list(method="Cosine",nn=5)))
algorithms <- list("random items" = list(name = "RANDOM", param = NULL), "popular items" = list(name = "POPULAR", param = NULL), "user-based CF" = list(name = "UBCF", param = list(method = "Jaccard", nn = 5)), "item-based CF" = list(name = "IBCF", param = list(method = "Jaccard", k = 50)), "association rules" = list(name = "AR",param = list(supp = 0.01, conf = 0.2, maxlen = 2)))
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

results
getConfusionMatrix(results)[[1]]
plot(results, annotate=TRUE)
plot(results, "prec/rec", annotate=TRUE)


rmarkdown::render("Report.Rmd")

