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