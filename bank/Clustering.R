Sys.setenv(LANG = "en")

bankruptcy <- read.csv(file="Предприятия-А.csv",stringsAsFactors = FALSE,  header=TRUE, sep=";")
bankruptcy <- as.data.frame(sapply(bankruptcy, gsub, pattern=",",replacement="."))

for (i in 2:6) bankruptcy[,i]  <- as.numeric(as.character(bankruptcy[,i]))
for (i in 2:6) bankruptcy[,i] <- 1/(1+(exp(-1*bankruptcy[,i])))
pairs(~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Автономность + Оборачиваемость.активов,data=bankruptcy,  main="Scatterplot Matrix")

bankruptcy$Банкрот <- NULL
bankruptcy_15 <- bankruptcy[sample(1:nrow(bankruptcy), 38, replace=FALSE),]
bankruptcy_85<-bankruptcy[!(bankruptcy$ID %in% bankruptcy_15$ID),]

rownames(bankruptcy_15)<-NULL 
rownames(bankruptcy_85)<-NULL
bankruptcy_15[,1] <- NULL
bankruptcy_85[,1] <- NULL
bank <- (nrow(bankruptcy_85) -1)*sum(apply(bankruptcy_85, 2,var))
for (i in 2:20) bank[i]  <- sum(kmeans(bankruptcy_85, center = i)$withinss)
plot(1:20, bank, type="b")

bank_kmeans <- kmeans(bankruptcy_85, centers=4, iter.max=10, nstart = 210)
bankruptcy_85$cluster <- factor(bank_kmeans$cluster)
#install.packages("devtools")
library("devtools")
#install_github("rCharts","ramnathv")
library("rCharts")

gplot <- nPlot(Ликвидность.активов ~  Рентабельность.активов, group = 'cluster', data = bankruptcy_85, type = 'scatterChart', tooltip = "function(item) {return 'hi'}")
gplot
