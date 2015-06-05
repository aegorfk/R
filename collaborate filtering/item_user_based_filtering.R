data <- read.csv(file="Data.csv")
data.germany$X <- NULL


#Item Based Collaborative Filtering
data.ibs <- (data[,!(names(data) %in% c("user"))])

getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

data.ibs.similarity  <- matrix(NA, nrow=ncol(data.ibs), ncol=ncol(data.ibs),
                           dimnames=list(colnames(data.ibs), colnames(data.ibs)))


for(i in 1:ncol(data.ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(data.ibs)) {
    # Fill in placeholder with cosine similarities
    data.ibs.similarity[i,j] <- getCosine(as.matrix(data.ibs[i]),as.matrix(data.ibs[j]))
  }
}

# Back to dataframe
data.ibs.similarity <- as.data.frame(data.ibs.similarity)
data.neighbours <- matrix(NA, nrow=ncol(data.ibs.similarity),ncol=11,dimnames=list(colnames(data.ibs.similarity)))


for(i in 1:ncol(data.ibs)) 
{
  data.neighbours[i,] <- (t(head(n=11,rownames(data.ibs.similarity[order(data.ibs.similarity[,i],decreasing=TRUE),][i]))))
}


#User Based Collaborative Filtering
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

holder <- matrix(NA, nrow=nrow(data),ncol=ncol(data)-1,dimnames=list((data$user),colnames(data[-1])))




# проходимся по пользователям (по строкам)
for(i in 1:nrow(holder)) 
{
  # проходимся по колонкам-объектам
  for(j in 1:ncol(holder)) 
  {
    # считываем значение строки и стобца, которые показывают конкретного пользователя и объект
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # Чтобы не рекомендовать продукт, который уже попробовали, заменяем его пустой строкой
 
    if(as.integer(data[data$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # Берем 10 соседей
      topN<-((head(n=11,(data.ibs.similarity[order(data.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # смотрим историю пользователя
      topN.purchases<- data[,c("user",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      # вычисляем рейтинг пользователя и объекта
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  }
}

data.user.scores <- holder


data.germany.user.scores.holder <- matrix(NA, nrow=nrow(data.germany.user.scores),ncol=100,dimnames=list(rownames(data.germany.user.scores)))
for(i in 1:nrow(data.germany.user.scores)) 
{
  data.germany.user.scores.holder[i,] <- names(head(n=100,(data.germany.user.scores[,order(data.germany.user.scores[i,],decreasing=TRUE)])[i,]))
}
