# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK


Sys.setenv(LANG = "en")
#install.packages("quantmod", dependencies=TRUE)
library("quantmod")
#install.packages("zoo", dependencies=TRUE)
library("zoo")
#install.packages("TTR", dependencies=TRUE)
library("TTR")
#install.packages("png", dependencies=TRUE)
library("png")
#install.packages("checkpoint", dependencies=TRUE)
#library("checkpoint")
#checkpoint ("2015-04-05", R.version = "3.1.2", checkpointLocation = "C:/Users/Екатерина/Documents/GitHub/R/trading")



#Считываем данные из файла, приведем их к виду, пригодному для анализа в R:
Quotes1 <- read.csv(file="HistoricalQuotes.csv",stringsAsFactors = FALSE, header=TRUE, sep=",", dec= ".")
for (i in 2:6) Quotes1[,i]  <- as.numeric(as.character(Quotes1[,i]))
#Инвертируем значения:
Quotes <- subset(Quotes1, select = c("date", "close", "volume",  "open", "high",  "low")); 

for (i in 1:nrow(Quotes1)) Quotes[i,]  <- Quotes1[nrow(Quotes1) - i + 1,]
Quotes[,1]  <- as.Date(Quotes[,1])
Quotes = Quotes[-1,]
rownames(Quotes)<-NULL
rm(Quotes1)


summary(Quotes)


#Линейные графики:
plot(Quotes[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
plot(Quotes[1:300,3], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Объем торгов", main="Акции компании ORACLE", ylim=c(0,203555500) )

#Переведем в удобоваримый вид для наших пакетов
ORCL <- xts(Quotes[, 2:6], order.by=as.POSIXct(Quotes[,1]))
ORCL <- ORCL[ ! duplicated( index(ORCL) ),  ]
#График цены в форме японских свечей:
chartSeries(ORCL,subset='2005-03-28::2005-10-26') 
#Столбиковый график:
barChart(ORCL,subset='2005-03-28::2005-10-26', theme='white.mono',bar.type='hlc')

#Скользящее среднее на основании цены закрытия, окно усреднения - 10 отсчетов:
chartSeries(ORCL, subset='2005-03-28::2005-10-26',TA="addSMA(10)")

#Скользящее среднее на основании цены закрытия, окно усреднения - 50 отсчетов:
chartSeries(ORCL, subset='2005-03-28::2005-10-26',TA="addSMA(50)")

#График сопоставления скользящих средних между собой:
for (i in 10:nrow(Quotes)) Quotes[i,7] <- mean(Quotes[(i-9):i,2])
plot(Quotes[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes[1:300,7], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[7] <- "SMAS"
for (i in 50:nrow(Quotes)) Quotes[i,8] <- mean(Quotes[(i-49):i,2])
lines(Quotes[1:300,8], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[8] <- "SMAL"
legend("topleft", c("SMA-S", "SMA-L"), col=c("blue", "orange"), lwd=4)


#График сопоставления экспоненциальных скользящих средних между собой:
Quotes[,9] <- EMA(Quotes[,2], n=10, ratio = 1-((10-1)/(10+1)))
plot(Quotes[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes[1:300,9], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[9] <- "EMAS"
Quotes[,10] <- EMA(Quotes[,2], n=50, ratio = 1 - ((50-1)/(50+1)))
lines(Quotes[1:300,10], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[10] <- "EMAL"
legend("topleft", c("EMA-S", "EMA-L"), col=c("blue", "orange"), lwd=4)



WMA <- function(L, data){
  wma_w <- function(L,n)
  {
    if((n >= 0)&(n < L)) (2/(L*(L+1)))*(L-n)
    else 0
  }
  x <- vector(mode="numeric", length=NROW(data))
  for (i in 1:NROW(data)){
    if (i<L) x[i] <- NA
    else {
      value = 0
      counter = 0
      for (j in i:(i-L+1)){
        value = value + data[j] * wma_w(L,counter)
        counter = counter + 1
      }
      x[i]<-value
    }
  }
  return(x)
}
Quotes[,11] <- WMA(10, Quotes[, 2])
Quotes[,12] <- WMA(50, Quotes[, 2])

#График сопоставления различных окон между собой:
plot(Quotes[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes[1:300,11], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[11] <- "WMAS"
lines(Quotes[1:300,12], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes)[12] <- "WMAL"
legend("topleft", c("WMA-S", "WMA-L"), col=c("blue", "orange"), lwd=4)




#Графики для короткого окна усреднения:
plot(Quotes[1:100,2], type="l", lwd=2, col="black", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes[1:100,7], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes[1:100,9], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes[1:100,11], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
legend("topleft", c("SMA-S", "EMA-S", "WMA-S"), col=c("red", "orange", "blue"), lwd=4)


#Графики для длинного окна усреднения:
plot(Quotes[1:100,2], type="l", lwd=2, col="black", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes[1:100,8], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes[1:100,10], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes[1:100,12], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
legend("topleft", c("SMA-L", "EMA-L", "WMA-L"), col=c("red", "orange", "blue"), lwd=4)





#Графики для EMA-S и EMA-L:
drawSquares <- function(data, start, plot=FALSE){
  vec <- vector()
  notFirst = FALSE;
  init = FALSE;
  ix = 50;
  miny = 0;
  maxy = 50;
  if (data[start,1] > data[start,2]){
    init = TRUE;
  }
  for (i in start:NROW(data)){
    if (init == TRUE){
      if (data[i,1]<data[i,2]){
        init=FALSE;
        vec <- c(vec,i)
        if (notFirst & plot)   rect(ix,miny,i,maxy, col=rgb(0.3,0.9,0.5,alpha=0.3));
        if (!notFirst) notFirst = TRUE;
        ix = i;
      }
    }else{
      if (data[i,1]>data[i,2]){
        init=TRUE;
        vec <- c(vec,i)
        if (notFirst & plot) rect(ix,miny,i,maxy, col=rgb(0.6,0.2,0.7,alpha=0.3));
        if (!notFirst) notFirst = TRUE;
        ix = i;
      }
    }
  }
  return(vec)
}






plot(Quotes[,2], xaxt='n',type="l", lwd=2, col= rgb(0,0,0,alpha = 0.4), xlab="", ylab="Цена закрытия", main="Акции компании ORACLE")
lines(Quotes[,9], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes[,10], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )

vec <- drawSquares(Quotes[,9:10], 50)
axis(side=1, at=vec,labels=Quotes[vec,1], las=2,cex.axis = 0.6,srt = 60)
legend("topleft", c("EMA-S", "EMA-L"), col=c("red", "orange"), lwd=2)





#imitation game
Quotes$budget <- Quotes[NROW(Quotes),2]*500
Quotes$papers <- 0
initx <- 50
intersections <- drawSquares(Quotes[,9:10], initx, plot = TRUE)
shallBuy <- FALSE
if (Quotes[initx,9] < Quotes[initx,10]) shallBuy <-TRUE
for (i in 1:NROW(intersections))
  {
  if (shallBuy){
    index <- intersections[i]
    price <- Quotes[index,2]
    budget <-  Quotes[index,13]
    papers <-  budget  %/% price
    budget <- budget %% price
    Quotes[(index:(NROW(Quotes))),14]<-papers
    Quotes[(index:(NROW(Quotes))),13]<-budget
    shallBuy <- FALSE
  }else{
    if (!shallBuy){
      index <- intersections[i]
      price <- Quotes[index,2]
      budget <-  Quotes[index,13] + price * Quotes[index,14]
      papers <-  0
      Quotes[(index:NROW(Quotes)),14]<-papers
      Quotes[(index:NROW(Quotes)),13]<-budget
      shallBuy <- TRUE
    }
  }
  
}
legend("topleft", c("EMA-S", "EMA-L"), col=c("red", "orange"), lwd=2)


#Попробуем добиться лучшего финансового результата, варьирую длительность окна усреднения:

# Quotes_S <- subset(Quotes, select = c("date", "close", "volume",  "open", "high",  "low"))
# Quotes_L <- subset(Quotes, select = c("date", "close", "volume",  "open", "high",  "low"))
# result <- data.frame(big = numeric(), small = numeric(), budget = numeric(), stock = numeric(), profit = numeric())
# 
# 
# for (small in 5:10) {
#   #SMA    
#   a <- ncol(Quotes_S)+1 #номер нашего испытания
#   for (i in small:nrow(Quotes_S)) Quotes_S[i,a] <- mean(Quotes_S[(i-(small - 1)):i,2])
#   colnames(Quotes_S)[ncol(Quotes_S)] <- paste("SMAS", small, sep=" ")
# 
#   #EMA
#   Quotes_S[,ncol(Quotes_S) + 1] <- EMA(Quotes_S[,2], n=small, ratio = 1-((small-1)/(small+1)))
#   colnames(Quotes_S)[ncol(Quotes_S)] <- paste("EMAS", small, sep=" ")
#   
#   #WMA
#   Quotes_S[,ncol(Quotes_S) + 1] <- WMA(small, Quotes_S[, 2])
#   colnames(Quotes_S)[ncol(Quotes_S)] <- paste("WMAS", small, sep=" ")
# 
#  }
# 
# 
# for (big in 20:50) {
#   #SMA    
#   a <- ncol(Quotes_L)+1 #номер нашего испытания
#   for (i in big:nrow(Quotes_L)) Quotes_L[i,a] <- mean(Quotes_L[(i-(big - 1)):i,2])
#   colnames(Quotes_L)[ncol(Quotes_L)] <- paste("SMAL", big, sep=" ")
#   
#   #EMA
#   Quotes_L[,ncol(Quotes_L) + 1] <- EMA(Quotes_L[,2], n=big, ratio = 1-((big-1)/(big+1)))
#   colnames(Quotes_L)[ncol(Quotes_L)] <- paste("EMAL", big, sep=" ")
#   
#   #WMA
#   Quotes_L[,ncol(Quotes_L) + 1] <- WMA(big, Quotes_L[, 2])
#   colnames(Quotes_L)[ncol(Quotes_L)] <- paste("WMAL", big, sep=" ")
#     
# }
# 
# 
# 
# 
# Quotes_strategy <- subset(Quotes, select = c("date", "close", "volume",  "open", "high",  "low"))
# #imitation game
# 
# for (big in 1:6) {
#   Quotes_L <- Quotes_L[,-1]
#   Quotes_S <- Quotes_S[,-1]
# }
# 
# 
# 
# #Игра в имитацию
# for (small in 1:(ncol(Quotes_S))) {
#   for (big in 1:(ncol(Quotes_L))) {
#     Quotes_strategy$budget <- Quotes_strategy[NROW(Quotes_strategy),2]*500
#     Quotes_strategy$papers <- 0
#     for (i in 1:50) if(is.na(Quotes_L[i,big])) initx <- i +1
#   
#     Quotes_strategy["Small"] <- Quotes_S[,small] 
#     Quotes_strategy["Big"] <- Quotes_L[,big] 
#   
#     intersections <- drawSquares(Quotes_strategy[,9:10], initx, plot=FALSE)
#     shallBuy <- FALSE
#     if (Quotes_strategy[initx,9] < Quotes_strategy[initx,10]) shallBuy <-TRUE
#     for (i in 1:NROW(intersections))
#     {
#       if (shallBuy){
#         index <- intersections[i]
#         price <- Quotes_strategy[index,2]
#         budget <-  Quotes_strategy[index,7]
#         papers <-  budget  %/% price
#         budget <- budget %% price
#         Quotes_strategy[(index:(NROW(Quotes_strategy))),8]<-papers
#         Quotes_strategy[(index:(NROW(Quotes_strategy))),7]<-budget
#         shallBuy <- FALSE
#       }else{
#         if (!shallBuy){
#           index <- intersections[i]
#           price <- Quotes_strategy[index,2]
#           budget <-  Quotes_strategy[index,7] + price * Quotes_strategy[index,8]
#           papers <-  0
#           Quotes_strategy[(index:NROW(Quotes_strategy)),8]<-papers
#           Quotes_strategy[(index:NROW(Quotes_strategy)),7]<-budget
#           shallBuy <- TRUE
#         }
#       }
#     }
#  
# 
#     result[nrow(result) + 1, 1] <- colnames(Quotes_L)[[big]]
#     result[nrow(result), 2] <- colnames(Quotes_S)[[small]]
#     result[nrow(result), 3] <- Quotes_strategy[nrow(Quotes_strategy),7]
#     result[nrow(result), 4] <- Quotes_strategy[nrow(Quotes_strategy),8]
#     result[nrow(result), 5] <- Quotes_strategy[nrow(Quotes_strategy), 7] + Quotes_strategy[nrow(Quotes_strategy),2]*Quotes_strategy[nrow(Quotes_strategy),8] - Quotes_strategy[1, 7]
#     Quotes_strategy<- Quotes_strategy[,-c(9,10)]
#   
#   }
# }

#result[which.max( result[,5] ),]
#sort <- result[order(result$profit),]
#for (i in 1:nrow(sort)) result[i,]  <- sort[nrow(sort) - i + 1,]
#rm(sort)
#rm(Quotes_ORCL_S,Quotes_ORCL_L, result)





# сделки с плечом и короткие сделки
Quotes_new <- subset(Quotes, select = c("date", "close",  "open", "high",  "low"))
for (i in 10:nrow(Quotes_new)) Quotes_new[i,6] <- mean(Quotes_new[(i-9):i,2])
colnames(Quotes_new)[6] <- "SMAS 10"
for (i in 29:nrow(Quotes)) Quotes_new[i,7] <- mean(Quotes_new[(i-28):i,2])
initx <- 29
colnames(Quotes_new)[7] <- "SMAL 29"
plot(Quotes_new[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE (лучшая стратегия)", ylim=c(10,16) )
lines(Quotes_new[1:300,6], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE (лучшая стратегия)" )
lines(Quotes_new[1:300,7], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE (лучшая стратегия)" )
intersections <- drawSquares(Quotes_new[,6:7], initx, plot = TRUE)
legend("topleft", c("SMA-S 10", "SMA-L 29"), col=c("blue", "orange"), lwd=4)



Quotes_new$budget <- Quotes_new[NROW(Quotes_new),2]*500
Quotes_new$papers <- 0
Quotes_new$budget_credit <- 0
Quotes_new$papers_credit <- 0



сredit <- 0
stocks <- 0 
shallBuy <- FALSE

if (Quotes_new[initx,6] < Quotes_new[initx,7]) shallBuy <-TRUE
for (i in 1:NROW(intersections) )
{
  index <- intersections[i]
  if (shallBuy){
    
    price <- Quotes_new[index,2]
    budget <-  Quotes_new[index,8]
    papers <- Quotes_new[index,9]
    budget_credit <- Quotes_new[index,10]
    papers_credit <- Quotes_new[index,11]
    
    
    papers <- ( 0.9992 * budget * 2 ) %/% price
    payment <- papers * price * 1.0008
    budget <- budget - payment/2
    budget_credit <- budget_credit + payment / 2
    papers <- papers - papers_credit
    papers_credit <- 0
    
    # вот тут мы покупаем что-то на наши средства
    Quotes_new[((index+1):(NROW(Quotes_new))),8]<-budget
    Quotes_new[((index+1):(NROW(Quotes_new))),9]<-papers
    Quotes_new[((index+1):(NROW(Quotes_new))),10] <- budget_credit
    Quotes_new[((index+1):(NROW(Quotes_new))),11] <- papers_credit
    
    
    shallBuy <- FALSE
  }else{
    price <- Quotes_new[index,2]
    budget <-  Quotes_new[index,8]
    papers <- Quotes_new[index,9]
    budget_credit <- Quotes_new[index,10]
    papers_credit <- Quotes_new[index,11]
    
    papers_credit <- papers_credit + papers
    budget <- papers * 2 * price * 0.9992 + budget - budget_credit
    budget_credit <- 0
    papers <- 0
    
    Quotes_new[((index+1):(NROW(Quotes_new))),8]<-budget
    Quotes_new[((index+1):(NROW(Quotes_new))),9]<-papers
    Quotes_new[((index+1):(NROW(Quotes_new))),10] <- budget_credit
    Quotes_new[((index+1):(NROW(Quotes_new))),11] <- papers_credit
    
    
    shallBuy <- TRUE     
  }  
}

Quotes_new$balance <- ( Quotes_new$papers - Quotes_new$papers_credit ) *Quotes_new$close + Quotes_new$budget - Quotes_new$budget_credit




rmarkdown::render("trading_oracle_nasdaq.Rmd")

