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
library(png)


#Считываем данные из файла, примедем их к виду, пригодному для анализа в R:
Quotes_ORCL <- read.csv(file="HistoricalQuotes.csv",stringsAsFactors = FALSE, header=TRUE, sep=",", dec= ".")
Quotes_ORCL = Quotes_ORCL[-1,]
rownames(Quotes_ORCL)<-NULL
for (i in 2:6) Quotes_ORCL[,i]  <- as.numeric(as.character(Quotes_ORCL[,i]))
#Инвертируем значения:
for (i in 1:2519) Quotes_ORCL[i,]  <- Quotes_ORCL[2519 - i + 1,]
Quotes_ORCL[,1]  <- as.Date(Quotes_ORCL[,1])
summary(Quotes_ORCL)

#Линейные графики:
plot(Quotes_ORCL[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
plot(Quotes_ORCL[1:300,3], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Объем торгов", main="Акции компании ORACLE", ylim=c(0,203555500) )

#Переведем в удобоваримый вид для наших пакетов
ORCL <- xts(Quotes_ORCL[, 2:6], order.by=as.POSIXct(Quotes_ORCL[,1]))
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
for (i in 10:2519) Quotes_ORCL[i,7] <- mean(Quotes_ORCL[(i-9):i,2])
plot(Quotes_ORCL[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes_ORCL[1:300,7], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[7] <- "SMAS"


for (i in 50:2519) Quotes_ORCL[i,8] <- mean(Quotes_ORCL[(i-49):i,2])
lines(Quotes_ORCL[1:300,8], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[8] <- "SMAL"


#График сопоставления экспоненциальных скользящих средних между собой:
Quotes_ORCL[,9] <- EMA(Quotes_ORCL[,2], n=10, ratio = 1-((10-1)/(10+1)))
plot(Quotes_ORCL[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes_ORCL[1:300,9], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[9] <- "EMAS"

Quotes_ORCL[,10] <- EMA(Quotes_ORCL[,2], n=50, ratio = 1 - ((50-1)/(50+1)))
lines(Quotes_ORCL[1:300,10], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[10] <- "EMAL"

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
      value = 0;
      counter = 0;
      for (j in i:(i-L+1)){
        value = value + data[j] * wma_w(L,counter)
        counter = counter + 1;
      }
      x[i]<-value;
    }
  }
  return(x)
}
Quotes_ORCL[,11] <- WMA(10, Quotes_ORCL[, 2])
Quotes_ORCL[,12] <- WMA(50, Quotes_ORCL[, 2])

#График сопоставления различных окон между собой:
plot(Quotes_ORCL[1:300,2], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes_ORCL[1:300,11], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[11] <- "WMAS"
lines(Quotes_ORCL[1:300,12], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
colnames(Quotes_ORCL)[12] <- "EMAL"




#Графики для короткого окна усреднения:
plot(Quotes_ORCL[1:100,2], type="l", lwd=2, col="black", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes_ORCL[1:100,7], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes_ORCL[1:100,9], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes_ORCL[1:100,11], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
legend("topleft", c("SMA-S", "EMA-S", "WMA-S"), col=c("red", "orange", "blue"), lwd=4)


#Графики для длинного окна усреднения:
plot(Quotes_ORCL[1:100,2], type="l", lwd=2, col="black", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,16) )
lines(Quotes_ORCL[1:100,8], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes_ORCL[1:100,10], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes_ORCL[1:100,12], type="l", lwd=2, col="blue", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
legend("topleft", c("SMA-L", "EMA-L", "WMA-L"), col=c("red", "orange", "blue"), lwd=4)


#Графики для SMA-S и SMA-L:
drawSquares <- function(data, start){
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
        if (notFirst)   rect(ix,miny,i,maxy, col=rgb(0.3,0.9,0.5,alpha=0.3));
        if (!notFirst) notFirst = TRUE;
        ix = i;
      }
    }else{
      if (data[i,1]>data[i,2]){
        init=TRUE;
        vec <- c(vec,i)
        if (notFirst) rect(ix,miny,i,maxy, col=rgb(0.6,0.2,0.7,alpha=0.3));
        if (!notFirst) notFirst = TRUE;
        ix = i;
      }
    }
  }
  return(vec)
}






plot(Quotes_ORCL[,2], xaxt='n',type="l", lwd=2, col= rgb(0,0,0,alpha = 0.4), xlab="", ylab="Цена закрытия", main="Акции компании ORACLE", ylim=c(10,25) )
lines(Quotes_ORCL[,7], type="l", lwd=2, col="red", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )
lines(Quotes_ORCL[,8], type="l", lwd=2, col="orange", xlab="Наблюдения", ylab="Цена закрытия", main="Акции компании ORACLE" )

vec <- drawSquares(Quotes_ORCL[,7:8], 50)
axis(side=1, at=vec,labels=Quotes_ORCL[vec,1], las=2,cex.axis = 0.6,srt = 60)
#legend("topleft", c("SMA-S", "SMA-L"), col=c("red", "orange"), lwd=2)


#imitation game
Quotes_ORCL$budget <- Quotes_ORCL[NROW(Quotes_ORCL),2]*500
Quotes_ORCL$papers <- 0
initx <- 50
intersections <- drawSquares(Quotes_ORCL[,7:8], initx)
shallBuy <- FALSE
if (Quotes_ORCL[initx,7] < Quotes_ORCL[initx,8]) shallBuy <-TRUE

for (i in 1:NROW(intersections))
  {
  if (shallBuy){
    index <- intersections[i]
    price <- Quotes_ORCL[index,2]
    budget <-  Quotes_ORCL[index,13]
    papers <-  budget  %/% price
    budget <- budget %% price
    Quotes_ORCL[(index:(NROW(Quotes_ORCL))),14]<-papers
    Quotes_ORCL[(index:(NROW(Quotes_ORCL))),13]<-budget
    shallBuy <- FALSE;
  }else{
    if (!shallBuy){
      index <- intersections[i]
      price <- Quotes_ORCL[index,2]
      budget <-  Quotes_ORCL[index,13] + price * Quotes_ORCL[index,14]
      papers <-  0
      Quotes_ORCL[(index:NROW(Quotes_ORCL)),14]<-papers
      Quotes_ORCL[(index:NROW(Quotes_ORCL)),13]<-budget
      shallBuy <- TRUE;
    }
  }
  
}



#rmarkdown::render("trading_oracle_nasdaq.Rmd")
