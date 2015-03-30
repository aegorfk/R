# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK


Sys.setenv(LANG = "en")
#install.packages("quantmod", dependencies=TRUE)
library("quantmod")
#install.packages("zoo", dependencies=TRUE)
library("zoo")
#install.packages("TTR", dependencies=TRUE)
library("TTR")




#��������� ������ �� �����, �������� �� � ����, ���������� ��� ������� � R:
Quotes_ORCL <- read.csv(file="HistoricalQuotes.csv",stringsAsFactors = FALSE, header=TRUE, sep=",", dec= ".")
Quotes_ORCL = Quotes_ORCL[-1,]
rownames(Quotes_ORCL)<-NULL
for (i in 2:6) Quotes_ORCL[,i]  <- as.numeric(as.character(Quotes_ORCL[,i]))
#����������� ��������:
for (i in 1:2519) Quotes_ORCL[i,]  <- Quotes_ORCL[2519 - i + 1,]
Quotes_ORCL[,1]  <- as.Date(Quotes_ORCL[,1])
summary(Quotes_ORCL)

#�������� �������:
plot(Quotes_ORCL[1:300,2], type="l", lwd=2, col="red", xlab="����������", ylab="���� ��������", main="����� �������� ORACLE", ylim=c(10,16) )
plot(Quotes_ORCL[1:300,3], type="l", lwd=2, col="blue", xlab="����������", ylab="����� ������", main="����� �������� ORACLE", ylim=c(0,203555500) )

#��������� � ������������ ��� ��� ����� �������
ORCL <- xts(Quotes_ORCL[, 2:6], order.by=as.POSIXct(Quotes_ORCL[,1]))
ORCL <- ORCL[ ! duplicated( index(ORCL) ),  ]
#������ ���� � ����� �������� ������:
chartSeries(ORCL,subset='2005-03-28::2005-10-26') 
#����������� ������:
barChart(ORCL,subset='2005-03-28::2005-10-26', theme='white.mono',bar.type='hlc')

#���������� ������� �� ��������� ���� ��������, ���� ���������� - 10 �������:
chartSeries(ORCL, subset='2005-03-28::2005-10-26',TA="addSMA(10)")

#���������� ������� �� ��������� ���� ��������, ���� ���������� - 10 �������:
chartSeries(ORCL, subset='2005-03-28::2005-10-26',TA="addSMA(50)")
