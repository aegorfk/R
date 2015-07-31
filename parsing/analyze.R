library(ggplot2)
library(tm)
library(wordcloud)
library(memoise)


apps_all_1 <- readRDS("Lifestyle_apps_1.RDS")
apps_all_2 <- readRDS("Lifestyle_apps_2.RDS")
apps_all_3 <- readRDS("Lifestyle_apps_3.RDS")
apps <- rbind(apps_all_1,apps_all_2, apps_all_3)



apps <- readRDS("Social_Networking_apps.RDS")
apps <- apps[ which(!(is.na(apps$released))), ]

# Количество приложений, у которых все поля заполнены
newdata <- na.omit(apps)

hist(apps$released, breaks = 100)
m <- ggplot(apps, aes(x = released), geom="density")
# m + geom_density(kernel = "rectangular") + labs(title = "Дата выпуска", x = "Год", y = "Плотность вероятности")
# m + geom_dotplot(binwidth = 2) + labs(title = "Дата выпуска", x = "Год", y = "Плотность")
# m <- ggplot(apps, aes(x = released), geom="density")

m + geom_histogram(binwidth = 1) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")



freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

#Наибольше всего приложений обновились или выпустились числа:
freqfunc(apps$released, 5)

#Смотрим самых плодовытых издателей
freqfunc22 <- function(x, n){
  tail(sort(table(unlist(as.character(x)))), n)
}
freqfunc22(apps$seller, 10)
newdata2 <- apps[ which(apps$seller=="LISA Ledeninformatiesystemen B.V."), ]


myCorpus = Corpus(VectorSource(apps$app_name))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus,removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)
v <- sort(rowSums(m), decreasing = TRUE)
head(v,14)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=30, colors=brewer.pal(8, 'Dark2'),)
wordcloud(d$word,d$freq, max.words=1000, min.freq=50, scale=c(4,.5), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
#wordcloud(names(d$word), d$freq, scale=c(9,.1),min.freq=20, max.words=Inf, random.order=F, rot.per=.4, colors=brewer.pal(8, "Dark2"))

#Смотрим приложения, у которых в названии самые популярные слова
#Помещаем их в data frame newdata
newdata <- data.frame(x= numeric(0))

for (i in 1:nrow(apps))
{
  #Важно выделить отдельное слово
  if ((grepl(" instagram ", tolower(apps[i,1]), perl=TRUE) == TRUE)||(grepl(" instagram,", tolower(apps[i,1]), perl=TRUE) == TRUE)||(grepl(" instagram.", tolower(apps[i,1]), perl=TRUE) == TRUE)||(grepl(" instagram!", tolower(apps[i,1]), perl=TRUE) == TRUE)){
    newdata <- rbind(newdata,apps[i,])
  }
}

newdata2 <- apps[ which(newdata$price!="Free"), ]


#Смотрим самых плодовытых издателей
freqfunc22 <- function(x, n){
  tail(sort(table(unlist(as.character(x)))), n)
}
freqfunc22(apps$seller, 10)

newdata <- apps[ which(apps$seller=='Bluumi'), ]


#Смотрим версии iOS и разрешенный возраст
newdata <- apps[ which(!is.na(apps$rated)), ]
qplot(factor(rated), data=newdata, geom="bar", fill=factor(requires))


#Вес приложенек
mean(na.omit(apps$size))
max(na.omit(apps$size))
newdata <- apps[ which(apps$size==0.1), ]
min(na.omit(apps$size))

#Наиболее применяемая цена:
freqfunc22(apps$price, 10)

#Рисуем пирожок
price = c("Free", "$2.99", "$1.99", "$0.99", "Other")
value = c(86, 8.5, 3.3, 1.2, 1)
pie(value, labels = price, main = "Цены на приложение", col=rainbow(15))

#Гистограмма по возрасту приложений:
barplot(apps$rated, main="Разрешенный возраст", ylab="Количество", names.arg=apps$rated, col=rainbow(15))

hist(apps$rated)


df <- data.frame(
  price <- c("Free", "$2.99", "$1.99", "$0.99", "Other"),
  value <- c(86, 8.5, 3.3, 1.2, 1)
)
label <- paste0(df$value, "%")
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp + annotate(geom = "text", y = df$value, x = 1, label = label) + coord_polar("y", start=0) + scale_fill_brewer(palette="Blues") + labs(title = "Стоимость приложения") + geom_bar(stat="identity", color='black') + guides(fill=guide_legend(override.aes=list(colour=NA)))


#Мозаичный график
library(vcd)
mosaic(data=apps,~  optimized_iPhone_5 + optimized_iPhone_6 + optimized_iPhone_6_Plus,shade=TRUE)
mosaic(data=apps,~  compatible_iPhone + compatible_iPad + compatible_iPod,shade=TRUE)

# Статус перехода на новые платформы
newdata <- apps[ which(apps$optimized_iPhone_5==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$optimized_iPhone_6==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$optimized_iPhone_6_Plus==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$compatible_iPhone==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$compatible_iPad==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$compatible_iPod==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))
newdata <- apps[ which(apps$watch=='Yes'), ]


(nrow(newdata)*100)/(nrow(apps))
нет <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(83, 18, 18, 96, 95, 95, 0.6)
)

есть <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(17, 82, 82, 4, 5, 5, 99.4)
)

нет$Адаптирован <- "Да"
есть$Адаптирован <- "Нет"
d <- rbind(нет, есть)
ggplot(data=d, aes(x = Устройство, y = Процент, fill = Адаптирован)) + geom_bar(stat="identity") +  scale_fill_brewer(palette="Spectral")

#Смотрим, какие только под часы:
newdata <- apps[ which(apps$watch=='Yes'), ]
newdata <- newdata[ which(newdata$compatible_iPhone==FALSE), ]
newdata <- newdata[ which(newdata$compatible_iPad==FALSE), ]
newdata <- newdata[ which(newdata$compatible_iPod==FALSE), ]

#Среднее количество отзывов на приложение:
mean(na.omit(apps$current_reviews))
# 1 оценку имеют приложения
newdata <- apps[ which(is.na(apps$rating_2)), ]
newdata2 <- newdata[ which(!is.na(newdata$rating_1)), ]
# 2 оценки имеют приложения
newdata <- apps[ which(is.na(apps$rating_3)), ]
newdata2 <- newdata[ which(!is.na(newdata$rating_2)), ]


#Наиболее используемые языки
myCorpus = Corpus(VectorSource(apps$language))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus,removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)
v <- sort(rowSums(m), decreasing = TRUE)
head(v,14)
