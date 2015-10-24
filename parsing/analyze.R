library(rmongodb)
library(jsonlite)
library(ggplot2)
library(tm)
library(wordcloud)
library(dplyr)
#library(plyr)
#library(memoise)




mongo <- mongo.create(host = "rkatelyapina9.cloudapp.net",name = "", db = "test")
#Проверка подключения
mongo.is.connected(mongo)
mongo.get.databases(mongo)
mongo.get.database.collections(mongo, db = "test")


tmp = mongo.find.one(mongo, ns = "test.apps")
find_all = mongo.find.all(mongo, ns = "test.apps")

find_all <- sapply(find_all, function(x) ifelse(x == "NULL", NA, x))
find_all <- t(find_all)
rownames(find_all)
colnames(find_all)
apps_all <- as.data.frame(find_all)
saveRDS(apps, "apps.rds")
apps_all <- readRDS("apps.rds")

#Делаем подвыборку
apps <- apps_all[ which((apps_all$primary_category == "Books")|(apps_all$second_category == "Games")), ]


#Чистим данные
#РАБОТАЕТ РИЛИ ДОЛГО
for (i in 1:nrow(apps))
{
  if(!(is.na(apps$current_rating[i])))
  {
    a <- apps$current_rating[i]
    if(length(a[[1]])==4){
      apps$current_reviews[i] <- a[[1]][3]
      apps$current_rating[i] <- as.numeric(a[[1]][1])
    }
    else if(length(a[[1]])==7){
      apps$current_reviews[i] <- a[[1]][6]
      apps$current_rating[i] <- as.numeric(a[[1]][1]) + 0.5
    }
  }
}


apps$updated <- Sys.Date()
for (i in 1:nrow(apps))
{
  a <- as.Date(as.POSIXct(apps$update[[i]][1], origin = "1582-10-14", tz = "GMT"))
  apps$updated[i] <- a
}

View(apps[1:50,])

apps$id <- as.numeric(apps$id)
apps$second_category <- as.character(apps$second_category)
apps$current_rating <- as.numeric(apps$current_rating)
apps$current_reviews<- as.numeric(apps$current_reviews)
apps$price <- as.character(apps$price)
apps$description <- as.character(apps$description)
apps$requires<- as.numeric(apps$requires)
apps$support <- as.character(apps$support)
apps$total_rating <- as.numeric(apps$total_rating)
apps$total_reviews <- as.numeric(apps$total_reviews)
#apps$ids <- as.numeric(apps$ids)
#apps$ids_simple <- as.numeric(apps$ids_simple)
apps$purchases_names <- as.character(apps$purchases_names)
apps$purchases_prices <- as.character(apps$purchases_prices)
apps$rating_1 <- as.numeric(apps$rating_1)
apps$rating_2 <- as.numeric(apps$rating_2)
apps$rating_3 <- as.numeric(apps$rating_3)
apps$review_1 <- as.character(apps$review_1)
apps$review_2 <- as.character(apps$review_2)
apps$review_3 <- as.character(apps$review_3)


#Удаляем из датасета удаленные приложения
apps <- apps[ which(!(is.na(apps$price))), ]

#Вычищаем данные
apps$rating <- 12
apps$ver <- 1.0
apps$optimized_iPhone_52 <- TRUE
apps$optimized_iPhone_62 <- TRUE
apps$optimized_iPhone_6_Plus2 <- TRUE
apps$compatible_iPhone2 <- TRUE
apps$compatible_iPad2 <- TRUE
apps$compatible_iPod2 <- TRUE
apps$watch2 <- TRUE

for (i in 1:nrow(apps))
{
  a <- as.numeric(apps$rated[i][[1]])
  apps$rating[i] <- a
  a <- as.logical(apps$optimized_iPhone_5[i][[1]])
  apps$optimized_iPhone_52[i] <- a
  a <- as.logical(apps$optimized_iPhone_6[i][[1]])
  apps$optimized_iPhone_62[i] <- a
  a <- as.logical(apps$optimized_iPhone_6_Plus[i][[1]])
  apps$optimized_iPhone_6_Plus2[i] <- a
  a <- as.logical(apps$compatible_iPhone[i][[1]])
  apps$compatible_iPhone2[i] <- a
  a <- as.logical(apps$compatible_iPad[i][[1]])
  apps$compatible_iPad2[i] <- a
  a <- as.logical(apps$compatible_iPod[i][[1]])
  apps$compatible_iPod2[i] <- a
  a <- as.logical(apps$watch[i][[1]])
  apps$watch2[i] <- a
  if (length(as.character(apps$version[i][[1]])) != 0)
  {
    a <- as.character(apps$version[i][[1]])
    apps$ver[i] <- a
  }
}

apps$rated <- NULL
apps$optimized_iPhone_5 <- NULL
apps$optimized_iPhone_6 <- NULL
apps$optimized_iPhone_6_Plus <- NULL
apps$compatible_iPhone <- NULL
apps$compatible_iPad <- NULL
apps$compatible_iPod <- NULL
apps$watch <- NULL
apps$update <- NULL

library(plyr) 
apps <- rename(apps,c('rating'='rated','optimized_iPhone_52'='optimized_iPhone_5', 'optimized_iPhone_62'='optimized_iPhone_6', 'optimized_iPhone_6_Plus2'='optimized_iPhone_6_Plus', 'compatible_iPhone2'='compatible_iPhone', 'compatible_iPad2'='compatible_iPad', 'compatible_iPod2'='compatible_iPod', 'watch2'='watch'))

#Делаем back up
saveRDS(apps, "Books_apps.RDS")








apps <- readRDS("Games_apps.RDS")

#Функция для подсчета частот
freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

#Посчитаем самую популярную вторую категорию
apps2 <- apps[ which((apps$second_category != "Books")), ]
freqfunc(apps2$second_category, 5)

# Количество приложений, у которых есть комментарии пользователей, отзывы и обновления
newdata <- apps[ which(!is.na(apps$updated) & !is.na(apps$rating_1) & !is.na(apps$review_1)), ]


apps2 <- apps[1:50,]
ggplot(apps, aes(x = updated)) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")
m <- ggplot(apps, aes(x = updated)) + geom_density()
m2 <- ggplot(apps2, aes(x = updated)) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")


hist(apps$updated, breaks = 100)
m <- ggplot(apps2, aes(x = apps$updated), geom="density")
# m + geom_density(kernel = "rectangular") + labs(title = "Дата выпуска", x = "Год", y = "Плотность вероятности")
# m + geom_dotplot(binwidth = 2) + labs(title = "Дата выпуска", x = "Год", y = "Плотность")
# m <- ggplot(apps, aes(x = released), geom="density")
m + geom_histogram(binwidth = 3) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")


#Наибольше всего приложений обновились или выпустились числа:
freqfunc(apps$updated, 5)

#Смотрим самых плодовытых издателей
freqfunc22 <- function(x, n){
  tail(sort(table(unlist(as.character(x)))), n)
}
freqfunc22(apps$seller, 10)
newdata2 <- apps[ which(apps$seller=="Libro Movil"), ]

#Сколько разрабов задействовано в категории:
a <- unique(apps[c("seller")])
rm(a)

#Считаем популярные слова в названии
apps <- apps[ which(!is.na(apps$name)), ]
myCorpus = Corpus(VectorSource(apps$name))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus,removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
ProjectSparse <- removeSparseTerms(myDTM, 0.98)
m = as.matrix(ProjectSparse)
v <- sort(rowSums(m), decreasing = TRUE)
head(v,140)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=30, colors=brewer.pal(8, 'Dark2'),)
wordcloud(d$word,d$freq, max.words=100, min.freq=1, scale=c(4,.5), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
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


#Смотрим версии iOS и разрешенный возраст
apps <- apps[ which(!is.na(apps$rated)), ]
qplot(factor(apps$rated), data=apps, geom="bar", fill=factor(requires)) + labs(title = "Возраст", y = "Количество выпущеных приложений")

newdata <- apps[ which(apps$requires==2), ]
a <- unique(newdata[c("seller")])

#Вес приложенек
mean(na.omit(apps$size))
max(na.omit(apps$size))
newdata <- apps[ which(apps$size==0.1), ]
min(na.omit(apps$size))

#Наиболее применяемая цена:
freqfunc22(apps$price, 10)

#Рисуем пирожок
price = c("Free", "$0.99", "$1.99", "$2.99", "Other")
value = c(42, 20, 16, 8, 14)
pie(value, labels = price, main = "Цены на приложение", col=rainbow(15))

#Самое дорогое приложение:
newdata <- apps[ which(apps$price=="$999.99"), ]

#Второй вариант визуализации цены
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
newdata <- apps[ which(apps$watch==TRUE), ]
(nrow(newdata)*100)/(nrow(apps))


нет <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(47, 8, 8, 75, 8, 75, 0.2)
)

есть <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(53, 92, 92, 25, 92, 25, 99.8)
)

нет$Адаптирован <- "Да"
есть$Адаптирован <- "Нет"
d <- rbind(нет, есть)
ggplot(data=d, aes(x = Устройство, y = Процент, fill = Адаптирован)) + geom_bar(stat="identity") +  scale_fill_brewer(palette="Set1")

#Смотрим, какие только под часы:
newdata <- apps[ which(apps$watch==TRUE), ]
newdata <- newdata[ which(newdata$compatible_iPhone==FALSE), ]
newdata <- newdata[ which(newdata$compatible_iPad==FALSE), ]
newdata <- newdata[ which(newdata$compatible_iPod==FALSE), ]

#Среднее количество отзывов на приложение:
mean(na.omit(apps$current_reviews))
#Средний рейтинг приложения:
mean(na.omit(apps$current_rating))
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





#Исследование доменных зон
#Выберем уникальные по сайту строки
a <- apps[!duplicated(apps$site), ]
newdata <- a[ which(!is.na(a$support)), ]
a$domain <-  TRUE

for (i in 1:nrow(a))
{
  if (length(as.character(a$site[i][[1]])) != 0)
  {
    if(grepl(".com", as.character(a$site[i][[1]]))){
      a$domain[i] <- TRUE }}
  else a$domain[i] <- FALSE
}
z <- a[ which(a$site == "http://etolstoy.ru" ), ]


#Исследуем версионность, сколько приложений еще не обновились
z <- apps[ which(apps$ver == "1.0" ), ]

#Анализируем скриншоты приложений: сколько верт, гор и под форм-факторы
apps$scr <-  0
for (i in 1:nrow(apps))
{
  apps$scr[i] <- length(apps$all_screenshots[[i]])
}

apps$gor_screenshots <- NULL
apps$vertical_screenshots <- NULL
apps$gor_screenshots <- 0
apps$vertical_screenshots <- 0
apps$screen_iphone <- 0
apps$screen_ipad <- 0
apps$screen_watch <- 0

# screen640x640 - iphone гор
# screen520x924 - iphone гор
# screen322x572 - iphone верт
# screen568x568 - iphone верт
# screen320x480 - iphone верт
# screen480x480 - ipad
# screen390x390 - watch

for (i in 1:nrow(apps))
{
  a <- apps$all_screenshots[i][[1]]
  for(j in 1:length(a))
  {
    if(length(a)!= 0){
      if(grepl("(screen640x640)|(screen520x924)", as.character(a[j][[1]]))){
        apps$gor_screenshots[i] <- apps$gor_screenshots[i] + 1 }
      if(grepl("(screen322x572)|(screen568x568)|(screen320x480)", as.character(a[j][[1]]))){
        apps$vertical_screenshots[i] <- apps$vertical_screenshots[i] + 1 }
      if(grepl("(screen480x480)", as.character(a[j][[1]]))){
        apps$screen_ipad[i] <- apps$screen_ipad[i] + 1 }
      if(grepl("(screen390x390)", as.character(a[j][[1]]))){
        apps$screen_watch[i] <- apps$screen_watch[i] + 1 }  
    }
  }
  apps$screen_iphone[i] <- apps$gor_screenshots[i] + apps$vertical_screenshots[i]
  if (length(a) != apps$screen_iphone[i] + apps$screen_ipad[i] + apps$screen_watch[i])  
    print(apps$name[i])
}


#Анализ скриншотов:
qplot(apps$scr)+  geom_histogram(breaks=seq(1, 15, by =1), aes(fill=..count..))  +
  labs(title = "Скриншоты", x = "Количество скриншотов", y = "Количество выпущеных приложений")

#Частота скриншотов:
freqfunc22(apps$scr, 10)
mean(apps$vertical_screenshots)
mean(apps$screen_iphone)
mean(apps$screen_ipad)
mean(apps$screen_watch)

#Анализ встроенных покупок:
apps$purchase <-  0
value <- 0 
for (i in 1:nrow(apps))
{
  apps$purchase[i] <- length(apps$purchases_prices[[i]]) 
}



newdata$purchase <-  0
value <- 0 
est.func <- c("x","x+1","x+2","x+3","x+4")
for (i in 1:nrow(newdata))
{
  newdata$purchase[1] <- apps$purchases_prices[[1]]
  
}
