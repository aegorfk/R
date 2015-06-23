# подключим модули
# install.packages('foreach', repos='http://cran.us.r-project.org')
# install.packages('rvest', repos='http://cran.us.r-project.org')
# install.packages('doMC', repos='http://cran.us.r-project.org')
# install.packages('selectr', repos='http://cran.us.r-project.org')
# install.packages('httr', repos='http://cran.us.r-project.org')
# install.packages('XML', repos='http://cran.us.r-project.org')
# install.packages('doParallel')


library(foreach)
library(rvest)
library(selectr)
library(doMC)
library(httr)
library(XML)
library(doParallel)




category <- c("Entertainment", "Finance", "Food & Drink")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-entertainment/id6016?mt=8", "https://itunes.apple.com/us/genre/ios-finance/id6015?mt=8", "https://itunes.apple.com/us/genre/ios-food-drink/id6023?mt=8")
categories <- data.frame(category, apple_category_link)
alfabet <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","*")
apps <- data.frame(app_name = character(0), app_link = character(0))
cl <- makeCluster(4, outfile="") # Register cluster
registerDoParallel(cl)




readUrl <- function(url) {
  out <- tryCatch(
{ 
  html(url)
},
error=function(cond) {
  message(paste("URL does not seem to exist:", url))
  message("Here's the original error message:")
  message(cond)
  return(NA)
},
warning=function(cond) {
  message(paste("URL caused a warning:", url))
  message("Here's the original warning message:")
  message(cond)
  # Choose a return value in case of warning
  return(NULL)
},
finally={
  message(paste("Processed URL:", url))
}
  )
}


foreach(i=1:4) %dopar% {
  
  # for(i in 1:1) {
  require("rvest")
  require("selectr")
  require("XML")
  require("httr")
  #   for (l in 1:length(alfabet)){
  for (l in 1:length(alfabet)){
    a <-  1
    current_page <- readUrl(paste(categories[i,2], "&letter=", alfabet[l], "&page=", a, sep=""))
    
    while(length(current_page %>% html_nodes(".paginate-more") %>% html_text()) != 0) {
      if (a == 1) {current_page <- readUrl(paste(categories[i,2], "&letter=", alfabet[l], "&page=", a, sep=""))}
      app_name <- current_page %>% html_nodes("#selectedcontent a") %>% html_text()
      app_link <- current_page %>% html_nodes("#selectedcontent a") %>% html_attr("href")
      app_names_at_page <- as.data.frame(cbind(app_name, app_link))
      
      for (n in 1:nrow(app_names_at_page)) app_names_at_page[n,3] <- as.character(categories[i,1])
      apps <- rbind(apps, app_names_at_page)
      current_page <- readUrl(current_page %>% html_nodes(".alpha+ .paginate .paginate-more") %>% html_attr("href"))
      a <- 0
    }
    #     saveRDS(apps, file = paste(categories[i,1], "_", alfabet[l], ".RDS", sep=""))
    #     rm(apps)
    #     apps <- data.frame(app_name = character(0), app_link = character(0))
  }
  saveRDS(apps, file = paste(categories[i,1], ".RDS", sep=""))
  rm(apps)
  apps <- data.frame(app_name = character(0), app_link = character(0))
}




#Парсер каждой странички
for (i in 1:nrow(categories))  {
  require("rvest")
  require("selectr")
  require("XML")
  require("httr")
  message(paste("PROCESSING CATEGORY: ", categories[i,1]))
  apps_all <- readRDS(paste(categories[i,1], ".RDS", sep=""))
  apps_all$app_name <- as.character(apps_all$app_name)
  apps_all$app_link <- as.character(apps_all$app_link)
  apps_all[,c("released","size","watch","language","seller","compatible_iPhone","compatible_iPad","compatible_iPod","rated","requires","optimized_iPhone_5",
              "optimized_iPhone_6","optimized_iPhone_6_Plus","description","updated","review_1","review_2","review_3","current_reviews",
              "current_rating","total_reviews","total_rating","rating_1","rating_2","rating_3",
              "price")] <- NA
  apps_all$released <- as.Date(apps_all$released)
  apps_all$compatible_iPhone <- as.logical(apps_all$compatible_iPhone)
  apps_all$compatible_iPad <- as.logical(apps_all$compatible_iPad)
  apps_all$compatible_iPod <- as.logical(apps_all$compatible_iPod)
  apps_all$optimized_iPhone_5 <- as.logical(apps_all$optimized_iPhone_5)
  apps_all$optimized_iPhone_6 <- as.logical(apps_all$optimized_iPhone_6)
  apps_all$optimized_iPhone_6_Plus <- as.logical(apps_all$optimized_iPhone_6_Plus)
  apps_all$size <- as.numeric(apps_all$size)
  
  ## ОТ СЮДА ПРОГОНЯЮ
  cl <- makeCluster(4, outfile="") # Register cluster
  registerDoParallel(cl)
  range <- 1:NROW(apps_all)
  if (i==1) range <- 39348:283340
  # foreach (a = range) %dopar% {
  for (a in 39348:283340) {
    message(paste("PROCESSING CATEGORY: ", categories[i,1],"; page ",a," out of ",range[NROW(range)]))
    require("rvest")
    require("selectr")
    require("XML")
    require("httr")
    current_page <- readUrl(apps_all[a,2])
    if (!is.null(current_page)){
      processedPage <- tryCatch({
        apps_all$released[a] <- as.Date.character((current_page %>% html_nodes(".release-date .label+ span") %>% html_text()), format="%b %d, %Y")
        size <- unlist(strsplit((current_page %>% html_nodes("#left-stack .list") %>% html_nodes("li"))[5] %>% html_text(),"Size: "))
        size <- unique(na.omit(as.numeric(unlist(strsplit(unlist(size), "[^0-9]+")))))
        apps_all$size[a] <- as.numeric(paste(size[1], ".", size[2], sep=""))
        
        watch <- unlist(strsplit(current_page %>% html_nodes("#watch-screenshots-swoosh h2") %>% html_text(),"Apple Watch: "))
        if (length(watch) == 0)  {apps_all$watch[a] <- "No"}  else apps_all$watch[a] <- "Yes"
        
        language <- unlist(strsplit(current_page %>% html_nodes(".language") %>% html_text(),"Languages: |Language: "))
        if (!is.null(language)) apps_all$language[a] <- language[language!=""]
        
        seller <- unlist(strsplit(current_page %>% html_nodes("#title h2") %>% html_text(),"By "))
        if (!is.null(seller)) {apps_all$seller[a] <- seller[seller!=""]}
        
        rated <- unlist(strsplit(current_page %>% html_nodes(".app-rating a") %>% html_text(),"Rated "))
        if ((rated != "Not yet rated")&&(!is.null(rated))) {apps_all$rated[a] <- unique(na.omit(as.numeric(unlist(strsplit(unlist(rated), "[^0-9]+")))))}
        
        
        compatible <- unlist(strsplit(current_page %>% html_nodes("#left-stack p") %>% html_text(),"Compatibility: "))
        requires <- unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(compatible)), ""))))
        apps_all$requires[a] <- paste(requires[1], ".", requires[2], sep="")
        if (!is.null(compatible)) 
        {
          compatible <- unlist(strsplit(compatible,"with | This app is optimized for "))
          if ((!is.null(compatible[3]))&grepl("5",compatible[3]))  {apps_all$optimized_iPhone_5[a] <- TRUE}  else apps_all$optimized_iPhone_5[a] <- FALSE
          if ((!is.null(compatible[3]))&grepl("6",compatible[3]))  {apps_all$optimized_iPhone_6[a] <- TRUE}  else apps_all$optimized_iPhone_6[a] <- FALSE
          if ((!is.null(compatible[3]))&grepl("Plus",compatible[3]))  {apps_all$optimized_iPhone_6_Plus[a] <- TRUE}  else apps_all$optimized_iPhone_6_Plus[a] <- FALSE
        }
        
        if (!is.null(compatible)) 
        {
          compatible <- unlist(strsplit(compatible[2],", |, and "))
          if ((compatible[1]=="iPhone")||(compatible[1]=="iPhone."))  {apps_all$compatible_iPhone[a] <- TRUE}  else apps_all$compatible_iPhone[a] <- FALSE
          if ((!is.null(compatible[2]))&&(!is.na(compatible[2]))&&((compatible[2]=="iPad")||(compatible[2]=="iPad.")))  {apps_all$compatible_iPad[a] <- TRUE}  else apps_all$compatible_iPad[a] <- FALSE
          if ((!is.null(compatible[3]))&&(!is.na(compatible[3]))&&(compatible[3]=="iPod touch."))  {apps_all$compatible_iPod[a] <- TRUE}  else apps_all$compatible_iPod[a] <- FALSE
        }
        
        
        description <- current_page %>% html_nodes(".product-review p") %>% html_text()
        apps_all$updated[a] <- description[2]
        apps_all$description[a] <- description[1]
        
        review <- unlist(strsplit(current_page %>% html_nodes(".customer-review .content") %>% html_text(),"\n    "))
        if (!is.null(review)) { review <- unlist(strsplit(review,"\n  "))
                                review <- review[review!="\n  "]}
        if (!is.null(review[1])) { apps_all$review_1[a] <- review[1]}
        if (!is.null(review[2])) { apps_all$review_2[a] <- review[2]}
        if (!is.null(review[3])) { apps_all$review_3[a] <- review[3]}  
        
        current_rating <- unlist(strsplit(current_page %>% html_nodes(".rating:nth-child(3)") %>% html_attr("aria-label")," stars| "))
        if (length(current_rating) == 7){
          apps_all$current_reviews[a] <- as.numeric(current_rating[6])
          apps_all$current_rating[a] <- as.numeric(current_rating[1]) + 0.5
        }  else {
          if (!is.null(current_rating[3])) apps_all$current_reviews[a] <- as.numeric(current_rating[3])
          if (!is.null(current_rating[1])) apps_all$current_rating[a] <- as.numeric(current_rating[1])
        }
        
        total_rating <- unlist(strsplit(current_page %>% html_nodes(".rating~ .rating") %>% html_attr("aria-label")," stars| "))
        if (length(total_rating) == 7){
          apps_all$total_reviews[a] <- as.numeric(total_rating[6])
          apps_all$total_rating[a] <- as.numeric(total_rating[1]) + 0.5
        }  else {
          if (!is.null(total_rating[3])) apps_all$total_reviews[a] <- as.numeric(total_rating[3])
          if (!is.null(total_rating[1])) apps_all$total_rating[a] <- as.numeric(total_rating[1])
        }
        
        rating <- unlist(strsplit(current_page %>% html_nodes(".customer-review .rating") %>% html_attr("aria-label")," stars| star"))
        if (!is.null(rating[1])) {apps_all$rating_1[a] <- rating[1]}  
        if (!is.null(rating[2])) {apps_all$rating_2[a] <- rating[2]}  
        if (!is.null(rating[3])) {apps_all$rating_3[a] <- rating[3]}  
        
        price <- current_page %>% html_nodes(".price") %>% html_text()
        if ((!is.null(price))&(length(price) != 0)) apps_all$price[a] <- price
        rm(compatible,current_page,current_rating,description,language,price,rated,rating,requires,review,seller,size,total_rating,watch)
      }, warning = function(war) {
        message(paste("Warning in parallel:  ",war))
        return(NULL)
        
      }, error = function(err) {
        message(paste("ERROR in parallel:  ",err))
        return(NULL)
        
      }, finally = {
        
      })
    }
    ## каждые 3000 строк сохраняемся
    if (a %% 3000 == 0){
      message(paste("SAVING TO FILE:",categories[i,1],"_apps", ".RDS", sep=""))
      saveRDS(apps_all, file = paste(categories[i,1],"_apps", ".RDS", sep=""))
    }
    Sys.sleep(2)
  }
  
  stopCluster(cl)
  ##ДО СЮДА
  saveRDS(apps_all, file = paste(categories[i,1],"_apps", ".RDS", sep=""))
  rm(apps_all)
}



stopCluster(cl)



saveRDS(apps_all[1:39347,], file = paste("Entertainment_apps_1", ".RDS", sep=""))








apps_health <- readRDS("Health & Fitness_apps.RDS")

hist(apps_health$released, breaks = 100)
m <- ggplot(apps_health, aes(x = released), geom="density")
# m + geom_density(kernel = "rectangular") + labs(title = "Дата выпуска", x = "Год", y = "Плотность вероятности")
# m + geom_dotplot(binwidth = 2) + labs(title = "Дата выпуска", x = "Год", y = "Плотность")
# m <- ggplot(apps_health, aes(x = released), geom="density")

m <- ggplot(apps_health, aes(x = released), geom="density")
m + geom_histogram(binwidth = 1) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")



freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

freqfunc(apps_health$released, 5)


myCorpus = Corpus(VectorSource(apps_health$app_name))
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
wordcloud(d$word,d$freq,min.freq=40, colors=brewer.pal(8, 'Dark2'),)
wordcloud(d$word,d$freq, max.words=1000, min.freq=10, scale=c(4,.5), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
wordcloud(names(d$word), d$freq, scale=c(9,.1),min.freq=20, max.words=Inf, random.order=F, rot.per=.4, colors=brewer.pal(8, "Dark2"))

#Смотрим самых плодовытых издателей
freqfunc22 <- function(x, n){
  tail(sort(table(unlist(as.character(x)))), n)
}
freqfunc22(apps_health$seller, 10)

newdata <- apps_health[ which(apps_health$seller=='Kick Your Apps, Inc.'), ]
m2 <- ggplot(newdata, aes(x = released), geom="density")
m2 + geom_histogram(binwidth = 1) + geom_density(kernel = "rectangular") + labs(title = "Выпуск приложений", x = "Дата", y = "Количество выпущеных приложений")



#Вес приложенек
mean(na.omit(apps_health$size))
max(na.omit(apps_health$size))
newdata <- apps_health[ which(apps_health$size==99.8), ]
min(na.omit(apps_health$size))

m <- ggplot(apps_health, aes(x = apps_health$price), geom="histogram")
# m + geom_density(kernel = "rectangular") + labs(title = "Дата выпуска", x = "Год", y = "Плотность вероятности")
m + geom_histogram(binwidth=10) 
+ labs(title = "Дата выпуска", x = "Год", y = "Плотность")

#Рисуем пирожок
df <- data.frame(
  price = c("$9.99","$3.99","$4.99","Other", "$2.99","$1.99","Free"),
  value = c(3, 2.5, 4, 4.5, 7, 13, 66)
)
label <- paste0(df$value, "%")
bp + annotate(geom = "text", y = df$value, x = 1, label = label)+  coord_polar("y", start=0) + scale_fill_brewer(palette="Greens") + labs(title = "Стоимость приложения") + geom_bar(stat="identity", color='black') + guides(fill=guide_legend(override.aes=list(colour=NA)))

#Мозаичный график
library(vcd)
mosaic(data=apps_health,~  optimized_iPhone_5 + optimized_iPhone_6 + optimized_iPhone_6_Plus,shade=TRUE)


# Статус перехода на новые платформы
newdata <- apps_health[ which(apps_health$watch=='Yes'), ]
есть <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(77, 14, 13, 90, 90, 90, 1)
)

newdata <- apps_health[ which(apps_health$optimized_iPhone_5=='TRUE'), ]
нет <- data.frame(
  Устройство = c("iPhone 5","iPhone 6","iPhone 6 Plus","iPhone","iPad","iPod","Watch"),
  Процент = c(23, 86, 87, 10, 10, 10, 99)
)

нет$Адаптирован <- "Да"
есть$Адаптирован <- "Нет"
d <- rbind(есть, нет)
ggplot(data=d, aes(x = Устройство, y = Процент, fill = Адаптирован)) + geom_bar(stat="identity")

