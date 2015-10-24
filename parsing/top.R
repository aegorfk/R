# подключим модули
# install.packages('foreach', repos='http://cran.us.r-project.org')
# install.packages('rvest', repos='http://cran.us.r-project.org')
# install.packages('doMC', repos='http://cran.us.r-project.org')
# install.packages('selectr', repos='http://cran.us.r-project.org')
# install.packages('httr', repos='http://cran.us.r-project.org')
# install.packages('XML', repos='http://cran.us.r-project.org')
# install.packages('doParallel')
# install.packages('rmongodb')
# install.packages('jsonlite')

library(foreach)
library(rvest)
library(selectr)
library(doMC)
library(httr)
library(XML)
library(doParallel)




readUrl <- function(url) {
  out <- tryCatch(
{ 
  read_html(url)
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
})}

#бесплатный ТОП
current_page <- readUrl("http://www.apple.com/itunes/charts/free-apps/")
free_names <- current_page %>% html_nodes(".chart-grid .more") %>% html_attr("href")

#платный ТОП
current_page <- readUrl("http://www.apple.com/itunes/charts/paid-apps/")
paid_names <- current_page %>% html_nodes(".chart-grid .more") %>% html_attr("href")



newdata <- data.frame()
free <- data.frame()
paid <- data.frame()
for (i in 1:100){
  #Обработка бесплатных
  id <- strsplit(as.character(free_names[i]),"id")
  id <- id[[1]][2]
  id <- strsplit(as.character(id),"mt")
  id <-  substr(id[[1]][1], 1, nchar(id[[1]][1])-1)
  
  newdata <- apps[ which(apps$id == id), ]
  free <- rbind(newdata, free)
  
  #Обработка платных
  id <- strsplit(as.character(paid_names[i]),"id")
  id <- id[[1]][2]
  id <- strsplit(as.character(id),"mt")
  id <-  substr(id[[1]][1], 1, nchar(id[[1]][1])-1)
  
  newdata <- apps[ which(apps$id == id), ]
  paid <- rbind(newdata, paid)
}


free <- free[which(free$second_category != "Entertainment"), ]

#Смотрим уникальных издателей
free_uniq <- unique(free$seller)
paid_uniq <- unique(paid$seller)
free_sellers <- data.frame()
paid_sellers <- data.frame()

#Выбираем из датасета все приложения издателя
for (i in 1:length(free_uniq)){
  newdata <- apps[ which(apps$seller == free_uniq[[i]]), ]
  free_sellers <- rbind(newdata, free_sellers)
}
for (i in 1:length(paid_uniq)){
  newdata <- apps[ which(apps$seller == paid_uniq[[i]]), ]
  paid_sellers <- rbind(newdata, paid_sellers)
}


#Функция для подсчета частот
freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

#Сколько приложений у каждого издателя
freqfunc(paid_sellers$seller, 100)
freqfunc(free_sellers$seller, 100)
apps2 <- apps[ which((apps$seller == "Rovio Entertainment Ltd")), ]


#Сколько в топе игр от новых компаний (скажем, у кого 1 или 2 игры)?
sellers_free <- data.frame()
sellers_paid <- data.frame()
for (i in 1:100){
  #заходим внутрь бесплатных
  current_page <- readUrl(as.character(free_names[i]))
  seller <- unlist(strsplit(current_page %>% html_nodes("#title h2") %>% html_text(),"By "))
  seller <- seller[seller!=""]
  
  newdata <- apps[ which(apps$seller == seller), ]
  sellers_free <- rbind(newdata, sellers_free)
  
  #заходим внутрь платных
  current_page <- readUrl(as.character(paid_names[i]))
  seller <- unlist(strsplit(current_page %>% html_nodes("#title h2") %>% html_text(),"By "))
  seller <- seller[seller!=""]
  
  newdata <- apps[ which(apps$seller == seller), ]
  sellers_paid <- rbind(newdata, sellers_paid)
}

sellers_uniq_paid <- data.frame()
sellers_uniq_free <- data.frame()

#Смотрим уникальные приложения
seller_paid_ids <- unique(sellers_paid$id)
for (i in 1:length(seller_paid_ids)){
  newdata <- apps[ which(sellers_paid$id == seller_paid_ids[[i]]), ]
  sellers_uniq_paid <- rbind(newdata, sellers_uniq_paid)
}
seller_free_ids <- unique(sellers_free$id)
for (i in 1:length(seller_free_ids)){
  newdata <- apps[ which(sellers_free$id == seller_free_ids[[i]]), ]
  sellers_uniq_free <- rbind(newdata, sellers_uniq_free)
}

freqfunc(sellers_uniq_free$seller, 1000)
freqfunc(sellers_uniq_paid$seller, 1000)


#смотрим, сколько 
tm2 <- as.POSIXct("31122014", format = "%d%m%Y")
