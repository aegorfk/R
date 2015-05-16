# подключим модули
# install.packages('foreach', repos='http://cran.us.r-project.org')
# install.packages('rvest', repos='http://cran.us.r-project.org')
# install.packages('doSNOW', repos='http://cran.us.r-project.org')
# install.packages('selectr', repos='http://cran.us.r-project.org')
# install.packages('httr', repos='http://cran.us.r-project.org')
# install.packages('XML', repos='http://cran.us.r-project.org')
# install.packages('doParallel')


library(foreach)
library(rvest)
library(selectr)
library(doSNOW)
library(httr)
library(XML)
library(doParallel)


#Взяли произвольную стартовую страницу, парсим категории приложений, смотрим ссылки
apple_start_page <- html("http://itunes.apple.com/us/genre/ios-reference/id6006?mt=8")
apple_category <- apple_start_page %>% html_nodes(".top-level-genre") %>% html_text()
apple_category_link<- apple_start_page %>% html_nodes(".top-level-genre") %>% html_attr("href")
categories <- as.data.frame(cbind(as.character(apple_category), apple_category_link))




#Параллелим на несколько машин
category <- c("Books", "Business", "Catalogs", "Education")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-books/id6018?mt=8", "https://itunes.apple.com/us/genre/ios-business/id6000?mt=8", "https://itunes.apple.com/us/genre/ios-catalogs/id6022?mt=8", "https://itunes.apple.com/us/genre/ios-education/id6017?mt=8")
categories <- data.frame(category, apple_category_link)


category <- c("Entertainment", "Finance", "Food & Drink", "Games")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-entertainment/id6016?mt=8", "https://itunes.apple.com/us/genre/ios-finance/id6015?mt=8", "https://itunes.apple.com/us/genre/ios-food-drink/id6023?mt=8", "https://itunes.apple.com/us/genre/ios-games/id6014?mt=8")
categories <- data.frame(category, apple_category_link)


category <- c("Health & Fitness", "Lifestyle", "Medical", "Music")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-health-fitness/id6013?mt=8", "https://itunes.apple.com/us/genre/ios-lifestyle/id6012?mt=8", "https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8", "https://itunes.apple.com/us/genre/ios-music/id6011?mt=8")
categories <- data.frame(category, apple_category_link)


category <- c("Navigation", "News", "Newsstand", "Photo & Video")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-navigation/id6010?mt=8", "https://itunes.apple.com/us/genre/ios-news/id6009?mt=8", "https://itunes.apple.com/us/genre/ios-newsstand/id6021?mt=8", "https://itunes.apple.com/us/genre/ios-photo-video/id6008?mt=8")
categories <- data.frame(category, apple_category_link)

category <- c("Productivity", "Reference", "Social Networking", "Sports")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-productivity/id6007?mt=8", "https://itunes.apple.com/us/genre/ios-reference/id6006?mt=8", "https://itunes.apple.com/us/genre/ios-social-networking/id6005?mt=8", "https://itunes.apple.com/us/genre/ios-sports/id6004?mt=8")
categories <- data.frame(category, apple_category_link)


category <- c("Travel", "Utilities", "Weather")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-travel/id6003?mt=8", "https://itunes.apple.com/us/genre/ios-utilities/id6002?mt=8", "https://itunes.apple.com/us/genre/ios-weather/id6001?mt=8")
categories <- data.frame(category, apple_category_link)



alfabet <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","*")
apps <- data.frame(app_name = character(0), app_link = character(0))


Sys.time()
foreach(i=1:4) %dopar%  {
  require("rvest")
  require("selectr")
  require("XML")
  require("httr")
  for (l in 1:length(alfabet)){
    
   tryCatch(current_page <- html(paste(categories[i,2], "&letter=", alfabet[l], sep="")), error = function(c) {
      
      message(paste("Скрипт оборвался на ", categories[i,2], "&letter=", alfabet[l], sep=""))
      Sys.sleep(10)
      current_page <- html(paste(categories[i,2], "&letter=", alfabet[l], sep=""))
    })
    
    
    #делаем ссылку на буквы
    a <-  1
    while(!is.null(current_page %>% html_nodes(".paginate-more") %>% html_text())!=FALSE) {
      
      tryCatch(current_page <- html(paste(categories[i,2], "&page=", l, sep="")), error = function(c) {
        
        message(paste("Скрипт оборвался на ", categories[i,2],  alfabet[l], l, sep=""))
        Sys.sleep(10)
        current_page <- html(paste(categories[i,2], "&page=", l, sep=""))
      })
      
      
      
      app_name <- current_page %>% html_nodes("#selectedcontent a") %>% html_text()
      app_link <- current_page %>% html_nodes("#selectedcontent a") %>% html_attr("href")
      app_names_at_page <- as.data.frame(cbind(app_name, app_link))
      
      a <- a + 1 
      for (n in 1:nrow(app_names_at_page)) app_names_at_page[n,3] <- as.character(categories[i,1])
      apps <- rbind(apps, app_names_at_page)
      
    }
    saveRDS(apps, file = paste(categories[i,1], "_", alfabet[l], ".RDS", sep=""))
    paste("Скачали", categories[i,1], "_", alfabet[l], sep="")
    Sys.time()
  }
  saveRDS(apps, file = paste(categories[i,1], ".RDS", sep=""))
  paste("Скачали", categories[i,1], sep="")
  Sys.time()
}

stopCluster(cl)
