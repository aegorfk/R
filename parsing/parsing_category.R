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



apple_start_page <- html("http://itunes.apple.com/us/genre/ios-reference/id6006?mt=8")
apple_category <- apple_start_page %>% html_nodes(".top-level-genre") %>% html_text()
apple_category_link<- apple_start_page %>% html_nodes(".top-level-genre") %>% html_attr("href")
categories <- as.data.frame(cbind(as.character(apple_category), apple_category_link))



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
})}



cl <- makeCluster(4, outfile="") # Register cluster
registerDoParallel(cl)

foreach(i=1:4) %dopar% {
  require("rvest")
  require("selectr")
  require("XML")
  require("httr")
  
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
  }
  apps$app_name <- as.character(apps$app_name)
  apps$app_link <- as.character(apps$app_link)
  saveRDS(apps, file = paste(categories[i,1], ".RDS", sep=""))
  rm(apps)
  apps <- data.frame(app_name = character(0), app_link = character(0))
}




#ЭТО НЕ ТОТ ПАРСЕР
#Парсер каждой странички
for (i in 1:nrow(categories))  {
  require("rvest")
  require("selectr")
  require("XML")
  require("httr")
  
  apps_all <- readRDS("News.RDS")
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
  #cl <- makeCluster(4, outfile="") # Register cluster
  #registerDoParallel(cl)
  #foreach (a = 1:170118) %dopar% {
  for (a in 1:29832) {
    
    require("rvest")
    require("selectr")
    require("XML")
    require("httr")
    current_page <- readUrl(apps_all[a,2])
    
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
    
  }
  
  stopCluster(cl)
  ##ДО СЮДА
  
  
  saveRDS(apps_all, file = paste(categories[1,1],"_apps", ".RDS", sep=""))
  rm(apps_all)
}



stopCluster(cl)
saveRDS(apps_all, file = paste("News", ".RDS", sep=""))

