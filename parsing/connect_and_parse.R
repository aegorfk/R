library(rvest)
library(selectr)
library(httr)
library(XML)
library(rmongodb)
library(jsonlite)


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
}
  )
}

apps_all <- readRDS("Education.RDS")
apps_all <- apps_all[1:3]
apps_all$app_link <- as.character(apps_all$app_link)
apps_all$app_name <- as.character(apps_all$app_name)
apps_all$V3 <- as.character(apps_all$V3)
mongo <- mongo.create(host = "rkatelyapina9.cloudapp.net",name = "", db = "test")
#Проверка подключения
mongo.is.connected(mongo)


for (a in 1:nrow(apps_all)) {
  
  rating_1 <- NA
  rating_2 <- NA
  rating_3 <- NA
  review_1 <- NA
  review_2 <- NA
  review_3 <- NA
  total_reviews <- NA
  current_reviews <- NA
  gorizontal_screenshots <- NA
  vertical_screenshots <- NA
  
  current_page <- readUrl(apps_all$app_link[a])
  
  if(!is.na(current_page)){
    #ID
    id <- strsplit(as.character(apps_all[a,2]),"id") 
    
    #Name from the link
    name <- strsplit(unlist(id[[1]][1]),"/")
    region <- name[[1]][4] #apps' market
    name <- name[[1]][6] #apps' name
    id <- strsplit(unlist(id[[1]][2]), "[^0-9]+")
    id <- id[[1]][1] #ID
    
    
    #App's screenshots
    vertical_screenshots <- current_page %>% html_nodes(".portrait") %>% html_attr("src") #vertical scrins
    gorizontal_screenshots <- current_page %>% html_nodes(".landscape") %>% html_attr("src") #gorizontal scrins
    all_screenshots <- append(vertical_screenshots, gorizontal_screenshots)
    
    #App's icon link
    icon <- current_page %>% html_nodes("#left-stack .artwork") %>% html_nodes("img") %>% html_attr("src-swap-high-dpi")
    icon <- icon[1]
    
    #App's main category
    primary_category <- current_page %>% html_nodes(".genre a span") %>% html_text()
    if(length(primary_category)!=0){
      if(primary_category != apps_all[a,3]) { second_category <-  apps_all[a,3] }
      else {second_category <-  NA}
    } else primary_category <- NA
    #App's release date or update date
    release <- current_page %>% html_nodes(".release-date span") %>% html_text()
    if(grepl("Updated",release[1])) 
    {
      update <- as.Date.character(release[2], format="%b %d, %Y")
      release <- NA
    } else {release <- as.Date.character(release[2], format="%b %d, %Y")
            update <- NA
    }
    
    #App's version
    version <- current_page %>% html_nodes(".release-date+ li .label+ span") %>% html_text()
    
    #Site
    site <- current_page %>% html_nodes(".see-all:nth-child(1)") %>% html_attr("href")
    
    #Support
    support <- current_page %>% html_nodes(".see-all+ .see-all") %>% html_attr("href")
    if(length(support) == 0) support <- NA
    
    #App's size
    size <- unlist(strsplit((current_page %>% html_nodes("#left-stack .list") %>% html_nodes("li"))[5] %>% html_text(),"Size: "))
    size <- unique(na.omit(as.numeric(unlist(strsplit(unlist(size), "[^0-9]+")))))
    size <- as.numeric(paste(size[1], ".", size[2], sep=""))
    
    #Apple Watch
    watch <- unlist(strsplit(current_page %>% html_nodes("#watch-screenshots-swoosh h2") %>% html_text(),"Apple Watch: "))
    if (length(watch) == 0)  {watch <- FALSE}  else watch <- TRUE
    
    #Languages
    language <- unlist(strsplit(current_page %>% html_nodes(".language") %>% html_text(),"Languages: |Language: "))
    if (!is.null(language)) language <- language[language!=""]
    
    #Seller
    seller <- unlist(strsplit(current_page %>% html_nodes("#title h2") %>% html_text(),"By "))
    if (!is.null(seller)) {seller <- seller[seller!=""]}
    
    #Age
    rated <- unlist(strsplit(current_page %>% html_nodes(".app-rating a") %>% html_text(),"Rated "))
    if ((rated != "Not yet rated")&&(!is.null(rated))) {rated <- unique(na.omit(as.numeric(unlist(strsplit(unlist(rated), "[^0-9]+")))))}
    
    #Compatibility
    compatible <- unlist(strsplit(current_page %>% html_nodes("#left-stack p") %>% html_text(),"Compatibility: "))
    requires <- unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(compatible)), ""))))
    requires <- paste(requires[1], ".", requires[2], sep="")
    if (!is.null(compatible)) 
    {
      compatible <- unlist(strsplit(compatible,"with | This app is optimized for "))
      if ((!is.null(compatible[3]))&grepl("5",compatible[3]))  {optimized_iPhone_5 <- TRUE}  else optimized_iPhone_5 <- FALSE
      if ((!is.null(compatible[3]))&grepl("6",compatible[3]))  {optimized_iPhone_6 <- TRUE}  else optimized_iPhone_6 <- FALSE
      if ((!is.null(compatible[3]))&grepl("Plus",compatible[3]))  {optimized_iPhone_6_Plus <- TRUE}  else optimized_iPhone_6_Plus <- FALSE
    }
    
    if (!is.null(compatible)) 
    {
      compatible <- unlist(strsplit(compatible[2],", |, and "))
      if ((compatible[1]=="iPhone")||(compatible[1]=="iPhone."))  {compatible_iPhone <- TRUE}  else compatible_iPhone <- FALSE
      if ((!is.null(compatible[2]))&&(!is.na(compatible[2]))&&((compatible[2]=="iPad")||(compatible[2]=="iPad.")))  {compatible_iPad <- TRUE}  else compatible_iPad <- FALSE
      if ((!is.null(compatible[3]))&&(!is.na(compatible[3]))&&(compatible[3]=="iPod touch."))  {compatible_iPod <- TRUE}  else compatible_iPod <- FALSE
    }
    
    #Description
    description <- current_page %>% html_nodes(".product-review p") %>% html_text()
    updated <- description[2]
    description <- description[1]
    
    #Reviews
    review <- unlist(strsplit(current_page %>% html_nodes(".customer-review .content") %>% html_text(),"\n    "))
    if (!is.null(review)) { review <- unlist(strsplit(review,"\n  "))
                            review <- review[review!="\n  "]}
    if (!is.null(review[1])) { review_1 <- review[1]}
    if (!is.null(review[2])) { review_2 <- review[2]}
    if (!is.null(review[3])) { review_3 <- review[3]}  
    
    #In app purchases
    purchases_names <- current_page %>% html_nodes(".in-app-title") %>% html_text()
    purchases_prices <- current_page %>% html_nodes(".in-app-price") %>% html_text()
    
    
    #Rating
    current_rating <- unlist(strsplit(current_page %>% html_nodes(".rating:nth-child(3)") %>% html_attr("aria-label")," stars| "))
    #Total rating
    total_rating <- unlist(strsplit(current_page %>% html_nodes(".rating~ .rating") %>% html_attr("aria-label")," stars| "))
    
    if((!is.null(total_rating))&&(!is.null(current_rating))){
      if (length(current_rating) == 7){
        current_reviews <- as.numeric(current_rating[6])
        current_rating <- as.numeric(current_rating[1]) + 0.5
      }  else {
        if (!is.null(current_rating[3])) current_reviews <- as.numeric(current_rating[3])
        if (!is.null(current_rating[1])) current_rating <- as.numeric(current_rating[1])
      }
      
      if (length(total_rating) == 7){
        total_reviews <- as.numeric(total_rating[6])
        total_rating <- as.numeric(total_rating[1]) + 0.5
      }  else {
        if (!is.null(total_rating[3])) total_reviews <- as.numeric(total_rating[3])
        if (!is.null(total_rating[1])) total_rating <- as.numeric(total_rating[1])
      }
    }else 
    {
      if (length(current_rating) == 7){
        total_reviews <- as.numeric(current_rating[6])
        total_rating <- as.numeric(current_rating[1]) + 0.5
      }  else {
        if (!is.null(current_rating[3])) total_reviews <- as.numeric(current_rating[3])
        if (!is.null(current_rating[1])) total_rating <- as.numeric(current_rating[1])
      }   
    }
    
    rating <- unlist(strsplit(current_page %>% html_nodes(".customer-review .rating") %>% html_attr("aria-label")," stars| star"))
    if (!is.null(rating[1])) {rating_1 <- rating[1]}  
    if (!is.null(rating[2])) {rating_2 <- rating[2]}  
    if (!is.null(rating[3])) {rating_3 <- rating[3]}  
    
    
    #Price
    price <- current_page %>% html_nodes(".price") %>% html_text()
    
    #Another apps by this publisher
    another_apps <- current_page %>% html_nodes("#left-stack .name") %>% html_attr("href")
    ids<-NULL
    if(length(another_apps)!=0){
      for (i in 1:length(another_apps)){
        #ID
        id2 <- strsplit(another_apps[[i]],"id") 
        id2 <- strsplit(unlist(id2[[1]][2]), "[^0-9]+")
        id2 <- id2[[1]][1]
        ids <- append(id2, ids)
      }
    }
    
    #Also bought
    simple_apps <- current_page %>% html_nodes(".name") %>% html_attr("href")
    ids_simple <- NULL
    if(length(simple_apps)!=0){
      for (i in 1:length(simple_apps)){
        #ID
        id2 <- strsplit(simple_apps[[i]],"id") 
        id2 <- strsplit(unlist(id2[[1]][2]), "[^0-9]+")
        id2 <- id2[[1]][1]
        ids_simple <- append(id2, ids_simple)
      }
    }
    
    if((mongo.is.connected(mongo) == TRUE)&&(!is.na(current_page))) {
      if(length(gorizontal_screenshots)==0) gorizontal_screenshots <- NA
      if(length(vertical_screenshots)==0) vertical_screenshots <- NA
      if(length(ids)==0) ids <- NA
      if(length(ids_simple)==0) ids_simple <- NA
      if(length(purchases_prices)==0) purchases_prices <- NA
      if(length(purchases_names)==0) purchases_names <- NA
      if(length(rating_1)==0) rating_1 <- NA
      if(length(rating_2)==0) rating_2 <- NA
      if(length(rating_3)==0) rating_3 <- NA
      if(length(release)==0) release <- NA
      if(length(review_1)==0) review_1 <- NA
      if(length(review_2)==0) review_2 <- NA
      if(length(review_3)==0) review_3 <- NA
      if(length(simple_apps)==0) simple_apps <- NA
      if(length(support)==0) support <- NA
      if(length(total_rating)==0) total_rating <- NA
      if(length(total_reviews)==0) total_reviews <- NA
      if(length(updated)==0) updated <- NA
      if(length(current_rating)==0) current_rating <- NA
      if(length(rating)==0) rating <- NA
      if(length(review)==0) review <- NA
      if(length(another_apps)==0) another_apps <- NA
      
      list <- list(id = id, language = language, region = region, link = apps_all[a,2],
                   name = apps_all[a,1], link_name=name, primary_category = primary_category,
                   second_category = second_category, current_rating = current_rating,
                   current_reviews = current_reviews, description = description, icon = icon, price = price,
                   rated = rated, release = release, requires = requires, seller = seller, site = site,
                   support = support, size = size, total_rating = total_rating, total_reviews = total_reviews, 
                   update = as.POSIXct(update, tz='MSK'), updated = updated, version = version, compatible_iPad = compatible_iPad,
                   compatible_iPhone = compatible_iPhone, compatible_iPod = compatible_iPod, watch = watch,
                   optimized_iPhone_5 = optimized_iPhone_5, optimized_iPhone_6 = optimized_iPhone_6, 
                   optimized_iPhone_6_Plus = optimized_iPhone_6_Plus, all_screenshots = all_screenshots,
                   gor_screenshots = length(gorizontal_screenshots), vertical_screenshots = length(vertical_screenshots),
                   ids = ids, ids_simple = ids_simple, purchases_names = purchases_names, 
                   purchases_prices = purchases_prices, rating_1 = rating_1, rating_2 = rating_2, rating_3 = rating_3,
                   review_1 = review_1, review_2 = review_2, review_3 = review_3)
      bson <- mongo.bson.from.list(list)
      mongo.insert(mongo, "test.apps", bson)
      message(paste("Added URL:", apps_all$app_link[a]))
    }
  }}
#rm(compatible,current_page,current_rating,description,language,price,rated,rating,requires,review,seller,size,total_rating,watch, current_reviews, total_reviews,another_apps,id2,ids,ids_simple,simple_apps)
#apps_all[ which(apps_all$app_link=="https://itunes.apple.com/us/app/bach-j.-s.-well-tempered-clavier/id779007949?mt=8"), ]