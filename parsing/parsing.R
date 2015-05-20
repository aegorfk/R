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


category <- c("Health & Fitness", "Lifestyle", "Medical", "Music")
apple_category_link <- c("https://itunes.apple.com/us/genre/ios-health-fitness/id6013?mt=8", "https://itunes.apple.com/us/genre/ios-lifestyle/id6012?mt=8", "https://itunes.apple.com/us/genre/ios-medical/id6020?mt=8", "https://itunes.apple.com/us/genre/ios-music/id6011?mt=8")
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

stopCluster(cl)
