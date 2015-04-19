#install.packages(twitteR, dependencies = TRUE)
library("twitteR")
#install.packages("plyr", dependencies = TRUE)
library("plyr")
#install.packages("stringr", dependencies = TRUE)
library("stringr")
#install.packages("wordcloud", dependencies = TRUE)
library(wordcloud)
#install.packages("tm", dependencies = TRUE)
library(tm)





findScore <- function(sentence, pos.words, neg.words){
  #uses words from corr.dictionaries
  require("stringr")
  #remove punctuation
  sentence <- gsub("[[:punct:]]", "", sentence)
  #remove control characters
  sentence <- gsub("[[:cntrl:]]", "", sentence)
  #remove digits
  sentence <- gsub("\\d+", '', sentence)
  
  tryTolower <- function(x) {
    y <- NA
    try_error <- tryCatch(tolower(x), error = function(e) e)
    if(!inherits(try_error, "error"))
    y <- tolower(x)
    return(y)    
    
  }  
  
  
  sentence <- sapply(sentence, tryTolower)
  
  word.list = str_split(sentence, '\\s+')
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  
  pos.indx <- pos.matches[!is.na(pos.matches)]
  neg.indx <- neg.matches[!is.na(neg.matches)]
  pos.matches = length(pos.indx)
  neg.matches = length(neg.indx)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  score <- sum(pos.matches) - sum(neg.matches)
  
  return(list(score, pos.indx, neg.indx, sentence))
} 




calcStats <- function(scores){
  ntot <- length(scores)
  npos <- sum(scores > 0)
  nzero <- sum(scores == 0)
  nneg <- sum(scores < 0)
  
  if (npos != 0)
    avpos <- sum(scores[scores > 0]) / npos
  else avpos <- 0
  if (nneg != 0)
    avneg <- sum(scores[scores < 0]) / nneg
  else avneg <- 0
  
  return (c(ntot, npos, nzero, nneg, avpos, avneg))
}




setup_twitter_oauth("BfoP1LtBMdghX1ULzfHNI93zI","wNPYpa0hyRQHbAa5cce8KFhLz7AAArle7YK2XNt4vWU96YyYT2","168628773-lhgkkgzHG8R5ZJuagQuWnpPDyHZJmd5I4BIFwJGL","2gROpJPcJLXj1qRe3C7jK4fp7p7pzSJfkCCGU32cVjgRj")
pos <- scan("positive-words.txt", what = "character")
neg <- scan("negative-words.txt", what = "character")



keywords <- "Russia"
nmax <- 180
location <- NULL


today <- Sys.Date()
since <- as.character(today)
until <- as.character(today + 1)


tweets <- searchTwitter(keywords, n = nmax,  since = since, until = until, geocode = location, lang = "en")


tweetdf <- data.frame()
tweetdf <- twListToDF(tweets)


scores <- laply(tweetdf$text, .fun = function(x)
{scl <- findScore(x, pos, neg); return(scl[[1]])})
stat1 <- calcStats(scores)



slices <- c(stat1[2], stat1[3],stat1[4])
lbls <- c("негативных", "нейтральных", "позитивных", stat1[2])
pie(slices, labels = lbls, main="Twitter hashtag sentiment")


#Корпус русского: http://study.mokoron.com/

pos <- read.csv("positive.csv", stringsAsFactors = FALSE, header=FALSE, sep=";")
neg <- read.csv("negative.csv", stringsAsFactors = FALSE, header=FALSE, sep=";")
neg <- rename(neg, c(V1 ="id", V2= "tdate", V3 = "tmane",  V4 = "ttext", V3 = "tmane",  V4 = "ttext", V5 = "ttype",  V6 = "trep", V7 = " tfav",  V8 = "tstcount", V9 = "tfol",  V10 = "tfrien", V11 = "tmane",  V12 = "listcount"))
pos <- rename(pos, c(V1 ="id", V2= "tdate", V3 = "tmane",  V4 = "ttext", V3 = "tmane",  V4 = "ttext", V5 = "ttype",  V6 = "trep", V7 = " tfav",  V8 = "tstcount", V9 = "tfol",  V10 = "tfrien", V11 = "tmane",  V12 = "listcount"))

keywords <- "hse"
nmax <- 180
location <- NULL
today <- Sys.Date()
since <- as.character(today)
until <- as.character(today + 1)

tweets <- searchTwitter(keywords, n = nmax,  since = since, until = until, geocode = location, lang = "ru")
tweetdf <- twListToDF(tweets)

# harvest some tweets
some_tweets = searchTwitter("ВШЭ", n=1500, lang="ru")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())




# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL





