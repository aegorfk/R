#Read data from file:
data <- read.csv(file="test.csv", header = FALSE, sep = ",")
colnames(data) <- c("okdp", "regionCode_supplier", "signDate", "executionDate", "price", "supplier_regNum", "supplier_inn","supplier_name")
data$okdp <- as.character(data$okdp)
data$regionCode_supplier <- as.numeric(as.character(data$regionCode_supplier))
data$signDate <- as.Date(data$signDate)
data$executionDate <- as.Date(data$executionDate)
data$price <- as.character(data$price)
data$supplier_regNum <- as.character(data$supplier_regNum)
data$supplier_inn <- as.character(data$supplier_inn)
data$supplier_name <- as.character(data$supplier_name)


#?????????????? ?????? ???????????????? ????????????
freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

#?????????????????? ?????????? ???????????????????? ???????????? ??????????????????
freqfunc(data$supplier_inn, 10)
#apps2 <- apps[ which((apps$second_category != "Books")), ]


library(tm)
library(ggplot2)
library(lsa)

# 1. Prepare mock data
text <- c("transporting food by cars will cause global warming. so we should go local.",
          "we should try to convince our parents to stop using cars because it will cause global warming.",
          "some food, such as mongo, requires a warm weather to grow. so they have to be transported to canada.",
          "a typical electronic circuit can be built with a battery, a bulb, and a switch.",
          "electricity flows from batteries to the bulb, just like water flows through a tube.",
          "batteries have chemical energe in it. then electrons flow through a bulb to light it up.",
          "birds can fly because they have feather and they are light.", "why some birds like pigeon can fly while some others like chicken cannot?",
          "feather is important for birds' fly. if feather on a bird's wings is removed, this bird cannot fly.")
view <- factor(rep(c("view 1", "view 2", "view 3"), each = 3))
df <- data.frame(text, view, stringsAsFactors = FALSE)

# prepare corpus
corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus  # check corpus

## A corpus with 9 text documents
