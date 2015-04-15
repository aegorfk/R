# Download packages
#install.packages("rvest")
#install.packages("shiny")

# Init packages
library("rvest")
library("shiny")


lego_movie <- html("http://www.imdb.com/title/tt1490017/")
lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()


lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()


x = scp("apptractorhad.cloudapp.net", "/", "AppTractor2015", user="azureuser")


runExample("01_hello") # a histogram
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer