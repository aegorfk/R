# В рамках данного задания проведено небольшое 
# эконометрическое исследование на данных Российского мониторинга 
# экономического положения и здоровья населения (РМЭЗ, он же RLMS). 
# Целью данного исследования является выявление факторов, влияющих на 
# размер заработной платы.


# devtools::install_github("bdemeshev/rlms")
library("rlms") # загрузка данных в формате rlms (spss)
library(plyr)
require(foreign)
library("ggplot2")
require(foreign)
require(MASS)


h_old <- read.rlms("r22i_os25a.sav")


# Формируем массив данных с отобранными переменными:
# rj13.2 - Заработная плата на основе опроса
# rh6  - Год рождения респондента
# r_int_y  - Год проведения опроса
# rh5 - Пол 
# r_diplom - Образование
# status - тип населенного пункта (размер населенного пункта может влиять положительно на средний уровень зарплат в нем)
# rj161.3y  - Трудовой стаж в годах на момент опроса (стаж может влиять на заработную плату положительно)

h <- subset(h_old, select=c(rj13.2 , rh6, r_int_y,  rh5, r_diplom, status, rj161.3y))
rm(h_old)
h <- na.omit(h)
h <- rename(h, c(rj13.2="payroll", rh6 = "birth", r_int_y = "age", rh5 = "sex", r_diplom = "education",  rj161.3y = "experience"))
h$age <- h$age - h$birth
h$birth <- NULL

saveRDS(h, file = "r22i_os25a.RDS") # сохраняем всё ценное в файл
h <- readRDS("r22i_os25a.RDS") # читаем из файла что там есть




for (i in 1:nrow(h)) h[i,7]  = ifelse((h[i,4]=="окончил 0 - 6 классов") || (h[i,5]=="незаконч среднее образование (7 - 8 кл)")|| (h[i,5]=="незаконч среднее образование (7 - 8 кл) + что-то еще"), 1, 0)
for (i in 1:nrow(h)) h[i,8]  = ifelse(h[i,4]=="законч среднее образование", 1, 0)
for (i in 1:nrow(h)) h[i,9]  = ifelse(h[i,4]=="законч среднее специальное образование", 1, 0)
for (i in 1:nrow(h)) h[i,10]  = ifelse(h[i,4]=="законч высшее образование и выше", 1, 0)
h <- rename(h, c(V7 ="ed_1", V8= "ed_2", V9 = "ed_3",  V10 = "ed_4"))


for (i in 1:nrow(h)) h[i,11]  = ifelse(h[i,5]=="областной центр ", 1, 0)
for (i in 1:nrow(h)) h[i,12]  = ifelse(h[i,5]=="город", 1, 0)
for (i in 1:nrow(h)) h[i,13]  = ifelse(h[i,5]=="ПГТ", 1, 0)
for (i in 1:nrow(h)) h[i,14]  = ifelse(h[i,5]=="село", 1, 0)
h <- rename(h, c(V11 = "st_1", V12 = "st_2", V13 = "st_3",  V14 = "st_4"))


for (i in 1:nrow(h)) h[i,15]  = ifelse(h[i,3]=="МУЖСКОЙ", 1, 0)
for (i in 1:nrow(h)) h[i,16]  = ifelse(h[i,3]=="ЖЕНСКИЙ", 1, 0)
h <- rename(h, c(V15 = "male", V16 = "female"))
h$status <- NULL
h$education <- NULL
h$sex <- NULL


summary(h)

 

sd(h$payroll)
sd(h$age)
sd(h$experience)


model <- lm(payroll ~ age + male + ed_2 + ed_3 + ed_4 + st_2  + st_3	+ st_4 + experience, data=h)

model2 <- rlm(payroll ~ age + male + ed_2 + ed_3 + ed_4 + st_2  + st_3  + st_4 + experience, data=h) 


glm.out <- step(glm(Банкрот ~ Ликвидность.активов + Рентабельность.активов + Доходность.активов + Оборачиваемость.активов, family=binomial, data=training_data))

rmarkdown::render("RLMS.Rmd")


