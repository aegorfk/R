# Esli russkie bukvi prevratilitis v krakozyabry,
# to File - Reopen with encoding... - UTF-8 - Set as default - OK


#install.packages("foreign")
library("foreign") # конвертация файлов Stata
#install.packages("dplyr")
library("dplyr") # манипуляции с данными
#install.packages("erer")
library("erer") # расчет предельных эффектов
#install.packages("vcd")
library("vcd") # графики для качественных данных
#install.packages("ggplot2")
library("ggplot2") # графики
#install.packages("reshape2")
library("reshape2") # манипуляции с данными
#install.packages("AUC")
library("AUC") # для ROC кривой


# читаем данные
women <- read.dta("womenwk_data.dta")

# при загрузке файлов R автоматом переделывает все строковые переменные в факторные
# эта шаманская команда просит R так не делать :)
options(stringsAsFactors=FALSE)

# смотрим на набор данных
glimpse(women)

#создаем переменную work
for (i in 1:nrow(women)) women[i,7]  = ifelse(women[i,5]==women[i,6], 1, 0)
for (i in 1:nrow(women)) women[i,7]  = ifelse(is.na(women[i,7]), 0, 1)
names(women) <- c("age","education","married", "children","wagefull","wage", "work")


# объясняем R, какие переменные считать факторными
women <- mutate(women,married=as.factor(married),work=as.factor(work))
summary(women)


# мозаичный график
mosaic(data=women,~married+children+work,shade=TRUE)

# график-виолончель
qplot(data=women,x=work,y=age,geom="violin")

# и "ящик с усами"
qplot(data=women,x=work,y=age,geom="boxplot")

# два варианта сглаженной функции плотности
qplot(data=women,x=age,y=..count..,fill=work,geom="density",position="stack")
qplot(data=women,x=age,y=..count..,fill=work,geom="density",position="fill")



# Оценивание логит и пробит моделей
m_logit <- glm(data=women, work~age + education + married + children,
               family=binomial(link="logit"),x=TRUE)
m_probit <- glm(data=women, work~age + education + married + children,
                family=binomial(link="probit"),x=TRUE)

# отчеты об оценке моделей
summary(m_logit)
#Все переменные в логит-модели значимы
summary(m_probit)




# оценка ковариационной матрицы оценок коэффициентов
vcov(m_logit)

# создаём новый массив данных для прогнозирования
newdata_women <- data.frame(women$age, women$education,	women$married,	women$children)
names(newdata_women) <- c("age","education","married", "children")


# прогнозируем по логит модели 
pr_logit <- predict(m_logit,newdata_women,se=TRUE)
# соединим прогнозы и новый массив данных в единую табличку:
newdata_pr <- cbind(newdata_women,pr_logit)
head(newdata_pr) # глянем на начало таблички

# применив логистическую функцию распределения получим границы доверительного интервала
newdata_pr <- mutate(newdata_pr,prob=plogis(fit),
                     left_ci=plogis(fit-1.95*se.fit),
                     right_ci=plogis(fit+1.95*se.fit))
head(newdata_pr) # глянем на результат

# посмотрим на графике как меняется доверительный интервал для вероятности
qplot(data=newdata_pr,x=age,y=prob,geom="line") +
  geom_ribbon(aes(ymin=left_ci,ymax=right_ci),alpha=0.2)


# проведем LR тест
# R при построении разных моделей автоматом использует максимальное количество полных наблюдений
# поэтому часто выходит, что ограниченная и неограниченная модель
# оцениваются на разном наборе данных
# но в таком случае их нельзя сравнивать с помощью LR теста
# поэтому мы сначала создадим набор данных t2 без пропущенных значений
# и на нем оценим короткую и длинную модели
# H0: beta(pclass)=0, beta(fare)=0

# women2 <- data.frame(women$age,women$education,women$married, as.numeric(women$children)==0, women$work)
# names(women2) <- c("age","education","married", "children", "work")
# women2$children <- 0
# 
# 
# 
# # оцениваем ограниченную модель
# m_logit2 <- glm(data=women2, work ~  age + education + married + children, family=binomial(link="logit"),x=TRUE)
# # проводим LR тест
# lrtest(m_logit,m_logit2)


maBina(m_logit) # предельные эффекты 

# усредненные предельные эффекты по всем жещинам
maBina(m_logit,x.mean = FALSE)

# обычный МНК
m_ols <- lm(data=women, as.numeric(work)~age + education + married + children)
summary(m_ols)

# прогнозы по обычному МНК
pr_ols <- predict(m_ols,newdata_women)
head(pr_ols)

# ROC кривая
# спрогнозируем скрытую переменную для исходного набора данных
pr_t <- predict(m_logit,women,se=TRUE)
# соединим прогнозы с исходным набором данных
women <- cbind(women,pr_t)
# применим логистическую функцию распределения, чтобы получить вероятности
women <- mutate(women,prob=plogis(fit))


# получим все данные для ROC кривой:
roc.data <- roc(women$prob,women$work)
str(roc.data)

# три графика для выбора порога отсечения
# по горизонтали --- пороги, по вертикали --- чувствительность 
# чувствительность = число верно предсказанных значений / общее количество женщин
qplot(x=roc.data$cutoffs,y=roc.data$tpr,geom="line", xlab = "Порог отсечения", ylab = 'Доля верно классифицированных работающих женщин')

# по горизонтали --- пороги, по вертикали --- процент ложноположительных прогнозов
# процент ложно положительных прогнозов =
# число не работающих ошибочно предсказанных работающих/общее число женщин
qplot(x=roc.data$cutoffs,y=roc.data$fpr,geom="line", xlab = "Порог отсечения", ylab = 'Доля не верно классифицированных не работающих женщин')

# по горизонтали --- процент ложноположительных прогнозов
# по вертикали --- чувствительность
qplot(x=roc.data$fpr,y=roc.data$tpr,geom="line", xlab = "Доля верно классифицированных работающих женщин", ylab = 'Доля не верно классифицированных не работающих женщин', main='ROC кривая')


rmarkdown::render("C:/Users/Екатерина/Documents/GitHub/R/logit_probit/logit_probit.Rmd")
