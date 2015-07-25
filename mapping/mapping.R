# В датасете ниже лежат отрезки (stepid), из которых составляются полилинии 
# (треки перемещений транспорта — routeid). Координаты отрезков — географические (lon, lat). 
# Дополнительные категории в датасете: age_group, foreign — характеристики перемещающегося 
# (возраст, приезжий или нет). Возможна ситуация, при которой отрезки разной длины могут 
# накладываться друг на друга, что для дальнейшей обработки неприемлимая ситуация. 
# Необходимо создать тест на выявление такой ситуации в данном датасете. 
# К тесту необходимо приложить методологию.


data <- read.csv("segments_1000.csv")
data$age_group <- as.factor(data$age_group)
data$foreign<- as.factor(data$foreign)
data$x <- data$end_lon - data$start_lon
data$y <- data$end_lat - data$start_lat
newdata <- data.frame(x= numeric(0)) # сюда заносим отрезки с одинаковым routeid
#newdata2 <- data.frame(x= numeric(0)) # технический data frame для сравнения, копия newdata2
newdata3 <- data.frame(x= numeric(0)) # сюда заносим пересекающиеся пары

# Для определения того, что отрезки наслаиваются друг на друга, достаточно знать, 
# что отрезки параллельны и хотя бы один из концов одного отрезка находится между 
# концами другого. Дла первого свойства необходимо, чтобы соответствующие координаты
# отрезков были пропорциональны. Для проверки второго условия, я находила угол
# между 2 векторами, которые были составлены из конца 1 отрезка и 2 концов другого отрезка.
for(a in 2:max(data$route_id))
{
  
  newdata <- data[ which(data$route_id==9),]
  
  
for (i in 1: nrow(newdata))
{
  for (j in 1: nrow(newdata))
  {
    #Проверка на параллельность
    if(i!=j) #исключаем сравнение одинаковых векторов
    {
      #проверяем коллинеарность векторов
      if((newdata$x[i]/newdata$x[j])==(newdata$y[i]/newdata$y[j]))
      {
        
        
        #Координаты 1 вектора
        x1 <- newdata$start_lon[i] - newdata$start_lon[j]
        y1 <- newdata$start_lat[i] - newdata$start_lat[j]
        #Координаты 2 вектора
        x2 <- newdata$end_lon[i] - newdata$start_lon[j]
        y2 <- newdata$end_lat[i] - newdata$start_lat[j]
        
        a <- (x1*x2 + y1*y2)/((sqrt(x1^2+y1^2))*(sqrt(x2^2+y2^2)))
        
        #Координаты 3 вектора
        x3 <- newdata$start_lon[i] - newdata$end_lon[j]
        y3 <- newdata$start_lat[i] - newdata$end_lat[j]
        #Координаты 4 вектора
        x4 <- newdata$end_lon[i] - newdata$end_lon[j]
        y4 <- newdata$end_lat[i] - newdata$end_lat[j]
        
        b <- (x3*x4 + y3*y4)/((sqrt(x3^2+y4^2))*(sqrt(x3^2+y4^2)))
        if((a==-1)||(b==-1)) 
        {
          newdata3 <- rbind(newdata3, newdata[j,])
          newdata3 <- rbind(newdata3, newdata[j,])
        }
        
      
        }
      }
    }
  }  




}
  
  
  





#скалярное произведение

#Координаты 1 вектора
x1 <- newdata$start_lon[i] - newdata$start_lon[j]
y1 <- newdata$start_lat[i] - newdata$start_lat[j]
#Координаты 2 вектора
x2 <- newdata$end_lon[i] - newdata$start_lon[j]
y2 <- newdata$end_lat[i] - newdata$start_lat[j]

a <- (x1*x2 + y1*y2)/((sqrt(x1^2+y1^2))*(sqrt(x2^2+y2^2)))

#Координаты 3 вектора
x1 <- newdata$start_lon[i] - newdata$end_lon[j]
y1 <- newdata$start_lat[i] - newdata$end_lat[j]
#Координаты 4 вектора
x2 <- newdata$end_lon[i] - newdata$end_lon[j]
y2 <- newdata$end_lat[i] - newdata$end_lat[j]

b <- (x3*x4 + y3*y4)/((sqrt(x3^2+y4^2))*(sqrt(x3^2+y4^2)))
if((a==-1)||(b==-1)) 
{
  newdata3 <- rbind(newdata3, newdata[j,])
  newdata3 <- rbind(newdata3, newdata[j,])
}



