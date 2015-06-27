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
newdata <- data.frame(x= numeric(0))
newdata2 <- data.frame(x= numeric(0))

for(i in 1:max(data$route_id))
{
  newdata <- data[ which(data$route_id==i),]
  if(nrow(newdata)>1)
    {
   for(j in 1:(nrow(newdata)-1))
     {
  
     #Координаты 1 вектора
     x1 <- newdata[j+1,3] - newdata[j,3]
     y1 <- newdata[j+1,4] - newdata[j,4]
     #Координаты 2 вектора
     x2 <- newdata[j+1,6] - newdata[j,3]
     y2 <- newdata[j+1,7] - newdata[j,4]
     
     a <- (x1*x2 + y1*y2)/((sqrt(x1^2+y1^2))*(sqrt(x2^2+y2^2)))
     
     #Координаты 3 вектора
     x3 <- newdata[j+1,3] - newdata[j,6]
     y3 <- newdata[j+1,4] - newdata[j,7]
     #Координаты 4 вектора
     x4 <- newdata[j+1,6] - newdata[j,6]
     y4 <- newdata[j+1,7] - newdata[j,7]
     
     b <- (x3*x4 + y3*y4)/((sqrt(x3^2+y4^2))*(sqrt(x3^2+y4^2)))
      if((a==-1)||(b==-1)) 
      {
        newdata2 <- rbind(newdata2, newdata[j,])
        newdata2 <- rbind(newdata2, newdata[j+1,])
      }
    }
  }
}










if(data$route_id[i]==data$route_id[i-1])
{
  #проверяем коллинеарность векторов
  if((data$x[i]/data$x[i-1])==(data$y[i]/data$y[i-1]))
  {
    
      newdata <- rbind(newdata, data[i-1,])
      newdata <- rbind(newdata, data[i,])
    }
  
}