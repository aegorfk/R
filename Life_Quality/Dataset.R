#install.packages("ggplot2", dependencies = TRUE)
library("ggplot2")
#install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

Sys.setenv(LANG = "en")

#Read data from file:
data <- read.csv(file="data.csv", header = TRUE, sep = ";", dec=",")
colnames(data) <- c('Country','Quality','Education','Life_duration','STEM','Development','Corruption','Unity','Safety','Brain_loss','GDP','Effectiveness','Consumption','Real_GDP_Growth','Finance','Drugs_alcohol','Crimes','Education_spends','Research_Development','Service','Illiteracy','Inflation','Unemployment','CO2','IT')
data = data[c(-19, -22, -24),]

for(i in 2:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}


str(data)
names(data)
summary(data)

hist(data$Quality)
qplot(data$Quality, data$Education, xlab = "Quality", ylab = "Education", main = "Dependency of Quality from Education")
qplot(data$Quality, data$Life_duration, xlab = "Quality", ylab = "Life_duration", main = "Dependency of Quality from Life_duration")

#Pair regressions models
data_reg <- lm(Quality ~ GDP, data = data)
data_reg <- lm(Quality ~ Consumption, data = data)
data_reg <- lm(Quality ~ Illiteracy, data = data)
data_reg <- lm(Quality ~ Research_Development, data = data)
data_reg <- lm(Quality ~ Crimes, data = data)
data_reg <- lm(Quality ~ Life_duration, data = data)
data_reg <- lm(Quality ~ CO2, data = data)


data_reg <- lm(Quality ~ Consumption + GDP + Life_duration + CO2 + Research_Development + Illiteracy + Crimes, data = data)

#Main features of regression
summary(data_reg)
plot(data_reg)


#Pearson correlation coefficient matrix:
corstarsl <- function(x)
{ 
  #Ininial matrix
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  #Levels of significance 
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  #Rounding for 2 symbols
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  #Building matrix
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  #Delete 1 triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  #Return of matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


#Subset of 7 variables for Pearson correlation matrix:
x <- subset(data, select = c("Quality","Consumption","Life_duration","CO2","Research_Development","Illiteracy","Crimes")); 

#Matrix
corstarsl(data.matrix(x));

#Final regression
data_reg <- lm(Quality ~ Consumption + Life_duration + CO2 + Research_Development + Illiteracy + Crimes, data = data)
summary(data_reg)
plot(data_reg)


