#Abre arquivo "Cadastral"
#install.packages('readr')
library(readr)
bike <- read_csv("C:/Users/901446665/Downloads/Final R/Bike_Sharing_day.csv")
#View(bike)

#Verifica cabeçalho do transacional
names(bike)
#Força instância do arquivo cadastral
attach(bike)

#1)	Classifique o tipo de variável disponíveis no banco de dados:#Ordena pelo ID a base de dados e insere novamente
bike <- bike[order(bike$instant,bike$dteday),]

#Remove duplicidades do arquivo cadastral e insere em A
bike_cleared <- unique(bike)

View(bike_cleared)

#Verifica volumetria entre estações
#season (1:springer, 2:summer, 3:fall, 4:winter)
table(bike_cleared$season)

#Verifica volumetria entre clima

#1: Clear, Few clouds, Partly cloudy, Partly cloudy; 
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist;
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds;
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

plot(bike_cleared$weathersit, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="Clima", ylab="Consumo", pch=20) 

#Verifica volumetria entre dia da semana
table(bike_cleared$weekday)
plot(bike_cleared$weekday, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="DU", ylab="Consumo", pch=20) 

#Verifica volumetria entre trabalho
table(bike_cleared$workingday)
plot(bike_cleared$workingday, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="WD", ylab="Consumo", pch=20) 

#abrangencia em dias da amostra
min(bike_cleared$dteday)
max(bike_cleared$dteday)

#fatores históricos mensais dão tendência para acúmulos de uso nos meses centrais do ano
plot(bike_cleared$mnth, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="Mon", ylab="Consumo", pch=20) 

#concentração em 1 e 2.5 na velocidade do vento (tendência à dias mais frescos)
table(bike_cleared$windspeed)
plot(bike_cleared$windspeed, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="Windspeed", ylab="Consumo", pch=20) 

#concentração dias mais frescos 
table(bike_cleared$hum)
plot(bike_cleared$hum, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="Humidity", ylab="Consumo", pch=20) 

#dias de feriados, usualmente não são usadas tendo maior concentração sob dias de trabalho
table(bike_cleared$holiday)
plot(bike_cleared$holiday, bike_cleared$cnt, main="Relação de agrupamento",
     xlab="holiday", ylab="Consumo", pch=20) 

#-----------------------------------------------

#inicia fatias da base temp
summary(bike_cleared$temp)

amplitude_temp <- (max(bike_cleared$temp)-min(bike_cleared$temp))/5
amplitude_temp

bike_cleared$fx_temp <- cut(bike_cleared$temp, breaks = c(0,0.16,0.32,0.48,0.64,0.80,0.96), labels = c('<=0.16','De 0.161 à 0.32','De 0.321 à 0.48','0.481 à 0.64','0.641 à 0.80','>= 0.801'), right = T)

summary(bike_cleared$fx_temp)


#inicia fatias da base atemp
summary(bike_cleared$atemp)

amplitude_atemp <- (max(bike_cleared$atemp)-min(bike_cleared$atemp))/5
amplitude_atemp

bike_cleared$fx_atemp <- cut(bike_cleared$atemp, breaks = c(0,0.15,0.30,0.45,0.60,0.75,0.90), labels = c('<=0.15','De 0.151 à 0.30','De 0.301 à 0.45','0.451 à 0.60','0.601 à 0.75','>= 0.751'), right = T)

summary(bike_cleared$fx_atemp)


#inicia fatias da base humidity
summary(bike_cleared$hum)

amplitude_hum <- (max(bike_cleared$hum)-min(bike_cleared$hum))/5
amplitude_hum

max(bike_cleared$hum)

bike_cleared$fx_hum <- cut(bike_cleared$hum, breaks = c(0,0.19,0.38,0.57,0.76,0.95,0.999), labels = c('<=0.19','De 0.191 à 0.38','De 0.381 à 0.57','0.571 à 0.76','0.761 à 0.95','>= 0.951'), right = T)

summary(bike_cleared$fx_atemp)

#inicia fatias da base windspeed
summary(bike_cleared$windspeed)

amplitude_wind <- (max(bike_cleared$windspeed)-min(bike_cleared$windspeed))/5
amplitude_wind

bike_cleared$fx_wind <- cut(bike_cleared$windspeed, breaks = c(0,0.09,0.18,0.36,0.45,0.54,0.999), labels = c('<=0.09 atemp','De 0.091 à 0.18','De 0.181 à 0.27','0.271 à 0.36','0.361 à 0.45','>= 0.451'), right = T)

summary(bike_cleared$fx_wind)


sd_casual<-sd(bike_cleared$casual);sd_casual
sd_casual<- sd(bike_cleared$casual)/mean(bike_cleared$casual);sd_casual

sd_registered<-sd(bike_cleared$registered);sd_registered
sd_registered<- sd(bike_cleared$registered)/mean(bike_cleared$registered);sd_registered

sd_cnt<-sd(bike_cleared$cnt);sd_cnt
sd_cnt<- sd(bike_cleared$cnt)/mean(bike_cleared$cnt);sd_cnt


representatividade_totaly <- sum(bike_cleared$casual)/sum(bike_cleared$cnt)

representatividade <- max(bike_cleared$casua/bike_cleared$cnt)
representatividade         

#install.packages("ggplot2")

attach(bike_cleared)
library(ggplot2)

#Gr?fico de dispersao para a associa??o entre notaavalia e Salario 
plot (x = bike_cleared$cnt, y = bike_cleared$temp ,
      main = "Gráfico de dispersão",
      xlab = "Humidade",
      ylab = "Consumo")

#Gr?fico de dispersao para a associa??o entre notaavalia e Salario 
plot (x = bike_cleared$cnt, y = bike_cleared$hum,
      main = "Gráfico de dispersão",
      xlab = "Temperatura",
      ylab = "Consumo")


boxplot(bike_cleared$cnt ~bike_cleared$season, main='Estação do Ano')

Bike_Cor <- subset(bike_cleared, select =c(season,holiday,weekday,workingday,weathersit,temp,atemp, hum,windspeed,cnt,registered,casual))
cor(Bike_Cor)


#install.packages(c("rpart","rattle"))
library(rpart)
library(rpart.plot)


# Estimado regras que estimam o aluguel
modelo_Valor_tree <- rpart (registered ~ fx_temp+fx_atemp+fx_hum+fx_wind, data=bike_cleared, 
                            cp = 0.001,minsplit = 15,maxdepth=10)

# Faz o Gr?fico; Mostrando graficamente as regras 
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE, 
           snip=FALSE)

mytree

# aplicandoo o modelo de ?rvore de Regress?ona base imoveis

Estima_mod_tree <- predict(modelo_Valor_tree,interval = "prediction")

rmse_tree <- sqrt(mean((bike_cleared$registered - Estima_mod_tree)^2))
rmse_tree

base_final <- cbind(bike_cleared,Estima_mod_tree)

View(base_final)
