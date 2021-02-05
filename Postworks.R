############ Postworks R ###############

soccer <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
#https://www.football-data.co.uk/mmz4281/1920/SP1.csv
################## ver el contenido de la tabla 
summary(soccer)
names(soccer)
dim(soccer)
head(soccer)
##################
Gs<-soccer[c("FTHG","FTAG")]
?table
#La funciòn table sirve para hacer tablas de contingencia

#Podemos por el momento dividir y multiplicar las entradas
#de la tabla por numeros cualquiera 
table(Gs$FTAG)/sum(table(Gs))*100
table(Gs$FTHG)/sum(table(Gs))*100
table(Gs)/sum(table(Gs))*100

####Postwork sesión 2 
library(dplyr)
url<-"https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url1<-"https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
url2<-"https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
list<-c(url1,url1,url2)
bases<-lapply(list, read.csv)
head(bases[[3]])
z<-mutate(bases[[3]],Date=as.Date(Date,"%d/%m/%Y"))
z1<-lapply(bases[c(1,2)],mutate,Date=as.Date(Date,"%d/%m/%y"))
z1[[3]]<-z
bases_l<-lapply(z1,select,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR)
Base_final<-do.call(rbind,bases_l)
head(Base_final)
dim(Base_final)
#class(bases[c(1,2)])

#Reto 3 BEDU
z1<-table(Base_final$FTHG)/sum(table(Base_final$FTHG))*100
z2<-table(Base_final$FTAG)/sum(table(Base_final$FTAG))*100
z<-table(select(Base_final,FTHG,FTAG))/sum(table(select(Base_final,FTHG,FTAG)))*100
as.data.frame(z1) %>%
  ggplot() + 
  aes(x=Var1,y=Freq) +
  geom_bar(stat="identity") + 
  ggtitle("Probabilidades_casa") +
  ylab("Probabilidad") +
  xlab("Mediciones") + 
  theme_light()
as.data.frame(z2) %>%
  ggplot() + 
  aes(x=Var1,y=Freq) +
  geom_bar(stat="identity") + 
  ggtitle("Probabilidades_visitante") +
  ylab("Probabilidades") +
  xlab("Mediciones") + 
  theme_light()
#table(z)
?geom_bar
heatmap(z)
#heatmap(matrix(data=1:9,nrow=3))

###Postwork 6
names(Base_final)
names(z)
?write.csv
Base_final<-rename(Base_final,date=Date ,home.team=HomeTeam ,away.team=AwayTeam ,home.score=FTHG, away.score=FTAG)
fil="C:\\Users\\Luis_Romero\\Documents\\Football2\\GitHub\\Programacion-con-R-Santander\\Sesion-05\\datos.csv"
write.csv(select(Base_final,date,home.team,home.score,away.team,away.score),
          file=fil,row.names=FALSE)
install.packages("fbRanks")
library(fbRanks)
read.csv(fil)
soccer<-create.fbRanks.dataframes(fil)
scores<-soccer$scores
teams<-soccer$teams
library("lubridate")
n<-ymd(unique(scores$date))
install.packages("lubridate")

max(ymd(n))
Ranking<-rank.teams(scores,teams,max.date=max(n),min.date=(min(n)))
predict(Ranking,date=n[length(n)])
#n[length(n)]
#soccer
#min(n)
#?create.fbRanks.dataframes
#######################################
############Postwork 6#################
#######################################
library(dplyr)
match.data<-read.csv("C:\\Users\\Luis_Romero\\Documents\\Football2\\GitHub\\Programacion-con-R-Santander\\Sesion-06\\Postwork\\match.data.csv")
head(match.data)
str(match.data)
match.data<-mutate(match.data,total=home.score+away.score,
                   date=as.Date(date,"%Y-%m-%d"),
                   mes_año=paste(as.character(format(as.Date(date,"%Y-%m-%d"),"%Y") ),
                                 as.character(format(as.Date(date,"%Y-%m-%d"),"%m"))))
w<-match.data %>% group_by(mes_año) %>% summarise(mean_total=mean(total))
as.data.frame(w)
ts.totales<-ts(as.data.frame(w)$mean_total,start=c(2010,08),frequency=12)
plot(ts.totales)

####match.data$date[1]

#postwork 7

setwd("C:\\Users\\Luis_Romero\\Documents\\Football2\\GitHub\\Programacion-con-R-Santander\\Sesion-07")
write.csv(mtcars,"mtcars.csv")

library(mongolite)

match <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv", row.names = 1)

Base_de_datos <- mongo(collection = "match", db = "match_games")
Base_de_datos$insert(match)
Base_de_datos$count()

Base_de_datos$find('
{
  "HomeTeam": "Real Madrid",
  "Date": "2015-12-09"
}
')

Base_de_datos$disconnect(T)
