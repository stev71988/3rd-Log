# Presentation of Firstborn Football work

# = Delete hash and execute
## = Notes
### = Definition 
#### = Old Code. Ignore

## Install Packages ##
# install.packages("PlayerRatings")
# install.packages("devtools")
# devtools::install_github("Rforecastio","hrbrmstr")
# install.packages("abind")
# install.packages("RCurl")

### PlayerRatings = Package within R that estimates Team Skill. Implemented methods include ELO, Glicko, and Stephenson
### Devtools = A collection of package development tools
### Rforecastio = R Package that accesses Forecast.io and gathers real-time weather data
### Abind = R Package that combines multidimensional arrays into single arrays

## Run Libraries ##
library(devtools)
library(PlayerRatings)
library(abind)
library(Rforecastio)
library(RCurl)


## Formats the Rating CSV's and adds columns for Game, Wins, Loss, Draw
CleanRatings = function(r) {
  r = r[,c(1,2)]
  colnames(r) = c("Player","Rating")
  r$Games = 0
  r$Win = 0
  r$Draw = 0
  r$Loss = 0
  r$Lag = 0
  return(r)
}


## Formats a schedule.  Schedules can be pulled from pro-football-reference.com as CSVs.  Requires plaintext format.
FormatWinSchedule = function(s)  {
  s$Win = 1
  for(i in 1:nrow(s)){
    if(s$PtsW[i] == s$PtsL[i]){
      s$Win[i] = 0.5
    }
  }
  s= s[,-c(2,3,4,6,8:13)]
  s = s[-c(which(s[,1] == "Week")),]
  labels = c('Week','Winner','Loser',"Win")
  colnames(s)= labels
  s$Week = as.numeric(s$Week)
  return(s)
}
## Removes column 2,3,5,7,8 from season file (day, dates, @,etc)
FormatFutureSchedule = function(s) {
  s = s[,-c(2,3,5,7,8)]
  s = s[-c(which(s[,1] == "Week")),]
  colnames(s) = c("Week", "Away", "Home")
  s$Week = as.numeric(s$Week)
  return(s)
}
## Creates a list by week from the edited season file. categorizes each week.
GetWeeks = function(s)   {
  for(i in 1:length(unique(s$Week))){
    assign(paste0("w", i), s[which(s$Week == i),])
  }
  if(length(unique(s$Week)) == 17){
    weeks = list(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17)
  } else if(length(unique(s$Week)) == 4){ 
    weeks = list(w1,w2,w3,w4)
  } else( weeks = NULL)
  
  return (weeks)
}
## 2011
git.11.0 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/PastSeason/r.11.0.csv")
r.11.0 <- read.csv(text = git.11.0)
r.11.0 = CleanRatings(r.11.0)
#### r.11.0 <- as.data.frame(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/r.11.0.csv", header=TRUE,stringsAsFactors = FALSE))
#### r.11.0 = CleanRatings(r.11.0)

## View the initial ratings for each team
View(r.11.0)
# write.csv(r.11.0, file = "r.11.0.csv",na = "", row.names = FALSE)

git.s.11 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/PastSeason/s.11.csv")
s.11 = read.csv(text = git.s.11, header=TRUE, stringsAsFactors = FALSE)
s.11 = FormatWinSchedule(s.11)
#### s.11 = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/s.11.csv",header=TRUE, stringsAsFactors = FALSE)
#### s.11 = FormatWinSchedule(s.11)

w.11 = GetWeeks(s.11)

r.11.1 = elo(w.11[[1]],status = r.11.0,kfac = 20, history = TRUE)
h.11.1 = abind(r.11.0, r.11.1$ratings, along = 3)
r.11.2 = elo(w.11[[2]],status = r.11.1$ratings,kfac = 20, history = TRUE)
h.11.2 = abind(h.11.1, r.11.2$ratings, along = 3)
r.11.3 = elo(w.11[[3]],status = r.11.2$ratings,kfac = 20, history = TRUE)
h.11.3 = abind(h.11.2, r.11.3$ratings, along = 3)
r.11.4 = elo(w.11[[4]],status = r.11.3$ratings,kfac = 20, history = TRUE)
h.11.4 = abind(h.11.3, r.11.4$ratings, along = 3)
r.11.5 = elo(w.11[[5]],status = r.11.4$ratings,kfac = 20, history = TRUE)
h.11.5 = abind(h.11.4, r.11.5$ratings, along = 3)
r.11.6 = elo(w.11[[6]],status = r.11.5$ratings,kfac = 20, history = TRUE)
h.11.6 = abind(h.11.5, r.11.6$ratings, along = 3)
r.11.7 = elo(w.11[[7]],status = r.11.6$ratings,kfac = 20, history = TRUE)
h.11.7 = abind(h.11.6, r.11.7$ratings, along = 3)
r.11.8 = elo(w.11[[8]],status = r.11.7$ratings,kfac = 20, history = TRUE)
h.11.8 = abind(h.11.7, r.11.8$ratings, along = 3)
r.11.9 = elo(w.11[[9]],status = r.11.8$ratings,kfac = 20, history = TRUE)
h.11.9 = abind(h.11.8, r.11.9$ratings, along = 3)
r.11.10 = elo(w.11[[10]],status = r.11.9$ratings,kfac = 20, history = TRUE)
h.11.10 = abind(h.11.9, r.11.10$ratings, along = 3)
r.11.11 = elo(w.11[[11]],status = r.11.10$ratings,kfac = 20, history = TRUE)
h.11.11 = abind(h.11.10, r.11.11$ratings, along = 3)
r.11.12 = elo(w.11[[12]],status = r.11.11$ratings,kfac = 20, history = TRUE)
h.11.12 = abind(h.11.11, r.11.12$ratings, along = 3)
r.11.13 = elo(w.11[[13]],status = r.11.12$ratings,kfac = 20, history = TRUE)
h.11.13 = abind(h.11.12, r.11.13$ratings, along = 3)
r.11.14 = elo(w.11[[14]],status = r.11.13$ratings,kfac = 20, history = TRUE)
h.11.14 = abind(h.11.13, r.11.14$ratings, along = 3)
r.11.15 = elo(w.11[[15]],status = r.11.14$ratings,kfac = 20, history = TRUE)
h.11.15 = abind(h.11.14, r.11.15$ratings, along = 3)
r.11.16 = elo(w.11[[16]],status = r.11.15$ratings,kfac = 20, history = TRUE)
h.11.16 = abind(h.11.15, r.11.16$ratings, along = 3)
r.11.17 = elo(w.11[[17]], status = r.11.16$ratings,kfac = 20, history = TRUE)
h.11.17 = abind(h.11.16, r.11.17$ratings, along = 3)

## View the ELO rating at the end of 2011
View(r.11.17$ratings)
#write.csv(r.11.17$ratings, file = "r.11.17.csv",na = "", row.names = FALSE)

#
# This function regresses all ratings toward mean by the square root of the distance from the mean
#
GetBaseRatings = function(old.r) {
  d = sqrt(abs(1500-old.r$ratings$Rating))
  new.r = old.r
  i = which(old.r$ratings$Rating>=1500)
  j = which(old.r$ratings$Rating<1500)
  d[i] = -d[i]
  
  new.r$ratings$Rating =  old.r$ratings$Rating+d
  new.r$ratings[,3:7] = 0
  return(new.r)
}




# 2012

r.12.0 = GetBaseRatings(r.11.17)

git.s.12 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/PastSeason/s.12.csv")
s.12 = read.csv(text = git.s.12, header=TRUE, stringsAsFactors = FALSE)
s.12 = FormatWinSchedule(s.12)
#### s.12 = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/s.12.csv",header=TRUE, stringsAsFactors = FALSE)
#### s.12 = FormatWinSchedule(s.12)

w.12 = GetWeeks(s.12)

r.12.1 = elo(w.12[[1]],status = r.12.0$ratings,kfac = 20, history = TRUE)
r.12.2 = elo(w.12[[2]],status = r.12.1$ratings,kfac = 20, history = TRUE)
r.12.3 = elo(w.12[[3]],status = r.12.2$ratings,kfac = 20, history = TRUE)
r.12.4 = elo(w.12[[4]],status = r.12.3$ratings,kfac = 20, history = TRUE)
r.12.5 = elo(w.12[[5]],status = r.12.4$ratings,kfac = 20, history = TRUE)
r.12.6 = elo(w.12[[6]],status = r.12.5$ratings,kfac = 20, history = TRUE)
r.12.7 = elo(w.12[[7]],status = r.12.6$ratings,kfac = 20, history = TRUE)
r.12.8 = elo(w.12[[8]],status = r.12.7$ratings,kfac = 20, history = TRUE)
r.12.9 = elo(w.12[[9]],status = r.12.8$ratings,kfac = 20, history = TRUE)
r.12.10 = elo(w.12[[10]],status = r.12.9$ratings,kfac = 20, history = TRUE)
r.12.11 = elo(w.12[[11]],status = r.12.10$ratings,kfac = 20, history = TRUE)
r.12.12 = elo(w.12[[12]],status = r.12.11$ratings,kfac = 20, history = TRUE)
r.12.13 = elo(w.12[[13]],status = r.12.12$ratings,kfac = 20, history = TRUE)
r.12.14 = elo(w.12[[14]],status = r.12.13$ratings,kfac = 20, history = TRUE)
r.12.15 = elo(w.12[[15]],status = r.12.14$ratings,kfac = 20, history = TRUE)
r.12.16 = elo(w.12[[16]],status = r.12.15$ratings,kfac = 20, history = TRUE)
r.12.17 = elo(w.12[[17]],status = r.12.16$ratings,kfac = 20, history = TRUE)

# 2013

r.13.0 = GetBaseRatings(r.12.17)

git.s.13 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/PastSeason/s.13.csv")
s.13 = read.csv(text = git.s.13, header=TRUE, stringsAsFactors = FALSE)
s.13 = FormatWinSchedule(s.13)
#### s.13 = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/s.13.csv",header=TRUE, stringsAsFactors = FALSE)
#### s.13 = FormatWinSchedule(s.13)

w.13 = GetWeeks(s.13)

r.13.1 = elo(w.13[[1]],status = r.13.0$ratings,kfac = 20, history = TRUE)
r.13.2 = elo(w.13[[2]],status = r.13.1$ratings,kfac = 20, history = TRUE)
r.13.3 = elo(w.13[[3]],status = r.13.2$ratings,kfac = 20, history = TRUE)
r.13.4 = elo(w.13[[4]],status = r.13.3$ratings,kfac = 20, history = TRUE)
r.13.5 = elo(w.13[[5]],status = r.13.4$ratings,kfac = 20, history = TRUE)
r.13.6 = elo(w.13[[6]],status = r.13.5$ratings,kfac = 20, history = TRUE)
r.13.7 = elo(w.13[[7]],status = r.13.6$ratings,kfac = 20, history = TRUE)
r.13.8 = elo(w.13[[8]],status = r.13.7$ratings,kfac = 20, history = TRUE)
r.13.9 = elo(w.13[[9]],status = r.13.8$ratings,kfac = 20, history = TRUE)
r.13.10 = elo(w.13[[10]],status = r.13.9$ratings,kfac = 20, history = TRUE)
r.13.11 = elo(w.13[[11]],status = r.13.10$ratings,kfac = 20, history = TRUE)
r.13.12 = elo(w.13[[12]],status = r.13.11$ratings,kfac = 20, history = TRUE)
r.13.13 = elo(w.13[[13]],status = r.13.12$ratings,kfac = 20, history = TRUE)
r.13.14 = elo(w.13[[14]],status = r.13.13$ratings,kfac = 20, history = TRUE)
r.13.15 = elo(w.13[[15]],status = r.13.14$ratings,kfac = 20, history = TRUE)
r.13.16 = elo(w.13[[16]],status = r.13.15$ratings,kfac = 20, history = TRUE)
r.13.17 = elo(w.13[[17]],status = r.13.16$ratings,kfac = 20, history = TRUE)

#2014

r.14.0 = GetBaseRatings(r.13.17)

git.s.14 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/PastSeason/s.14.csv")
s.14 = read.csv(text = git.s.14, header=TRUE, stringsAsFactors = FALSE)
s.14 = FormatWinSchedule(s.14)
#### s.14 = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/s.14.csv",header=TRUE, stringsAsFactors = FALSE)
#### s.14 = FormatWinSchedule(s.14)

w.14 = GetWeeks(s.14)

r.14.1 = elo(w.14[[1]],status = r.14.0$ratings,kfac = 20, history = TRUE)
r.14.2 = elo(w.14[[2]],status = r.14.1$ratings,kfac = 20, history = TRUE)
r.14.3 = elo(w.14[[3]],status = r.14.2$ratings,kfac = 20, history = TRUE)
r.14.4 = elo(w.14[[4]],status = r.14.3$ratings,kfac = 20, history = TRUE)
r.14.5 = elo(w.14[[5]],status = r.14.4$ratings,kfac = 20, history = TRUE)
r.14.6 = elo(w.14[[6]],status = r.14.5$ratings,kfac = 20, history = TRUE)
r.14.7 = elo(w.14[[7]],status = r.14.6$ratings,kfac = 20, history = TRUE)
r.14.8 = elo(w.14[[8]],status = r.14.7$ratings,kfac = 20, history = TRUE)
r.14.9 = elo(w.14[[9]],status = r.14.8$ratings,kfac = 20, history = TRUE)
r.14.10 = elo(w.14[[10]],status = r.14.9$ratings,kfac = 20, history = TRUE)
r.14.11 = elo(w.14[[11]],status = r.14.10$ratings,kfac = 20, history = TRUE)
r.14.12 = elo(w.14[[12]],status = r.14.11$ratings,kfac = 20, history = TRUE)
r.14.13 = elo(w.14[[13]],status = r.14.12$ratings,kfac = 20, history = TRUE)
r.14.14 = elo(w.14[[14]],status = r.14.13$ratings,kfac = 20, history = TRUE)
r.14.15 = elo(w.14[[15]],status = r.14.14$ratings,kfac = 20, history = TRUE)
r.14.16 = elo(w.14[[16]],status = r.14.15$ratings,kfac = 20, history = TRUE)
r.14.17 = elo(w.14[[17]],status = r.14.16$ratings,kfac = 20, history = TRUE)

##View ELO rating at the end of 2014
View(r.14.17$ratings)
# write.csv(r.14.17$ratings, file = "r.14.17.csv",na = "", row.names = FALSE)

# # write.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/")
  # 
  # predict.outcome = function(elo.a,elo.b){
  #   return(1/(10^(-(elo.a-elo.b)/400)+1))
  # }
  # 
  # 
  # 
  # get.team.ratings = function(t, ...){
  #   
  #   ratings = list(...)
  #   num.weeks = length(ratings)
  #   team.ratings = c()
  #   
  #   for(i in 1:num.weeks){
  #     team.ratings[i] = ratings[[i]]$ratings$Rating[which(ratings[[i]]$ratings$Player == t)]
  #   }
  #   return(team.ratings)
  # }
  # 
  # 
  # 
  # #
  # # this function plots the entire season of the specified team anb returns a vector of the elo ratings from each week.
  # #
  # plot.season = function(t,sn){
  #   if(sn == 11) {results = get.team.ratings(t, r.11.1, r.11.2,r.11.3, r.11.4, r.11.5, r.11.5, r.11.6, r.11.7, r.11.8, r.11.9, r.11.10, r.11.11, r.11.12, r.11.13, r.11.14, r.11.15, r.11.16)} 
  #   else if(sn == 12) {results = get.team.ratings(t, r.12.1, r.12.2,r.12.3, r.12.4, r.12.5, r.12.5, r.12.6, r.12.7, r.12.8, r.12.9, r.12.10, r.12.11, r.12.12, r.12.13, r.12.14, r.12.15, r.12.16)} 
  #   else if(sn == 13){results = get.team.ratings(t, r.13.1, r.13.2,r.13.3, r.13.4, r.13.5, r.13.5, r.13.6, r.13.7, r.13.8, r.13.9, r.13.10, r.13.11, r.13.12, r.13.13, r.13.14, r.13.15, r.13.16)} 
  #   else if(sn == 14){results = get.team.ratings(t, r.14.1, r.14.2,r.14.3, r.14.4, r.14.5, r.14.5, r.14.6, r.14.7, r.14.8, r.14.9, r.14.10, r.14.11, r.14.12, r.14.13, r.14.14, r.14.15, r.14.16)}
  #  plot(results, type = "l")
  # }
  # 
  # 
  # 
  # 
  # 
  # 
  # ##
  # ## Subjective Variables and Prediction
  # ##
  # 
  # g = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away", "Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Morale","Travel","Popularity","Gamma"))))
  # t = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/travel.csv", header = FALSE, na.strings="0", stringsAsFactors = FALSE)
  # tz = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/timezones.csv", header = TRUE, stringsAsFactors = FALSE)
  # tz$Zone = as.numeric(tz$Zone)
  # 
  # 
  # GetTravel = function(a,b){
  #   GetDistance = function(a,b){
  #     # return( t[which(t[1,]==a), which(t[,1] == b)])
  #     dist = t[which(t[1,]==a), which(t[,1] == b)] 
  #     if(dist < 200){ 
  #       d = 0}
  #     else if(dist > 200 && dist<1000) {
  #       d = 1}
  #     else if(dist >= 1000 && dist<2000) {
  #       d = 2}
  #     else if(dist >= 2000) { 
  #       d = 3}
  #     return(d)
  #   }
  #   
  #   d.tz = function(a,b){
  #     d = abs(tz[which(tz[,1] == a),2] - tz[which(tz[,1] == b),2])
  #     return(d)
  #   }
  #   score= (as.numeric(d.tz(a,b)) + as.numeric(GetDistance(a,b)))
  #   if( score > 5){score = 5}
  #   
  #   return(-score) # negative because home team is favored.
  # }
  # 
  # 
  # # This function sets the gamma parameters assuming the team in the current Winner Column is the home team
  # # 
  # #
  # 
  # # SetGamma = function(s) {
  # #   g = list()
  # #   gamma.list = list()
  # #   for(i in 1:nrow(s)){
  # #     tg = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away", "Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Morale","Travel","Popularity","Gamma"))))
  # #     tg$Away = s$Away[i]
  # #     tg$Home = s$Home[i]
  # #     a = tg$Away
  # #     h = tg$Home
  # #     tg$Travel = get.travel(a, h)
  # #     tg$Temperature = get.weather(a,h)
  # #     tg$Age = GetAge(a,h)
  # #     tg$Oweight = GetOWeight(a,h)
  # #     tg$Experience = GetExperience(a,h)
  # #     tg$Stadium = GetStadium(h)
  # #     tg$Coaching = GetCoaching(a,h)
  # #     gamma.list[[i]] = tg$Gamma[1]
  # #     g[[i]] = tg
  # #   }
  # #   df <- do.call(rbind.data.frame,g)
  # #   colnames(df) = colnames(tg) 
  # #   return (df)
  # # }
  # 
  # # SetGamma = function(s)   {
  # #   g = list()
  # #   gamma.list = list()
  # #   for(i in 1:nrow(s)){
  # #     tg = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away","Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Travel","Gamma"))))
  # #     tg$Away = s$Away[i]
  # #     tg$Home = s$Home[i]
  # #     a = tg$Away
  # #     h = tg$Home
  # #     tg$Travel = get.travel(a, h)
  # #     tg$Temperature = get.weather(a,h)
  # #     tg$Age = GetAge(a,h)
  # #     tg$Oweight = GetOWeight(a,h)
  # #     tg$Experience = GetExperience(a,h)
  # #     tg$Stadium = GetStadium(h)
  # #     tg$Coaching = GetCoaching(a,h)
  # #     tg$`Power Combo` = GetPowerCombo(a,h)
  # #     g[[i]] = tg
  # #   }
  # #   df <- do.call(rbind.data.frame,g)
  # #   colnames(df) = colnames(tg) 
  # #   
  # #   return (df)
  # # }
  # AdjustGammaWeights= function(g,c){
  #   for(i in 1:length(c)){
  #     g[,(i+2)] = g[,(i+2)]*c[i]
  #   }
  #   return(g)
  # }
  # 
  # 
  # 
  # 
  # # Player Absence  
  # # elo.old current elo rating, DYAR player's current DYAR, num.games the number of games used in the player's DYAR
  # # Y average team yards per game
  # 
  # # PlayerAbsence = function(elo.old, DYAR, Y){
  # # elo.new = elo.old - (((Y - (DYAR))/Y)*.75*(elo.old - 1500))
  # # return (elo.new)
  # # }
  # 
  # 
  # 
  # 
  # # Weather
  # #
  # #
  # 
  # 
  # locs = read.csv(file="/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/lat.long.csv", header = TRUE,stringsAsFactors = FALSE)
  # forecasts = list() 
  # # for(i in 1:nrow(l)){
  # #   temp.forecast = get_current_forecast(locs$Lat[i],locs$Long[i])$daily
  # #   forecasts[[i]] = (temp.forecast$apparentTemperatureMax + temp.forecast$apparentTemperatureMin)/2
  # # }
  # # 
  # # forecasts <- do.call(rbind.data.frame,forecasts)
  # # colnames(forecasts) = c("0","1", "2","3","4","5","6","7")
  # # rownames(forecasts) = locs[,1]
  # # class(forecasts[4,5])
  # # This needs work
  # #
  # #
  # 
  # for(i in 1:nrow(locs)){
  #   temp.forecast = get_current_forecast(locs$Lat[i],locs$Long[i])$daily
  #   forecasts[[i]] = (temp.forecast$apparentTemperatureMax + temp.forecast$apparentTemperatureMin)/2
  # }
  # 
  # forecasts <- do.call(rbind.data.frame,forecasts)
  # colnames(forecasts) = c("0","1", "2","3","4","5","6","7")
  # rownames(forecasts) = locs[,1]
  # 
  # GetWeather = function(a,h){
  #   w.a = mean(as.numeric(forecasts[a,]))
  #   w.h = mean(as.numeric(forecasts[h,]))
  #   diff = abs(w.a-w.h)
  #   
  #   if(diff < 15) {return(0) }
  #      else if(diff>=15 & diff < 30){ return (1)}
  #      else if(diff>=30 & diff < 40){ return (2)}
  #      else if(diff>=40 & diff < 50){ return (3)}
  #      else if(diff>=50 & diff < 60){ return (4)}
  #      else if(diff>60){ return (5)}
  #   return(diff)
  # }
  # 
  # 
  # player.files <- list.files(path = "/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player",pattern="players.*.txt")
  # teams.names = c("arz", "atl","bal","buf","car","chi","cin","cle","dal","den","det","gb","hou","ind","jax","kc","mia","min","ne","no","nyg","nyj","oak","phi","pit","sd","sea","sf","stl","tb","ten","was")
  # roster = list()
  # setwd = path = "/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player"  
  #   for(i in 1:length(player.files)){
  #     temp = read.csv(paste("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player/",player.files[[i]], sep = ""), stringsAsFactors = FALSE )
  #     temp = temp[,c("X","Pos","Age","Wt","Yrs")]
  #     temp$Yrs = as.numeric(temp$Yrs)
  #     temp$Age = as.numeric(temp$Age)
  #     temp$Rooks = 0
  #     # temp$Rooks[which(is.na(temp$Yrs))] = 1
  #     temp = na.omit(temp)
  #     roster[[i]] = temp
  #     }
  # names(roster) = teams.names
  # 
  # player.stats = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player.stats.csv") 
  # # stadium data from here: https://www.reddit.com/r/Madden/comments/2f2ux6/heres_a_spreadsheet_of_franchise_health/
  # # Each of the 5 top home-record teams got an additional point.  Denver also got an additional point for high altitude.
  # stadium = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/stadium.csv", stringsAsFactors = FALSE)  
  # coaches = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/coaching.csv",stringsAsFactors = FALSE)
  # for(i in 1:nrow(player.stats)) {
  #   player.stats$Age[i] = mean(na.omit(roster[[i]]$Age[which(na.omit(roster[[i]]$Age != 0))]))
  #   player.stats$Weight[i] = mean(na.omit(roster[[i]]$Wt[which(na.omit(roster[[i]]$Wt != 0))]))
  #   player.stats$Experience[i] = mean(na.omit(roster[[i]]$Yrs[which(na.omit(roster[[i]]$Yrs != 0))]))
  # }
  # 
  # GetAge = function(a,h){
  #   age.a = player.stats$Age[which(player.stats$Team == a)]
  #   age.h = player.stats$Age[which(player.stats$Team == h)]
  #   diff = age.a - age.h
  #   return(diff)
  # }
  # 
  # GetLineWeight = function(a,h){
  #   weight.a = player.stats$Weight[which(player.stats$Team == a)]
  #   weight.h = player.stats$Weight[which(player.stats$Team == h)]
  #   diff = weight.a - weight.h
  #   return(diff)
  # }
  # GetExperience = function(a,h){
  #   experience.a = player.stats$Experience[which(player.stats$Team == a)]
  #   experience.h = player.stats$Experience[which(player.stats$Team == h)]
  #   diff = experience.a - experience.h
  #   return(diff)
  # }
  # GetStadium = function(h) {
  #   stadium.h = stadium$Score[which(stadium$Team == h)]
  #   return (-stadium.h)
  # }
  # GetCoaching = function(a,h) {
  #   coaching.a = coaches$Coaching[which(coaches$Team == a)]
  #   coaching.h = coaches$Coaching[which(coaches$Team == h)]
  #   diff = coaching.a - coaching.h
  #   return(diff)
  # }
  # GetMorale = function(a,h,hist){
  #   d = dim(hist)[3]
  #   for(i in d:1){
  #     hist[which,]    
  #   }
  # }
  # 
  # 
  # # 2015 Season Predictions
  # # 
  # #
  # s.15 = FormatFutureSchedule(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/s.15.csv", header = TRUE, stringsAsFactors = FALSE))
  # w.15 = GetWeeks(s.15)
  # r.15.0 = GetBaseRatings(r.14.16)
  # # write.csv(r.15.0, file ="/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/r.15.0")
  # g.15.1 = SetGamma(w.15[[1]])
  # 
  # gammas = g.15.1$Gamma
  # p.15.1 = predict.ratings(r.15.0, w.15[[1]], tng.p = 0, gamma.p = gammas)
  #   
  # 
  # 
  # 
  # 
  # 
  # #
  # # Quick Viewing of some common frames
  # #
  # # 
  # # View(r.11.0)
  # # View(r.11.17$ratings)
  # # View(w.15[[1]])
  # # View(g.15.1)
  # # View(p.15.1)
  # 
=======
#### write.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/")
#### 
####  # predict.outcome = function(elo.a,elo.b){
####  #   return(1/(10^(-(elo.a-elo.b)/400)+1))
####  # }
####  # 
####  # 
####  # 
####  # get.team.ratings = function(t, ...){
####  #   
####  #   ratings = list(...)
####  #   num.weeks = length(ratings)
####  #   team.ratings = c()
####  #   
####  #   for(i in 1:num.weeks){
####  #     team.ratings[i] = ratings[[i]]$ratings$Rating[which(ratings[[i]]$ratings$Player == t)]
####  #   }
####  #   return(team.ratings)
####  # }
####  # 
####  # 
####  # 
####  # #
####  # # this function plots the entire season of the specified team anb returns a vector of the elo ratings from each week.
####  # #
####  # plot.season = function(t,sn){
####  #   if(sn == 11) {results = get.team.ratings(t, r.11.1, r.11.2,r.11.3, r.11.4, r.11.5, r.11.5, r.11.6, r.11.7, r.11.8, r.11.9, r.11.10, r.11.11, r.11.12, r.11.13, r.11.14, r.11.15, r.11.16)} 
####  #   else if(sn == 12) {results = get.team.ratings(t, r.12.1, r.12.2,r.12.3, r.12.4, r.12.5, r.12.5, r.12.6, r.12.7, r.12.8, r.12.9, r.12.10, r.12.11, r.12.12, r.12.13, r.12.14, r.12.15, r.12.16)} 
####  #   else if(sn == 13){results = get.team.ratings(t, r.13.1, r.13.2,r.13.3, r.13.4, r.13.5, r.13.5, r.13.6, r.13.7, r.13.8, r.13.9, r.13.10, r.13.11, r.13.12, r.13.13, r.13.14, r.13.15, r.13.16)} 
####  #   else if(sn == 14){results = get.team.ratings(t, r.14.1, r.14.2,r.14.3, r.14.4, r.14.5, r.14.5, r.14.6, r.14.7, r.14.8, r.14.9, r.14.10, r.14.11, r.14.12, r.14.13, r.14.14, r.14.15, r.14.16)}
####  #  plot(results, type = "l")
####  # }
####  # 
####  # 
####  # 
####  # 
####  # 
####  # 
####  # ##
####  # ## Subjective Variables and Prediction
####  # ##
####  # 
####  # g = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away", "Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Morale","Travel","Popularity","Gamma"))))
####  # t = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/travel.csv", header = FALSE, na.strings="0", stringsAsFactors = FALSE)
####  # tz = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/timezones.csv", header = TRUE, stringsAsFactors = FALSE)
####  # tz$Zone = as.numeric(tz$Zone)
####  # 
####  # 
####  # GetTravel = function(a,b){
####  #   GetDistance = function(a,b){
####  #     # return( t[which(t[1,]==a), which(t[,1] == b)])
####  #     dist = t[which(t[1,]==a), which(t[,1] == b)] 
####  #     if(dist < 200){ 
####  #       d = 0}
####  #     else if(dist > 200 && dist<1000) {
####  #       d = 1}
####  #     else if(dist >= 1000 && dist<2000) {
####  #       d = 2}
####  #     else if(dist >= 2000) { 
####  #       d = 3}
####  #     return(d)
####  #   }
####  #   
####  #   d.tz = function(a,b){
####  #     d = abs(tz[which(tz[,1] == a),2] - tz[which(tz[,1] == b),2])
####  #     return(d)
####  #   }
####  #   score= (as.numeric(d.tz(a,b)) + as.numeric(GetDistance(a,b)))
####  #   if( score > 5){score = 5}
####  #   
####  #   return(-score) # negative because home team is favored.
####  # }
####  # 
####  # 
####  # # This function sets the gamma parameters assuming the team in the current Winner Column is the home team
####  # # 
####  # #
####  # 
####  # # SetGamma = function(s) {
####  # #   g = list()
####  # #   gamma.list = list()
####  # #   for(i in 1:nrow(s)){
####  # #     tg = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away", "Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Morale","Travel","Popularity","Gamma"))))
####  # #     tg$Away = s$Away[i]
####  # #     tg$Home = s$Home[i]
####  # #     a = tg$Away
####  # #     h = tg$Home
#### # #     tg$Travel = get.travel(a, h)
####  # #     tg$Temperature = get.weather(a,h)
####  # #     tg$Age = GetAge(a,h)
####  # #     tg$Oweight = GetOWeight(a,h)
####  # #     tg$Experience = GetExperience(a,h)
####  # #     tg$Stadium = GetStadium(h)
####  # #     tg$Coaching = GetCoaching(a,h)
####  # #     gamma.list[[i]] = tg$Gamma[1]
####  # #     g[[i]] = tg
####  # #   }
####  # #   df <- do.call(rbind.data.frame,g)
####  # #   colnames(df) = colnames(tg) 
####  # #   return (df)
####  # # }
####  # 
####  # # SetGamma = function(s)   {
####  # #   g = list()
####  # #   gamma.list = list()
####  # #   for(i in 1:nrow(s)){
####  # #     tg = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away","Home", "Oweight","Age","Coaching","Rookies","Experience","Power Combo", "Temperature","Stadium","Travel","Gamma"))))
####  # #     tg$Away = s$Away[i]
####  # #     tg$Home = s$Home[i]
####  # #     a = tg$Away
####  # #     h = tg$Home
####  # #     tg$Travel = get.travel(a, h)
####  # #     tg$Temperature = get.weather(a,h)
####  # #     tg$Age = GetAge(a,h)
####  # #     tg$Oweight = GetOWeight(a,h)
####  # #     tg$Experience = GetExperience(a,h)
####  # #     tg$Stadium = GetStadium(h)
####  # #     tg$Coaching = GetCoaching(a,h)
####  # #     tg$`Power Combo` = GetPowerCombo(a,h)
####  # #     g[[i]] = tg
####  # #   }
####  # #   df <- do.call(rbind.data.frame,g)
####  # #   colnames(df) = colnames(tg) 
####  # #   
####  # #   return (df)
####  # # }
####  # AdjustGammaWeights= function(g,c){
####  #   for(i in 1:length(c)){
####  #     g[,(i+2)] = g[,(i+2)]*c[i]
####  #   }
####  #   return(g)
####  # }
####  # 
####  # 
####  # 
####  # 
####  # # Player Absence  
####  # # elo.old current elo rating, DYAR player's current DYAR, num.games the number of games used in the player's DYAR
####  # # Y average team yards per game
####  # 
####  # # PlayerAbsence = function(elo.old, DYAR, Y){
####  # # elo.new = elo.old - (((Y - (DYAR))/Y)*.75*(elo.old - 1500))
####  # # return (elo.new)
####  # # }
####  # 
####  # 
####  # 
####  # 
####  # # Weather
####  # #
####  # #
####  # 
####  # 
####  # locs = read.csv(file="/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/lat.long.csv", header = TRUE,stringsAsFactors = FALSE)
####  # forecasts = list() 
####  # # for(i in 1:nrow(l)){
####  # #   temp.forecast = get_current_forecast(locs$Lat[i],locs$Long[i])$daily
####  # #   forecasts[[i]] = (temp.forecast$apparentTemperatureMax + temp.forecast$apparentTemperatureMin)/2
####  # # }
####  # # 
####  # # forecasts <- do.call(rbind.data.frame,forecasts)
####  # # colnames(forecasts) = c("0","1", "2","3","4","5","6","7")
####  # # rownames(forecasts) = locs[,1]
####  # # class(forecasts[4,5])
####  # # This needs work
####  # #
####  # #
####  # 
####  # for(i in 1:nrow(locs)){
####  #   temp.forecast = get_current_forecast(locs$Lat[i],locs$Long[i])$daily
####  #   forecasts[[i]] = (temp.forecast$apparentTemperatureMax + temp.forecast$apparentTemperatureMin)/2
####  # }
####  # 
####  # forecasts <- do.call(rbind.data.frame,forecasts)
####  # colnames(forecasts) = c("0","1", "2","3","4","5","6","7")
####  # rownames(forecasts) = locs[,1]
####  # 
####  # GetWeather = function(a,h){
####  #   w.a = mean(as.numeric(forecasts[a,]))
####  #   w.h = mean(as.numeric(forecasts[h,]))
####  #   diff = abs(w.a-w.h)
####  #   
####  #   if(diff < 15) {return(0) }
####  #      else if(diff>=15 & diff < 30){ return (1)}
####  #      else if(diff>=30 & diff < 40){ return (2)}
####  #      else if(diff>=40 & diff < 50){ return (3)}
####  #      else if(diff>=50 & diff < 60){ return (4)}
####  #      else if(diff>60){ return (5)}
####  #   return(diff)
####  # }
####  # 
####  # 
####  # player.files <- list.files(path = "/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player",pattern="players.*.txt")
####  # teams.names = c("arz", "atl","bal","buf","car","chi","cin","cle","dal","den","det","gb","hou","ind","jax","kc","mia","min","ne","no","nyg","nyj","oak","phi","pit","sd","sea","sf","stl","tb","ten","was")
####  # roster = list()
####  # setwd = path = "/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player"  
####  #   for(i in 1:length(player.files)){
####  #     temp = read.csv(paste("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player/",player.files[[i]], sep = ""), stringsAsFactors = FALSE )
####  #     temp = temp[,c("X","Pos","Age","Wt","Yrs")]
####  #     temp$Yrs = as.numeric(temp$Yrs)
####  #     temp$Age = as.numeric(temp$Age)
####  #     temp$Rooks = 0
####  #     # temp$Rooks[which(is.na(temp$Yrs))] = 1
####  #     temp = na.omit(temp)
####  #     roster[[i]] = temp
####  #     }
####  # names(roster) = teams.names
####  # 
####  # player.stats = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/player.stats.csv") 
####  # # stadium data from here: https://www.reddit.com/r/Madden/comments/2f2ux6/heres_a_spreadsheet_of_franchise_health/
####  # # Each of the 5 top home-record teams got an additional point.  Denver also got an additional point for high altitude.
####  # stadium = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/stadium.csv", stringsAsFactors = FALSE)  
####  # coaches = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/coaching.csv",stringsAsFactors = FALSE)
####  # for(i in 1:nrow(player.stats)) {
####  #   player.stats$Age[i] = mean(na.omit(roster[[i]]$Age[which(na.omit(roster[[i]]$Age != 0))]))
####  #   player.stats$Weight[i] = mean(na.omit(roster[[i]]$Wt[which(na.omit(roster[[i]]$Wt != 0))]))
####  #   player.stats$Experience[i] = mean(na.omit(roster[[i]]$Yrs[which(na.omit(roster[[i]]$Yrs != 0))]))
####  # }
####  # 
####  # GetAge = function(a,h){
####  #   age.a = player.stats$Age[which(player.stats$Team == a)]
####  #   age.h = player.stats$Age[which(player.stats$Team == h)]
####  #   diff = age.a - age.h
####  #   return(diff)
####  # }
####  # 
####  # GetLineWeight = function(a,h){
####  #   weight.a = player.stats$Weight[which(player.stats$Team == a)]
####  #   weight.h = player.stats$Weight[which(player.stats$Team == h)]
####  #   diff = weight.a - weight.h
####  #   return(diff)
####  # }
####  # GetExperience = function(a,h){
####  #   experience.a = player.stats$Experience[which(player.stats$Team == a)]
####  #   experience.h = player.stats$Experience[which(player.stats$Team == h)]
####  #   diff = experience.a - experience.h
####  #   return(diff)
####  # }
####  # GetStadium = function(h) {
####  #   stadium.h = stadium$Score[which(stadium$Team == h)]
####  #   return (-stadium.h)
####  # }
####  # GetCoaching = function(a,h) {
####  #   coaching.a = coaches$Coaching[which(coaches$Team == a)]
####  #   coaching.h = coaches$Coaching[which(coaches$Team == h)]
####  #   diff = coaching.a - coaching.h
####  #   return(diff)
####  # }
####  # GetMorale = function(a,h,hist){
####  #   d = dim(hist)[3]
####  #   for(i in d:1){
####  #     hist[which,]    
####  #   }
####  # }
####  # 
####  # 
####  # # 2015 Season Predictions
####  # # 
####  # #
####  # s.15 = FormatFutureSchedule(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/s.15.csv", header = TRUE, stringsAsFactors = FALSE))
####  # w.15 = GetWeeks(s.15)
####  # r.15.0 = GetBaseRatings(r.14.16)
####  # # write.csv(r.15.0, file ="/Users/stephen.sim/Desktop/STEPHEN.SIM/R STUDIO/Football/Football/source/r.15.0")
####  # g.15.1 = SetGamma(w.15[[1]])
####  # 
####  # gammas = g.15.1$Gamma
####  # p.15.1 = predict.ratings(r.15.0, w.15[[1]], tng.p = 0, gamma.p = gammas)
####  #   
####  # 
####  # 
####  # 
####  # 
####  # 
####  # #
####  # # Quick Viewing of some common frames
####  # #
####  # # 
####  # # View(r.11.0)
####  # # View(r.11.17$ratings)
####  # # View(w.15[[1]])
####  # # View(g.15.1)
####  # # View(p.15.1)
####  # 
# >>>>>>> origin/master
