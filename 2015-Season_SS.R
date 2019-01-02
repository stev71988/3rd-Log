## Football Season

# = Delete hash and execute
## = Notes
### = Definition 
#### = Old Code. Ignore

## Run script in Terminal to set up Excel Exports
# LD_LIBRARY_PATH=$(/usr/libexec/java_home)/jre/lib/server: open -a RStudio
# install.packages(“rJava”)
# install.packages(“xlsxjars”)
# install.packages(“xlsx”)
# library(rJava)
# library(xlsxjars)
# library(xlsx)

#cat(lsf.str())
#good tool to check errors. Prints out list of functions that correctly fired.

## Install Packages ##
install.packages("PlayerRatings")
install.packages("devtools")
install.packages("abind")
install.packages("RCurl")
# devtools::install_github("Rforecastio","hrbrmstr")

### PlayerRatings = Package within R that estimates Team Skill. Implemented methods include ELO, Glicko, and Stephenson
### Devtools = A collection of package development tools
### Rforecastio = R Package that accesses Forecast.io and gathers real-time weather data
### Abind = R Package that combines multidimensional arrays into single arrays

## Run Libraries ##
library(PlayerRatings)
library(abind)
library(devtools)
library(RCurl)
# library(Rforecastio)

##removes column 2,3,5,7,8 from season file (day, dates, @,etc)
FormatFutureSchedule = function(s)   {
  s = s[,-c(2,3,5,7,8)]
  s = s[-c(which(s[,1] == "Week")),]
  colnames(s) = c("Week", "Away", "Home")
  s$Week = as.numeric(s$Week)
  return(s)
}
##creates a list by week from the edited season file. categorizes each week.
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
##takes the end of year rating and reverts the score closer towards the mean.
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
##looks into DYAR-Absent file. If team has injured player, their DYAR effects overall ELO.
PlayerAbsence = function(elos, dyars){
  
  PlayerAbsenceDelta = function(elo.old, DYAR, Y)   {
    elo.new = elo.old - ((1-((Y - (DYAR))/Y))*2.0*(elo.old - 1500))
    return (elo.new)
  }
  
  for(i in 1:nrow(dyars)){
    team.name = dyars$Team[i]
    t.old = elos$ratings$Rating[which(elos$ratings$Player == team.name)]
    t.DYAR = dyars$DYAR[i]
    t.Y = dyars$Yards[i]
    t.new = PlayerAbsenceDelta(t.old,t.DYAR,t.Y)
    elos$ratings$Rating[which(elos$ratings$Player == team.name)] = t.new
    
    t.delta = ((t.old-t.new)/31)
    
    for(i in (c(1:32)[-which(elos$ratings$Player == team.name)])){
      old = elos$ratings$Rating[i]
      elos$ratings$Rating[i] = old + t.delta
    }
  }
  elos$ratings = elos$ratings[order(-elos$ratings$Rating),]
  
  return(elos)
}
##looks into DYAR-Return file. If a player returns from the DYAR list, we have to manually input their name back into the system to offset the ELO difference
NewPlayerReturn = function(absentelos, dyars, baseelos){
  PlayerReturnDelta = function(elo.old, DYAR, Y, elo.baseold)   {
    elo.new = elo.old + ((1-((Y - (DYAR))/Y))*2*(elo.baseold - 1500))
    return (elo.new)
  }
  
  for(i in 1:nrow(dyars)){
    team.name = dyars$Team[i]
    t.old = absentelos$ratings$Rating[which(absentelos$ratings$Player == team.name)]
    t.baseold = baseelos$ratings$Rating[which(baseelos$ratings$Player == team.name)]
    
    t.DYAR = dyars$DYAR[i]
    t.Y = dyars$Yards[i]
    t.new = PlayerReturnDelta(t.old,t.DYAR,t.Y,t.baseold)
    absentelos$ratings$Rating[which(absentelos$ratings$Player == team.name)] = t.new
    
    t.delta = ((t.old-t.new)/31)
    
    for(i in (c(1:32)[-which(absentelos$ratings$Player == team.name)])){
      old = absentelos$ratings$Rating[i]
      absentelos$ratings$Rating[i] = old + t.delta
    }
  }
  absentelos$ratings = absentelos$ratings[order(-absentelos$ratings$Rating),]
  
  return(absentelos)
}

##Creating the Gamma Table
player.stats = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/gamma/player.stats.csv") 
t = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/travel.csv", header = FALSE, na.strings="0", stringsAsFactors = FALSE)
tz = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/timezones.csv", header = TRUE, stringsAsFactors = FALSE)
stadium = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/stadium.csv", header = TRUE, stringsAsFactors = FALSE)
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo.csv", header = TRUE, stringsAsFactors = FALSE)
coaches = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/coaching.csv", header = TRUE, stringsAsFactors = FALSE)
tz$Zone = as.numeric(tz$Zone)

##formula for each gamma
SetGamma = function(s)   {
  #   locs = read.csv(file="/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/gamma/lat.long.csv", header = TRUE,stringsAsFactors = FALSE)
  #   forecasts = list() 
  #     for(i in 1:nrow(locs)){
  #       temp.forecast = get_current_forecast(locs$Lat[i],locs$Long[i])$daily
  #       forecasts[[i]] = (temp.forecast$apparentTemperatureMax + temp.forecast$apparentTemperatureMin)/2
  #     }
  #     
  #     forecasts <- do.call(rbind.data.frame,forecasts)
  #     colnames(forecasts) = c("0","1", "2","3","4","5","6","7")
  #     rownames(forecasts) = locs[,1]
  #   
  #   GetWeather = function(a,h)   {
  #     w.a = mean(as.numeric(forecasts[a,]))
  #     w.h = mean(as.numeric(forecasts[h,]))
  #     diff = abs(w.a-w.h)
  #     
  #     if(diff < 15){return (0)}
  #     else if(diff>=15 & diff < 30){ return (1)}
  #     else if(diff>=30 & diff < 40){ return (2)}
  #     else if(diff>=40 & diff < 50){ return (3)}
  #     else if(diff>=50 & diff < 60){ return (4)}
  #     else if(diff>60){ return (5)}
  #     return(diff)
  #   }
  GetAge = function(a,h)   {
    age.a = player.stats$Age[which(player.stats$Team == a)]
    age.h = player.stats$Age[which(player.stats$Team == h)]
    diff = age.a - age.h
    return(diff)
  }
  GetLineWeight = function(a,h)   {
    weight.a = player.stats$Weight[which(player.stats$Team == a)]
    weight.h = player.stats$Weight[which(player.stats$Team == h)]
    diff = weight.a - weight.h
    return(diff)
  }
  GetExperience = function(a,h)   {
    experience.a = player.stats$Experience[which(player.stats$Team == a)]
    experience.h = player.stats$Experience[which(player.stats$Team == h)]
    diff = experience.a - experience.h
    return(diff)
  }
  GetStadium = function(h)   {
    stadium.h = stadium$Score[which(stadium$Team == h)]
    return (-stadium.h)
  }
  GetCoaching = function(a,h)   {
    coaching.a = coaches$Coaching[which(coaches$Team == a)]
    coaching.h = coaches$Coaching[which(coaches$Team == h)]
    diff = coaching.a - coaching.h
    return(diff)
  }
  GetPowerCombo = function(a,h)   {
    pc.a = Power.Combo$Total[which(Power.Combo$Team == a)]
    pc.h = Power.Combo$Total[which(Power.Combo$Team == h)]
    
    diff = pc.a - pc.h
    return(diff)
  }
  GetTravel = function(a,b)   {
    GetDistance = function(a,b){
      dist = t[which(t[1,]==a), which(t[,1] == b)] 
      if(dist < 200){ 
        d = 0}
      else if(dist > 200 && dist<1000) {
        d = 1}
      else if(dist >= 1000 && dist<2000) {
        d = 2}
      else if(dist >= 2000) { 
        d = 3}
      return(d)
    }
# Removed GetPowerCombo from here

    d.tz = function(a,b){
      d = abs(tz[which(tz[,1] == a),2] - tz[which(tz[,1] == b),2])
      return(d)
    }
    score= (as.numeric(d.tz(a,b)) + as.numeric(GetDistance(a,b)))
    if( score > 5){score = 5}
    
    return(-score) # negative because home team is favored.
  }
  
  ##referencing the gamma formulas to generate data. 
  g = list()
  gamma.list = list()
  for(i in 1:nrow(s)){
    tg = as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away","Home", "LineWeight","Age","Coaching","Experience","Stadium","Travel","PowerCombo","Gamma"))))
    tg$Away = s$Away[i]
    tg$Home = s$Home[i]
    a = tg$Away
    h = tg$Home
    tg$Travel = GetTravel(a, h)
    #     tg$Temperature = GetWeather(a,h)
    tg$Age = GetAge(a,h)
    tg$LineWeight = GetLineWeight(a,h)
    tg$Experience = GetExperience(a,h)
    tg$Stadium = GetStadium(h)
    tg$Coaching = GetCoaching(a,h)
    tg$PowerCombo = GetPowerCombo(a,h)
    g[[i]] = tg
  }
  ##compiles the list of gammas and creates a table
  df <- do.call(rbind.data.frame,g)
  colnames(df) = colnames(tg) 
  
  return (df)
}

##We set scale for each gamma. 
##This is where we can adjust the gammas.
WeighGammas = function(gam){
  #   gam$Temperature = (gam$Temperature * 2)
  gam$Stadium = (gam$Stadium * 3) 
  gam$Travel = (gam$Travel * 1)
  gam$Experience = (gam$Experience * 5)
  gam$PowerCombo = (gam$PowerCombo * 5)
  gam$LineWeight = (gam$LineWeight * 1)
  gam$Age = (gam$Age * 2)
  gam$Coaching = (gam$Coaching * 2)
  for(i in 1:nrow(gam)){
    gam$Gamma[i] = sum(gam[i,-c(1,2)])
  }
  ##Overall Scale is applied across all gammas
  gam$Gamma = gam$Gamma * 1.2
  return(gam)
}

##finding the data for each gamma, but does not take the difference.
GetGammaRaw = function(s) {
  #   GetWeatherRaw = function(a,h)   {
  #     w.a = mean(as.numeric(forecasts[a,]))
  #     w.h = mean(as.numeric(forecasts[h,]))
  #     result = cbind(w.a,w.h)
  #     
  #     return(result)
  #   }
  GetAgeRaw = function(a,h)   {
    age.a = player.stats$Age[which(player.stats$Team == a)]
    age.h = player.stats$Age[which(player.stats$Team == h)]
    result = cbind(age.a,age.h)
    return(result)
  }
  GetLineWeightRaw = function(a,h)   {
    weight.a = player.stats$Weight[which(player.stats$Team == a)]
    weight.h = player.stats$Weight[which(player.stats$Team == h)]
    result = cbind(weight.a,weight.h)
    return(result)
  }
  GetExperienceRaw = function(a,h)   {
    experience.a = player.stats$Experience[which(player.stats$Team == a)]
    experience.h = player.stats$Experience[which(player.stats$Team == h)]
    result = cbind(experience.a,experience.h)
    return(result)
  }
  GetStadiumRaw = function(h)   {
    stadium.h = stadium$Score[which(stadium$Team == h)]
    return (stadium.h)
  }
  GetCoachingRaw = function(a,h)   {
    coaching.a = coaches$Coaching[which(coaches$Team == a)]
    coaching.h = coaches$Coaching[which(coaches$Team == h)]
    result  = cbind(coaching.a,coaching.h)
    return(result)
  }
  GetTravelRaw = function(a,b)   {
    GetDistance = function(a,b){
      # return( t[which(t[1,]==a), which(t[,1] == b)])
      dist = t[which(t[1,]==a), which(t[,1] == b)] 
      return(dist)
    }
    
    d.tz = function(a,b){
      d = abs(tz[which(tz[,1] == a),2] - tz[which(tz[,1] == b),2])
      return(d)
    }
    timezoneDiff = as.numeric(d.tz(a,b))
    travelDistance = as.numeric(GetDistance(a,b))
    
    result = cbind(travelDistance, timezoneDiff)
    
    return(result) # negative because home team is favored.
  }
  GetPowerComboORaw = function(a,h){
    pc.a.o = Power.Combo$Offense[which(Power.Combo$Team == a)]
    pc.h.o = Power.Combo$Offense[which(Power.Combo$Team == h)]
    
    
    result = cbind(pc.a.o,pc.h.o)
    return(result)
  }
  GetPowerComboDRaw = function(a,h){
    pc.a.d = Power.Combo$Defense[which(Power.Combo$Team == a)]
    pc.h.d = Power.Combo$Defense[which(Power.Combo$Team == h)]
    
    result = cbind(pc.a.d,pc.h.d)
    return(result)
  }  
  g = list()
  gamma.list = list()
  for(i in 1:nrow(s)){
    tgr = as.data.frame(matrix(c(0,0), nrow = 1, byrow = TRUE, dimnames = list(NULL,c("Away","Home"))))
    tgr$Away = s$Away[i]
    tgr$Home = s$Home[i]
    a = tgr$Away
    h = tgr$Home
    
    tgr = cbind(tgr,GetLineWeightRaw(a,h))
    
    tgr = cbind(tgr, GetAgeRaw(a,h))
    tgr = cbind(tgr,GetCoachingRaw(a,h))
    tgr = cbind(tgr,GetExperienceRaw(a,h))
    tgr = cbind(tgr,GetPowerComboORaw(a,h))
    tgr = cbind(tgr,GetPowerComboDRaw(a,h))
    #     tgr = cbind(tgr,GetWeatherRaw(a,h))
    tgr = cbind(tgr, GetStadiumRaw(h))
    tgr =  cbind(tgr,GetTravelRaw(a, h))
    
    g[[i]] = tgr
  }
  df <- do.call(rbind.data.frame,g)
  colnames(df) = c("Away","Home","Weight Away","Weight Home","Age Away","Age Home","Coaching Away","Coaching Home","Experience Away","Experience Home","PowerCombo Off Away","PowerCombo Off Home", "PowerCombo Def Away","PowerCombo Def Home", "Stadium Difficuly","Travel Distance","Timezone Difference")
  df = cbind(df[,c(1,2)], round(df[,-c(1,2)], 2))
  return (df)
}

##Adds a Probability column and Prediction column
predict.ratings = function(object.p, newdata.p, tng.p, gamma.p){
  df = newdata.p
  df$Probability = predict(object.p, newdata = newdata.p, tng = tng.p, gamma = gamma.p)
  df$Prediction = p.15.1 = predict(object.p, newdata = newdata.p, tng = tng.p, gamma = gamma.p, thresh = 0.5)
  return(df)
}

##Season Prep
s.15 = FormatFutureSchedule(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/PastSeason/s.15.csv", header = TRUE, stringsAsFactors = FALSE))
w.15 = GetWeeks(s.15)
o.15 = w.15
# forecastio_api_key()
# b1b3edd223a2b7a9c705b1ddd63ae005



## Week 1
#looks up player absences for week 0.
DYAR.a.1 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.a.1.csv", stringsAsFactors = FALSE))
r.15.1 = GetBaseRatings(r.14.17)
r.15.1 = PlayerAbsence(r.15.1,DYAR.a.1)

#####
# git.15.1 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/FNF-Results/p.15.1.csv")
# p.15.1 <- read.csv(text = git.15.1)
# p.15.1 = CleanRatings(p.15.1)
# r.15.1$ratings = p.15.1
#####

g.15.1 = SetGamma(w.15[[1]])
g.15.1 = WeighGammas(g.15.1)
g.15.1.raw = GetGammaRaw(w.15[[1]])
# View(g.15.1)
# View(g.15.1.raw)
View(r.15.1$ratings)

p.15.1.gamma = predict.ratings(r.15.1,w.15[[1]],tng.p = 0, gamma.p = g.15.1$Gamma)
p.15.1.nogamma = predict.ratings(r.15.1,w.15[[1]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.1.gamma)
# View(p.15.1.nogamma)



## Export Files
# write.xlsx(p.15.1.gamma, file = "p.15.1(test).xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.1.nogamma, file = "p.15.1(test).xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.1, file = "p.15.1(test).xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.1.raw, file = "p.15.1(test).xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.1(test).xlsx", sheetName="Ranking", append=TRUE)

## Week 2
## Manually input wins/losses for prior week
o.15[[1]]$Win = c(0,0,1,1,1,0,0,1,0,0,0,1,1,0,0,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.2 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.2.csv", stringsAsFactors = FALSE))
DYAR.a.2 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.a.2.csv", stringsAsFactors = FALSE))

### Rankings ###

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.2.elo = elo(o.15[[1]],status = r.15.1$ratings, kfac = 20)
# View(r.15.2.elo$ratings)

### DYAR ###
r.15.2.dyar = r.15.2.elo
r.15.2.absence = PlayerAbsence(r.15.2.dyar, DYAR.a.2)
r.15.2.return = NewPlayerReturn(r.15.2.absence, DYAR.r.2,r.15.2.dyar)
# View(r.15.2.dyar$ratings)
# View(r.15.2.absence$ratings)
# View(r.15.2.return$ratings)

##Change week# in names
g.15.2 = SetGamma(w.15[[2]])
g.15.2 = WeighGammas(g.15.2)
g.15.2.raw = GetGammaRaw(w.15[[2]])
# View(g.15.2)
# View(g.15.2.raw)


p.15.2.gamma = predict.ratings(r.15.2.dyar,w.15[[2]],tng.p = 0, gamma.p = g.15.2$Gamma)
p.15.2.nogamma = predict.ratings(r.15.2.dyar,w.15[[2]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.2.gamma)
# View(p.15.2.nogamma)

## Export Files
# write.xlsx(p.15.2.gamma, file = "p.15.2(adj).xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.2.nogamma, file = "p.15.2(adj).xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.2, file = "p.15.2(adj).xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.2.raw, file = "p.15.2(adj).xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.2(adj).xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2$ratings, file = "p.15.2(adj).xlsx", sheetName="RankingWk2", append=TRUE)

## Week 3
## Manually input wins/losses for prior week
o.15[[2]]$Win = c(1,1,0,1,0,0,0,1,1,0,0,0,0,1,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.3 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.3(ss).csv", stringsAsFactors = FALSE))
DYAR.a.3 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.3(ss).csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.3.elo = elo(o.15[[2]],status = r.15.2.elo$ratings, kfac = 20)
# View(r.15.3.elo$ratings)

### DYAR ###
r.15.3.dyar = r.15.3.elo
r.15.3.absence = PlayerAbsence(r.15.3.dyar, DYAR.a.3)
r.15.3.return = NewPlayerReturn(r.15.3.absence, DYAR.r.3,r.15.3.dyar)
# View(r.15.2.dyar$ratings)
# View(r.15.2.absence$ratings)
# View(r.15.2.return$ratings)

##Change week# in names
g.15.3 = SetGamma(w.15[[3]])
g.15.3 = WeighGammas(g.15.3)
g.15.3.raw = GetGammaRaw(w.15[[3]])
# View(g.15.3)
# View(g.15.3.raw)

p.15.3.gamma = predict.ratings(r.15.3.dyar,w.15[[3]],tng.p = 0, gamma.p = g.15.3$Gamma)
p.15.3.nogamma = predict.ratings(r.15.3.dyar,w.15[[3]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.3.gamma)
# View(p.15.3.nogamma)

## Export Files
# write.xlsx(p.15.3.gamma, file = "p.15.3(ss).xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.3.nogamma, file = "p.15.3(ss).xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.3, file = "p.15.3(ss).xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.3.raw, file = "p.15.3(ss).xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.3(ss).xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.3(ss).xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.3(ss).xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.3.dyar$ratings, file = "p.15.3(ss).xlsx", sheetName="RankingWk3(DYAR)", append=TRUE)

## Week 4
## Manually input wins/losses for prior week
o.15[[3]]$Win = c(0,0,1,1,0,0,0,1,1,1,1,0,1,0,1,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
# DYAR.r.4 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.4.csv", stringsAsFactors = FALSE))
# DYAR.a.4 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.4.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.4.elo = elo(o.15[[3]],status = r.15.3.elo$ratings, kfac = 20)
# View(r.15.4.elo$ratings)

### DYAR ###
# r.15.3.dyar = r.15.3.elo
# r.15.3.absence = PlayerAbsence(r.15.3.dyar, DYAR.a.3)
# r.15.3.return = NewPlayerReturn(r.15.3.absence, DYAR.r.3,r.15.3.dyar)
# View(r.15.2.dyar$ratings)
# View(r.15.2.absence$ratings)
# View(r.15.2.return$ratings)

##Change week# in names
g.15.4 = SetGamma(w.15[[4]])
g.15.4 = WeighGammas(g.15.4)
g.15.4.raw = GetGammaRaw(w.15[[4]])
# View(g.15.4)
# View(g.15.4.raw)

# View(r.15.4.elo$ratings)

p.15.4.gamma = predict.ratings(r.15.4.elo,w.15[[4]],tng.p = 0, gamma.p = g.15.4$Gamma)
p.15.4.nogamma = predict.ratings(r.15.4.elo,w.15[[4]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.4.gamma)
# View(p.15.4.nogamma)

## Export Files
# write.xlsx(p.15.4.gamma, file = "p.15.4.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.4.nogamma, file = "p.15.4.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.4, file = "p.15.4.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.4.raw, file = "p.15.4.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.4.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.4.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.4.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.4.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.4.dyar$ratings, file = "p.15.4.xlsx", sheetName="RankingWk4(DYAR)", append=TRUE)

## Week 5
## Manually input wins/losses for prior week
o.15[[4]]$Win = c(1,0,1,0,0,0,1,0,0,1,0,1,0,1,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.5 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.5.csv", stringsAsFactors = FALSE))
DYAR.a.5 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.5.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.5.elo = elo(o.15[[4]],status = r.15.4.elo$ratings, kfac = 25)
# View(r.15.5.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk5Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.5.dyar = r.15.5.elo
r.15.5.absence = PlayerAbsence(r.15.5.dyar, DYAR.a.5)
r.15.5.return = NewPlayerReturn(r.15.5.absence, DYAR.r.5,r.15.5.dyar)
r.15.5.newelo = r.15.5.return
# View(r.15.5.dyar$ratings)
# View(r.15.5.absence$ratings)
# View(r.15.5.return$ratings)

##Change week# in names
g.15.5 = SetGamma(w.15[[5]])
g.15.5 = WeighGammas(g.15.5)
g.15.5.raw = GetGammaRaw(w.15[[5]])
# View(g.15.5)
# View(g.15.5.raw)

p.15.5.gamma = predict.ratings(r.15.5.newelo,w.15[[5]],tng.p = 0, gamma.p = g.15.5$Gamma)
p.15.5.nogamma = predict.ratings(r.15.5.newelo,w.15[[5]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.5.gamma)
# View(p.15.5.nogamma)

## Export Files
# write.xlsx(p.15.5.gamma, file = "p.15.5.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.5.nogamma, file = "p.15.5.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.5, file = "p.15.5.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.5.raw, file = "p.15.5.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.5.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.5.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.5.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.5.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.5.xlsx", sheetName="RankingWk5", append=TRUE)

## Week 6
## Manually input wins/losses for prior week
o.15[[5]]$Win = c(1,0,0,0,1,1,0,1,0,1,1,1,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.6 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.6.csv", stringsAsFactors = FALSE))
DYAR.a.6 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.6.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.6.elo = elo(o.15[[5]],status = r.15.5.elo$ratings, kfac = 25)
# View(r.15.6.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk6Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.6.dyar = r.15.6.elo
r.15.6.absence = PlayerAbsence(r.15.6.dyar, DYAR.a.6)
r.15.6.return = NewPlayerReturn(r.15.6.absence, DYAR.r.6,r.15.6.dyar)
r.15.6.newelo = r.15.6.return
# View(r.15.6.dyar$ratings)
# View(r.15.6.absence$ratings)
# View(r.15.6.return$ratings)

##Change week# in names
g.15.6 = SetGamma(w.15[[6]])
g.15.6 = WeighGammas(g.15.6)
g.15.6.raw = GetGammaRaw(w.15[[6]])
# View(g.15.6)
# View(g.15.6.raw)

p.15.6.gamma = predict.ratings(r.15.6.newelo,w.15[[6]],tng.p = 0, gamma.p = g.15.6$Gamma)
p.15.6.nogamma = predict.ratings(r.15.6.newelo,w.15[[6]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.6.gamma)
# View(p.15.6.nogamma)

## Export Files
# write.xlsx(p.15.6.gamma, file = "p.15.6.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.6.nogamma, file = "p.15.6.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.6, file = "p.15.6.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.6.raw, file = "p.15.6.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.6.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.6.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.6.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.6.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.6.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.6.xlsx", sheetName="RankingWk6", append=TRUE)

## Week 7
## Manually input wins/losses for prior week
o.15[[6]]$Win = c(0,1,1,0,1,0,0,1,0,1,0,0,1,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.7 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.7.csv", stringsAsFactors = FALSE))
DYAR.a.7 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.7.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.7.elo = elo(o.15[[6]],status = r.15.6.elo$ratings, kfac = 25)
# View(r.15.7.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk7Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.7.dyar = r.15.7.elo
r.15.7.absence = PlayerAbsence(r.15.7.dyar, DYAR.a.7)
r.15.7.return = NewPlayerReturn(r.15.7.absence, DYAR.r.7,r.15.7.dyar)
r.15.7.newelo = r.15.7.return
# View(r.15.7.dyar$ratings)
# View(r.15.7.absence$ratings)
# View(r.15.7.return$ratings)

##Change week# in names
g.15.7 = SetGamma(w.15[[7]])
g.15.7 = WeighGammas(g.15.7)
g.15.7.raw = GetGammaRaw(w.15[[7]])
# View(g.15.7)
# View(g.15.7.raw)

p.15.7.gamma = predict.ratings(r.15.7.newelo,w.15[[7]],tng.p = 0, gamma.p = g.15.7$Gamma)
p.15.7.nogamma = predict.ratings(r.15.7.newelo,w.15[[7]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.7.gamma)
# View(p.15.7.nogamma)

## Export Files
# write.xlsx(p.15.7.gamma, file = "p.15.7.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.7.nogamma, file = "p.15.7.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.7, file = "p.15.7.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.7.raw, file = "p.15.7.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.7.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.7.xlsx", sheetName="RankingWk7", append=TRUE)


## Week 8
## Manually input wins/losses for prior week
o.15[[7]]$Win = c(1,1,1,0,0,0,1,0,0,1,0,0,0,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.8 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.8.csv", stringsAsFactors = FALSE))
DYAR.a.8 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.8.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.8.elo = elo(o.15[[7]],status = r.15.7.elo$ratings, kfac = 25)
# View(r.15.8.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk8Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.8.dyar = r.15.8.elo
r.15.8.absence = PlayerAbsence(r.15.8.dyar, DYAR.a.8)
r.15.8.return = NewPlayerReturn(r.15.8.absence, DYAR.r.8,r.15.8.dyar)
r.15.8.newelo = r.15.8.return
# View(r.15.8.dyar$ratings)
# View(r.15.8.absence$ratings)
# View(r.15.8.return$ratings)

##Change week# in names
g.15.8 = SetGamma(w.15[[8]])
g.15.8 = WeighGammas(g.15.8)
g.15.8.raw = GetGammaRaw(w.15[[8]])
# View(g.15.8)
# View(g.15.8.raw)

p.15.8.gamma = predict.ratings(r.15.8.newelo,w.15[[8]],tng.p = 0, gamma.p = g.15.8$Gamma)
p.15.8.nogamma = predict.ratings(r.15.8.newelo,w.15[[8]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.8.gamma)
# View(p.15.8.nogamma)

## Export Files
# write.xlsx(p.15.8.gamma, file = "p.15.8.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.8.nogamma, file = "p.15.8.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.8, file = "p.15.8.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.8.raw, file = "p.15.8.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.8.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.8.xlsx", sheetName="RankingWk8", append=TRUE)


## Week 9
## Manually input wins/losses for prior week
o.15[[8]]$Win = c(0,1,1,1,0,0,1,0,0,0,1,0,0,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.9 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.9.csv", stringsAsFactors = FALSE))
DYAR.a.9 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.9.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.9.elo = elo(o.15[[8]],status = r.15.8.elo$ratings, kfac = 25)
# View(r.15.8.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk9Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.9.dyar = r.15.9.elo
r.15.9.absence = PlayerAbsence(r.15.9.dyar, DYAR.a.9)
r.15.9.return = NewPlayerReturn(r.15.9.absence, DYAR.r.9,r.15.9.dyar)
r.15.9.newelo = r.15.9.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.9 = SetGamma(w.15[[9]])
g.15.9 = WeighGammas(g.15.9)
g.15.9.raw = GetGammaRaw(w.15[[9]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.9.gamma = predict.ratings(r.15.9.newelo,w.15[[9]],tng.p = 0, gamma.p = g.15.9$Gamma)
p.15.9.nogamma = predict.ratings(r.15.9.newelo,w.15[[9]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.9.gamma)
# View(p.15.9.nogamma)

## Export Files
# write.xlsx(p.15.9.gamma, file = "p.15.9.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.9.nogamma, file = "p.15.9.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.9, file = "p.15.9.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.9.raw, file = "p.15.9.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.9.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.9.xlsx", sheetName="RankingWk9", append=TRUE)


## Week 10
## Manually input wins/losses for prior week
o.15[[9]]$Win = c(0,0,0,0,1,0,0,0,0,1,0,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.10 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.10.csv", stringsAsFactors = FALSE))
DYAR.a.10 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.10.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.10.elo = elo(o.15[[9]],status = r.15.9.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk10Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.10.dyar = r.15.10.elo
r.15.10.absence = PlayerAbsence(r.15.10.dyar, DYAR.a.10)
r.15.10.return = NewPlayerReturn(r.15.10.absence, DYAR.r.10,r.15.10.dyar)
r.15.10.newelo = r.15.10.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.10 = SetGamma(w.15[[10]])
g.15.10 = WeighGammas(g.15.10)
g.15.10.raw = GetGammaRaw(w.15[[10]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.10.gamma = predict.ratings(r.15.10.newelo,w.15[[10]],tng.p = 0, gamma.p = g.15.10$Gamma)
p.15.10.nogamma = predict.ratings(r.15.10.newelo,w.15[[10]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.9.gamma)
# View(p.15.9.nogamma)

## Export Files
# write.xlsx(p.15.10.gamma, file = "p.15.10.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.10.nogamma, file = "p.15.10.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.10, file = "p.15.10.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.10.raw, file = "p.15.10.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.10.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk9", append=TRUE)
# write.xlsx(r.15.10.elo$ratings, file = "p.15.10.xlsx", sheetName="RankingWk10", append=TRUE)

## Week 11
## Manually input wins/losses for prior week
o.15[[10]]$Win = c(1,1,1,1,0,1,1,0,0,1,1,1,1,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.11 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.11.csv", stringsAsFactors = FALSE))
DYAR.a.11 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.11.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.11.elo = elo(o.15[[10]],status = r.15.10.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk11Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.11.dyar = r.15.11.elo
r.15.11.absence = PlayerAbsence(r.15.11.dyar, DYAR.a.11)
r.15.11.return = NewPlayerReturn(r.15.11.absence, DYAR.r.11,r.15.11.dyar)
r.15.11.newelo = r.15.11.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.11 = SetGamma(w.15[[11]])
g.15.11 = WeighGammas(g.15.11)
g.15.11.raw = GetGammaRaw(w.15[[11]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.11.gamma = predict.ratings(r.15.11.newelo,w.15[[11]],tng.p = 0, gamma.p = g.15.11$Gamma)
p.15.11.nogamma = predict.ratings(r.15.11.newelo,w.15[[11]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.9.gamma)
# View(p.15.9.nogamma)

## Export Files
# write.xlsx(p.15.11.gamma, file = "p.15.11.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.11.nogamma, file = "p.15.11.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.11, file = "p.15.11.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.11.raw, file = "p.15.11.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.11.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk9", append=TRUE)
# write.xlsx(r.15.10.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk10", append=TRUE)
# write.xlsx(r.15.11.elo$ratings, file = "p.15.11.xlsx", sheetName="RankingWk11", append=TRUE)

## Week 12
## Manually input wins/losses for prior week
o.15[[11]]$Win = c(0,1,0,1,0,0,1,1,1,0,0,0,1,0)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.12 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.12.csv", stringsAsFactors = FALSE))
DYAR.a.12 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.12.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.12.elo = elo(o.15[[11]],status = r.15.11.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk12Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.12.dyar = r.15.12.elo
r.15.12.absence = PlayerAbsence(r.15.12.dyar, DYAR.a.12)
r.15.12.return = NewPlayerReturn(r.15.12.absence, DYAR.r.12,r.15.12.dyar)
r.15.12.newelo = r.15.12.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.12 = SetGamma(w.15[[12]])
g.15.12 = WeighGammas(g.15.12)
g.15.12.raw = GetGammaRaw(w.15[[12]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.12.gamma = predict.ratings(r.15.12.newelo,w.15[[12]],tng.p = 0, gamma.p = g.15.12$Gamma)
p.15.12.nogamma = predict.ratings(r.15.12.newelo,w.15[[12]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.9.gamma)
# View(p.15.9.nogamma)

## Export Files
# write.xlsx(p.15.12.gamma, file = "p.15.12.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.12.nogamma, file = "p.15.12.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.12, file = "p.15.12.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.12.raw, file = "p.15.12.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.12.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk9", append=TRUE)
# write.xlsx(r.15.10.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk10", append=TRUE)
# write.xlsx(r.15.11.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk11", append=TRUE)
# write.xlsx(r.15.12.elo$ratings, file = "p.15.12.xlsx", sheetName="RankingWk12", append=TRUE)


## Week 13
## Manually input wins/losses for prior week
o.15[[12]]$Win = c(0,1,1,1,0,0,0,1,0,0,1,0,1,0,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.13 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.13.csv", stringsAsFactors = FALSE))
DYAR.a.13 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.13.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.13.elo = elo(o.15[[12]],status = r.15.12.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk13Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.13.dyar = r.15.13.elo
r.15.13.absence = PlayerAbsence(r.15.13.dyar, DYAR.a.13)
r.15.13.return = NewPlayerReturn(r.15.13.absence, DYAR.r.13,r.15.13.dyar)
r.15.13.newelo = r.15.13.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.13 = SetGamma(w.15[[13]])
g.15.13 = WeighGammas(g.15.13)
g.15.13.raw = GetGammaRaw(w.15[[13]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.13.gamma = predict.ratings(r.15.13.newelo,w.15[[13]],tng.p = 0, gamma.p = g.15.13$Gamma)
p.15.13.nogamma = predict.ratings(r.15.13.newelo,w.15[[13]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.9.gamma)
# View(p.15.9.nogamma)

## Export Files
# write.xlsx(p.15.13.gamma, file = "p.15.13.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.13.nogamma, file = "p.15.13.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.13, file = "p.15.13.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.13.raw, file = "p.15.13.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.13.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk9", append=TRUE)
# write.xlsx(r.15.10.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk10", append=TRUE)
# write.xlsx(r.15.11.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk11", append=TRUE)
# write.xlsx(r.15.12.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk12", append=TRUE)
# write.xlsx(r.15.13.elo$ratings, file = "p.15.13.xlsx", sheetName="RankingWk13", append=TRUE)

## Week 14
## Manually input wins/losses for prior week
o.15[[13]]$Win = c(1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.14 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.14.csv", stringsAsFactors = FALSE))
DYAR.a.14 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.14.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.14.elo = elo(o.15[[13]],status = r.15.13.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk14Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.14.dyar = r.15.14.elo
r.15.14.absence = PlayerAbsence(r.15.14.dyar, DYAR.a.14)
r.15.14.return = NewPlayerReturn(r.15.14.absence, DYAR.r.14,r.15.14.dyar)
r.15.14.newelo = r.15.14.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.14 = SetGamma(w.15[[14]])
g.15.14 = WeighGammas(g.15.14)
g.15.14.raw = GetGammaRaw(w.15[[14]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.14.gamma = predict.ratings(r.15.14.newelo,w.15[[14]],tng.p = 0, gamma.p = g.15.14$Gamma)
p.15.14.nogamma = predict.ratings(r.15.14.newelo,w.15[[14]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.14.gamma)
# View(p.15.9.nogamma)

#####
git.15.14 = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/FNF-Results/p.15.14.csv")
git.15.14.gamma = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/FNF-Results/p.15.14.gamma.csv")
git.15.14.nogamma = getURL("https://raw.githubusercontent.com/stev71988/Firstborn_Football/master/FNF-Results/p.15.14.gammaraw.csv")
p.15.14 <- read.csv(text = git.15.14)
p.15.14.gamma <- read.csv(text = git.15.14.gamma)
p.15.14.nogamma <- read.csv(text = git.15.14.nogamma)
# p.15.1 = CleanRatings(p.15.1)
r.15.15$ratings = p.15.15
#####

## Export Files
# write.xlsx(p.15.14.gamma, file = "p.15.14.xlsx", sheetName="Predictions", append=FALSE)
# write.xlsx(p.15.14.nogamma, file = "p.15.14.xlsx", sheetName="NoGamma", append=TRUE)
# write.xlsx(g.15.14, file = "p.15.14.xlsx", sheetName="GammaScale", append=TRUE)
# write.xlsx(g.15.14.raw, file = "p.15.14.xlsx", sheetName="GammaRaw", append=TRUE)
# write.xlsx(r.15.1$ratings, file = "p.15.14.xlsx", sheetName="RankingWk1", append=TRUE)
# write.xlsx(r.15.2.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk2", append=TRUE)
# write.xlsx(r.15.3.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk3", append=TRUE)
# write.xlsx(r.15.4.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk4", append=TRUE)
# write.xlsx(r.15.5.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk5", append=TRUE)
# write.xlsx(r.15.6.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk6", append=TRUE)
# write.xlsx(r.15.7.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk7", append=TRUE)
# write.xlsx(r.15.8.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk8", append=TRUE)
# write.xlsx(r.15.9.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk9", append=TRUE)
# write.xlsx(r.15.10.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk10", append=TRUE)
# write.xlsx(r.15.11.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk11", append=TRUE)
# write.xlsx(r.15.12.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk12", append=TRUE)
# write.xlsx(r.15.13.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk13", append=TRUE)
# write.xlsx(r.15.14.elo$ratings, file = "p.15.14.xlsx", sheetName="RankingWk14", append=TRUE)

## Week 15
## Manually input wins/losses for prior week
o.15[[14]]$Win = c(1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1)

##Look up newly injured players. Add their names into file named "DYAR-Absent"
##Look up returning players. Add their names into file named "DYAR-Return"
DYAR.r.15 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Return/DYAR.R.15.csv", stringsAsFactors = FALSE))
DYAR.a.15 = na.omit(read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/DYAR-Absent/DYAR.A.15.csv", stringsAsFactors = FALSE))

##Applies the actual outcome of games into ELO and generates new Ratings
r.15.15.elo = elo(o.15[[14]],status = r.15.14.elo$ratings, kfac = 25)
# View(r.15.10.elo$ratings)

## Impact Players with Updated Madden Ratings
Power.Combo = read.csv("/Users/stephen.sim/Desktop/STEPHEN.SIM/RSTUDIO/Football/Football/source/Gamma/PowerCombo(Wk15Update).csv", header = TRUE, stringsAsFactors = FALSE)

### DYAR ###
r.15.15.dyar = r.15.15.elo
r.15.15.absence = PlayerAbsence(r.15.15.dyar, DYAR.a.15)
r.15.15.return = NewPlayerReturn(r.15.15.absence, DYAR.r.15,r.15.15.dyar)
r.15.15.newelo = r.15.15.return
# View(r.15.9.dyar$ratings)
# View(r.15.9.absence$ratings)
# View(r.15.9.return$ratings)

##Change week# in names
g.15.15 = SetGamma(w.15[[15]])
g.15.15 = WeighGammas(g.15.15)
g.15.15.raw = GetGammaRaw(w.15[[15]])
# View(g.15.9)
# View(g.15.9.raw)

p.15.15.gamma = predict.ratings(r.15.15.newelo,w.15[[15]],tng.p = 0, gamma.p = g.15.15$Gamma)
p.15.15.nogamma = predict.ratings(r.15.15.newelo,w.15[[15]],tng.p = 0, gamma.p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# View(p.15.15.gamma)
# View(p.15.9.nogamma)

## Export Files
write.xlsx(p.15.15.gamma, file = "p.15.15.xlsx", sheetName="Predictions", append=FALSE)
write.xlsx(p.15.15.nogamma, file = "p.15.15.xlsx", sheetName="NoGamma", append=TRUE)
write.xlsx(g.15.15, file = "p.15.15.xlsx", sheetName="GammaScale", append=TRUE)
write.xlsx(g.15.15.raw, file = "p.15.15.xlsx", sheetName="GammaRaw", append=TRUE)
write.xlsx(r.15.1$ratings, file = "p.15.15.xlsx", sheetName="RankingWk1", append=TRUE)
write.xlsx(r.15.2.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk2", append=TRUE)
write.xlsx(r.15.3.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk3", append=TRUE)
write.xlsx(r.15.4.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk4", append=TRUE)
write.xlsx(r.15.5.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk5", append=TRUE)
write.xlsx(r.15.6.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk6", append=TRUE)
write.xlsx(r.15.7.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk7", append=TRUE)
write.xlsx(r.15.8.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk8", append=TRUE)
write.xlsx(r.15.9.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk9", append=TRUE)
write.xlsx(r.15.10.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk10", append=TRUE)
write.xlsx(r.15.11.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk11", append=TRUE)
write.xlsx(r.15.12.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk12", append=TRUE)
write.xlsx(r.15.13.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk13", append=TRUE)
write.xlsx(r.15.14.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk14", append=TRUE)
write.xlsx(r.15.15.elo$ratings, file = "p.15.15.xlsx", sheetName="RankingWk15", append=TRUE)
