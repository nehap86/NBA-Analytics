#NBA Analytics Project

library(dplyr)
library(prob)
library(SportsAnalytics) 
library(sampling)
library(tidyverse)
library(lubridate)
library(stringr)
library(readr) 
library(googleVis)
library(rvest)
library(plyr)
library(RCurl)
library(RJSONIO)

##Preparing The Data

nba1718 <- fetch_NBAPlayerStatistics("17-18")
head(nba1718)
str(nba1718)

#------- only select columns of interest & preprocess data
nba1718 <- nba1718 %>%
  select(League,Name,Team,GamesPlayed,TotalMinutesPlayed,
         FieldGoalsMade,FieldGoalsAttempted,PersonalFouls,Disqualifications,TotalPoints) 

#completecases
nba1718 <- nba1718[complete.cases(nba1718),] 
nba1718$Name <- sapply(nba1718$Name,as.character) 
#Removing unwanted character from Player-name
nba1718$Name <- gsub(".*,","",nba1718$Name)
#remove white-Space
nba1718$Name <- str_trim(nba1718$Name,side = "both")


#Analyzing the Data
attach(nba1718)
summary(nba1718)

#Visualize 
ggplot(data = nba1718, aes(x = as.factor(Team))) + geom_bar(aes(fill = as.factor(Team))) + 
  labs(x = "Team", y = "League Played", title = "Number of times-Team played League", fill = "Team") + theme(plot.title = element_text(hjust = 0.5))

#Team vs GamesPlayed
nba1718 %>% group_by(Team,GamesPlayed)
summary(GamesPlayed)
plot(Team,GamesPlayed,col=hcl(2),main="GamesPlayed by Each Team",xlab = "Teams",ylab ="Number of Games Played", las=2)

Max.Games.Team <- nba1718 %>% group_by(Team) %>% summarize(GamesPlayed = max(GamesPlayed))
Max.Games.Team
plot(Max.Games.Team$GamesPlayed, xlab="Team",ylab="GamesPlayed",type="b",pch=16,xaxt='n',col="red",main="Maximum number of Games Played by a Team")
axis(1, at=seq(1,33,by=1),labels=Max.Games.Team$Team, las = 2)

#GoalAttempted Vs GoalMade

summary(FieldGoalsAttempted)
summary(FieldGoalsMade)

plot(FieldGoalsAttempted,FieldGoalsMade,xlab="Goals-Attempted",ylab="Goals-Made", main="Goal Made vs Goal-Attempted ",pch =16,col="dark blue")

Max.Team.Goals <- nba1718 %>% group_by(Team) %>% summarize(GoalsMade=max(FieldGoalsMade))
Max.Team.Goals

plot(Max.Team.Goals$GoalsMade , xlab="Team",ylab="GoalsMade",type="b",pch=16,xaxt='n',col="red",main="Maximum number of Goals by a Team")
axis(1, at=seq(1,33,by=1),labels=Max.Games.Team$Team, las = 2)

average_success_for_goals <- 100*(mean(FieldGoalsMade)/mean(FieldGoalsAttempted))
average_success_for_goals

#Player vs TotalPoints(Top20)
Max.Player.Points <- nba1718 %>% group_by(Name) %>% summarize(TotalPoints=max(TotalPoints))
Max.Player.Points <- Max.Player.Points %>% top_n(20)

plot(Max.Player.Points$TotalPoints, xlab="Name",ylab="TotalPoints",type="b",pch=16,xaxt='n',col="dark green",main="Top 20 Players-with Maximum Points")
axis(1, at=seq(1,20,by=1),labels=Max.Player.Points$Name,las=2)



#Examine the distribution of the data.
#TotalPoints

z1 <- sort(TotalPoints)

#---Normal Distribution
d <- pnorm(z1,mean(z1),sd(z1))
plot(z1, d, type="l",lwd = 2.5,xlab="TotalPoints",ylab="PDF",main = "PDF plot of Total Points by Teams")

d <- round(rnorm(z1,mean(z1),sd(z1)))
plot(z1, sort(d), type="l",lwd = 2.5,xlab="TotalPoints",ylab="PDF",main = "PDF plot of Total Points by Teams")



#---Central Limit Theorem

hist(z1,col=hcl(0))
mean(z1)
sd(z1)

par(mfrow=c(1,3))
samples=1000
sample.size=10
z2<-numeric(samples)
for (i in 1:samples){
  z2[i]<-mean(sample(z1,size=sample.size))
}
hist(z2,main = "Histogram(size=1000)",col = "light blue")

sample.size=500

z3<-numeric(samples)
for (i in 1:samples){
  z3[i]<-mean(sample(z1,size=sample.size))
}
hist(z3,main = "Histogram(size=500)",col = "light green")


#---Simple Random Sampling #GamesPlayed
par(mfrow=c(1,1))
z <- srswor(200,nrow(nba1718))
z <- nba1718[z != 0,]  

p <- prop.table(table(z$GamesPlayed))
plot(p,xlab="Games-Played", main ="Simple Random Sampling",xaxt="n",col="brown")
abline(h=0)
axis(side = 1, at = 0:82, labels = TRUE)

#---Systematic Sampling
N <- nrow(nba1718)
n <- 540
k <- ceiling(N / n)
k
#random item from first group
r <- sample(k,1)
r
s <- seq(r,by=k,length=n)
sample3 <- nba1718[s,]


p1 <- prop.table(table(sample3$GamesPlayed))
plot(p1,xlab="Games-Played", main ="Systematic Sampling",xaxt="n",col ="dark green")
abline(h=0)
axis(side = 1, at = 0:82, labels = TRUE)

#---Changing categorical to numeric for inclusionprobability
GP <- inclusionprobabilities(GamesPlayed,540)
length(GP)
sum(GP)
s1 <- UPsystematic(GP)
sample <- nba1718[s1 != 0,]

p2 <- prop.table(table(sample$GamesPlayed))
plot(p2,xlab="Games-Played", main ="InclusionProbability",xaxt="n",col="dark blue")
abline(h=0)
axis(side = 1, at = 0:82, labels = TRUE)

#---Stratified Sampling
nba1718 <- nba1718[order(Team),]
freq <- table(Team)

size <- 540 * freq / sum(freq);size <- round(size);size <- as.vector(size)
size <- size[size != 0]
sample1 <- strata(nba1718,stratanames = c("Team"),size = size ,method = "srswr",description=TRUE)


p3 <- prop.table(table(sample1$Team))*100
plot(p3,xlab="Team", main ="Stratified Sampling- for Team",xaxt="n",col= "red")
abline(h=0)
axis(side = 1, at = 0:34, labels = TRUE)

#------Team Performance NBA 2007-2008-------#


nba0708 <- fetch_NBAPlayerStatistics("07-08")
attach(nba0708)

nba0708.bos <- subset(nba0708, Team == 'BOS')

#Q1

nba0708.bos%>%
  mutate(three.point.percentage=ThreesMade/ThreesAttempted)%>%
  filter(three.point.percentage==max(three.point.percentage,na.rm=T))%>%
  select(Name,three.point.percentage)

#Q2
nba0708.bos%>%
  filter(TotalMinutesPlayed==max(TotalMinutesPlayed))%>%
  select(Name,TotalMinutesPlayed)

#Q3
nba0708.bos%>%
  filter(Steals==max(Steals))%>%
  select(Name,Steals)


#5 teams in descending order
url <- read_html("https://www.basketball-reference.com/leagues/NBA_2008.html")
data<-html_nodes(x=url,css=".full_table .left+ .right , .full_table .left")%>%html_text()

team<-data[c(TRUE,FALSE)] # extract teams' names
wins<-data[c(FALSE,TRUE)]
#clean data
team<-gsub(pattern =  "\\([^()]+\\)","",team) 
team<-gsub("*", "", team)
team<-gsub("\\*", "", team)
team<-gsub(intToUtf8(160),'',team)
teamwins<-data.frame(team,wins)

teamwins$wins<-as.numeric(as.character(teamwins$wins))
Top.5<-teamwins%>%top_n(5,wins)
Top.5

#Alternate-Top 5 Wins
Top.5.Team <- nba0708 %>% select(Team,TotalPoints)
Top.5.Team <- Top.5.Team[order(Top.5.Team$TotalPoints,decreasing = T),]
head(Top.5.Team,5)

#Five Google Charts
#Chart 1 (Pie-chart)

best_players <- nba0708[order(nba0708$TotalPoints,decreasing = TRUE),][1:10,c(2,21)]
best_players_pie <- gvisPieChart(best_players, options=list(width=700, height=450))
plot(best_players_pie)
print(best_players_pie, file = "gauge_gp.html")


#Chart2 (colunmn-chart)
chart2 <- 
  gvisColumnChart(
    nba0708.bos,
    xvar = "Name",
    yvar = c("TotalPoints","GamesPlayed"),
    options=list(
      legend="top",
      height=500, width=850))
plot(chart2)
print(chart2, file = "chart2.html")

#Chart-3(Line Chart)

by_points<- aggregate(TotalPoints,by=list(Team),FUN=sum)
names(by_points) <- c("Team","TotalPoints")

by_games <- aggregate(GamesPlayed,by=list(Team),FUN =sum)
names(by_games) <- c("Team","GamesPlayed")

merged <- merge(by_games,by_points,by="Team",sort=F)
merged

#LineChart
Line <- gvisLineChart(merged,options=list(width=800, height=500))
plot(Line)
print(Line, file = "Line.html")

#ColumnChart
Column <- gvisColumnChart(merged,options=list(width=800, height=500))
plot(Column)
print(Column, file = "column.html")
#MergedChart
merged_Chart<- gvisMerge(Line,Column)
plot(merged_Chart)
print(merged_Chart, file = "merged.html")


#Chart4(Combo-Chart)
combo_Chart <- gvisComboChart(merged,xvar = "Team",yvar = c("TotalPoints","GamesPlayed"),
                              options = list(width=1400,seriesType = "line",series = '{1:{type : "bars"}}'))
plot(combo_Chart)
print(combo_Chart, file = "combo.html")

bubble <- gvisBubbleChart(merged,idvar="Team",xvar="TotalPoints",yvar ="GamesPlayed",
                          options=list(colors="['#2ca25f']",
                                       title = "Relation between Points and Gamesplayed for Teams"))
plot(bubble)
print(bubble, file = "bubble.html")

#Chart5(gauge-Chart)
#TotalPoints
gauge_chart_points <- gvisGauge(by_points, options=list(min=0, max=4030,
                                                        redFrom=2000, redTo=4030, 
                                                        yellowFrom=1000, yellowTo=2000,
                                                        greenFrom=0, greenTo=1000, 
                                                        width=650, height=500))
plot(gauge_chart_points)
print(gauge_chart_points, file = "gauge_tp.html")

gauge_gamesplayed <- gvisGauge(by_games, options=list(min=0, max=4030,
                                                      redFrom=2000, redTo=4030, 
                                                      yellowFrom=1000, yellowTo=2000,
                                                      greenFrom=0, greenTo=1000, 
                                                      width=650, height=500))
plot(gauge_gamesplayed)
print(gauge_gamesplayed, file = "gauge_gp.html")

#GeoChart

url2 <- read_html("http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
data2<-html_nodes(x=url2,css="td:nth-child(3) a , .a-right a")%>%html_text()

year<-word(data2[c(TRUE,FALSE)],-1)
country<-gsub("\r\n\t\t\t\t\t \r\n\t\t\t\t  ","",data2[c(FALSE,TRUE)])

winner<-data.frame(year,country)
data1<-winner%>%group_by(year,country)

G1a <- gvisGeoChart(data1, locationvar='country', colorvar='year')
plot(G1a)
print(G1a, file = "GeoChart.html")


