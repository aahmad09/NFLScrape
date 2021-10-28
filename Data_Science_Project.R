#nfldata <- read_csv("C:\Users\tlisa\OneDrive\Desktop\BAT\nfldata.csv")
#?read.csv
getwd()
#nfldata <- read.csv("nfldata.csv", stringsAsFactors = F)
nfldata <- read.csv("https://query.data.world/s/eug2qr4cjbphf5r2ptlcja4axircmi", header=TRUE, stringsAsFactors=FALSE)
view(nfldata)
#View(nfldata)
#glimpse(nfldata)

nfldata2013 <- nfldata %>% 
  filter(Season == "2013/2014") %>%
  select(Team.1, Team.2, Score.Team.1, Score.Team.2)%>%
  mutate(Winner = (if(Score.Team.1 > Score.Team.2) Team.1 else Team.2))%>%
  mutate(Loser =  (if(Score.Team.1 > Score.Team.2) Team.2 else Team.1))
  
teams<-sort(unique(c(nfldata2013$Team.1,nfldata2013$Team.2)))
         
View(nfldata2013)
#glimpse(nfldata2013)


nfldata2013$Winner<-0; nfldata2013$Loser<-0;nfldata2013$Team1Number<-0;nfldata2013$Team2Number<-0
wins<-rep(0,length(teams))
losses<-rep(0,length(teams))
adjmatrix<-matrix(0,nrow=length(teams),ncol=length(teams))
#adjmatrix
PD<-rep(0,length(teams))
PF<-rep(0,length(teams))
PA<-rep(0,length(teams))
for(i in 1:nrow(nfldata2013)){
  nfldata2013$Team1Number[i]<-which(teams==nfldata2013$Team.1[i])
  nfldata2013$Team2Number[i]<-which(teams==nfldata2013$Team.2[i])
  PF[nfldata2013$Team1Number[i]]<-PF[nfldata2013$Team1Number[i]]+nfldata2013$Score.Team.1[i]
  PF[nfldata2013$Team2Number[i]]<-PF[nfldata2013$Team2Number[i]]+nfldata2013$Score.Team.2[i]
  PA[nfldata2013$Team1Number[i]]<-PA[nfldata2013$Team1Number[i]]+nfldata2013$Score.Team.2[i]
  PA[nfldata2013$Team2Number[i]]<-PA[nfldata2013$Team2Number[i]]+nfldata2013$Score.Team.1[i]
  ifelse(nfldata2013$Score.Team.2[i]>nfldata2013$Score.Team.1[i],
         nfldata2013$Winner[i]<-which(teams==nfldata2013$Team.2[i]),
         nfldata2013$Winner[i]<-which(teams==nfldata2013$Team.1[i]))
  ifelse(nfldata2013$Score.Team.2[i]<nfldata2013$Score.Team.1[i],
         nfldata2013$Loser[i]<-which(teams==nfldata2013$Team.2[i]),
         nfldata2013$Loser[i]<-which(teams==nfldata2013$Team.1[i]))
  adjmatrix[nfldata2013$Winner[i],nfldata2013$Loser[i]]=adjmatrix[nfldata2013$Winner[i],nfldata2013$Loser[i]]+1
  wins[nfldata2013$Winner[i]]<-wins[nfldata2013$Winner[i]]+1
  losses[nfldata2013$Loser[i]]<-losses[nfldata2013$Loser[i]]+1
}
gp=wins+losses
pct=round(wins/gp,3)
Standings<-data.frame(Team=teams,GamesPlayed=gp,W=wins,L=losses,PCT=pct)
#Standings

CM<- -1*(adjmatrix+t(adjmatrix))+diag(gp+2)
B<-1+(wins-losses)/2
cr<-solve(CM,B)
Standings$ColleyRating<-cr
Standings$Rank<-rank(-cr)
Standings <- Standings %>% 
  arrange(desc(ColleyRating))
Standings
