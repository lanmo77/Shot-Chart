# load library

library("MASS")
library(reshape2) 
library(grid)
library(gridExtra)
library(png)
library(RCurl)

# MVP reference:https://en.wikipedia.org/wiki/NBA_Most_Valuable_Player_Award
playerinfo <- c(
  '201939',  'Stephen Curry', '2014-15','guard',
  '201142', 'KEVIN DURANT', '2013-14','forward',
  '2544', 'LeBron James1', '2012-13','forward',
  '2544', 'LeBron James2', '2011-12','forward',
  '201565', 'Derrick Rose', '2010-11','guard',
  '2544', 'LeBron James3', '2009-10','forward',
  '2544','LeBron James4', '2008-09','forward',
  '977','Kobe Bryant', '2007-08','guard',
  '1717','Dirk Nowitzki', '2006-07','forward'
)

playerinfo=as.data.frame(t(matrix(playerinfo,ncol=9)))
names(playerinfo)=c('id','name','season','position')

playerID.list=playerinfo$id
season.list=playerinfo$season

#SeasonType is required; TeamID is required; PlayerID is required; The GameID property is required.; 
#The Outcome property is required.; The Location property is required.; Month is required; 
#The SeasonSegment property is required.; The DateFrom property is required.; 
#The DateTo property is required.; OpponentTeamID is required; 
#The VsConference property is required.; The VsDivision property is required.; 
#The Position property is required.; The RookieYear property is required.; 
#The GameSegment property is required.; Period is required; LastNGames is required; 
#The ContextMeasure property is required.

for (i in 1:9){
playerID=playerID.list[i]
season=season.list[i]
shotURL <- paste('http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=',season,'&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=',playerID,'&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=',season,'&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0', sep = "")

# import from JSON
shotData <- fromJSON(file = shotURL, method="C")


# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

# store the data

file.name=paste(playerinfo$name[i],'.csv',sep='')


write.csv(shotDataf,file=file.name)

print(i)
}

#load the data
# change it to your own working directory
SC=read.csv("Stephen Curry.csv")
KD=read.csv("KEVIN DURANT.csv")
LJ1=read.csv('LeBron James1.csv')
LJ2=read.csv("LeBron James2.csv")
DR=read.csv("Derrick Rose.csv")
LJ3=read.csv("LeBron James3.csv")
LJ4=read.csv("LeBron James4.csv")
KB=read.csv("Kobe Bryant.csv")
DN=read.csv("Dirk Nowitzki.csv")

data1=filter(DN,LOC_Y<=470)
x=data1$LOC_X
y=data1$LOC_Y
i=9
playerID=playerID.list[i]
season=season.list[i]

# Visualization
tit=paste(playerinfo$name[i],'for',season)



# Plot the ggplot version
ggplot() + aes(x=x, y=y) +
  #annotation_custom(court)+ 
  #stat_density2d(geom="tile", aes(fill=..density..^0.25), contour=FALSE) + 
  stat_density2d(geom="tile", aes(fill=..density..^0.3), contour=FALSE) + 
  scale_fill_gradientn(name='frequency',colours = colorRampPalette(c("transparent", 'green',"yellow", "orange", "red"))(20))+
  ggtitle(tit)+
  geom_path(data = court_points,aes(x = x*10, y = y*10-47.5, group = desc)) 



# scrape player photo and save as a raster object
playerImg.URL <- paste("http://stats.nba.com/media/players/132x132/",playerID,".png", sep="")
playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)), 
                        width=unit(0.15, "npc"), height=unit(0.15, "npc"))

name1=paste(playerinfo$name[i],'heat.jpg')
dev.copy(jpeg,filename=name1);
dev.off ();

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)


ggplot(data1, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))+
  ggtitle(tit)+
  geom_path(data = court_points,aes(x = x*10, y = y*10-47.5, group = desc))


# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)

name2=paste(playerinfo$name[i],'point.jpg')
dev.copy(jpeg,filename=name2);
dev.off ();




################################################
ggplot(KD, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('Kevin Durant for 2013-2014')

ggplot(LJ1, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('LeBron James for 2012-2013')

ggplot(LJ2, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('LeBron James for 2011-2012')

ggplot(DR, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('Derrick Rose for 2010-2011')

ggplot(LJ3, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('LeBron James for 2009-2010')

ggplot(LJ4, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('LeBron James for 2008-2009')

ggplot(KB, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('Kobe Bryant for 2007-2008')

ggplot(DN, aes(x=LOC_X, y=LOC_Y)) +geom_point(aes(colour = EVENT_TYPE))+ggtitle('Dirk Nowitzki for 2006-2007')


