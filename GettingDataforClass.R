library(rvest)
library(tidyverse)
library(DescTools)
library(stringr)



getID<- function(fullName) {
  #fullName="Martijn Willemsen"
  #fullName="Jan de Jong"
  search<-paste0("https://www.osta.nl/index.php?ZoekStr=",gsub(" ","+",fullName))
  peoplePage<-read_html(search)
  skaters<-data.frame(peoplePage %>%  html_nodes(xpath="//*[@id='main']/table")%>%html_table())
  if (length(skaters)>0){
    skaters$id <- peoplePage %>% html_nodes(xpath = "//*[@id='main']/table/tr/td[1]/a") %>% html_attr("href") %>% substr(6,100) 
    skaters$lastSeason<-str_sub(skaters$Seizoen.en.,-4,-1)
  }
  else
  {
    id<- peoplePage %>% html_nodes(xpath = "//*[@id='tijden']/input[1]") %>% html_attr("value")
    seasons<-peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h2[1]") %>% html_text2()
    lastSeason<-substr(seasons, 6, 9)
    Vereniging<-peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h3[1]") %>% html_text2()
    Vereniging<-strsplit(Vereniging,", ")[[1]][2]
    skaters<-data.frame(cbind(id,lastSeason, Vereniging))
    skaters$Naam=fullName
  }
  return(filter(skaters, str_length(id)>17 & lastSeason=="2025") %>% select(id, Naam, Vereniging))
}

getAllRaces<- function(skate_id, season="all") {
  l<-paste0("https://www.osta.nl/index.php?pid=",skate_id,"&Seizoen=",season,"&Afstand=&perAfstand=0&Verkort=1")
  ar<-read_html(l)
  allraces <-ar %>%  html_nodes(xpath="//div[@class='seizoen']")
  s<-allraces %>% html_nodes(xpath="h2") %>% html_text()
  season<-substr(s,1,4)
  cat<-substr(s,11,100)
  
  racesL<-data.frame()
  
  for (i in 1:length(allraces))
  {
    r <-allraces[[i]] %>%  html_table()
    if (nrow(r)==0) {next} # skip if no content for this season
    names(r)<-c("date","track","distance","time", "note")
    r<-filter(r,time!="")
    links <-allraces[[i]] %>% html_nodes(xpath = "table/tr/td[4]/a") %>% html_attr("href")
    r$season=season[i]
    r$cat=cat[i]
    r$id=skate_id
    r$link=links
    racesL<-rbind(racesL,r)
  }
  if (nrow(racesL)>0)
  {
    racesL<-filter(racesL, distance>300)
    racesL$endtime<-HmsToSec(lapply(racesL$time, function(x) {ifelse(lengths(regmatches(x, gregexpr(":", x)))>0, paste("00:", x, sep=""),paste("00:00:", x, sep=""))}))
    racesL$date<-strftime(as.POSIXct(racesL$date, format="%d-%m-%Y"))
  }
  return(racesL)
}

getLaps <- function(link)
{
  page<-read_html(paste0("https://www.osta.nl/",link))
  times<-page %>% html_nodes(xpath="//*[@id='main']/table")%>%html_table() %>% data.frame()
  #correct for laptimes larger than 60s 
  times$Rondetijd<-HmsToSec(lapply(times$Rondetijd, function(x) {ifelse(lengths(regmatches(x, gregexpr(":", x)))>0, paste("00:", x, sep=""),paste("00:00:", x, sep=""))}))
  laptimes<-times$Rondetijd*100
  laptimes[1]<-as.numeric(as.character(times$Tussentijd[1]))*100
  return(laptimes)
}
#first get ID from osta: you often get multiple versions of the same person (just like in the app)
i<-getID("Martijn Willemsen")
#get races from this person (pick the first in the list) for a particular season
dt<-getAllRaces(i$id[1], season="2024")
# get lap times from the link in the race data 
# this function produces a warning but seems to work
getLaps(dt$link[1])
