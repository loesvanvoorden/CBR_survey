library(DT)
library(shiny)
library(shinyjs)
library(DescTools)
library(FNN)
library(rvest)
library(tidyverse)
library(RColorBrewer)

#setwd("C:/Users/mcwil/OneDrive/JADS_HumansData/Rondetijden")
m3000<-read_csv("data/3kmn.csv") %>% filter(cat!="pup")
pbs<-filter(m3000, pb)
cb=inner_join(pbs,filter(m3000, !pb),  by="PersonID", suffix=c("_p", "_n"), relationship = "many-to-many")
candidates<-data.frame()

getAllRaces<- function(skate_id)
{
  l<-paste0("https://www.osta.nl/index.php?pid=",skate_id,"&Seizoen=ALL&Afstand=&perAfstand=0&Verkort=1")
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

predict_pb <- function (q, cb, ft_range=500, age_range=5, adjust=FALSE, npb = TRUE, pb500 = FALSE, pb1000 = FALSE, pb1500=FALSE, sameTrack=FALSE, noCases=10)
{
  #get gender from query
  q_gender=q$Gender_n;
  q_track=q$Track_n;
  # filter cases based on input vars
  cases<-filter(cb, Gender_n==q_gender & PersonID!=q$PersonID)
  if (age_range>0) {cases<-filter(cases, age_perf_p<q$age_perf_n+age_range & age_perf_p>q$age_perf_n-age_range)}
  if (ft_range>0) {cases<-filter(cases, endtime_n<q$endtime_n+ft_range & endtime_n>q$endtime_n - ft_range)}
  if (sameTrack) {cases<-filter(cases, Track_n==q_track)}
  a<-ifelse(adjust, "^a", "^")
  p500 <- ifelse(pb500, paste(a,"r_500_.?_n|",sep = ""), "")  
  p1000 <- ifelse(pb1000, paste(a,"r_1000_.?_n|",sep = ""), "")  
  p1500 <- ifelse(pb1500, paste(a,"r_1500_.?_n|",sep = ""), "")  
  np <-ifelse(npb, paste(a,"r_.?_n|",sep = ""),"")
  regStr = paste(p500, p1000, p1500, np, sep="")
  regStr= substr(regStr, 1, nchar(regStr)-1)
  #filter out duplicate cases (esp. for predictions with npb=F we have many )
  # this does not cover duplicate cases if one skater has two similar races like the query case (when npb=true)
  cases<-cases %>% select(matches(regStr), PersonID, matches(".*_p"),endtime_n,Track_n, -age_perf_n) %>% distinct()
  #write.csv(cases, "test.csv")
  nk=min(noCases, nrow(cases))
  # generate y
  if (adjust) {y=cases$aendtime_p} else {y=cases$endtime_p}
  #generate X cases
  X=select(cases,matches(regStr))
  #generate prediction case
  X.t = select(q,matches(regStr))
  #predict endtime
  knn=knn.reg(X,X.t,y ,k=nk)
  if (adjust) {pred = trkcor[q$Track_p]*knn$pred} else {pred=knn$pred}
  
  # get indexes of similar cases for pace prediction
  nni<-get.knnx(X,X.t,k=nk)
  # retrieve their full case
  nn <-cases %>% filter(row_number() %in% nni$nn.index )
  #calculate relative paces of personal best for pace prediction
  rel_paces = colMeans(select(nn,matches("^dr.?_p")))
  #calculate absolute pace prediction
  pred_pb_paces = rel_paces*(pred/7.5)
  cases_p<-select(nn,matches("^r_.?_p"))
  cases_n<-select(nn,matches(regStr),endtime_n,Track_n)
  pb_short<-select(nn, endtime_500_p,trk_500_p, endtime_1000_p, trk_1000_p, endtime_1500_p, trk_1500_p)
  #return prediction and similar cases
  return(list(c(nk, pred, pred_pb_paces),cbind(select(nn, age_perf_p, Track_p,endtime_p),cases_p),cases_n, pb_short))
  
}

#generate all track correction vectors
trkcor<-unlist(select(filter(read_csv("data/trackcor.csv"),distance==3000),-distance))
trkcor500<-unlist(select(filter(read_csv("data/trackcor.csv"),distance==500),-distance))
trkcor1000<-unlist(select(filter(read_csv("data/trackcor.csv"),distance==1000),-distance))
trkcor1500<-unlist(select(filter(read_csv("data/trackcor.csv"),distance==1500),-distance))

m3000<-arrange(m3000,Name, cat, rank)
lb<-c("1" = "0-25%", "2"= "26-50%", "3"="51-75%", "4"="76-100%")

m3000<-m3000 %>% mutate(cat = factor(cat, levels=c("jun","sen", "mas")))

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Schaatsvoorspellingen 3000m"),p("Martijn Willemsen (JADS&TU/e: m.c.willemsen@tue.nl), feb 2024"),
  
  # Sidebar layout with a input and output definitions ----
    fluidRow(
      column(4,
          wellPanel(
            p("Met deze app voorspel je hoe snel een (langebaan)schaatser een 3000m kan rijden op basis van zijn beste tijden (SB/PB) op de kortere afstanden en/of een eerdere tijd op de 3000m. De app zoekt naar vergelijkbare schaatsers die op een zelfde manier hun races opbouwen maar die een betere PB hebben bereikt: dat geeft dan een voorspelling van een haalbare 3000m PB."),
            h3("Gegevens schaatser"),p("Type een deel van een naam in en druk op zoek om de data van deze schaatser te zoeken bij OSTA"),
            fluidRow(column(6, textInput(inputId = "naam",
                        label = "Geef naam:", value="")),actionButton("findName","Zoek schaatser")),
            fluidRow(column(12,
          uiOutput("secondSelection"))
          ))
      ),
      column(8,     
             htmlOutput("summary"),
             fluidRow(
               column(6,# Output: HTML table with requested number of observations ----
                      h2("Beste 5 races"),
                       tableOutput("viewBest")
               ),
               column(6,
                      h2("Laatste 5 races"),
                      tableOutput("viewSeason")    
               )
             ))
    ),
  wellPanel(fluidRow(column(8,h3("Rondentijden van de schaatser"),p("Kies uit eerdere beste tijden (dit seizoen, vorig seizoen of alles) of vul zelf rondetijden per afstand in. Selecteer welke afstanden je wilt gebruiken voor de voorspelling")),
                     column(4,radioButtons("PBrace", "automatische selectie SB/PB:",choiceNames=list("meest recent seizoen","voorlaatste seizoen", "alles"), choiceValues = list("last","prev","all"), inline=T))),
  fluidRow(
    column(4,
            checkboxInput("m500", "Gebruik 500m", value=T),
            conditionalPanel(
              condition = "input.m500 == true",
              htmlOutput("time500"),
            fluidRow(column(6,
                            splitLayout(cellWidths = c("50%", "50%"),numericInput(inputId = "open_500", label = "opening (sec)",  min = 9, max=59, value=NULL, step = .1),
                                        numericInput(inputId = "r1_500", label = "r1 (sec)",  min =20, max=59, value=NULL, step=.1))),
                        column(6,selectInput(inputId = "Track_500",label = "Baan 500m:", choices = names(trkcor)))), 
            fluidRow(column(12, selectInput(inputId = "sel500",label = "Kies tijd:", choices="")))),
            checkboxInput("m1000", "Gebruik 1000m", value=T),
            conditionalPanel(
              condition = "input.m1000 == true",
              htmlOutput("time1000"),
              fluidRow(column(9,
            splitLayout(cellWidths = c("34%", "33%","33%"),
                        numericInput(inputId = "open_1000", label = "opening (sec)",  min = 14, max=59, value=NULL, step = .1),
                        numericInput(inputId = "r1_1000", label = "r1 (sec)",  min =20, max=59, value=NULL, step=.1),
                        numericInput(inputId = "r2_1000", label = "r2 (sec)", min =20, max=59, value=NULL, step = .1))),
                        column(3,selectInput(inputId = "Track_1000",label = "Baan 1000m:",choices = names(trkcor)))), 
            fluidRow(column(12,selectInput(inputId = "sel1000",label = "Kies tijd:", choices="")))
                )),
              column(4, 
           checkboxInput("m1500", "Gebruik 1500m", value=T),
            conditionalPanel(
              condition = "input.m1500 == true",
              htmlOutput("time1500"),
              splitLayout(cellWidths = c("25%", "25%","25%", "25%"),
                        numericInput(inputId = "open_1500", label = "opening (sec)",  min = 14, max=59, value=NULL, step = .1),
                        numericInput(inputId = "r1_1500", label = "r1 (sec)",  min =20, max=59, value=NULL, step=.1),
                        numericInput(inputId = "r2_1500", label = "r2 (sec)", min =20, max=59, value=NULL, step = .1),
                        numericInput(inputId = "r3_1500", label = "r3 (sec)", min =20, max=59, value=NULL, step = .1)),
            selectInput(inputId = "Track_1500",
                        label = "Baan 1500m:",
                        choices = names(trkcor)),
			fluidRow(column(12,selectInput(inputId = "sel1500",label = "Kies tijd:", choices="")))
			)),		
          column(4,
            checkboxInput("m3000", "Gebruik 3000m", value=T),
            conditionalPanel(
              condition = "input.m3000 == true",
              htmlOutput("time3000"), 
              splitLayout(cellWidths = c("25%", "25%","25%", "25%"),numericInput(inputId = "opening",
                                                            label = "opening (sec)",
                                                            min = 15, max=59, value=NULL, step = .1),
                  numericInput(inputId = "r1",
                               label = "r1 (sec)",
                               min =20, max=59, value=NULL, step=.1),
      numericInput(inputId = "r2",
                   label = "r2 (sec)",
                   min =20, max=59, value=NULL, step = .1),
      numericInput(inputId = "r3",
                   label = "r3 (sec)",
                   min =20, max=59, value=NULL, step = .1)),
		splitLayout(cellWidths = c("25%", "25%","25%", "25%"),
                  numericInput(inputId = "r4", label = "r4 (sec)",  min = 20, max=59, value=NULL, step = .1),
                  numericInput(inputId = "r5", label = "r5 (sec)",  min =20, max=59, value=NULL, step=.1),
                   numericInput(inputId = "r6", label = "r6 (sec)", min =20, max=59, value=NULL, step = .1),
                  numericInput(inputId = "r7",label = "r7 (sec)", min =20, max=59, value=NULL, step = .1)),
      selectInput(inputId = "Track_3000",
                  label = "Baan 3000m:",
                  choices = names(trkcor)), selectInput(inputId = "sel3000",label = "Kies tijd:", choices=""))))),
  fluidRow(
    column(4,
           wellPanel(h3("Bereken voorspelling"),
        p("Kies voor welke baan en leeftijd je een PB wilt voorspellen. Als PBs op (erg) verschillende banen zijn gereden, is de baan correctie soms nuttig. Deze rekent alle tijden om naar baan AL zodat ze beter vergelijkbaar zijn en rekent ze na de voorspelling terug naar de baan waarvoor de voorspelling gevraagd wordt."),
      fluidRow(column(4,selectInput(inputId = "pb_track",
                  label = "Baan voor PB:",
                  choices = names(trkcor))),
               column(8,checkboxInput("adjust","Baan correctie (als PBs op verschillende banen)", value=T))),
              p("Kies hieronder waarop we de gezochte vergelijkbare schaatsers op moeten filteren (leeftijd/eindtijd)"),
      fluidRow(column(4,checkboxInput("age_r","filter voor leeftijd schaatsers", value=F)),column(4,numericInput(inputId = "agePB", label = "Leeftijd PB", value=NULL, step=1)),conditionalPanel(
        condition = "input.age_r == true",column(4,numericInput(input="agerange", "filter +/- jaar", min=1, max=80, value=5, step=1)))), 
      fluidRow(column(6,checkboxInput("ft_r","filter voor eindtijd 3000m", value=T)),conditionalPanel(
        condition = "input.ft_r == true",column(6,numericInput(input="ftrange", "filter +/- secs", min=1, max=30, value=5, step=1)))), 
      p("Bereken een voorspelde eindtijd en pacing (rondetijdenverloop) voor deze schaatser"),
      fluidRow(column(12,actionButton("showPred","Bereken voorspelling"))),
      fluidRow(column(12,checkboxInput("showCases","Laat vergelijkbare cases (schaatsers) en hun tijden zien waarop voorspellingen gebaseerd zijn.", 
                                      value=F))))
      ),
    column(8,
     htmlOutput("PBheader"),
     fluidRow(
       column(8,# Output: HTML table with requested number of observations ----
              tableOutput("viewPB")
       ),
       column(4,
              plotOutput("plotPB")    
       )
     ),
     conditionalPanel(
       condition = "input.showCases == true", 
       conditionalPanel(
         condition = "input.m1500 == true | input.m1000==true | input.m500==true|input.m3000 == true", 
         h3("Hierop zijn de voorspellingen gebaseerd:"),
         p("Dit zijn de tijden van vergelijkbare schaatsers de lijken op de huidige schaatser in de opbouw van hun races en eindtijden. Op basis van deze gelijkenis zijn deze schaatsers gekozen voor de voorspelling. Hun beste 3000m wordt gebruikt voor de 3000m voorspelling. Selecteer een of meer schaatsers om hun rondetijden/pacing profiel te zien in de grafieken hieronder."),
         fluidRow(
           column(12,# Output: HTML table with requested number of observations ----
                  DT::dataTableOutput("viewCases"))
          )
         ),
       fluidRow(column(12, htmlOutput("headerPBshorter"))),
      fluidRow(column(3, conditionalPanel(
         condition = "input.m1000 == true",
         plotOutput("plotPB1000Cases"))),
         column(4, conditionalPanel(
           condition = "input.m1500 == true",
           plotOutput("plotPB1500Cases"))),
         column(5,conditionalPanel(
           condition = "input.m3000 == true",
           
           plotOutput("plotNPCases")))    
       ),
       fluidRow(column(7, conditionalPanel(
         condition = "input.m1500 == true | input.m1000==true | input.m500==true", 
         p("Rondetijden gereden op de kortere PB afstanden."),
         div(tableOutput("viewPBCases"),style = "font-size:75%"))
       ),
       column(5, conditionalPanel(
         condition = "input.m3000 == true", 
         p("Rondetijden gereden op een eerdere (niet-PB) 3000m."),
         div(tableOutput("viewNPCases"),style = "font-size:75%"))
         )
       ))
       
       
       )
     
  
  )
  )



# Define server logic to summarize and view selected dataset ----
server <- function(input, output,session) {

#create data frame for scraped races
races<-data.frame()
  
#updates the select box based on the category
  getName <- eventReactive(input$findName, 
                {input$naam})
  
  output$secondSelection <- renderUI({
    #find skater in OSTA
    search<-paste0("https://www.osta.nl/index.php?ZoekStr=",gsub(" ","+",getName()))
    peoplePage<-read_html(search)
    skaters<-data.frame(peoplePage %>%  html_nodes(xpath="//*[@id='main']/table")%>%html_table())
    if (length(skaters)>0){
      skaters$id <- peoplePage %>% html_nodes(xpath = "//*[@id='main']/table/tr/td[1]/a") %>% html_attr("href") %>% substr(6,100) 
    }
    else
    {
      id<- peoplePage %>% html_nodes(xpath = "//*[@id='tijden']/input[1]") %>% html_attr("value")
      seasons<-peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h2[1]") %>% html_text2()
      Vereniging<-peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h3[1]") %>% html_text2()
      Vereniging<-strsplit(Vereniging,", ")[[1]][2]
      skaters<-data.frame(cbind(id, Vereniging))
      skaters$Naam<-getName()
      skaters$Seizoen.en.=substr(seasons, 1,9)
      skaters$Categorie=substr(seasons, 11, nchar(seasons))
      skaters<-select(skaters, id, Naam, Categorie, "Seizoen.en.", Vereniging)
    }
      # filter those with vantage times (from 2016)
    candidates<<-filter(skaters, str_length(id)>20) 
    skaters<-candidates%>%unite("label", Naam:Vereniging, sep=" ")

    radioButtons("rb", "Kies een rijder", choiceNames = skaters$label, choiceValues=skaters$id)
    })
    
  
  observeEvent(input$rb,{
    # if skater is selected get names from the internal database or find on OSTA
    if (!input$rb %in% races$id)
    {newraces<-getAllRaces(input$rb)
    if (nrow(filter(newraces, id==input$rb))>0)
    {
    racesL<-filter(newraces, id==input$rb) %>% group_by(distance) %>% mutate(lastSeason=max(season)) %>%  group_by(distance, season) %>%
      mutate(SB=min(endtime)) %>% ungroup(season) %>% mutate(curSB=ifelse(SB==endtime & season==lastSeason, 1, 0)) %>% 
      mutate(PB=ifelse(endtime==min(endtime),1,0)) %>%ungroup()
    racesL$prevSB<-ifelse(dense_rank(racesL$season)==(max(dense_rank(racesL$season)-1)) & racesL$SB==racesL$endtime,1,0)
    racesL$flag<-ifelse(racesL$curSB, " SB", "")
    racesL$flag<-paste0(racesL$flag, ifelse(racesL$PB, " PB", ""))
    races<<-rbind(races,racesL)}
    }
    
    if (nrow(filter(races, id==input$rb))>0)
    {
      
      #get minimum age
    lastrace<-filter(races, id==input$rb) %>% arrange(desc(date)) %>% slice_head(n=1)
    lastcat<-substr(lastrace$cat,2,3)
    age=switch(lastcat,C1=13, C2=14, B1=15, B2=16, A1=17, A2=18, N1=19, N2=20, N3=21, N4=22, SA=23, SB=30, lastcat)
    updateNumericInput(session, "agePB", value=as.numeric(age))
    
    
    # put the races in the selection boxes
    r500<-filter(races, id==input$rb & distance==500)%>%arrange(desc(date))
    if (nrow(r500)>0) { 
    r500<-select(r500, date, track, time, flag, link, curSB, prevSB, PB)%>% unite(label, date:flag, sep="|") %>%mutate(label=str_replace_all(label,"[|]$",""))
    
    sel<-""
    if (input$PBrace=="last") {sel=filter(r500, curSB==1)$label}
    if (input$PBrace=="prev") {sel=filter(r500, prevSB==1)$label}
    if (input$PBrace=="all" | sel=="") {sel=filter(r500, PB==1)$label}
    updateCheckboxInput(session, "m500", value=T)
    updateSelectInput(session, "sel500",choices=r500$label, selected=sel)}
    else
    {updateCheckboxInput(session, "m500", value=F)
    }
    r1000<-filter(races, id==input$rb &  distance==1000)%>%arrange(desc(date))
    if (nrow(r1000)>0) {
    r1000<-select(r1000, date, track, time, flag, link, curSB, prevSB, PB)%>% unite(label, date:flag, sep="|")%>%mutate(label=str_replace_all(label,"[|]$",""))
    sel<-""
    if (input$PBrace=="last") {sel=filter(r1000, curSB==1)$label}
    if (input$PBrace=="prev") {sel=filter(r1000, prevSB==1)$label}
    if (input$PBrace=="all" | sel=="") {sel=filter(r1000, PB==1)$label}
    updateCheckboxInput(session, "m1000", value=T)
    updateSelectInput(session, "sel1000",choices=r1000$label, selected=sel)}
    else
    {updateCheckboxInput(session, "m1000", value=F)
    }
    r1500<-filter(races, id==input$rb &  distance==1500)%>%arrange(desc(date))
    if (nrow(r1500)>0) {
    r1500<-select(r1500, date, track, time, flag,link, curSB, prevSB, PB)%>% unite(label, date:flag, sep="|")%>%mutate(label=str_replace_all(label,"[|]$",""))
    sel=""
    if (input$PBrace=="last") {sel=filter(r1500, curSB==1)$label}
    if (input$PBrace=="prev") {sel=filter(r1500, prevSB==1)$label}
    if (input$PBrace=="all" | sel=="") {sel=filter(r1500, PB==1)$label}
    updateCheckboxInput(session, "m1500", value=T)
    updateSelectInput(session, "sel1500",choices=r1500$label,selected=sel)}
    else
    {updateCheckboxInput(session, "m1500", value=F)
    }
	r3000<-filter(races, id==input$rb & distance==3000)%>%arrange(desc(date))
	  if (nrow(r3000)>0) {
	  r3000<-select(r3000, date, track, time, flag, link, curSB, prevSB, PB)%>% unite(label, date:flag, sep="|")%>%mutate(label=str_replace_all(label,"[|]$",""))
    if (input$PBrace=="last") {sel=filter(r3000, curSB==1)$label}
    if (input$PBrace=="prev") {sel=filter(r3000, prevSB==1)$label}
    if (input$PBrace=="all" | sel=="") {sel=filter(r3000, PB==1)$label}
	  updateCheckboxInput(session, "m3000", value=T)
	  updateSelectInput(session, "sel3000",choices=r3000$label,selected=sel)}
	else
	{updateCheckboxInput(session, "m3000", value=F)
	}
    }
  })
  
  observeEvent(input$PBrace,{
    req(input$rb)
    if (input$PBrace=="last") {racesP<-filter(races, id==input$rb) %>% filter(curSB==1)}
    if (input$PBrace=="prev") {racesP<-filter(races, id==input$rb) %>% filter(prevSB==1)}
    if (input$PBrace=="all") {racesP<-filter(races, id==input$rb) %>% filter(PB==1)}
    if (nrow(racesP)>0) {
    labs<-select(racesP, date, track, time, flag, distance, link)%>% unite(label, date:flag, sep="|")%>%mutate(label=str_replace_all(label,"[|]$",""))
    sel500<-filter(labs, distance==500)
    if (nrow(sel500)>0 & input$m500) {updateSelectInput(session, "sel500",selected=sel500$label)}
    sel1000<-filter(labs, distance==1000)
    if (input$m1000 & nrow(sel1000)>0) {updateSelectInput(session, "sel1000",selected=sel1000$label)}
    sel1500<-filter(labs, distance==1500)
    if (input$m1500 & nrow(sel1500)>0) {updateSelectInput(session, "sel1500",selected=sel1500$label)}
    sel3000<-filter(labs, distance==3000)
    if (input$m3000 & nrow(sel3000)>0) {updateSelectInput(session, "sel3000",selected=sel3000$label)}
    }
  })

  dataAll <- reactive({
    return(m3000)
  })
  
 observeEvent(input$sel500,{
   req(input$rb)
   s<-strsplit(input$sel500, "[|]")[[1]]
   link<-filter(races, id==input$rb & distance==500 & date==s[1] & track==s[2] & time==s[3])$link
   laps<-getLaps(link)/100
   updateNumericInput(session, "open_500", value=laps[1])
   updateNumericInput(session, "r1_500", value=laps[2])
   updateSelectInput(session, "Track_500", selected=s[2])
   
 })
 
 observeEvent(input$sel1000,{
   req(input$rb)
   s<-strsplit(input$sel1000, "[|]")[[1]]
   link<-filter(races, id==input$rb & distance==1000 & date==s[1] & track==s[2] & time==s[3])$link
   laps<-getLaps(link)/100
   updateNumericInput(session, "open_1000", value=laps[1])
   updateNumericInput(session, "r1_1000", value=laps[2])
   updateNumericInput(session, "r2_1000", value=laps[3])
   updateSelectInput(session, "Track_1000", selected=s[2])
   
 })
 
 observeEvent(input$sel1500,{
   req(input$rb)
   s<-strsplit(input$sel1500, "[|]")[[1]]
   link<-filter(races, id==input$rb & distance==1500 & date==s[1] & track==s[2] & time==s[3])$link
   laps<-getLaps(link)/100
   updateNumericInput(session, "open_1500", value=laps[1])
   updateNumericInput(session, "r1_1500", value=laps[2])
   updateNumericInput(session, "r2_1500", value=laps[3])
   updateNumericInput(session, "r3_1500", value=laps[4])
   updateSelectInput(session, "Track_1500", selected=s[2])
   
 })
 observeEvent(input$sel3000,{
   req(input$rb)
   s<-strsplit(input$sel3000, "[|]")[[1]]
   link<-filter(races, id==input$rb & distance==3000 & date==s[1] & track==s[2] & time==s[3])$link
   laps<-getLaps(link)/100
   updateNumericInput(session, "opening", value=laps[1])
   updateNumericInput(session, "r1", value=laps[2])
   updateNumericInput(session, "r2", value=laps[3])
   updateNumericInput(session, "r3", value=laps[4])
   updateNumericInput(session, "r4", value=laps[5])
   updateNumericInput(session, "r5", value=laps[6])
   updateNumericInput(session, "r6", value=laps[7])
   updateNumericInput(session, "r7", value=laps[8])
   updateSelectInput(session, "Track_3000", selected=s[2])
   
 })
 
  # Generate a summary of the dataset ----
  output$summary <- renderText({
    req(input$rb)
    skater<-filter(candidates, id==input$rb)
    gender<-substr(skater$Categorie,1,1)
    seasons<-strsplit(skater$Seizoen.en.,"-")
    firstSeason<-as.numeric(seasons[[1]][1])
    lastSeason<-as.numeric(seasons[[1]][2])-1
    paste0(firstSeason, "to", lastSeason)
    paste("<h2>naam:", skater$Naam, "</h2>geslacht:",gender,"seizoenen:", firstSeason, "to", lastSeason, "vereniging: ", skater$Vereniging)
    
    })
  
  output$time1500<-renderText({
    endtime<-input$open_1500+input$r1_1500+input$r2_1500+input$r3_1500
    if (is.na(endtime)) {endtimeStr<-"..."} else {
    endtimeStr<-substr(SecToHms(endtime, digit=2),4,12)}
  paste("<h4><b>1500m PB tijd:", endtimeStr, "</b></h4>")
    })
  output$time500<-renderText({
    endtime<-input$open_500+input$r1_500
    if (is.na(endtime)) {endtimeStr<-""} else {
      endtimeStr<-substr(SecToHms(endtime, digit=2),4,12)}
    paste("<h4><b>500m PB tijd:", endtimeStr, "<b></h4>")
  })
  output$time1000<-renderText({
    endtime<-input$open_1000+input$r1_1000+input$r2_1000
    if (is.na(endtime)) {endtimeStr<-""} else {
      endtimeStr<-substr(SecToHms(endtime, digit=2),4,12)}
    paste("<h4><b>1000m PB tijd:", endtimeStr, "</b></h4>")
  })
	output$time3000<-renderText({
    endtime<-input$opening+input$r1+input$r2+input$r3+input$r4+input$r5+input$r6+input$r7
    if (is.na(endtime)) {endtimeStr<-""} else {
    endtimeStr<-substr(SecToHms(endtime, digit=2),4,12)}
  paste("<h4><b>3000m PB tijd:", endtimeStr, "</b></h4>")
    })								  
  
  output$headerNPB3000<-renderText({
    if (input$adjust) {extra<-"(ongerekend naar baan AL)"} else {extra=""}
    paste("<h4>Vorige PBs op 3000m van geselecteerde schaatsers", extra, "</h4>")})
  
  output$headerPBshorter<-renderText({
  if (input$adjust) {extra<-"(ongerekend naar baan AL)"} else {extra=""}
  if (input$m3000) {extra2<-"en een eerdere (niet-PB) 3000m"} else {extra2=""}
  paste("<h4>PB tijden op de kortere afstanden", extra2, extra, "</h4>")})

  
  # Show the first "n" observations ----

  output$viewBest <- renderTable({
    req(input$rb)
    r<-filter(races, id==input$rb & distance==3000)%>%arrange(endtime) %>%select(date, cat, track, time)
    head(r,5)
    }
  )
  output$viewSeason <- renderTable({
    req(input$rb)
    r<-filter(races, id==input$rb & distance==3000)%>%arrange(desc(date)) %>%select(date, cat, track, time)
    head(r,5)
  }
  )
  
  tablePB<-reactive({
    
    #races<-dataUserRank() 
    
    T3000<-input$Track_3000;
    opening<-input$opening*100;aopening=round(opening/trkcor[T3000],2);
     r1<-input$r1*100;ar1=round(r1/trkcor[T3000],2);
    r2<-input$r2*100;ar2=round(r2/trkcor[T3000],2);
    r3<-input$r3*100;ar3=round(r3/trkcor[T3000],2);
    r4<-input$r4*100;ar4=round(r4/trkcor[T3000],2);
    r5<-input$r5*100;ar5=round(r5/trkcor[T3000],2);
    r6<-input$r6*100;ar6=round(r6/trkcor[T3000],2);
    r7<-input$r7*100;ar7=round(r7/trkcor[T3000],2);	
    endtime=opening+r1+r2+r3+r4+r5+r6+r7;
    if (is.na(endtime)) {endtime=30000; ft=0; valid3000=FALSE} else {ft=500; valid3000=TRUE}
    if (is.na(input$agePB)) {agePB=30; ap=0} else {agePB=input$agePB;ap=5}
    
    T500<-input$Track_500;
    op_5<-input$open_500*100;aop_5=round(op_5/trkcor500[T500],2);
    r1_5<-input$r1_500*100;ar1_5=round(r1_5/trkcor500[T500],2);
    T1000<-input$Track_1000;
    op_1<-input$open_1000*100;aop_1=round(op_1/trkcor1000[T1000],2);
    r1_1<-input$r1_1000*100;ar1_1=round(r1_1/trkcor1000[T1000],2);
    r2_1<-input$r2_1000*100;ar2_1=round(r2_1/trkcor1000[T1000],2);
    T1500<-input$Track_1500;
    op_15<-input$open_1500*100;aop_15=round(op_15/trkcor1500[T1500],2);
    r1_15<-input$r1_1500*100;ar1_15=round(r1_15/trkcor1500[T1500],2);
    r2_15<-input$r2_1500*100;ar2_15=round(r2_15/trkcor1500[T1500],2);
    r3_15<-input$r3_1500*100;ar3_15=round(r3_15/trkcor1500[T1500],2);
    skater<-filter(candidates, id==input$rb)
    gender<-substr(skater$Categorie,1,1)
    
    query<-tibble(Track_n=input$Track_3000,
                      Track_p=input$pb_track,
                     endtime_n=endtime,
                     age_perf_n=agePB,
                     Gender_n = gender,
                     PersonID = input$rb,
                     r_500_0_n=op_5,ar_500_0_n=aop_5,
                     r_500_1_n=r1_5,ar_500_1_n=ar1_5,
                     r_1000_0_n=op_1,ar_1000_0_n=aop_1,
                     r_1000_1_n=r1_1,ar_1000_1_n=ar1_1,
                     r_1000_2_n=r2_1,ar_1000_2_n=ar2_1,
                    r_1500_0_n=op_15,ar_1500_0_n=aop_15,
                     r_1500_1_n=r1_15,ar_1500_1_n=ar1_15,
                     r_1500_2_n=r2_15,ar_1500_2_n=ar2_15,
                     r_1500_3_n=r3_15,ar_1500_3_n=ar3_15,
                     r_0_n=opening,ar_0_n=aopening,
                     r_1_n=r1,ar_1_n=ar1,
                     r_2_n=r2,ar_2_n=ar2,
                     r_3_n=r3,ar_3_n=ar3,
                     r_4_n=r4,ar_4_n=ar4,
                     r_5_n=r5,ar_5_n=ar5,
                     r_6_n=r6,ar_6_n=ar6,
                     r_7_n=r7,ar_7_n=ar7)
    #merge q with existing data such that the predict function works consistently (bug to be fixed)
    q<-merge(cb[1,],query, all=T)[2,1:40]
    
    #use finish time and age range filtering if required
    if (!input$m3000) {updateCheckboxInput(session, "ft_r", value=F)}
    ft<-ifelse(input$ft_r & input$m3000, input$ftrange*100, 0)
    ap<-ifelse(input$age_r, input$agerange, 0)
    
    fullpred<-predict_pb(q,cb, ft_range=ft, age_range=ap, npb=input$m3000, pb500=input$m500, pb1000=input$m1000, pb1500 = input$m1500, adjust=input$adjust)
    pred<-fullpred[[1]]
    
    endtime<-substr(SecToHms(endtime/100, digit=2),4,12)
    endtime_pred<-substr(SecToHms(pred[2]/100, digit=2),4,12)
    if (valid3000) {userpb<-c("huidig PB", T3000, endtime, opening/100,r1/100,r2/100,r3/100,r4/100,r5/100,r6/100,r7/100)}
    predpb<-c("voorspeld PB",input$pb_track,endtime_pred ,round(pred[3:10]/100,2))
    if (valid3000) {pb<-data.frame(rbind(predpb, userpb))} else {pb<-data.frame(rbind(predpb))}
    names(pb)<-c("type", "baan", "tijd","opening","r1", "r2", "r3", "r4", "r5", "r6", "r7" )
        
    cs<-fullpred[[2]]
    csnp<-fullpred[[3]]
    pbshort<-fullpred[[4]]
    namestrpb<-NULL
    pbstimes<-NULL
    if (input$m500) {namestrpb<-c(namestrpb, "PB 500m")
    pbstimes<-cbind(pbstimes, paste0(substr(SecToHms(pbshort$endtime_500_p/100, digit=2),4,12)," (",pbshort$trk_500_p,")"))}
    if (input$m1000) { namestrpb<-c(namestrpb, "PB 1000m")
    pbstimes<-cbind(pbstimes, paste0(substr(SecToHms(pbshort$endtime_1000_p/100, digit=2),4,12)," (", pbshort$trk_1000_p,")"))}
    if (input$m1500) { namestrpb<-c(namestrpb, "PB 1500m")
    pbstimes<-cbind(pbstimes, paste0(substr(SecToHms(pbshort$endtime_1500_p/100, digit=2),4,12), " (",pbshort$trk_1500_p,")"))}
    if (input$m3000) { namestrpb<-c(namestrpb, "non-PB 3000m")
    pbstimes<-cbind(pbstimes, paste0(substr(SecToHms(csnp$endtime_n/100, digit=2),4,12), " (",csnp$Track_n,")"))}
    
    cstable<-data.frame(cbind(sprintf("%02i",seq.int(nrow(cs))),round(cs$age_perf_p,0), pbstimes, paste0(substr(SecToHms(cs$endtime_p/100, digit=2),4,12), " (",cs$Track_p,")" )))
    names(cstable)<-c("case", "leeftijd", namestrpb, "PB 3000m")
    
    
    if (input$m3000)
      {csnp3k<-cbind(sprintf("%02i",seq.int(nrow(csnp))), round(csnp[1:8]/100,2))
        names(csnp3k)<-c("case", "opening","r1", "r2", "r3", "r4", "r5", "r6", "r7")
        if (input$adjust) {npbtimes<-c(aopening, ar1, ar2, ar3, ar4, ar5,ar6, ar7)} else (npbtimes=c(opening, r1, r2, r3, r4, r5, r6, r7))
          npbtimes<-c(0, round(npbtimes/100,2))
          
        csnp3k<-rbind(csnp3k,npbtimes)
        #skater<-filter(candidates, id==input$rb)$Naam
        csnp3k$case[csnp3k$case=="0"]=filter(candidates, id==input$rb)$Naam
        
    } else {csnp3k<-NULL}
    
    if (input$m1500|input$m1000|input$m500)
    {
      startpos<-ifelse(input$m3000, 9, 1)
      csnpPB<-cbind(sprintf("%02i",seq.int(nrow(csnp))), round(csnp[startpos:(length(csnp)-2)]/100,2))
    namestr<-c("case")
    
    if (input$m500) {namestr<-c(namestr, "opening 500", "r1 500")}
    if (input$m1000) {namestr<-c(namestr, "opening 1000", "r1 1000", "r2 1000")}
    if (input$m1500) {namestr<-c(namestr, "opening 1500", "r1 1500", "r2 1500","r3 1500")}
    
    npbtimes<-NULL
    if (input$adjust) {
      if (input$m500) {npbtimes<-c(npbtimes, aop_5, ar1_5)}
      if (input$m1000) {npbtimes<-c(npbtimes, aop_1, ar1_1, ar2_1)}
      if (input$m1500) {npbtimes<-c(npbtimes, aop_15, ar1_15, ar2_15, ar3_15)}
    }
      else
      {
        if (input$m500){npbtimes<-c(npbtimes, op_5, r1_5)}
        if (input$m1000) {npbtimes<-c(npbtimes, op_1, r1_1, r2_1)}
        if (input$m1500) {npbtimes<-c(npbtimes, op_15, r1_15, r2_15, r3_15)}
    
      }
    npbtimes<-c(0, round(npbtimes/100,2))
    csnpPB<-rbind(csnpPB,npbtimes)
    names(csnpPB)<-namestr
    csnpPB$case[csnpPB$case=="0"]=filter(candidates, id==input$rb)$Naam
    
    } else {csnpPB<-NULL}
   
  list(pb,cstable, csnp3k,csnpPB)
    }
  )
  
   observeEvent(input$showPred, 
  
  {output$PBheader<-renderText({
    #skater<-filter(candidates, id==input$rb)
    #gender<-substr(skater$Categorie,1,1)
    #geslacht <-ifelse(gender=="H", "Man", "Vrouw")
    #name=ifelse(input$naam!="...",input$naam, paste(geslacht," met leeftijd van ", input$agePB, " jaar", sep=""))
    paste("<h3>Voorspelde tijd voor ", filter(candidates, id==input$rb)$Naam,  "op baan", input$pb_track, "</h4>")
      })
    
    
    output$viewPB <- renderTable({
      
      tablePB()[[1]]
    })
    
    output$plotPB<-renderPlot({
      udatag<-gather(tablePB()[[1]],key="roundstr",value="time",opening,r1,r2,r3,r4,r5,r6,r7)
      udatag$roundstr<-ifelse(udatag$roundstr=="opening", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      udatag3<-group_by(udatag, type, round) %>% summarize(time=mean(as.numeric(time))) 
      ggplot(udatag3)+geom_line(aes(y=time,x=round, color=type))+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.7, 0.2))+ guides(color=guide_legend(title="races"))
      
    })
    output$viewCases <- DT::renderDataTable(
      tablePB()[[2]], options = list(searching=F, ordering=F, paging=F, selection='single'), rownames=F
      
    )
    
    output$plotCases<-renderPlot({
      name=filter(candidates, id==input$rb)$Naam
      udatag<-gather(tablePB()[[2]],key="roundstr",value="time",opening,r1,r2,r3,r4,r5,r6,r7)
      udatag$roundstr<-ifelse(udatag$roundstr=="opening", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      #udatag$label=paste(round(udatag$age,0), udatag$track, udatag$endtime)
      #udatag3<-group_by(udatag, label, round) %>% summarize(time=mean(as.numeric(time))) 
      
      ggplot(filter(udatag,case!=name))+geom_line(aes(y=time,x=round, group=case), color="grey")+geom_line(data=filter(udatag,case==name),aes(y=time,x=round, color=case), size=2)+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.8, 0.4))+ guides(color=guide_legend(title="PB 3000m voor case")) + scale_color_brewer(palette = "Set3", drop=F)
      
      
    })
    output$viewPBCases <- renderTable({
      
      filter(tablePB()[[4]],row_number()%in%c(input$viewCases_rows_selected,11))
    })
    output$plotNPCases<-renderPlot({
      name=filter(candidates, id==input$rb)$Naam
      udatag<-tablePB()[[3]] %>% mutate(show=row_number()%in%c(input$viewCases_rows_selected,11))
      udatag<-gather(udatag,key="roundstr",value="time",opening,r1,r2,r3,r4,r5,r6,r7)
      udatag$roundstr<-ifelse(udatag$roundstr=="opening", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      ggplot(filter(udatag,!show))+geom_line(aes(y=time,x=round,group=case), color="grey")+geom_line(data=filter(udatag,show), aes(y=time,x=round, color=case), size=2)+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.8, 0.4))+ guides(color=guide_legend(title="Niet-PB 3000m voor case"))+ scale_color_brewer(palette = "Set3", drop=F)
      
    })
    output$viewNPCases <- renderTable({
      
      filter(tablePB()[[3]],row_number()%in%c(input$viewCases_rows_selected,11))
    })
    output$plotPB1000Cases<-renderPlot({
      if (input$m1000){
      name=filter(candidates, id==input$rb)$Naam
      udatag<-tablePB()[[4]] %>% mutate(show=row_number()%in%c(input$viewCases_rows_selected,11))
      udatag<-gather(udatag,key="roundstr",value="time","opening 1000","r1 1000","r2 1000")
      udatag$roundstr<-ifelse(udatag$roundstr=="opening 1000", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      ggplot(filter(udatag,!show))+geom_line(aes(y=time,x=round,group=case), color="grey")+geom_line(data=filter(udatag,show), aes(y=time,x=round, color=case), size=2)+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.8, 0.4))+ guides(color=guide_legend(title="PB 1000 voor case"))+ scale_color_brewer(palette = "Set3", drop=F)
      }
      
    })
   output$plotPB1500Cases<-renderPlot({
      if (input$m1500){
        name=filter(candidates, id==input$rb)$Naam
        udatag<-tablePB()[[4]] %>% mutate(show=row_number()%in%c(input$viewCases_rows_selected,11))
        udatag<-gather(udatag,key="roundstr",value="time","opening 1500","r1 1500","r2 1500","r3 1500")
      udatag$roundstr<-ifelse(udatag$roundstr=="opening 1500", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      #udatag$case<-as.factor(udatag$case)
      ggplot(filter(udatag,!show))+geom_line(aes(y=time,x=round,group=case), color="grey")+geom_line(data=filter(udatag,show), aes(y=time,x=round, color=case), size=2)+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.8, 0.4))+ guides(color=guide_legend(title="PB 1500 voor case"))+ scale_color_brewer(palette = "Set3", drop=F)
      }
    }) 
    
  }
  )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
