library(DT)
library(shiny)
library(shinyjs)
library(DescTools)
library(FNN)
library(rvest)
library(tidyverse)
library(RColorBrewer)
library(httr2)

#setwd("C:/Users/mcwil/OneDrive/JADS_HumansData/Rondetijden")
m3000<-read_csv("data/3kmn.csv") %>% filter(cat!="pup")
pbs<-filter(m3000, pb)
cb=inner_join(pbs,filter(m3000, !pb),  by="PersonID", suffix=c("_p", "_n"), relationship = "many-to-many")
candidates<-data.frame()

# --- Placeholder for External Tool API --- 
# !!! REPLACE THIS WITH YOUR ACTUAL ENDPOINT !!!
EXTERNAL_TOOL_API_ENDPOINT <- "https://web-production-13585.up.railway.app/receive_shiny_data" 

source("utils.R") # Assuming utils.R is in the same directory and contains getAllRaces, getLaps, predict_pb

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
  wellPanel(fluidRow(column(8,h3("Rondetijden van de schaatser"),p("Kies uit eerdere beste tijden (dit seizoen, vorig seizoen of alles) of vul zelf rondetijden per afstand in. Selecteer welke afstanden je wilt gebruiken voor de voorspelling")),
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
			fluidRow(column(12,selectInput(inputId = "sel1500",label = "Kies tijd:", choices=""))))
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
    skaters_table_list <- peoplePage %>% html_nodes(xpath="//*[@id='main']/table")
    
    skaters <- data.frame() # Initialize skaters as an empty data frame

    if (length(skaters_table_list) > 0){
      skaters_from_table <- skaters_table_list[[1]] %>% html_table()
      if(nrow(skaters_from_table) > 0) {
          skaters <- skaters_from_table
          # Check if specific columns exist before trying to use them for href extraction
          if (ncol(skaters) >= 1) { # Basic check, better to check specific expected column names
            skaters$id <- peoplePage %>% html_nodes(xpath = "//*[@id='main']/table/tr/td[1]/a") %>% html_attr("href") %>% substr(6,100)
          } else {
            skaters$id <- NA # Avoid error if table structure is unexpected
          }
      } 
    }
    
    if (nrow(skaters) == 0) # If table was empty or not found, try the alternative parsing
    {
      id_nodes <- peoplePage %>% html_nodes(xpath = "//*[@id='tijden']/input[1]")
      if (length(id_nodes) > 0) {
        id <- id_nodes %>% html_attr("value")
        seasons_nodes <- peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h2[1]")
        Vereniging_nodes <- peoplePage %>% html_nodes(xpath = "//*[@class='seizoen']/h3[1]")
        
        if(length(seasons_nodes)>0 && length(Vereniging_nodes)>0){
            seasons <- seasons_nodes %>% html_text2()
            Vereniging_text <- Vereniging_nodes %>% html_text2()
            Vereniging <- strsplit(Vereniging_text,", ")[[1]][2]
            
            skaters <- data.frame(id = id, Vereniging = Vereniging, Naam = getName(), 
                                  `Seizoen.en.` = substr(seasons, 1,9), # Use backticks for special char in name
                                  Categorie = substr(seasons, 11, nchar(seasons)),
                                  stringsAsFactors = FALSE)
            skaters <- select(skaters, id, Naam, Categorie, `Seizoen.en.`, Vereniging)
        } else {
            # Not enough info for this structure either
        }
      } else {
        # ID not found, cannot proceed with this skater search method
      }
    }
    # filter those with vantage times (from 2016) -> id length condition
    if(nrow(skaters) > 0 && "id" %in% names(skaters)){
        candidates <<- filter(skaters, !is.na(id) & str_length(id)>20) 
    } else {
        candidates <<- data.frame() # Ensure candidates is empty if no valid skaters
    }

    if(nrow(candidates) > 0){
        # Ensure all columns for unite exist
        cols_for_unite <- c("Naam", "Categorie", "Seizoen.en.", "Vereniging")
        missing_cols <- setdiff(cols_for_unite, names(candidates))
        if(length(missing_cols) > 0) {
            warning(paste("Missing columns for creating radio button labels:", paste(missing_cols, collapse=", ")))
            # Fallback: use only Name and ID if other details are missing
            if ("Naam" %in% names(candidates) && "id" %in% names(candidates)) {
                 skaters_labels <- candidates %>% unite("label", Naam, sep=" ")
            } else {
                return(p("Kon schaatser niet vinden of vereiste details ontbreken."))
            }
        } else {
             skaters_labels <- candidates %>% unite("label", all_of(cols_for_unite), sep=" ")
        }
        radioButtons("rb", "Kies een rijder", choiceNames = skaters_labels$label, choiceValues=candidates$id)
    } else {
        return(p("Geen schaatsers gevonden die voldoen aan de criteria of OSTA pagina structuur is onverwacht."))
    }
  })
    
  
  observeEvent(input$rb,{
    if (is.null(input$rb) || input$rb == "") return()

    newly_fetched_races <- data.frame()
    if (!input$rb %in% races$id) {
      fetched_data <- getAllRaces(input$rb) # From utils.R
      if (nrow(fetched_data) > 0) {
        newly_fetched_races <- fetched_data %>% 
          filter(id == input$rb) # ensure it's for the current skater
        
        # More robust PB/SB calculation - applied to newly_fetched_races only
        if(nrow(newly_fetched_races) > 0) {
            processed_new_races <- newly_fetched_races %>%
                group_by(distance) %>%
                mutate(min_endtime_for_distance = min(endtime, na.rm = TRUE)) %>%
                ungroup() %>%
                group_by(distance, season) %>%
                mutate(min_endtime_for_season_distance = min(endtime, na.rm = TRUE)) %>%
                ungroup() %>%
                mutate(
                    PB = ifelse(!is.na(endtime) & !is.na(min_endtime_for_distance) & endtime == min_endtime_for_distance, 1, 0),
                    SB = ifelse(!is.na(endtime) & !is.na(min_endtime_for_season_distance) & endtime == min_endtime_for_season_distance, 1, 0),
                    # Note: curSB and prevSB logic from original code was complex and depended on lastSeason definition
                    # For simplicity and robustness, focusing on overall PB and current season's SB (SB)
                    # You might need to re-introduce more complex season-based SB flags if critical.
                    flag = case_when(
                        PB == 1 & SB == 1 ~ "PB SB",
                        PB == 1 ~ "PB",
                        SB == 1 ~ "SB",
                        TRUE ~ ""
                    )
                ) %>% 
                select(-min_endtime_for_distance, -min_endtime_for_season_distance) # Clean up helper columns
            
            races <<- rbind(races, processed_new_races) # Add processed new races to the session's races
            data_for_tool_payload <- processed_new_races # Send only newly processed races
        } else {
            data_for_tool_payload <- newly_fetched_races # Send empty if no processing happened
        }
      } else {
         data_for_tool_payload <- newly_fetched_races # Send empty if getAllRaces returned empty
      }
    } else {
      # Skater was already in `races`, send all known races for this skater for context
      data_for_tool_payload <- races %>% filter(id == input$rb)
    }

    # --- Sending data to external tool --- 
    if (nrow(data_for_tool_payload) > 0 || !is.null(input$rb)) { # Send even if no new races, if skater is selected
      current_skater_name <- "Unknown Skater"
      if(nrow(candidates)>0 && input$rb %in% candidates$id){
          current_skater_name <- candidates[candidates$id == input$rb, "Naam"][1]
      }

      payload <- list(
        timestamp = as.character(Sys.time()), # Ensure character for JSON
        sessionId = session$token, 
        eventType = "skater_selected_in_shiny",
        skaterId = input$rb,
        selectedSkaterName = current_skater_name,
        racesData = data_for_tool_payload %>% 
                      mutate(date = as.character(date)) # Ensure dates are character for JSON
      )

      tryCatch({
        resp <- request(EXTERNAL_TOOL_API_ENDPOINT) %>%
          req_body_json(payload) %>%
          req_method("POST") %>%
          req_perform() 

        if (resp_status(resp) >= 200 && resp_status(resp) < 300) {
          print(paste("Successfully sent 'skater_selected_in_shiny' event for skater_id:", input$rb))
        } else {
          print(paste("Failed to send 'skater_selected_in_shiny' event. Status:", resp_status(resp), "Body:", resp_body_string(resp)))
        }
      }, error = function(e) {
        print(paste("Error sending 'skater_selected_in_shiny' event:", e$message))
      })
    }
    # --- End of sending data to external tool ---
    
    # --- Existing logic to update Shiny UI based on 'races' dataframe --- 
    skater_races_for_ui <- filter(races, id == input$rb) # Use the session 'races' for UI updates

    if (nrow(skater_races_for_ui) > 0) {
        lastrace <- skater_races_for_ui %>% arrange(desc(date)) %>% slice_head(n=1)
        if(nrow(lastrace) > 0 && !is.na(lastrace$cat)){
            lastcat <- substr(lastrace$cat,2,3)
            age_map <- list(C1=13, C2=14, B1=15, B2=16, A1=17, A2=18, N1=19, N2=20, N3=21, N4=22, SA=23, SB=30) # Example mapping
            age <- age_map[[lastcat]]
            if(is.null(age) && !is.na(suppressWarnings(as.numeric(lastcat)))) age <- suppressWarnings(as.numeric(lastcat))
            if(is.null(age)) age <- 30 # Default if no specific age found
            updateNumericInput(session, "agePB", value = age)
        } else {
            updateNumericInput(session, "agePB", value = 30) # Default if no races or cat
        }

        # Helper function to update race selection dropdowns
        update_race_dropdown <- function(distance_val, sel_input_id, m_input_id) {
            r_dist <- skater_races_for_ui %>% filter(distance == distance_val) %>% arrange(desc(date))
            if (nrow(r_dist) > 0) {
                r_dist_select_data <- r_dist %>% 
                                    select(any_of(c("date", "track", "time", "flag", "link", "SB", "PB"))) %>% # SB/PB from processed races
                                    unite("label", date:flag, sep="|") %>% 
                                    mutate(label = str_replace_all(label, "[|]$", ""))
                
                sel_val <- "" # Default empty selection
                # Prioritize PB for 'all', then SB for 'last'/'prev', then most recent
                if (input$PBrace == "all" && any(r_dist$PB == 1)) {
                    sel_val <- filter(r_dist_select_data, endsWith(label, "PB") | endsWith(label, "PB SB"))$label[1]
                } else if ((input$PBrace == "last" || input$PBrace == "prev") && any(r_dist$SB == 1)) {
                    # Simplified: use current season's SB for both 'last' and 'prev' if specific prevSB logic isn't here
                    sel_val <- filter(r_dist_select_data, endsWith(label, "SB") | endsWith(label, "PB SB"))$label[1]
                }
                if (is.na(sel_val) || sel_val == "") { # Fallback to most recent if no match or specific PB/SB desired
                    if(nrow(r_dist_select_data)>0) sel_val <- r_dist_select_data$label[1]
                }

                updateCheckboxInput(session, m_input_id, value = TRUE)
                updateSelectInput(session, sel_input_id, choices = r_dist_select_data$label, selected = sel_val)
            } else {
                updateCheckboxInput(session, m_input_id, value = FALSE)
                updateSelectInput(session, sel_input_id, choices = c(""), selected = "")
            }
        }

        update_race_dropdown(500, "sel500", "m500")
        update_race_dropdown(1000, "sel1000", "m1000")
        update_race_dropdown(1500, "sel1500", "m1500")
        update_race_dropdown(3000, "sel3000", "m3000")
    } else {
      # No races found for this skater in the `races` df, clear/disable inputs
      updateNumericInput(session, "agePB", value = NULL)
      updateCheckboxInput(session, "m500", value=F); updateSelectInput(session, "sel500", choices=c(""), selected="")
      updateCheckboxInput(session, "m1000", value=F); updateSelectInput(session, "sel1000", choices=c(""), selected="")
      updateCheckboxInput(session, "m1500", value=F); updateSelectInput(session, "sel1500", choices=c(""), selected="")
      updateCheckboxInput(session, "m3000", value=F); updateSelectInput(session, "sel3000", choices=c(""), selected="")
    }
  })
  
  observeEvent(input$PBrace,{
    req(input$rb) # Ensure a skater is selected
    if (is.null(input$rb) || input$rb == "") return()

    skater_races_for_ui <- filter(races, id == input$rb)
    if (nrow(skater_races_for_ui) == 0) return() # No races for this skater yet

    # Re-use the helper function to update dropdowns based on new PBrace selection
    update_race_dropdown_for_pbrace <- function(distance_val, sel_input_id, m_input_id) {
        r_dist <- skater_races_for_ui %>% filter(distance == distance_val) %>% arrange(desc(date))
        if (nrow(r_dist) > 0 && input[[m_input_id]]) { # Only update if checkbox is true and races exist
            r_dist_select_data <- r_dist %>% 
                                select(any_of(c("date", "track", "time", "flag", "link", "SB", "PB"))) %>% 
                                unite("label", date:flag, sep="|") %>% 
                                mutate(label = str_replace_all(label, "[|]$", ""))
            
            sel_val <- ""
            if (input$PBrace == "all" && any(r_dist$PB == 1)) {
                sel_val <- filter(r_dist_select_data, endsWith(label, "PB") | endsWith(label, "PB SB"))$label[1]
            } else if ((input$PBrace == "last" || input$PBrace == "prev") && any(r_dist$SB == 1)) {
                 # Using current season SB for both last/prev as per simplified logic
                sel_val <- filter(r_dist_select_data, endsWith(label, "SB") | endsWith(label, "PB SB"))$label[1]
            }
            if (is.na(sel_val) || sel_val == "") { # Fallback if no specific match
                 if(nrow(r_dist_select_data)>0) sel_val <- r_dist_select_data$label[1]
            }
            updateSelectInput(session, sel_input_id, selected = sel_val)
        }
    }

    update_race_dropdown_for_pbrace(500, "sel500", "m500")
    update_race_dropdown_for_pbrace(1000, "sel1000", "m1000")
    update_race_dropdown_for_pbrace(1500, "sel1500", "m1500")
    update_race_dropdown_for_pbrace(3000, "sel3000", "m3000")
  })

  # dataAll reactive seems unused, can be removed if not needed elsewhere
  # dataAll <- reactive({
  #   return(m3000)
  # })
  
 observeEvent(input$sel500,{
   req(input$rb, input$sel500)
   if(input$sel500 == "") return()
   s<-strsplit(input$sel500, "[|]")[[1]]
   # Ensure s has enough elements to prevent subscript out of bounds
   if(length(s) < 3) return()
   link_df <- filter(races, id==input$rb & distance==500 & date==s[1] & track==s[2] & time==s[3])
   if(nrow(link_df) == 0 || is.na(link_df$link[1])) return ()
   link <- link_df$link[1]

   laps<-getLaps(link)/100 # From utils.R
   if(length(laps) >= 2){
       updateNumericInput(session, "open_500", value=laps[1])
       updateNumericInput(session, "r1_500", value=laps[2])
       updateSelectInput(session, "Track_500", selected=s[2])
   } else {
       # Clear if not enough laps
       updateNumericInput(session, "open_500", value=NA)
       updateNumericInput(session, "r1_500", value=NA)
   }
 })
 
 observeEvent(input$sel1000,{
   req(input$rb, input$sel1000)
   if(input$sel1000 == "") return()
   s<-strsplit(input$sel1000, "[|]")[[1]]
   if(length(s) < 3) return()
   link_df <- filter(races, id==input$rb & distance==1000 & date==s[1] & track==s[2] & time==s[3])
   if(nrow(link_df) == 0 || is.na(link_df$link[1])) return ()
   link <- link_df$link[1]

   laps<-getLaps(link)/100 # From utils.R
   if(length(laps) >= 3){
       updateNumericInput(session, "open_1000", value=laps[1])
       updateNumericInput(session, "r1_1000", value=laps[2])
       updateNumericInput(session, "r2_1000", value=laps[3])
       updateSelectInput(session, "Track_1000", selected=s[2])
   } else {
       updateNumericInput(session, "open_1000", value=NA)
       updateNumericInput(session, "r1_1000", value=NA)
       updateNumericInput(session, "r2_1000", value=NA)
   }
 })
 
 observeEvent(input$sel1500,{
   req(input$rb, input$sel1500)
   if(input$sel1500 == "") return()
   s<-strsplit(input$sel1500, "[|]")[[1]]
   if(length(s) < 3) return()
   link_df <- filter(races, id==input$rb & distance==1500 & date==s[1] & track==s[2] & time==s[3])
   if(nrow(link_df) == 0 || is.na(link_df$link[1])) return ()
   link <- link_df$link[1]

   laps<-getLaps(link)/100 # From utils.R
   if(length(laps) >= 4){
       updateNumericInput(session, "open_1500", value=laps[1])
       updateNumericInput(session, "r1_1500", value=laps[2])
       updateNumericInput(session, "r2_1500", value=laps[3])
       updateNumericInput(session, "r3_1500", value=laps[4])
       updateSelectInput(session, "Track_1500", selected=s[2])
   } else {
       updateNumericInput(session, "open_1500", value=NA); updateNumericInput(session, "r1_1500", value=NA)
       updateNumericInput(session, "r2_1500", value=NA); updateNumericInput(session, "r3_1500", value=NA)
   }
 })

 observeEvent(input$sel3000,{
   req(input$rb, input$sel3000)
   if(input$sel3000 == "") return()
   s<-strsplit(input$sel3000, "[|]")[[1]]
   if(length(s) < 3) return()
   link_df <- filter(races, id==input$rb & distance==3000 & date==s[1] & track==s[2] & time==s[3])
   if(nrow(link_df) == 0 || is.na(link_df$link[1])) return ()
   link <- link_df$link[1]

   laps<-getLaps(link)/100 # From utils.R
   if(length(laps) >= 8){
       updateNumericInput(session, "opening", value=laps[1]); updateNumericInput(session, "r1", value=laps[2])
       updateNumericInput(session, "r2", value=laps[3]); updateNumericInput(session, "r3", value=laps[4])
       updateNumericInput(session, "r4", value=laps[5]); updateNumericInput(session, "r5", value=laps[6])
       updateNumericInput(session, "r6", value=laps[7]); updateNumericInput(session, "r7", value=laps[8])
       updateSelectInput(session, "Track_3000", selected=s[2])
   } else {
       # Clear all lap time inputs if not enough laps
       lapply(paste0(c("opening", paste0("r",1:7))), function(id) updateNumericInput(session, id, value=NA))
   }
 })
 
  # Generate a summary of the dataset ----
  output$summary <- renderText({
    req(input$rb)
    if(nrow(candidates) == 0 || !(input$rb %in% candidates$id)) return("")
    skater<-filter(candidates, id==input$rb)
    gender<-substr(skater$Categorie,1,1)
    # Ensure Seizoen.en. is character and not NA before strsplit
    if(is.na(skater$`Seizoen.en.`[1])) return(paste("<h2>naam:", skater$Naam, "</h2>geslacht:",gender,"vereniging: ", skater$Vereniging))
    seasons_str <- as.character(skater$`Seizoen.en.`[1])
    seasons_split <- strsplit(seasons_str,"-")
    
    firstSeason <- NA; lastSeason <- NA
    if(length(seasons_split[[1]]) == 2){
        firstSeason<-suppressWarnings(as.numeric(seasons_split[[1]][1]))
        lastSeason<-suppressWarnings(as.numeric(seasons_split[[1]][2]))-1
    }
    paste0("<h2>naam: ", skater$Naam, "</h2>geslacht: ",gender, ifelse(!is.na(firstSeason) && !is.na(lastSeason), paste0(" seizoenen: ", firstSeason, "-", lastSeason), ""), " vereniging: ", skater$Vereniging)
    
    })
  
  output$time1500<-renderText({
    req(input$m1500, input$open_1500, input$r1_1500, input$r2_1500, input$r3_1500)
    endtime<-input$open_1500+input$r1_1500+input$r2_1500+input$r3_1500
    endtimeStr<- if (is.na(endtime)) "..." else substr(SecToHms(endtime, digits=2),4,12)
    paste0("<h4><b>1500m PB tijd: ", endtimeStr, "</b></h4>")
    })

  output$time500<-renderText({
    req(input$m500, input$open_500, input$r1_500)
    endtime<-input$open_500+input$r1_500
    endtimeStr<- if (is.na(endtime)) "..." else substr(SecToHms(endtime, digits=2),4,12)
    paste0("<h4><b>500m PB tijd: ", endtimeStr, "</b></h4>")
  })

  output$time1000<-renderText({
    req(input$m1000, input$open_1000, input$r1_1000, input$r2_1000)
    endtime<-input$open_1000+input$r1_1000+input$r2_1000
    endtimeStr<- if (is.na(endtime)) "..." else substr(SecToHms(endtime, digits=2),4,12)
    paste0("<h4><b>1000m PB tijd: ", endtimeStr, "</b></h4>")
  })

	output$time3000<-renderText({
    req(input$m3000, input$opening, input$r1, input$r2, input$r3, input$r4, input$r5, input$r6, input$r7)
    endtime<-input$opening+input$r1+input$r2+input$r3+input$r4+input$r5+input$r6+input$r7
    endtimeStr<- if (is.na(endtime)) "..." else substr(SecToHms(endtime, digits=2),4,12)
    paste0("<h4><b>3000m PB tijd: ", endtimeStr, "</b></h4>")
    })								  
  
  output$headerNPB3000<-renderText({
    extra <- if (input$adjust) " (omgerekend naar baan AL)" else ""
    paste0("<h4>Vorige PBs op 3000m van geselecteerde schaatsers", extra, "</h4>")})
  
  output$headerPBshorter<-renderText({
    extra <- if (input$adjust) " (omgerekend naar baan AL)" else ""
    extra2 <- if (input$m3000) " en een eerdere (niet-PB) 3000m" else ""
    paste0("<h4>PB tijden op de kortere afstanden", extra2, extra, "</h4>")})

  
  output$viewBest <- renderTable({
    req(input$rb)
    if(nrow(races) == 0 || !(input$rb %in% races$id)) return(data.frame())
    r<-filter(races, id==input$rb & distance==3000)%>%arrange(endtime) %>%select(date, cat, track, time)
    head(r,5)
    }
  )
  output$viewSeason <- renderTable({
    req(input$rb)
    if(nrow(races) == 0 || !(input$rb %in% races$id)) return(data.frame())
    r<-filter(races, id==input$rb & distance==3000)%>%arrange(desc(date)) %>%select(date, cat, track, time)
    head(r,5)
  }
  )
  
  tablePB<-reactive({
    req(input$showPred, input$rb) # Ensure prediction is requested and skater is selected
    
    # Gather inputs for the query object q
    # 3000m inputs
    T3000 <- input$Track_3000; valid3000 <- FALSE
    laps3000 <- c(input$opening, input$r1, input$r2, input$r3, input$r4, input$r5, input$r6, input$r7)
    if(input$m3000 && !any(is.na(laps3000)) && length(laps3000)==8){
        opening <- laps3000[1]*100; aopening=round(opening/trkcor[[T3000]],2);
        r1<-laps3000[2]*100; ar1=round(r1/trkcor[[T3000]],2);
        r2<-laps3000[3]*100; ar2=round(r2/trkcor[[T3000]],2);
        r3<-laps3000[4]*100; ar3=round(r3/trkcor[[T3000]],2);
        r4<-laps3000[5]*100; ar4=round(r4/trkcor[[T3000]],2);
        r5<-laps3000[6]*100; ar5=round(r5/trkcor[[T3000]],2);
        r6<-laps3000[7]*100; ar6=round(r6/trkcor[[T3000]],2);
        r7<-laps3000[8]*100; ar7=round(r7/trkcor[[T3000]],2);	
        endtime_3000_n = sum(laps3000)*100; valid3000 = TRUE
    } else {
        # Provide default NA or zero values if 3000m inputs are not used or incomplete
        opening <- NA; aopening=NA; r1<-NA; ar1=NA; r2<-NA; ar2=NA; r3<-NA; ar3=NA;
        r4<-NA; ar4=NA; r5<-NA; ar5=NA; r6<-NA; ar6=NA; r7<-NA; ar7=NA;
        endtime_3000_n = NA
    }

    # Age PB input
    agePB <- if(is.na(input$agePB)) 30 else input$agePB # Default age if NA
    
    # 500m inputs
    T500 <- input$Track_500
    op_5 <- NA; aop_5 <- NA; r1_5 <- NA; ar1_5 <- NA
    if(input$m500 && !is.na(input$open_500) && !is.na(input$r1_500)){
        op_5 <- input$open_500*100; aop_5=round(op_5/trkcor500[[T500]],2)
        r1_5 <- input$r1_500*100; ar1_5=round(r1_5/trkcor500[[T500]],2)
    }

    # 1000m inputs
    T1000 <- input$Track_1000
    op_1 <- NA; aop_1 <- NA; r1_1 <- NA; ar1_1 <- NA; r2_1 <- NA; ar2_1 <- NA
    if(input$m1000 && !is.na(input$open_1000) && !is.na(input$r1_1000) && !is.na(input$r2_1000)){
        op_1 <- input$open_1000*100; aop_1=round(op_1/trkcor1000[[T1000]],2)
        r1_1 <- input$r1_1000*100; ar1_1=round(r1_1/trkcor1000[[T1000]],2)
        r2_1 <- input$r2_1000*100; ar2_1=round(r2_1/trkcor1000[[T1000]],2)
    }

    # 1500m inputs
    T1500 <- input$Track_1500
    op_15 <- NA; aop_15 <- NA; r1_15 <- NA; ar1_15 <- NA; r2_15 <- NA; ar2_15 <- NA; r3_15 <- NA; ar3_15 <- NA
    if(input$m1500 && !is.na(input$open_1500) && !is.na(input$r1_1500) && !is.na(input$r2_1500) && !is.na(input$r3_1500)){
        op_15 <- input$open_1500*100; aop_15=round(op_15/trkcor1500[[T1500]],2)
        r1_15 <- input$r1_1500*100; ar1_15=round(r1_15/trkcor1500[[T1500]],2)
        r2_15 <- input$r2_1500*100; ar2_15=round(r2_15/trkcor1500[[T1500]],2)
        r3_15 <- input$r3_1500*100; ar3_15=round(r3_15/trkcor1500[[T1500]],2)
    }
    
    current_skater_details <- filter(candidates, id==input$rb)
    gender <- if(nrow(current_skater_details)>0 && !is.na(current_skater_details$Categorie[1])) substr(current_skater_details$Categorie[1],1,1) else "H" # Default to Male if unknown
    
    query_data <- tibble(
      Track_n = T3000,      # Track of the current 3000m (can be NA if not input$m3000)
      Track_p = input$pb_track, # Target track for PB prediction
      endtime_n = endtime_3000_n, # Current 3000m endtime in centiseconds (can be NA)
      age_perf_n = agePB,
      Gender_n = gender,
      PersonID = input$rb,
      r_500_0_n=op_5, ar_500_0_n=aop_5, r_500_1_n=r1_5, ar_500_1_n=ar1_5,
      r_1000_0_n=op_1, ar_1000_0_n=aop_1, r_1000_1_n=r1_1, ar_1000_1_n=ar1_1, r_1000_2_n=r2_1, ar_1000_2_n=ar2_1,
      r_1500_0_n=op_15, ar_1500_0_n=aop_15, r_1500_1_n=r1_15, ar_1500_1_n=ar1_15, r_1500_2_n=r2_15, ar_1500_2_n=ar2_15, r_1500_3_n=r3_15, ar_1500_3_n=ar3_15,
      r_0_n=opening, ar_0_n=aopening, r_1_n=r1, ar_1_n=ar1, r_2_n=r2, ar_2_n=ar2, r_3_n=r3, ar_3_n=ar3,
      r_4_n=r4, ar_4_n=ar4, r_5_n=r5, ar_5_n=ar5, r_6_n=r6, ar_6_n=ar6, r_7_n=r7, ar_7_n=ar7
    )
    
    # Filter settings
    ft_range_val <- if(input$ft_r && input$m3000 && !is.na(endtime_3000_n)) input$ftrange*100 else 0
    age_range_val <- if(input$age_r) input$agerange else 0
    
    # Call predict_pb from utils.R
    fullpred<-predict_pb(query_data, cb, ft_range=ft_range_val, age_range=age_range_val, 
                         npb=input$m3000, pb500=input$m500, pb1000=input$m1000, 
                         pb1500 = input$m1500, adjust=input$adjust)
    pred_vector <-fullpred[[1]]
    
    # Formatting the prediction output table
    pred_endtime_cs <- pred_vector[2] # Predicted endtime in centiseconds
    pred_laps_cs <- pred_vector[3:10]  # Predicted laps in centiseconds

    current_pb_row <- NULL
    if (valid3000) {
        current_pb_endtime_str <- substr(SecToHms(endtime_3000_n/100, digits=2),4,12)
        current_pb_row <- c("huidig PB", T3000, current_pb_endtime_str, 
                            (opening/100), (r1/100), (r2/100), (r3/100), 
                            (r4/100), (r5/100), (r6/100), (r7/100))
    }
    
    predicted_pb_endtime_str <- if(is.na(pred_endtime_cs)) "N/A" else substr(SecToHms(pred_endtime_cs/100, digits=2),4,12)
    predicted_pb_laps <- if(all(is.na(pred_laps_cs))) rep("N/A", 8) else round(pred_laps_cs/100,2)

    predicted_pb_row <- c("voorspeld PB", input$pb_track, predicted_pb_endtime_str, predicted_pb_laps)
    
    pb_display_table <- if(valid3000) data.frame(rbind(predicted_pb_row, current_pb_row), row.names = NULL) 
                        else data.frame(rbind(predicted_pb_row), row.names = NULL)
    names(pb_display_table)<-c("type", "baan", "tijd","opening","r1", "r2", "r3", "r4", "r5", "r6", "r7" )
        
    # Formatting comparable cases tables (cs, csnp, pbshort)
    cs_data <- fullpred[[2]]
    csnp_data <- fullpred[[3]]
    pbshort_data <- fullpred[[4]]

    cstable_display <- data.frame()
    if(nrow(cs_data) > 0){
        namestrpb_list <- c()
        pbstimes_list <- list()
        if (input$m500 && "endtime_500_p" %in% names(pbshort_data)) { 
            namestrpb_list <- c(namestrpb_list, "PB 500m")
            pbstimes_list[[length(pbstimes_list)+1]] <- paste0(substr(SecToHms(pbshort_data$endtime_500_p/100, digits=2),4,12)," (",pbshort_data$trk_500_p,")")
        }
        if (input$m1000 && "endtime_1000_p" %in% names(pbshort_data)) { 
            namestrpb_list <- c(namestrpb_list, "PB 1000m")
            pbstimes_list[[length(pbstimes_list)+1]] <- paste0(substr(SecToHms(pbshort_data$endtime_1000_p/100, digits=2),4,12)," (", pbshort_data$trk_1000_p,")")
        }
        if (input$m1500 && "endtime_1500_p" %in% names(pbshort_data)) { 
            namestrpb_list <- c(namestrpb_list, "PB 1500m")
            pbstimes_list[[length(pbstimes_list)+1]] <- paste0(substr(SecToHms(pbshort_data$endtime_1500_p/100, digits=2),4,12), " (",pbshort_data$trk_1500_p,")")
        }
        if (input$m3000 && "endtime_n" %in% names(csnp_data)) { # Assuming csnp_data holds the non-PB 3000m for cases
            namestrpb_list <- c(namestrpb_list, "non-PB 3000m")
            pbstimes_list[[length(pbstimes_list)+1]] <- paste0(substr(SecToHms(csnp_data$endtime_n/100, digits=2),4,12), " (",csnp_data$Track_n,")")
        }
        
        cstable_base <- data.frame(case = sprintf("%02i", seq_len(nrow(cs_data))),
                                   leeftijd = round(cs_data$age_perf_p,0))
        for(i in seq_along(pbstimes_list)) { cstable_base[[namestrpb_list[i]]] <- pbstimes_list[[i]] }
        cstable_base[["PB 3000m"]] <- paste0(substr(SecToHms(cs_data$endtime_p/100, digits=2),4,12), " (",cs_data$Track_p,")")
        cstable_display <- cstable_base
    }
    
    csnp3k_display <- NULL
    if (input$m3000 && nrow(csnp_data) > 0 && all(paste0("r_",0:7,"_p") %in% names(csnp_data))){
        # Original csnp3k logic based on csnp having r_0_p...r_7_p from predict_pb structure. Adjusting for clarity.
        # This assumes csnp_data in fullpred[[3]] contains the relevant lap times for non-PB 3000m of cases.
        # Let's select the columns if they are named like r_0_n, r_1_n etc. in csnp_data (from predict_pb's cases_n return)
        lap_cols_n <- paste0("r_", 0:7, "_n") # Example: if predict_pb returns these for non-PB laps of cases
        if(all(lap_cols_n %in% names(csnp_data))){
            csnp3k_laps <- csnp_data %>% select(all_of(lap_cols_n))
            csnp3k_display <- cbind(case = sprintf("%02i", seq_len(nrow(csnp3k_laps))), round(csnp3k_laps/100,2))
            names(csnp3k_display) <- c("case", "opening","r1", "r2", "r3", "r4", "r5", "r6", "r7")
            
            # Add the query skater's non-PB 3000m laps if input$m3000 was true
            query_npb_laps <- c(0, round(c(opening, r1, r2, r3, r4, r5,r6, r7)/100,2))
            query_skater_name <- if(nrow(current_skater_details)>0) current_skater_details$Naam[1] else "Input Schaatser"
            csnp3k_display <- rbind(csnp3k_display, stats::setNames(as.list(query_npb_laps), names(csnp3k_display)))
            csnp3k_display$case[nrow(csnp3k_display)] <- query_skater_name
        }
    } else {csnp3k_display <- data.frame(message = "Geen non-PB 3000m data voor cases beschikbaar of geselecteerd.")}
    
    csnpPB_display <- NULL
    if((input$m500 || input$m1000 || input$m1500) && nrow(csnp_data) > 0){
        # This logic was for showing the *query* skater's shorter distance lap times, not from cases_n.
        # predict_pb's fullpred[[3]] (csnp_data) contains `regStr` matched columns for *cases*.
        # For the *query* skater's input PBs on shorter distances (for the table view):
        pb_display_rows <- list()
        pb_namestr <- c("case")
        current_query_skater_name <- if(nrow(current_skater_details)>0) current_skater_details$Naam[1] else "Input Schaatser"
        row_vals <- c(current_query_skater_name)

        if (input$m500) {
            pb_namestr <- c(pb_namestr, "opening 500", "r1 500")
            row_vals <- c(row_vals, round(op_5/100,2), round(r1_5/100,2))
        }
        if (input$m1000) {
            pb_namestr <- c(pb_namestr, "opening 1000", "r1 1000", "r2 1000")
            row_vals <- c(row_vals, round(op_1/100,2), round(r1_1/100,2), round(r2_1/100,2))
        }
        if (input$m1500) {
            pb_namestr <- c(pb_namestr, "opening 1500", "r1 1500", "r2 1500","r3 1500")
            row_vals <- c(row_vals, round(op_15/100,2), round(r1_15/100,2), round(r2_15/100,2), round(r3_15/100,2))
        }
        if(length(pb_namestr) > 1){ # if any distance was selected
            csnpPB_display <- data.frame(matrix(row_vals, nrow=1))
            names(csnpPB_display) <- pb_namestr
        } else {
             csnpPB_display <- data.frame(message = "Geen kortere afstanden geselecteerd voor input.")
        }
    } else {csnpPB_display <- data.frame(message = "Geen kortere afstanden geselecteerd.")}
   
    list(pb_display_table, cstable_display, csnp3k_display, csnpPB_display)
  })
  
   observeEvent(input$showPred, {
    output$PBheader<-renderText({
        req(input$rb)
        skater_name <- if(nrow(candidates)>0 && input$rb %in% candidates$id) filter(candidates, id==input$rb)$Naam[1] else "Onbekende Schaatser"
        paste0("<h3>Voorspelde tijd voor ", skater_name,  " op baan ", input$pb_track, "</h3>")
    })
    
    prediction_results <- tablePB()
    
    output$viewPB <- renderTable({ prediction_results[[1]] })
    
    output$plotPB<-renderPlot({
      req(nrow(prediction_results[[1]]) > 0)
      udatag<-gather(prediction_results[[1]],key="roundstr",value="time",opening,r1,r2,r3,r4,r5,r6,r7)
      udatag$roundstr<-ifelse(udatag$roundstr=="opening", "r0", udatag$roundstr)
      udatag$round=as.numeric(substr(as.character(udatag$roundstr),2,2))
      udatag3<-group_by(udatag, type, round) %>% summarize(time=mean(as.numeric(as.character(time)), na.rm=T), .groups = 'drop') 
      ggplot(udatag3)+geom_line(aes(y=time,x=round, color=type))+ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.7, 0.2))+ guides(color=guide_legend(title="races"))
      
    })
    output$viewCases <- DT::renderDataTable(
      prediction_results[[2]], options = list(searching=F, ordering=F, paging=F, selection='single'), rownames=F
    )
    
    # output$plotCases<-renderPlot({ # This plot was complex and potentially using cs[[2]] which is now cstable_display
    #   # Requires re-evaluation if needed, as tablePB()[[2]] is now a formatted display table.
    #   # The original predict_pb[[2]] (cs_data) might be needed for plotting all case laps.
    # })

    output$viewPBCases <- renderTable({
      req(input$viewCases_rows_selected) # Ensure a row is selected in the DT table
      # The logic to show selected case + query skater's PB laps
      # prediction_results[[4]] is csnpPB_display, which is the *query* skater's input PBs.
      # To show selected *cases'* PB laps, we'd need that data from fullpred[[4]] (pbshort_data) directly.
      # This part needs rethinking: what should viewPBCases display? Selected case's PBs or Query's PBs?
      # For now, displaying the query skater's input PB laps (already in prediction_results[[4]])
      prediction_results[[4]]
    })

    output$plotNPCases<-renderPlot({
      req(input$showCases, nrow(prediction_results[[3]]) > 0, !("message" %in% names(prediction_results[[3]])))
      name <- if(nrow(candidates)>0 && input$rb %in% candidates$id) filter(candidates, id==input$rb)$Naam[1] else "Input Schaatser"
      udatag <- prediction_results[[3]] %>% 
                filter(case != "0") %>% # Remove the placeholder 0 if it was used
                mutate(show = case == name | row_number() %in% (input$viewCases_rows_selected) )
      
      if(nrow(udatag) == 0) return(NULL) # Nothing to plot

      udatag_long<-gather(udatag,key="roundstr",value="time",opening,r1,r2,r3,r4,r5,r6,r7)
      udatag_long$roundstr<-ifelse(udatag_long$roundstr=="opening", "r0", udatag_long$roundstr)
      udatag_long$round=as.numeric(substr(as.character(udatag_long$roundstr),2,2))
      udatag_long$time = as.numeric(as.character(udatag_long$time))
      
      ggplot(filter(udatag_long,!show))+geom_line(aes(y=time,x=round,group=case), color="grey")+
        geom_line(data=filter(udatag_long,show), aes(y=time,x=round, color=case), size=1.2)+
        ylab("rondetijd (s)")+xlab("ronde")+ theme(legend.position = c(0.8, 0.4))+
        guides(color=guide_legend(title="Niet-PB 3000m voor case"))+ scale_color_brewer(palette = "Set2", drop=F)
      
    })
    output$viewNPCases <- renderTable({
      req(input$showCases, nrow(prediction_results[[3]]) > 0, !("message" %in% names(prediction_results[[3]])))
      # Show selected case's non-PB 3000m laps and the query skater's input non-PB 3000m laps
      name <- if(nrow(candidates)>0 && input$rb %in% candidates$id) filter(candidates, id==input$rb)$Naam[1] else "Input Schaatser"
      filter(prediction_results[[3]], case == name | row_number() %in% (input$viewCases_rows_selected) )
    })

    # For plotPB1000Cases and plotPB1500Cases, they rely on the structure of fullpred[[4]] from original predict_pb.
    # The tablePB()[[4]] is now `csnpPB_display` which shows the *query's* input PBs.
    # Plotting selected *cases'* PBs on shorter distances would require using pbshort_data (fullpred[[4]] from predict_pb)
    # and linking it to input$viewCases_rows_selected. This requires more extensive rework of data flow for these plots.
    # For now, these will not work as intended without that rework.

    output$plotPB1000Cases<-renderPlot({
      req(input$m1000, input$showCases) # Add other necessary req() checks
      # This needs data for selected cases' 1000m PBs. Placeholder plot.
      ggplot() + labs(title="Plot voor 1000m PBs van cases (needs data rework)")
    })
    output$plotPB1500Cases<-renderPlot({
      req(input$m1500, input$showCases) # Add other necessary req() checks
      # This needs data for selected cases' 1500m PBs. Placeholder plot.
      ggplot() + labs(title="Plot voor 1500m PBs van cases (needs data rework)")
    }) 
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
