library(shiny);library(ggplot2);library(lubridate);library(leaflet);library(DT);library(shinylogs);library(bslib);library(data.table);library(dplyr);library(shinydashboard)
source("ggplot_themes.R")
theme_set(tema)

# Read in data ----

fread("leaflet_points.csv") -> leaf_points

sf::st_read("leaflet_lakes.sqlite") %>% 
  sf::st_simplify(d= 1) %>% 
  rename(StedID = stedid, Stedtekst = stedtekst) -> leaf_lakes

# UI ----
ui <- page_navbar(
  id = "inTabset",
  title = "Sø kemiske data",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "Introduktion", 
            HTML("
                 <h1>Velkommen til denne hjemmeside om kemiske forhold i de danske søer</h1>
                 <h2> Inspirationen til denne side </h2>
                 <p>
                 Denne hjemmeside er lavet for at muliggøre alle at følge den kemiske udvikling i de danske søer.
                 I en verden af misinformation og 'alternative'-fortællinger føler jeg at det er nødvendigt at sørge for at tal og undersøgelser om den danske natur er offentligt tilgængelige.
                 Ikke blot i form af tabeller for faglærte at undersøge, men også for lægmanden der brænder for den natur som vi færdes i, og kan følge udviklingen på tæt hold.
                 Det er ofte at passioneret ildsjæle har en meget bedre forståelse for de økologiske nicher som de kender som deres baglomme.
                 Netop fordi de har færdes der i mange timer, på forskellige tidspunkter på året. Har set området gro, vokse, blomstre, forsvinde og gå i forfald. 
                 Jeg håber at jeg kan være med til at styrke samarbejdet mellem de lidenskabelige og videnskabelige ved at lave dette værktøj i form af denne side. 
                 </p>
                 <h2> How-to </h2>
                 <p>
                 Start med at vælge næste fane i toppen af siden, den der hedder 'Valg af sø'. 
                 Her vil du finde de søer og målepunkter som der findes data fra. Et kort vil komme frem efter et par sekunder. 
                 På kortet kan du vælge den sø du er interesseret i ved at klikke på den.
                 Nedenfor vil der komme en tabel med alle de kemiske undersøgelser der er foretaget her.
                 Endvidere, kan du trykke på knappen for at få en grafisk fremstilling. Ved at trykke på knappen rykkes du til næste fane. 
                 Her kan du få en grafisk fremstilling af dataet fra søen, hvor du kan vælge hvilke parametre og tidsinterval du er intereseret i.
                 <br>
                 God fornøjelse.
                 </p>
                 <h2> De ferske vande i Danmark </h2>
                 <p>
                 I Danmark findes der mere end 180.000 søer og damme over 1 hektar. På trods af dette er det et fåtal af disse der er i god økologisk tilstand. 
                 Vi er i Danmark forpligtet til at opfylde EU's vandrammedirektiv, hvori et af kravene er at alle landets søer har mindst god økologisk tilstand i 2027.
                 I den seneste opgørelse af de økologiske og kemiske tilstande af søerne fra 2023, hvor 986 søer blev undersøgt var kun 5 i god tilstand, og rapporten spår en dyster fremtid for søerne.
                 Jævnfør en rapport fra 2019, spås det at ca. 56% af søerne ikke når målet om god økologisk tilstand inden 2027.
                 I 2023 konkluderede man mere end 30% af søerne er i ringe økologisk tilstand, samt at hhv. 23% er i dårlig og moderat tilstand.
                 Denne ringe kvalitet i vores søer skyldes hovedsagligt høje tilførelser af næringsstofferne, fosfor (P) og kvælstof (N),
                 </p>
                 <h2> Datasættet </h2>
                 <p>
                 Det kemiske datasæt der er brugt på denne side, stammer fra <a href='https://kemidata.miljoeportal.dk'>Miljøportalen</a>, hvor det kan hentes af alle, dog kun i tabel-form. 
                 Endvidere, er det muligt at hente data for mange flere parametre end blot de kemiske, også de biologiske og til dels fysiske. 
                 For at sikre at dataet er ajour hentes det en gang om måneden. 
                 <br>
                 <br>
                 Denne hjemmeside er lavet af <a href='mailto:Jonassoe@biology.sdu.dk'</a> Jonas Stage Sø</a>, Ph.D., ferskvandsbiolog, Postdoc ved Syddansk Universitet.
                 <br>
                 <br>
                 <br>
                 <br>
                 <br>
                 </p>
                 ")
            ),
  nav_panel(title = "Valg af sø", 
            HTML("
                 <h2>På denne side kan du vælge hvilken sø du vil undersøge nærmere</h2> 
                 <p>
                 Siden kan tage et par sekunder om at indlæse.
                 Vælg den sø du gerne vil se data fra.
                 Du kan undersøge kortet for hvilke søer der ligger data for, ved at forstørre kortet.
                 Når du har fundet en sø eller målepunkt du gerne vil undersøge, klik da på det. 
                 Søerne som fremstår som blå på kortet er de søer hvor der ligger kemiske data tilgængelige.
                 <br>
                 I nogle tilfælde overlapper målepunkterne ikke med en sø, disse vil derfor blive vidst som en prik på kortet. 
                 <br>
                 </p>
                 "
                ),
            leafletOutput("lake_picker_map", width = "100%", height = "120vh"),
            actionButton("page_change",label="Når du har valgt en sø, kan du skifte til grafisk fremstilling ved at trykke her."),
            HTML("
                 <p>
                 Her nedenfor vil du se en tabel med alle de målinger der er lavet for den sø du har valgt.
                 Du kan også sortere, filtrere eller søge i målingerne.
                 </p>
                 "),
            dataTableOutput("outText"),
            ),
  nav_panel(title = "Grafisk illustration af data", 
            value = "chem_data",
            HTML("
                 <p>
                 Her kan du se en grafisk fremstilling af den sø du valgte på foregående side. 
                 Du skal starte med at vælge hvilke stofparametre du vil undersøge før den grafiske fremstilling kommer frem. 
                 <br>
                 Du kan i venstre side vælge hvilke stofparametre og tidsinterval du vil undersøge nærmere. <br>
                 </p>
                 "),
            layout_sidebar(
              sidebar = sidebar(
              checkboxGroupInput("analysis_plot_select", "Vælg stofparametre at undersøge:"),
              sliderInput("chemistry_year_select", "Vælg tidsintervallet at undersøge:", min = 1900, max = 2025,value = c(1900,2025), sep = "")
                             ),
              plotOutput("chem_plot_output")
                     )
            ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Researchgate", href = "https://www.researchgate.net/profile/Jonas-Stage-So")),
    nav_item(tags$a("Github", href = "https://github.com/JonasStage")),
    nav_item(tags$a("SDU PURE", href = "https://portal.findresearcher.sdu.dk/da/persons/jonassoe")),
    nav_item(tags$a("Google Scholar", href = "https://scholar.google.com/citations?user=-6tGaCoAAAAJ&hl=da"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  track_usage(storage_mode = store_json(path = "logs/"))
  # Change page ----
  observeEvent(input$page_change, {
    updateNavlistPanel(
      session = getDefaultReactiveDomain(),
      inputId = "inTabset",
      selected = "chem_data"
    )
    })
  
  # Lake picker ----
  output$lake_picker_map <- renderLeaflet({
  leaflet() %>% 
    setView(11.764443365420146,55.95939198374305, zoom = 7) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addCircleMarkers(data = leaf_points, lng = ~long, lat = ~lat,stroke =F, radius = 4,label = ~Stedtekst,color = "black", fillOpacity = 1,layerId = ~StedID) %>% 
    addPolygons(data = leaf_lakes, label = ~Stedtekst, stroke = F, fillColor = "royalblue", fillOpacity = 1, layerId = ~gml_id)
  })
  # Environmental data ----
  ## Read in chemistry data for the specified lake ----
  values <- reactiveValues(select_id = NULL)
  
  observeEvent(input$lake_picker_map_marker_click, {
    values$select_id <- input$lake_picker_map_marker_click$id
  })
  
  observeEvent(input$lake_picker_map_shape_click, {
    leaf_lakes %>% 
      filter(gml_id == input$lake_picker_map_shape_click$id) %>% 
      pull(StedID) -> values$select_id 
  
  print(values$select_id)
    })
  
 filtered_data <- reactive({
   req(values$select_id)
   

    fread("chemistry_data.csv", sep = ";", dec = ",",
          select = c("StedID","Stedtekst","Vandområde","Dato","Link","Prøvetype","Dybde (m)",
                     "Faktiske dybder (m)","Analysefraktion","Stofparameter","Resultat-attribut",
                     "Resultat","Enhed","Detektionsgrænse LD","Kvantifikationsgrænse LQ","Kvalitetsmærke"))[StedID %in% values$select_id] %>% 
      .[, c("Dato","parameter_unit") := .(dmy_hms(Dato), paste0(Stofparameter," (",Enhed,")"))] -> filtered_data
    
    updateCheckboxGroupInput(inputId = "analysis_plot_select", choices = sort(unique(filtered_data[,Stofparameter])))
    updateSliderInput(inputId = "chemistry_year_select", min = year(min(filtered_data[,Dato])), max = year(max(filtered_data[,Dato])))
    
    return(filtered_data)
  })
  
 output$outText <- renderDataTable(filtered_data()[, .(Vandområde,Dato = as.Date(Dato),Prøvetype, `Dybde (m)` = round(`Dybde (m)`,2), Stofparameter= as.factor(Stofparameter),`Resultat-attribut`, Resultat, Enhed)], filter = "top")
 
 # Plotting items ----

 output$chem_plot_output <- renderPlot({
   req(input$analysis_plot_select)
   
   labels <- filtered_data() %>% 
     distinct(Enhed, Stofparameter)
   
   labels$Enhed -> units_df
   names(units_df) <- labels$Stofparameter 
   
   filtered_data() %>% 
     filter(Stofparameter %in% input$analysis_plot_select & 
            between(year(Dato), input$chemistry_year_select[1], input$chemistry_year_select[2])) %>% 
     ggplot(aes(Dato, Resultat)) + 
     geom_point(size = 3, shape = 21,aes(fill = Stofparameter)) + 
     geom_line(aes(col = Stofparameter)) + 
     scale_x_datetime(date_labels = "%F") +
     facet_grid(parameter_unit~1, scales = "free_y", switch = "y")  +
     theme(strip.placement = "outside", 
           strip.background = element_blank(),
           strip.text.x = element_blank()) + 
     labs(x= "Dato",
          y = "")-> chem_plot
   
   print(chem_plot)
 })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
