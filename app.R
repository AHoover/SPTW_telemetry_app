## R Shiny Application for South Pacific TurtleWatch Telemetry Model Prediction Map Web Hosting
### author: "Aimee L Hoover"
### date: "Winter 2020/2021"
### subtitle: "Predicting Eastern Pacific Leatherback Movement Using Telemetry Data based upon [Predicting residence time using a continuous-time discrete-space model of leatherback turtle satellite telemetry data](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644)" 

########
# Set R Options
# Load Libraries

options("rgdal_show_exportToProj4_warnings" = "none")
options(curl_interrupt = FALSE)
httr::config(connecttimeout = 60)

library(rsconnect);library(shiny);library(leaflet);library(viridis);library(stringr);library(raster);library(maptools);library(rgdal);library(rgeos);library(tidyverse);library(RColorBrewer);library(shinycssloaders);library(shinyBS)

########


########
# Load Data

## Define year and month of interest for prediction

source('data/define_year_month.R')
predictyear <<- '2021'
predictmonth <<- 'July'

define_year_month(predictyear, str_pad(match(predictmonth,month.name), 2, pad='0'))
file_dater <- paste0(year,"_",month)

## Load Prediction; set color palette

filename_predict <- paste0("data/Prediction_", month.abb[as.numeric(month)], year, "_0.1deg.tif") # Alternatively, tolower(month.abb[as.numeric(month)]) if lowercase month

predictraster <- raster(filename_predict)
palpredict <- colorNumeric("magma", values(predictraster), reverse=FALSE, na.color = "transparent")

## Load country and EEZ map data
### Subsets were created from marineregions.org

SPshpallsubset <- read_rds("data/SPshpallsubset.rds") 

countriessubset <- read_rds("data/countriessubset.rds") 

## Load Center on Biological Diversity's 
## Ecologically or Biologically Significant Marine Areas (EBSAs)

EBSA_all <- read_rds("data/EBSAs_CBD.rds") 

### Subset to allow for desired mapping 

ebsas <- EBSA_all[-which(EBSA_all$NAME == 'Costa Rica Dome'|EBSA_all$NAME == 'Galapagos'|EBSA_all$NAME == 'Peru Upwelling Cores'|EBSA_all$NAME == 'Cordillera Malpelo'),]

ebsas_small <- EBSA_all[which(EBSA_all$NAME == 'Galapagos'|EBSA_all$NAME == 'Peru Upwelling Cores'|EBSA_all$NAME == 'Cordillera Malpelo'),]

CRD <- EBSA_all[which(EBSA_all$NAME == 'Costa Rica Dome'),]

## Load Marine Protected Areas and other protected areas

allmpas <- read_rds("data/MPAs_WDPA.rds")

## Load GFW Fishing Effort at 0.1 degrees for 2012-2016

fisheries = stack("data/Effort_Fishing_01deg_2012-2016.tif")
names(fisheries) = paste0(c("Effort2012","Effort2013","Effort2014","Effort2015","Effort2016"))

## Load GFW 2016 Fishing Effort by Gear for 2016

gear2016 = stack("data/Effort_FishingByGear_01deg_2016.tif")
names(gear2016) = c("Drifting_longlines", "Fixed_gear", "Other_fishing", "Purse_seines", "Squid_jigger", "Trawlers")

########


########
# User_Interface

# A UI with tabPanels and tabsetPanels

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    h1 {
    font: 'Montserrat';
    font-size: 30px;
    text-align: -webkit-center;
    font-weight: bold;
    }
    h2 {
    font: 'Montserrat';
    font-size: 26px;
    }
    .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
    color: #a2b03a;
    /* color: black; */
    box-shadow: 3px 2px 8px 0px #3c4b57;
    font-weight: bold;
    font-size:105%;
    }
    .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
    color: #ffb93f;
    background-color: transparent;
    font-weight: bold;
    }
    "))
  ),
  titlePanel(h1("South Pacific TurtleWatch Model"), windowTitle = "SPTW Telemetry Model"),
    navbarPage(title = div(img(src = "upwell_green_gray.png", style="margin-top:0px;padding-left:4px;padding-bottom:10px;padding-top:2px", height = 55),"Eastern Pacific Leatherback Movement", style = "margin-top:-13px"),
                 
               tabPanel("Prediction Map",
                        
                        fluidRow(column(width = 2),
                          column(
                            br(),
                            p(h2("Predicting Eastern Pacific Leatherback Movement Using Telemetry Data", style = "color:black;background-color:lavender;padding:15px;border-radius:10px;font-weight:bold")),
                            p("South Pacific TurtleWatch uses methods based upon",
                                     a(href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644", target = "_blank", em("Predicting residence time using a continuous‐time discrete‐space model of leatherback turtle satellite telemetry data"), target = "_blank"),style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:110%"), width = 8),
                          br(),
                          br(),
                          fluidRow(column(width = 12, 
                                          tags$head(
                                            tags$style(HTML("  .panel-heading {
                                         padding: 4px 20px;
                                         border-bottom: 1px solid transparent;
                                         .border-top-radius((@panel-border-radius - 1));
                                         
                                         > .dropdown .dropdown-toggle {
                                           color: inherit;
                                         }
                                       }"))),
                            bsCollapse(id = 'textpanels', multiple = FALSE,
                                    
                            bsCollapsePanel(title = p('>  What is South Pacific TurtleWatch?', style = "padding:4px;background-size:200%;font-weight:bold;margin-top:0px;margin-bottom:0px;font-size:95%;border-radius:2px"), p('South Pacific TurtleWatch is a collaborative effort to understand the habitat utilization of adult Eastern Pacific leatherbacks to better management and conservation goals of this highly migratory species. This tool is updated monthly, offering stakeholders and the public near real-time estimates of leatherback movements. It offers the opportunity for dynamic ocean management, management that changes with time and space; people and animals utilize given areas differently as their surrounding environment changes, but most management areas are static and cannot take into consideration the frequent movements of highly migratory species. We have multiple models under development, which examine and predict the movements of these critically endangered marine megafauna, each offering a unique perspective of leatherback species distribution based on available information. The current model shown below is based on data from satellite-tagged leatherbacks. It predicts the amount of time leatherbacks are expected to spend in a given area based on environmental factors - factors that play a role in their movements. Darker colors indicate leatherbacks would move more quickly through a region, while lighter colors indicate slower movements, prolonging their time spent in an area. The enviromental components can be viewed in the second tab in the top navigation bar. A leatherback habitat utilization developed using fisheries observations, sightings or interactions from fishing vessels, will eventually be added. More information can be found in ', a(em("Using fisheries observation data to develop a predictive species distribution model for endangered sea turtles"), href = "https://doi.org/10.1111/csp2.349", target = "_blank", .noWS = "outside"), ".")),
                                                                 bsCollapsePanel(title = p('>  Leatherback Tagging', style = "padding:4px;background-size:200%;font-weight:bold;margin-top:0px;margin-bottom:0px;font-size:95%;border-radius:2px"), p('Eastern Pacific leatherbacks were satellite-tagged with between 2003 and 2014. This telemetry, or remotely-sensed data, is pivotal in understanding when and where leatherbacks move because the ocean is a vast area, far from other means of observation. Satellite tags provided an average of half a year of leatherback movement, with the longest track spanning nearly 1.5 years. Leatherback tracks that went into the model were based on daily location estimates from these tags. We only included periods when leatherbacks were not breeding. Because they behave much differently when breeding, our prediction estimates presented here do not fully capture slow-moving coastal leatherbacks during nesting times (approximately October - March).')),
                                                                 bsCollapsePanel(title = p('>  Costa Rica Dome [Ecologically or Biologically Significant Marine Area (EBSA)]', style = "padding:4px;background-size:200%;font-weight:bold;margin-top:0px;margin-bottom:0px;font-size:95%"), p('The Costa Rica Dome is an important habitat area - a biological hotspot - for many marine species, such as fisheries-important tuna and blue whales that breed and calve in the area, because upwelling brings cold, nutrient-rich waters to the Dome. It forms a migratory corridor for leatherbacks leaving Costa Rican nesting beaches. The female leatherbacks departing these Costa Rican nesting beaches are critical to the survival of the species, and thus, this area should be avoided when leatherbacks are more likely to be using this corridor. It is important to note the Costa Rica Dome on the map (light blue area) is an average position of the Costa Rica Dome throughout a given year. It is not a stationary feature; each year it strengthens and moves offshore as it grows, beginning near the coast in February, building and moving offshore around the middle of the year, and disappearing around December before the yearly cycle begins again.')))))),
                            br(),
                            hr(),
                        fluidRow(column(width = 2),
                          column(width = 8,
                            h2(p(predictmonth, predictyear, style = "color:#a2b03a;text-align:center; font-size: 125%; font-weight: bold")))),
                            br(),
                         
               sidebarLayout(
                 sidebarPanel(
                   p("Global Fishing Watch (GFW) Fishing Data", style = "text-align:center"), style = "border:white; background-color:lavendar;font-size:115%;font-color:#3c4b57;padding:15px;padding-top:10px", width = 2,
                   hr(style = "border-color:#cd6ebe;opacity:0.8;margin-top:10px;margin-bottom:20px;"),
                   tags$head(
                     tags$style(HTML(
                       ".checkbox {margin:0;text-align:left}
                        .checkbox p {margin:0;text-align:left}
                        .shiny-input-container {margin-bottom:0}
                        .control-label {text-align:center;margin-bottom:0}
                        .label {text-align:center}
                        .shiny-input-container {text-align:center}"))),
                   selectInput(inputId = "Fisheries", label = "Yearly Fishing Effort (hr/km^2)", choices = c("2016" = 5, "2015" = 4, "2014" = 3, "2013" = 2, "2012" = 1), selected = "2016"),
                   checkboxInput("PlotFisheries", "Add Fishing Effort", FALSE),
                   hr(style = "border-color:#cd6ebe;opacity:0.2;margin-top:10px;margin-bottom:10px;"),
                   checkboxInput("ChangePlotRange", "Add Fishing Effort > 0.1", FALSE),
                   selectInput("colors", "Change Color Scale", choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "YlOrRd"),
                   hr(style="margin-top:15px;margin-bottom:15px;border-color: #cd6ebe;opacity:0.4;border-top: #cd6ebe dashed 1.5px;"),
                   selectInput(inputId = "Fishinggear", label = "2016 Fishing Effort by Gear (hr/km^2)", choices = c("Trawlers" = 6, "Drifting longlines" = 1, "Fixed gear" = 2, "Other fishing" = 3, "Purse seines" = 4, "Squid jigger" = 5), selected = "Trawlers"),
                   checkboxInput("PlotGear", "Plot 2016 Fishing Effort by Gear", FALSE),
                   tags$head(tags$style(type = "text/css", ".shiny-input-container {margin-bottom: 0px;}",".checkbox {margin-bottom: 0px;}")),
                   p("(May be slow to plot)", style = "font-size:75%;font-color:#757575;padding:0.1px;text-align:center"),
                   div(style = "display:inline-block;width:10%;margin-left:5%;max-width:100%;",
                       actionButton(inputId = "hideFisheries",
                         label = HTML("Clear <br/>Fishing <br/>Effort"), style = "font-weight:bold;text-align:center;font-size:105%;color:#a2b03a;padding:15px;border:2px;box-shadow: 0 0 11px 2px #a2b03a;/* box-shadow: 0 0 black; */box-shadow: 4px 4px 20px 4px #a2b03a")),
                   br(),
                   br(),
                   sliderInput("opacity", HTML("Residence Time Opacity <br/>(No Map &#8596 Visible Map)"), min = 0, max = 1, value = 0.7, step = 0.1),
                   hr(style="border-color:#cd6ebe;opacity:0.2"),
                   p("GFW data are based on vessel AIS, thus representing a minimum estimate of fishing occurring in these areas (e.g. dependent on satellite coverage and AIS usage). More information can be found below.", style = "font-size:65%;font-color:#3c4b57;padding:5px")
                   ), 
                  mainPanel(width = 10,
                    fluidRow(
                      column(p('Eastern Pacific leatherback predictions for ', predictmonth,predictyear), withSpinner(leafletOutput("prediction", height = '625px'), type = 6, color = "#a2b03a", size = 0.8, hide.ui = FALSE, proxy.height = '625px'), width = 10, absolutePanel(draggable = T,top = 0, left = 0, right = 0, tags$div(h2(style="text-align:center;color:#FF828C;padding:0px;background-color:rgba(180,180,180,0.3);margin-right:20px;margin-left:10px", tags$b(tags$em("::Under Construction - Experimental Product::")))))),
                      br(),
                      column(width = 2, 
                             a("Download South Pacific TurtleWatch Data", href = "https://github.com/AHoover/SPTW_telemetry_app/", target = "_blank"), style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:110%",
                      br(),
                      
                      ),
                    ),
                    fluidRow(
                      br(),
                      column(p("Leatherback Residence Time (Days) along the Eastern Pacific with Exclusive Economic Zones (shown in gray) and Costa Rica Dome important leatherback habitat (shown in light blue)", style = "text-align:center;color:#A2B03A;padding:2px;font-size:105%"), width = 10)),
                      br(),
                      br(),
                      fluidRow(column(p("Satellite data for this model were obtained from NASA, NOAA, including the ", a("ERDDAP interface", href = "https://coastwatch.pfeg.noaa.gov/erddap", target = "_blank", .noWS = "outside"), ", and the E.U. Copernicus Marine Service Information. Mapped EEZ data were obtained from marineregions.org (Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at ", a("https://www.marineregions.org/", href = "https://www.marineregions.org/", target = "_blank", .noWS = "outside"), ". ", a("https://doi.org/10.14284/386", href = "https://doi.org/10.14284/386", target = "_blank", .noWS = "outside"), "). Data for Ecologically or Biologically Significant Marine Areas (EBSAs) were obtained from chm.cbd.int (CBD (2021). Ecologically or biologically significant marine areas. Available online at ", a("https://www.cbd.int/ebsa/", href = "https://www.cbd.int/ebsa/", target = "_blank", .noWS = "outside"), "). Data for Marine Protected Areas (MPAs) were obtained from protectedplanet.net (UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], March 2021, Cambridge, UK: UNEP-WCMC and IUCN. Available at: ",a("https://www.protectedplanet.net/", href = "https://www.protectedplanet.net/", target = "_blank", .noWS = "outside"), "). For Global Fishing Watch data, cells were resampled from high-resolution 0.01 degree data to 0.1 degree cells, summing data across each cell to obtain total coverage for all fisheries. Cells in which fishing effort was zero indicate vessels were present, but they were determined not to be actively fishing. Areas without values did not have vessels present. Global Fishing Watch data were obtained from globalfishingwatch.org (Global Fishing Watch (2020), Global Fishing Watch map and data. Available online at ",a("https://globalfishingwatch.org/", href = "https://globalfishingwatch.org/", target = "_blank", .noWS = "outside"), "). More information and further data can be found there or in ", a("Tracking the global footprint of fisheries", href = "https://science.sciencemag.org/content/359/6378/904", target = "_blank", .noWS = "outside"), ".", style = "text-align:justify;color:white;background-color:gray;padding:15px;border-radius:10px", .noWS = c("after-begin", "before-end")),
                    width = 12)),
                    )
                ),
               br(),
               br(),
               ),
      
      tabPanel('Environmental Variables',

               fluidRow(column(width = 2),
                        column(
                          br(),
                          p("Satellite data are incorporated into the model framework that predicts leatherback movement in the Eastern Pacific region; these data include sea surface temperature (SST), front density index (Front Density), sea surface height (SSH), and bathymetry", style = "color:black;text-align:center"), style = "text-align:justify;color:black;background-color:lavender;padding:10px;border-radius:10px;font-size: 120%", width = 8)),
                br(),
                hr(),
                h2(p('Monthly environmental data for', style = "color:black;text-align:center; font-size: 125%; font-weight: bold")), h2(p(predictmonth,predictyear, style = "color:#a2b03a;text-align:center; padding: 0px;font-size: 125%; font-weight: bold")),
                br(),

              tags$style(HTML("
             .tabbable > .nav > li > a {background-color: lavender;  border: 2px; color:#a2b03a; font-weight: bold;font-size: 14px}
             .tabbable > .nav > li[class=active] > a {
              background-color: #3c4b57;
              border-color: #a2b03a;
              color: #46b1e6;
              font-size: 16px;
              }")),

            tabsetPanel(
                tabPanel('SST',
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                              div(img(src = paste0("sst_05deg_",year,"_",month,".jpg"), height = '600px', width = '800px'),style = "text-align: center"), 
                                    column(p("Sea Surface Temperature (\u00B0C) along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:center;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("Front Density",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                  div(img(src = paste0("fpi_05deg_", file_dater, ".jpg"), align='center', height = '600px', width = '800px'), style = "text-align: center"),
                                      column(p("Front Density along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:center;color:#A2B03A;padding:2px;font-size: 105%"), width=12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("SSH",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                  div(img(src = paste0("ssh_05deg_",year,"_",month,".jpg"), align='center', height = '600px', width = '800px'), style="text-align: center"),
                                      column(p("Sea Surface Height (SSH) (m) (absolute height) along the Eastern Pacific with Exclusive Economic Zones (shown in gray); data generated using E.U. Copernicus Marine Service Information", style="text-align:center;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("Bathymetry",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                div(img(src = "bathy_05deg.jpg", align = 'center', height = '600px', width = '800px'), style = "text-align: center"),
                                       column(p("Bathymetry (m) along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:center;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
                                    width = 12),
                    br(),
                    ))

                  )
      )
    ),
  div(class = "footer",
    includeHTML("www/mapfooter.html") # Add footer across all pages
  )
)

########


########
# Server

## Create prediction map using Leaflet that includes the EEZ of each country in the Eastern Pacific area of interest
### Imperfect country map: only simple polygons, not exact coastline match
### Memory issues with shinyapps.io server version -- data-heavy items have been replaced with still images 

server <- shinyServer(function(input,output,session) {
  
  output$prediction <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
    addRasterImage(predictraster, colors = palpredict, opacity = 0.7, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "predictionmap") %>% # Use FALSE if error in palette occurs
    addLegend(pal = palpredict,values = values(predictraster), title = "Residence <br>Time (Days)") %>%
      addMapPane("country", zIndex = 400) %>% 
      addMapPane("EBSAs", zIndex = 420) %>% 
      addMapPane("EBSAs_small", zIndex = 430) %>% 
      addMapPane("EEZ", zIndex = 410) %>% 
      addMapPane("MPAs", zIndex = 430) %>%
      addPolygons(data = ebsas, weight=1.5, label = ~NAME, fillOpacity = 0.3, color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs"), group = "EBSAs") %>% 
      addPolygons(data = ebsas_small, weight = 1.5,label=~NAME, fillOpacity = 0.3, color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "EBSAs") %>% 
      addPolygons(data = CRD, weight = 1.5,label=~NAME, fillOpacity = 0.5, color = "#46b1e6", highlightOptions = highlightOptions(color = "white", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "Costa Rica Dome <br>EBSA") %>% #176302 #cf5a0c
      addPolygons(data = SPshpallsubset, weight = 1.5,  opacity = 0.6, fillOpacity = 0.4, label = ~geoname, color = "#3c4b57", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "EEZ"), group = "EEZs") %>%
      addPolygons(data = allmpas, weight = 1.5, opacity = 0.8, fillOpacity = 0.3, label=~LABEL, color = "goldenrod", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "MPAs"), group = "MPAs and Other <br>Protected Areas") %>%
      addPolygons(data = countriessubset, weight = 1.5, label = ~NAME, fillOpacity = 0.4, color = "#a2b03a", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3, bringToFront = TRUE), options = pathOptions(pane = "country")) %>% 
      setView(-105, -8, zoom = 3) %>% 
      # Add layer controls
      addLayersControl(
        #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("EBSAs", "Costa Rica Dome <br>EBSA", "MPAs and Other <br>Protected Areas", "EEZs"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("EBSAs","MPAs and Other <br>Protected Areas")) %>% 
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center;font-weight:bold;font-size:110%;\">Areas of Interest</label>');
        }
    ")
  })

  filteredmap <- reactive({

    fisheries[[as.numeric(input$Fisheries)]]

  })
  
  filteredgear <- reactive({

     gear2016[[as.numeric(input$Fishinggear)]]

   })
  

  proxy <- leafletProxy("prediction")

  observeEvent(input$opacity, {
    proxy %>% clearGroup(group = "predictionmap") %>%
      addRasterImage(predictraster, colors = palpredict, opacity = input$opacity, maxBytes=40 * 1024 * 1024, group = "predictionmap")
  })
  
  observeEvent(input$PlotFisheries, {

    setbins <- c(0, 0.1, 0.5, 1, 2.5, 5, 10, 15, 20, 22)
    colorpal <- reactive({colorBin(input$colors, values(filteredmap()), bins = setbins, na.color = "transparent")})

    #Always clear the group first on the observed event
    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend") # Removes legend on click

    if(input$PlotFisheries){
      observe({
        pal <- colorpal()
        proxy %>%
          removeControl(legend) %>% # Removes legend on year change
          addRasterImage(filteredmap(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group") %>%
          addLegend(pal = pal, values = values(filteredmap()), title = paste("Fishing Effort <br> ", parse_number(names(filteredmap()))), group = "Fisheries.group", layer = "Fisherieslegend")})
    }
  })

  observeEvent(input$ChangePlotRange, {

    setbins <- c(0.1, 0.5, 1, 2.5, 5, 10, 15, 20, 22);
    colorpal <- reactive({colorBin(input$colors, values(filteredmap()), bins = setbins, na.color = "transparent")})

    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend")
    
    if(input$ChangePlotRange){
      observe({
        pal <- colorpal()
        proxy %>%
          removeControl(legend) %>% # Removes legend on year change
          addRasterImage(filteredmap(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group") %>%
          addLegend(pal = pal, values = values(filteredmap()),title = paste("Fishing Effort >= 0.1 <br> ", parse_number(names(filteredmap()))), group = "Fisheries.group", layer = "Fisherieslegend")})
    }
  })
  
  observeEvent(input$PlotGear, {
    
    setbins <- c(0, 0.1, 0.5, 1, 2.5, 5, 10, 15)
    paletterev <- rev(viridis(6))
    colorpal <- reactive({colorBin(paletterev, values(filteredgear()), bins = setbins, na.color = "transparent")})
    
    #Always clear the group first on the observed event
    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend") # Removes legend on click
    
    if(input$PlotGear){
      observe({
        pal <- colorpal()
        proxy %>%
          removeControl(legend) %>% # Removes legend on year change
          addRasterImage(filteredgear(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group") %>%
          addLegend(pal = pal, values = values(filteredgear()), title = paste("2016 Fishing Effort <br> ",sub("_"," ", names(filteredgear()))), group = "Fisheries.group", layer = "Fisherieslegend")})
    }
  })

  observeEvent(input$hideFisheries, {
    leafletProxy("prediction") %>% removeShape("foo") %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend")

    updateCheckboxInput(session = session, inputId = "PlotFisheries", value = is.null(input$PlotFisheries))
    updateCheckboxInput(session = session, inputId = "ChangePlotRange", value = is.null(input$ChangePlotRange))
    updateCheckboxInput(session=session, inputId="PlotGear", value = is.null(input$PlotGear))
  })


})

########


########
# Run Application

shinyApp(ui = ui, server = server)

########END
