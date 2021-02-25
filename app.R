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

library(rsconnect);library(shiny);library(leaflet);library(viridis);library(stringr);library(raster);library(maptools);library(rgdal);library(rgeos);library(tidyverse)

########


########
# Load Data

## Define year and month of interest for prediction

source('data/define_year_month.R')
predictyear <<- '2021'
predictmonth <<- 'January'

define_year_month(predictyear, str_pad(match(predictmonth,month.name), 2, pad='0'))
file_dater <- paste0(year,"_",month)

## Load Prediction; set color palette

filename_predict <- paste0("data/Prediction_", tolower(month.abb[as.numeric(month)]), year, "_0.5deg.tif")

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
    "))
  ),
  titlePanel(h1("South Pacific TurtleWatch Model"), windowTitle = "SPTW Telemetry Model"),
    navbarPage(title = div(img(src = 'upwell_green_gray.png', style="margin-top:0px;padding-left:4px;padding-bottom:10px;padding-top:2px", height = 55),"Eastern Pacific Leatherback Movement", style = "margin-top:-13px"),
                 
               tabPanel('Prediction Map',
                        
                        fluidRow(column(width = 2),
                          column(
                            br(),
                            p(h2('Predicting Eastern Pacific Leatherback Movement Using Telemetry Data',style = "color:black;background-color:lavender;padding:15px;border-radius:10px;font-weight: bold")),
                            br(),
                                   p("South Pacific TurtleWatch uses methods based upon",
                                     a(href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644", em("Predicting residence time using a continuous‐time discrete‐space model of leatherback turtle satellite telemetry data"),target = "_blank"),style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 110%"),width = 8)),
                            br(),
                            hr(),
                            h2(p(predictmonth,predictyear,style = "color:#a2b03a;text-align:center; font-size: 125%; font-weight: bold")),
                            br(),
  
               sidebarLayout(
                 sidebarPanel(
                   a("Download SPTW Data",href = "https://github.com/AHoover/SPTW_telemetry_app/"),
                   style = "padding:15px;border:white;background-color:lavender", width = 2), 
                  mainPanel(
                    fluidRow(
                      column(p('Eastern Pacific leatherback predictions for ', predictmonth,predictyear), leafletOutput("prediction", height = '600px'),width = 12,absolutePanel(draggable = T,top = 0, left = 0, right = 0, tags$div(h2(style="text-align:center;color:#FF828C;padding:0px;background-color:rgba(180,180,180,0.3);border-radius:0px", tags$b(tags$em("::Under Construction - Experimental Product::"))))))),
                    fluidRow(
                      br(),
                      column(p("Leatherback Residence Time (Days) along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:center;color:#A2B03A;padding:2px;font-size: 105%"), width = 12)),
                      )
                ),
               br(),
               br(),
                fluidRow(column(width = 2), column(p("Satellite data for this model were obtained from NASA, NOAA, including the ", a("ERDDAP interface", href = "https://coastwatch.pfeg.noaa.gov/erddap", .noWS = "outside"), ", and the E.U. Copernicus Marine Service Information. Mapped EEZ data were obtained from marineregions.org (Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at ", a("https://www.marineregions.org/", href = "https://www.marineregions.org/", .noWS = "outside"), ". ", a("https://doi.org/10.14284/386", href = "https://doi.org/10.14284/386", .noWS = "outside"), "). Data for Ecologically or Biologically Significant Marine Areas (EBSAs) were obtained from chm.cbd.int (CBD (2021). Ecologically or biologically significant marine areas. Available online at ", a("https://www.cbd.int/ebsa/", href = "https://www.cbd.int/ebsa/", .noWS = "outside"), ").", style = "text-align:justify;color:white;background-color:gray;padding:15px;border-radius:10px", .noWS = c("after-begin", "before-end")),
                    width = 8)),
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
                                  div(img(src = paste0("ssh_05deg_",year,"_",month,".jpg"),align='center', height = '600px', width = '800px'), style="text-align: center"),
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
    addRasterImage(predictraster, colors = palpredict, opacity = 0.8, maxBytes = 40 * 1024 * 1024, project = TRUE) %>% # Use FALSE if error in palette occurs
    addLegend(pal = palpredict,values = values(predictraster), title = "Residence <br>Time (Days)") %>%
      addMapPane("country", zIndex = 400) %>% 
      addMapPane("EBSAs", zIndex = 420) %>% 
      addMapPane("EBSAs_small", zIndex = 430) %>% 
      addMapPane("EEZ", zIndex = 410) %>% 
      addMapPane("MPAs", zIndex = 430) %>%
      addPolygons(data = ebsas, weight=1.5,label=~NAME, fillOpacity = 0.3,color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5,bringToFront = TRUE), options = pathOptions(pane = "EBSAs"), group = "EBSAs") %>% 
      addPolygons(data = ebsas_small, weight=1.5,label=~NAME, fillOpacity = 0.3,color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "EBSAs") %>% 
      addPolygons(data = CRD, weight=1.5,label=~NAME, fillOpacity = 0.5,color = "#46b1e6", highlightOptions = highlightOptions(color = "white", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "Costa Rica Dome <br>EBSA") %>% #176302 #cf5a0c
      addPolygons(data = SPshpallsubset, weight=1.5,  opacity = 0.8, fillOpacity = 0.5, label=~geoname, color = "#3c4b57", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "EEZ"), group = "EEZs") %>%
      addPolygons(data = allmpas, weight=1.5,  opacity = 0.8, fillOpacity = 0.3, label=~LABEL, color = "goldenrod", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "MPAs"), group = "MPAs and Other <br>Protected Areas") %>%
      addPolygons(data = countriessubset, weight=1.5, label=~NAME, fillOpacity = 0.4, color = "#a2b03a", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3, bringToFront = TRUE), options = pathOptions(pane = "country")) %>% 
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

})

########


########
# Run Application

shinyApp(ui = ui, server = server)

########END
