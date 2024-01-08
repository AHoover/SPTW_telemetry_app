## R Shiny Application for South Pacific TurtleWatch Telemetry Model Prediction Map Web Hosting
### author: "Aimee L Hoover"
### date: "Winter 2020/2021"
### subtitle: "Predicting Eastern Pacific Leatherback Movement Using Telemetry Data based upon [Predicting residence time using a continuous-time discrete-space model of leatherback turtle satellite telemetry data](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644)" 

########
# Set R Options
# Load Libraries

options("rgdal_show_exportToProj4_warnings" = "none")
options(curl_interrupt = FALSE)
options(repos = c(CRAN = "https://cran.rstudio.com/")) # Set for running batch deployment
httr::config(connecttimeout = 60)

library(rsconnect);library(shiny);library(leaflet);library(viridis);library(stringr);library(raster);library(tidyverse);library(RColorBrewer);library(shinycssloaders);library(shinyBS);library(htmlwidgets);library(shinyscreenshot);library(shinybusy);library(waiter);library(slickR);library(sf);library(terra)

# Set a loading screen theme

waiting_screen <- tagList(
  spin_loaders(id = 39, color="#a2b03a"),
  span(strong(em("Loading South Pacific TurtleWatch...")), style="color:#a2b03a;font-size:120%;padding-left:22px"),
tags$style(
  ".waiter-overlay-content{
    position: absolute;
    top: 50px; /*50 pixels from the top*/
    left: 38%; /*48% from the left*/
  }
  .loaderz-39 {
    width: 0.75em;
    height: 0.75em;}
  .loaderz-39:before {
    width: 1.5em;
    height: 5em;}
  .loaderz-39:after {height: 1.5em;
    width: 5em;}
  "
)
) 

########


########
# Load Data

## Define year and month of interest for prediction

# Extract month (most-recent month with data available) and year of interest

currentmonth <- as.numeric(format(Sys.Date(), format="%m"))
message(paste("Current month is", currentmonth))

if(currentmonth > 1){
  previousmonth <- currentmonth - 1
} else{
  previousmonth <- 12
}

predictmonth <<- month.name[previousmonth]

month <- str_pad(match(predictmonth,month.name), 2, pad='0')
message(paste("Prediction month is", predictmonth, "and month is", month))

# Extract year of interest

currentyear <- as.numeric(format(Sys.Date(), format="%Y"))
message(paste("Current year is", currentyear))

if(currentmonth == 1){
  predictyear <- currentyear - 1
} else{
  predictyear <- currentyear
}

year <- predictyear
file_dater <- paste0(year,"_",month)

## Load Prediction; set color palette
### Confirm data in folder is up-to-date for the most recent month.
#### This is to fix an error for early in each month where the previous month's data are not yet available to update predictions. 

tryCatch(
  
  exp = {
    
    predictintensity <- suppressWarnings(rast(paste0('data/intensity_',year,month,'.tif')))
    predictspat <- suppressWarnings(terra::rast(read_rds(paste0("data/Prediction_", month.abb[as.numeric(month)], year, "_0.1deg_spat.rds")[1])))
    
  },
  
   # If there is an error, search for file in data folder instead. This could be done from the start, if preferable. 
  
  error = function(e){
    
    year <- str_extract(list.files('data/', pattern = 'Prediction'),'([0-9])+')[1]
    month <- str_pad(match(str_extract(list.files('data/', pattern = 'Prediction'), paste(month.abb, collapse="|")), month.abb), 2, pad='0')[1]
    file_dater <- paste0(year,"_", month)
    
    # Reset prediction year and month for later code 
    
    predictyear <- year
    predictmonth <- month.name[as.numeric(month)]
    message(paste("Prediction month is", predictmonth, "and month is", month))
    message(paste("Most recent month's data unavailable."))
    
    predictspat <- suppressWarnings(terra::rast(read_rds(paste0("data/Prediction_", month.abb[as.numeric(month)], year, "_0.1deg_spat.rds")[1])))
    predictintensity <- suppressWarnings(terra::rast(paste0('data/intensity_', year, month,'.tif')))
    
  }
)

## For the purposes of the presentation, create a higher resolution map
predictintensity = disagg(predictintensity, fact = 5)

### Create color palette for intensity prediction raster, which will be on the original plot
palpredictint <- colorNumeric("viridis", terra::values(predictintensity), reverse=FALSE, na.color = "transparent")
palpredictint <- colorBin('viridis', terra::values(predictintensity), bins = c(-999,0.17,0.19,0.21,0.23,0.26,999), na.color = "transparent")

# Create labels for legend
labeller <- function(type, breaks) {
  return(c("0 to 0.17","0.17 to 0.19","0.19 to 0.21",
           "0.21 to 0.23","0.23 to 0.26", "0.26 above"))
}

palpredict <- colorNumeric("magma", terra::values(predictspat), reverse=FALSE, na.color = "transparent")

# Change projection of raster to match that required for addRasterImage
## 'addRasterImage currently works only with EPSG:3857 Web Mercator)'
predictintensity = project(predictintensity, 'EPSG:3857')

## Load country and EEZ map data
### Subsets were created from marineregions.org

PacificEEZ <- read_rds("data/PacificEEZs.rds") 
# terra::crs(predictspat, describe=TRUE, proj=TRUE) = raster::crs(PacificEEZ) # WKT compliant to remove errors

countriessubset <- read_rds("data/countriessubset.rds") 

## Load Center on Biological Diversity's 
## Ecologically or Biologically Significant Marine Areas (EBSAs)

EBSA_all <- read_rds("data/EBSAs_CBD.rds") 

### Subset to allow for desired mapping 

ebsas <- EBSA_all[-which(EBSA_all$NAME == 'Costa Rica Dome'|EBSA_all$NAME == 'Galapagos'|EBSA_all$NAME == 'Peru Upwelling Cores'|EBSA_all$NAME == 'Cordillera Malpelo'),]

ebsas_small <- EBSA_all[which(EBSA_all$NAME == 'Galapagos'|EBSA_all$NAME == 'Peru Upwelling Cores'|EBSA_all$NAME == 'Cordillera Malpelo'),] # These need to be separated in order to plot above others, allowing for the user to interact with the zones

CRD <- EBSA_all[which(EBSA_all$NAME == 'Costa Rica Dome'),] # Add a special layer for the Costa Rica Dome

## Load Marine Protected Areas and other protected areas

allmpas <- read_rds("data/MPAs_WDPA_simplified.rds")

## Load GFW Fishing Effort at 0.1 degrees for 2017-2020

fisheries <- rast("data/Effort_Fishing_01deg_2017-2020.tif")
names(fisheries) <- paste0(c("Effort2017", "Effort2018", "Effort2019", "Effort2020"))

## Load GFW Fishing Effort by Gear for 2020

gear2020 <- rast("data/Effort_FishingByGear_01deg_2020.tif")
names(gear2020) <- c("Other_fishing_gears", "Squid_jigger", "Trawlers", "Set_longlines", "Other_purse_seines", "Tuna_purse_seines", "Drifting_longlines", "Purse_seines", "Pole_and_line", "Set_gillnets", "Trollers")

## Load Hidden Markov Model Relative Risk of Interaction Analysis and analysis area
### g1 = drifting longlines, g2 = fishing, g3 = purse seines, g4 = pole and line, g5 = set gillnets, g6 = set longlines, g7 = squid jiggers, g8 = trawlers, g9 = tuna purse seines

load(paste0("data/risk_g1-9_s123_", month, ".rda"))

# Replace first instance of NA rasters to a low value to prevent leaflet from crashing when selecting gears that do not have any data
for(i in 1:dim(riskstack)[3]){
  if(is.na(terra::values(riskstack[[i]]))[1] == TRUE){
    riskstack[[i]][1] = 0
  }
}

maprisk = readRDS("data/fisheries_risk_area.rds")

### Load personalized color palette for HMM results

source("data/sptw_brewer_pal.R")

reducedcolors = c('YlOrBr', "YlOrRd", "RdBu", "Oranges", "Purples", "Reds")
colorlist.risk = brewer.pal.info[row.names(brewer.pal.info) %in% reducedcolors,]

## Load Archived Images

# imgs <- list.files("./archive", pattern=".jpg", full.names = TRUE)

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
    /*.slick-track { variableWidth: true;*/
    /*max-width: 100%; */
    /*display:block}*/
    /*.slick-slide img{min-width: 100%;*/
    /*height: auto;*/
    /*}*/
    
    "))
  ),
 useWaiter(),
 waiterPreloader(html = waiting_screen, color = "#3c4b57"),
 
  titlePanel(h1("South Pacific TurtleWatch Model"), windowTitle = "SPTW Telemetry Model"),
    navbarPage(title = div(img(src = "upwell_green_gray.png", style="margin-top:0px;padding-left:4px;padding-bottom:10px;padding-top:2px", height = 55), "Eastern Pacific Leatherback Movement", style = "margin-top:-13px"),
               id = "maptabs", 
               
               tabPanel("Prediction Map",
                        fluidRow(column(width = 2),
                                 column(
                                   br(),
                                   p(h2("Predicting Eastern Pacific Leatherback Movement Using Telemetry and Fisheries Observation Data", style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-weight:bold")),
                                   p("South Pacific TurtleWatch uses methods based upon",
                                     a(class = "two", href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644", target = "_blank", em("Predicting residence time using a continuous‐time discrete‐space model of leatherback turtle satellite telemetry data"), target = "_blank"),'and', a(class = "two", href = "https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecs2.4375", target = "_blank", em("Integrating telemetry and point observations to inform management and conservation of migratory marine species"), target = "_blank"), style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:110%"), width = 8),
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
                                       }
                                        .panel-title>a:link{
                                        padding:4px;background-size:200%;font-weight:bold;margin-top:0px;margin-bottom:0px;font-size:95%;text-align:left
                                                                   }"))),
                                                 suppressWarnings(bsCollapse(id = 'textpanels', multiple = FALSE,
                                                            
                                                            bsCollapsePanel('>  What is South Pacific TurtleWatch?', p('South Pacific TurtleWatch is a collaborative effort to understand the habitat utilization of adult Eastern Pacific leatherbacks to better management and conservation goals of this highly migratory species. This tool is updated monthly, offering stakeholders and the public near real-time estimates of leatherback movements. It offers the opportunity for dynamic ocean management, management that changes with time and space; people and animals utilize given areas differently as their surrounding environment changes, but most management areas are static and cannot take into consideration the frequent movements of highly migratory species. We have multiple models under development, which examine and predict the movements of these critically endangered marine megafauna, each offering a unique perspective of leatherback species distribution based on available information. The current residency time model shown below is based on data from satellite-tagged leatherbacks alone. It predicts the amount of time leatherbacks are expected to spend in a given area based on environmental factors - factors that play a role in their movements. Darker colors indicate leatherbacks would move more quickly through a region, while lighter colors indicate slower movements, prolonging their time spent in an area. The enviromental components can be viewed in the second tab in the top navigation bar. The leatherback intensity model combines these data from satellite-tagged leatherbacks with fisheries observations to estimate leatherback intensity (\u03BB) (density of locations per unit area) based on monthly environmental conditions. A leatherback habitat utilization map developed using fisheries observations, sightings or interactions from fishing vessels, may eventually be added. More information can be found in ', a(class = "two", em("Using fisheries observation data to develop a predictive species distribution model for endangered sea turtles"), href = "https://doi.org/10.1111/csp2.349", target = "_blank", .noWS = "outside"), ".")),
                                                            bsCollapsePanel('>  Leatherback Tagging', p('Eastern Pacific leatherbacks were satellite-tagged with between 2003 and 2014. This telemetry, or remotely-sensed, data are pivotal in understanding when and where leatherbacks move because the ocean is a vast area, far from other means of observation. Satellite tags provided an average of half a year of leatherback movement, with the longest track spanning nearly 1.5 years. Leatherback tracks that went into the model were based on daily location estimates from these tags. We only included periods when leatherbacks were not breeding. Because they behave much differently when breeding, our prediction estimates presented here do not fully capture slow-moving coastal leatherbacks during nesting times (approximately October - March).')))))),
                        br(),
                        hr(),
                        fluidRow(column(width = 2),
                                 column(width = 8,
                                        h2(p(predictmonth, predictyear, style = "color:#a2b03a; text-align:center; font-size: 125%; font-weight: bold")))),
                        fluidRow(
                                  br(),
                                  column(width = 2, 
                                        a(class = "two", "Download South Pacific TurtleWatch Data", href = "https://github.com/AHoover/SPTW_telemetry_app/", target = "_blank"), style = "text-align:center; color:black; background-color:lavender; padding:15px;border-radius:10px;font-size:110%",
                                  br(),
                                  br(),
                                  actionButton("StaticMap", strong(HTML("Download <br/> Residency <br/>Map")), style = "color: black; background-color:white; border-color: #a2b03a; box-shadow: 5px 5px #a2b03a; border: 2px solid #a2b03a"),
                                  br(),
                                  br(),
                                  actionButton("StaticMap2", strong(HTML("Download <br/> Intensity <br/>Map")), style = "color: black; background-color:white; border-color: #a2b03a; box-shadow: 5px 5px #a2b03a; border: 2px solid #a2b03a"),
                                  br(),
                                  br(),
                                  actionButton(inputId = "moveToInteractive", label = HTML("Want a deeper dive? <br/> Try our interactive <br/> map")),
                                             
                              tags$style("#moveToInteractive {
                                color: #46b1e6; 
                                background-color:transparent;
                                border-color: lavender;
                                font-size:102%;
                                padding:0px}
                                #moveToInteractive:hover {
                                color: #ffb93f;
                                background-color: inherit;
                                font-weight: bold;
                                font-size:100%}"),
                              tags$head(tags$style(".modal-dialog{width:825px}")),
                                      ),
                              column(width = 10,
                                        div(class="img-fluid img-thumbnail", img(src = paste0("Prediction_", month.abb[as.numeric(month)], year, "_0.1deg.jpg"), height = 'auto', width = '100%',style ="display:block")), p("Leatherback Residence Time (Days) along the Eastern Pacific", style = "color:#A2B03A;padding:2px;padding-left:20px;margin-top:10px;font-size:105%;margin-top:0px;text-align:left"),
                                     br(),
                                     div(class="img-fluid img-thumbnail", img(src = paste0("Intensity_prediction_", month.abb[as.numeric(month)], year, ".jpg"), height = 'auto', width = '100%',style ="display:block")), p("Leatherback Intensity (\u03BB) along the Eastern Pacific", style = "color:#A2B03A;padding:2px;padding-left:20px;margin-top:10px;font-size:105%;margin-top:0px;text-align:left"),
                                        ),
                                    ),
                                    br(),
                                    br(),
                                    fluidRow(column(width = 2),
                                             column(p("Satellite data for this model were obtained from NASA, NOAA, including the ", a(class = "two", "ERDDAP interface", href = "https://coastwatch.pfeg.noaa.gov/erddap", target = "_blank", .noWS = "outside"), ", and the E.U. Copernicus Marine Service Information. Mapped EEZ data were obtained from marineregions.org (Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at ", a(class = "two", "https://www.marineregions.org/", href = "https://www.marineregions.org/", target = "_blank", .noWS = "outside"), ". ", a(class = "two", "https://doi.org/10.14284/386", href = "https://doi.org/10.14284/386", target = "_blank", .noWS = "outside"), "). Data for Ecologically or Biologically Significant Marine Areas (EBSAs) were obtained from chm.cbd.int (CBD (2021). Ecologically or biologically significant marine areas. Available online at ", a(class = "two", "https://www.cbd.int/ebsa/", href = "https://www.cbd.int/ebsa/", target = "_blank", .noWS = "outside"), "). Data for Marine Protected Areas (MPAs) were obtained from protectedplanet.net (UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], March 2021, Cambridge, UK: UNEP-WCMC and IUCN. Available at: ",a(class = "two", "https://www.protectedplanet.net/", href = "https://www.protectedplanet.net/", target = "_blank", .noWS = "outside"), "). For Global Fishing Watch data, cells were resampled from high-resolution 0.01 degree data to 0.1 degree cells, summing data across each cell to obtain total coverage for all fisheries. Cells in which fishing effort was zero indicate vessels were present, but they were determined not to be actively fishing. Areas without values did not have vessels present. Global Fishing Watch data were obtained from globalfishingwatch.org (Global Fishing Watch (2021), Global Fishing Watch map and data. Available online at ",a(class = "two", "https://globalfishingwatch.org/", href = "https://globalfishingwatch.org/", target = "_blank", .noWS = "outside"), "). Copyright 2022, Global Fishing Watch, Inc. Accessed on 07 June 2021. " ,a(class = "two", "https://globalfishingwatch.org/", href = "https://globalfishingwatch.org/", target = "_blank", .noWS = "outside"), ". Disclaimer: “Global Fishing Watch has made every attempt to ensure the completeness, accuracy and reliability of the information provided on this Site. However, due to the nature and inherent limitations in source materials for information provided, Global Fishing Watch qualifies all designations of vessel fishing activity, including synonyms of the term “fishing activity,” such as “fishing” or “fishing effort,” as “apparent,” rather than certain.  And accordingly, the information is provided “as is” without warranty of any kind.” More information and further data can be found there or in ", a(class = "two", "Tracking the global footprint of fisheries", href = "https://science.sciencemag.org/content/359/6378/904", target = "_blank", .noWS = "outside"), ".", style = "text-align:justify;color:white;background-color:gray;padding:15px;border-radius:10px", .noWS = c("after-begin", "before-end")),
                                                    width = 10)),
                        br(),
                        br(),
                        # fluidRow(column(width = 2),
                        #          column(width = 10,
                        #          textOutput("slickrImgName"),
                        #          slickROutput("slickr", width = '500px'))),
                        # br(),
                        # br(),
               )),
                 
               tabPanel("Interactive Map",

                        fluidRow(column(width = 2),
                                 div(class = "visible-sm-block visible-md-block visible-lg-block", 
                                     column(
                                   br(),
                                   p(h2("Predicting Eastern Pacific Leatherback Movement Using Telemetry and Fisheries Observation Data", style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-weight:bold")),
                                   p("South Pacific TurtleWatch uses methods based upon",
                                     a(class = "two", href = "https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2644", target = "_blank", em("Predicting residence time using a continuous‐time discrete‐space model of leatherback turtle satellite telemetry data"), target = "_blank"), 'and', a(class = "two", href = "https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecs2.4375", target = "_blank", em("Integrating telemetry and point observations to inform management and conservation of migratory marine species"), target = "_blank"), style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:110%"), width = 8),
                                 br(),
                                 br(),
                                 ),
                                 fluidRow(column(width = 12, 
                                                 tags$head(
                                                   tags$style(HTML("  .panel-heading {
                                         padding: 4px 20px;
                                         border-bottom: 1px solid transparent;
                                         .border-top-radius((@panel-border-radius - 1));
                                         
                                         > .dropdown .dropdown-toggle {
                                           color: inherit;
                                         }
                                       }
                                        .panel-title>a:link{
                                        padding:4px;background-size:200%;font-weight:bold;margin-top:0px;margin-bottom:0px;font-size:95%;text-align:left
                                                                   }"))),
                                                 suppressWarnings(bsCollapse(id = 'textpanels2', multiple = FALSE,
                                                            bsCollapsePanel(title = '>  Costa Rica Dome [Ecologically or Biologically Significant Marine Area (EBSA)]', p('The Costa Rica Dome is an important habitat area - a biological hotspot - for many marine species, such as fisheries-important tuna and blue whales that breed and calve in the area, because upwelling brings cold, nutrient-rich waters to the Dome. It forms a migratory corridor for leatherbacks leaving Costa Rican nesting beaches. The female leatherbacks departing these Costa Rican nesting beaches are critical to the survival of the species, and thus, this area should be avoided when leatherbacks are more likely to be using this corridor. It is important to note the Costa Rica Dome on the map (light blue area) is an average position of the Costa Rica Dome throughout a given year. It is not a stationary feature; each year it strengthens and moves offshore as it grows, beginning near the coast in February, building and moving offshore around the middle of the year, and disappearing around December before the yearly cycle begins again.')),

                                                            bsCollapsePanel('>  Relative Risk of Interaction (Fisheries)', p('Monthly relative risk of interaction is calculated for Eastern Pacific leatherbacks in different behavioral states:', HTML("<ul><li>S1 - transiting</li><li>S2 - residential/foraging</li><li>S3 - deep diving/exploratory</li></ul>"), 'These are currently provided for GFW gear types: drifting (pelagic) longline, fishing, purse seines, pole and line, set gillnets, set longlines, squid jiggers, trawlers, and tuna purse seines. Missing maps indicate no data are available for selected fishing gear behavioral state. More information on these dynamic maps can be found in Barbour et al. (2023): ', a(class = "two", em("Incorporating multidimensional behavior into a risk management tool for a critically endangered and migratory species"), href = "https://doi.org/10.1111/cobi.14114", target = "_blank", .noWS = "outside"), HTML(paste0('. <br><br>The fisheries risk zone represents the bounding box for the risk analysis. The risk analysis does not examine interaction between fisheries and leatherbacks outside this box. <br><br> Maps showing as "NA" in the legend do not have enough data to generate risk maps for the month and fishing gear of interest.')))))))),
                                 div(class = "visible-sm-block visible-md-block visible-lg-block", 
                          br(),
                                 ),
                          hr(),
                        fluidRow(column(width = 2),
                          column(width = 8,
                            h2(p(predictmonth, predictyear, style = "color:#a2b03a;text-align:center; font-size: 125%; font-weight: bold"))),
                          column(width = 2,
                                 a(class = "two", "Download South Pacific TurtleWatch Data", href = "https://github.com/AHoover/SPTW_telemetry_app/", target = "_blank"), style = "text-align:center;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:110%")),
                            br(),
                        fluidRow(column(width = 2,
                                        tags$head(
                                          tags$style(".modal-dialog{ width:825px}",
                                                     HTML(
                                            ".checkbox {margin:0;text-align:left}
                        .checkbox p {margin:0;text-align:left}
                        .radio label {margin:0;text-align:left}
                        .radio label {margin:0;text-align:left}
                        .shiny-input-container {margin-bottom:0px}
                        .control-label {text-align:center;margin-bottom:0px}
                        .label {text-align:center}
                        .shiny-input-container {text-align:-webkit-left}
                        .legend {line-height:15px}
                        .leaflet-top .leaflet-control {margin-top:3px}
                        .leaflet-bottom .leaflet-control {margin-bottom:2px}
                        .leaflet-control-layers label {margin-top: 0px; margin-bottom: 0px}
                        "))),
                                        p("Relative Risk of Interaction Between Leatherbacks and Fishing Gear", style = "text-align:center"), style = "border:white; background-color:lavender;font-size:115%;font-color:#3c4b57;padding:15px;padding-top:10px", 
                                        hr(style = "border-color:#cd6ebe;opacity:0.5;margin-top:10px;margin-bottom:20px;"),
                                        selectInput(inputId = "Riskfishinggear", label = "Fishing Gear", choices = c("Drifting longlines" = 1, "Purse seines" = 3, "Pole and line" = 4, "Set gillnets" = 5, "Set longlines" = 6, "Squid jiggers" = 7, "Trawlers" = 8, "Tuna purse seines" = 9, "Other fishing gears" = 2), selected = "Drifting longlines"),
                                        selectInput("colorsRisk", "Change Color Scale", choices = rownames(subset(colorlist.risk, category %in% c("seq", "div"))), selected = "YlOrBr"),
                                        hr(style="margin-top:15px;margin-bottom:15px;border-color: #cd6ebe;opacity:0.4;border-top: #cd6ebe dashed 1.5px;"),
                                        radioButtons("PlotRisk", HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom:10px;\">Select Behavior of Interest</label>'), choices = c('Transiting' = 1, 'Residential/ Foraging' = 2, 'Deep diving/ Exploratory' = 3), selected = FALSE),
                                        br(),
                                        actionButton(inputId = "hideFisheriesRisk",
                                                     label = HTML("Clear <br/>Risk of <br/>Interaction"), style = "font-weight:bold;text-align:center;font-size:105%;color:#FD6B31;padding:15px;border:2px;box-shadow: 0 0 11px 2px #FD6B31;/* box-shadow: 0 0 black; */box-shadow: 4px 4px 20px 4px #FD6B31"),
                                        tags$head(tags$style(type = "text/css", ".shiny-input-container {margin-bottom: 0px;}",".checkbox {margin-bottom: 0px;}")),
                        ),
                        column(width = 8, add_busy_bar(color = "#a2b03a",centered = TRUE),
                        p('Eastern Pacific leatherback predictions for ', predictmonth,predictyear), withSpinner(div(class = 'leaflet-control-zoom-in-test', leafletOutput("prediction", height = '625px'), type = 6, color = "#a2b03a", size = 1.2, hide.ui = FALSE, proxy.height = '625px')), absolutePanel(draggable = T,top = 0, left = 0, right = 0, tags$div(h2(style="text-align:center;color:#FF828C;padding:0px;background-color:rgba(180,180,180,0.3);margin-right:20px;margin-left:10px", tags$b(tags$em("::Under Construction - Experimental Product::"))))), p("Leatherback Residence Time (Days) along the Eastern Pacific with Exclusive Economic Zones (shown in gray) and Costa Rica Dome important leatherback habitat (shown in light blue)", style = "text-align:center;color:#A2B03A;padding:2px;font-size:105%"),
                        div(class = "visible-md-block visible-lg-block", br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        p("Satellite data for this model were obtained from NASA, NOAA, including the ", a(class = "two", "ERDDAP interface", href = "https://coastwatch.pfeg.noaa.gov/erddap", target = "_blank", .noWS = "outside"), ", and the E.U. Copernicus Marine Service Information. Mapped EEZ data were obtained from marineregions.org (Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at ", a(class = "two", "https://www.marineregions.org/", href = "https://www.marineregions.org/", target = "_blank", .noWS = "outside"), ". ", a(class = "two", "https://doi.org/10.14284/386", href = "https://doi.org/10.14284/386", target = "_blank", .noWS = "outside"), "). Data for Ecologically or Biologically Significant Marine Areas (EBSAs) were obtained from chm.cbd.int (CBD (2021). Ecologically or biologically significant marine areas. Available online at ", a(class = "two", "https://www.cbd.int/ebsa/", href = "https://www.cbd.int/ebsa/", target = "_blank", .noWS = "outside"), "). Data for Marine Protected Areas (MPAs) were obtained from protectedplanet.net (UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], March 2021, Cambridge, UK: UNEP-WCMC and IUCN. Available at: ",a(class = "two", "https://www.protectedplanet.net/", href = "https://www.protectedplanet.net/", target = "_blank", .noWS = "outside"), "). For Global Fishing Watch data, cells were resampled from high-resolution 0.01 degree data to 0.1 degree cells, summing data across each cell to obtain total coverage for all fisheries. Cells in which fishing effort was zero indicate vessels were present, but they were determined not to be actively fishing. Areas without values did not have vessels present. Global Fishing Watch data were obtained from globalfishingwatch.org (Global Fishing Watch (2020), Global Fishing Watch map and data. Available online at ",a(class = "two", "https://globalfishingwatch.org/", href = "https://globalfishingwatch.org/", target = "_blank", .noWS = "outside"), "). More information and further data can be found there or in ", a(class = "two", "Tracking the global footprint of fisheries", href = "https://science.sciencemag.org/content/359/6378/904", target = "_blank", .noWS = "outside"), ".", style = "text-align:justify;color:white;background-color:gray;padding:15px;border-radius:10px", .noWS = c("after-begin", "before-end"))),
                        ),
                        br(),
                   column(width = 2,                   
                   p("Global Fishing Watch (GFW) Fishing Data", style = "text-align:center"), style = "border:white; background-color:lavender;font-size:115%;font-color:#3c4b57;padding:15px;padding-top:10px",
                   hr(style = "border-color:#cd6ebe;opacity:0.8;margin-top:10px;margin-bottom:20px;"),
                   tags$head(
                     tags$style(HTML(
                       ".checkbox {margin:0;text-align:left}
                        .checkbox p {margin:0;text-align:left}
                        .radio label {margin:0;text-align:left}
                        .radio label {margin:0;text-align:left}
                        .shiny-input-container {margin-bottom:0px}
                        .control-label {text-align:center;margin-bottom:0px}
                        .label {text-align:center}
                        .shiny-input-container {text-align:-webkit-left}
                        .legend {line-height:15px}
                        .leaflet-top .leaflet-control {margin-top:3px}
                        .leaflet-bottom .leaflet-control {margin-bottom:2px}
                        .leaflet-control-layers label {margin-top: 0px; margin-bottom: 0px}
                        "))),
                   selectInput(inputId = "FisheriesYear", label = "Yearly Fishing Effort (hr/km^2)", choices = c("2020" = 4, "2019" = 3, "2018" = 2, "2017" = 1), selected = "2020"),
                   hr(style = "border-color:#cd6ebe;opacity:0.2;margin-top:10px;margin-bottom:2.5px;"),
                   radioButtons("GFWPlots", strong("Plot:"), choices = c("Fishing Effort" = 1, "Fishing Effort > 0.1" = 2), selected = character(0)),
                   selectInput("colors", "Change Color Scale", choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))), selected = "YlOrRd"),
                   hr(style="margin-top:15px;margin-bottom:15px;border-color: #cd6ebe;opacity:0.4;border-top: #cd6ebe dashed 1.5px;"),
                   selectInput(inputId = "Fishinggear", label = "2020 Fishing Effort by Gear (hr/km^2)", choices = c("Drifting longlines" = 7, "Tuna purse seines" = 6, "Squid jigger" = 2, "Purse seines" = 8, "Other purse seines" = 5, "Pole and line" = 9, "Set longlines" = 4, "Trawlers" = 3, "Trollers" = 11, "Set gillnets" = 10, "Other fishing gears" = 1), selected = "Trollers"),
                   checkboxInput("PlotGear", "Plot 2020 Fishing Effort by Gear", FALSE),
                   div(style = "display:inline-block;width:10%;margin-left:5%;max-width:100%;margin-top:5px",
                       actionButton(inputId = "hideFisheries",
                         label = HTML("Clear <br/>Fishing <br/>Effort"), style = "font-weight:bold;text-align:center;font-size:105%;color:#a2b03a;padding:15px;border:2px;box-shadow: 0 0 11px 2px #a2b03a;/* box-shadow: 0 0 black; */box-shadow: 4px 4px 20px 4px #a2b03a")),
                   br(),
                   br(),
                   sliderInput("opacity", HTML("Intensity <br/>(No Map &#8596 Visible Map)"), min = 0, max = 1, value = 0.7, step = 0.1),
                   sliderInput("opacity_residence", HTML("Residence Time Opacity <br/>(No Map &#8596 Visible Map)"), min = 0, max = 1, value = 0.7, step = 0.1),
                   actionButton("usermap", label = strong(HTML("Download <br/>User Map")), style="color: black; background-color:white;border-color:#a2b03a;box-shadow: 5px 5px #a2b03a;border:2px solid #a2b03a;
                        "),
                   hr(style="border-color:#cd6ebe;opacity:0.2"),
                   p("GFW data are based on vessel AIS, thus representing a minimum estimate of fishing occurring in these areas (e.g. dependent on satellite coverage and AIS usage). More information can be found to the right.", style = "font-size:65%;font-color:#3c4b57;padding:5px")
                   ),
                   ), 

                      br(),
                      br(),
                    )),
      
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
                              div(class="img-fluid img-thumbnail",img(src = paste0("sst_05deg_",year,"_",month,".jpg"), height = 'auto', width = '100%'),style = "text-align: center"), 
                                    column(p("Sea Surface Temperature (\u00B0C) along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:left;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("Front Density",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                  div(class="img-fluid img-thumbnail", img(src = paste0("fpi_05deg_", file_dater, ".jpg"), align='center', height = 'auto', width = '100%'), style = "text-align: center"),
                                      column(p("Front Density along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:left;color:#A2B03A;padding:2px;font-size: 105%"), width=12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("SSH",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                  div(class="img-fluid img-thumbnail", img(src = paste0("ssh_05deg_",year,"_",month,".jpg"), align='center', height = 'auto', width = '100%'), style="text-align: center"),
                                      column(p("Sea Surface Height (SSH) (m) (absolute height) along the Eastern Pacific with Exclusive Economic Zones (shown in gray); data generated using E.U. Copernicus Marine Service Information", style="text-align:left;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
                                    width = 12),
                    br(),
                    )),

                tabPanel("Bathymetry",
                  br(),
                  br(),
                  fluidRow(column(width = 1), column(
                                div(class="img-fluid img-thumbnail", img(src = "bathy_05deg.jpg", align = 'center', height = 'auto', width = '100%'), style = "text-align: center"),
                                       column(p("Bathymetry (m) along the Eastern Pacific with Exclusive Economic Zones (shown in gray)", style = "text-align:left;color:#A2B03A;padding:2px;font-size: 105%"), width = 12),
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

server <- shinyServer(function(input,output,session) {
  
  output$prediction <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
    addRasterImage(predictintensity, colors = palpredictint, opacity = 0.7, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "Intensity") %>% # Use FALSE if error in palette occurs
    addLegend(pal = palpredictint, values = terra::values(predictintensity), title = "Intensity \u03BB ", layer = "Predictionlegend", labFormat = labeller, group = "Intensity") %>%
      addRasterImage(predictspat, colors = palpredict, opacity = 0.7, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "Residence Time") %>%
      addLegend(pal = palpredict, values = terra::values(predictspat), title = "Residence <br>Time (Days)", layer = "Predictionlegend_tel", group = "Residence Time") %>%
      addMapPane("country", zIndex = 400) %>% 
      addMapPane("EBSAs", zIndex = 420) %>% 
      addMapPane("EBSAs_small", zIndex = 430) %>% 
      addMapPane("EEZ", zIndex = 410) %>% 
      addMapPane("MPAs", zIndex = 430) %>%
      addMapPane("Relative Risk of Interaction", zIndex = 400) %>%
      addPolygons(data = ebsas, weight=1.5, label = ~NAME, fillOpacity = 0.3, color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs"), group = "EBSAs") %>% 
      addPolygons(data = ebsas_small, weight = 1.5, label=~NAME, fillOpacity = 0.3, color = "white", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "EBSAs") %>% 
      addPolygons(data = CRD, weight = 1.5,label=~NAME, fillOpacity = 0.5, color = "#46b1e6", highlightOptions = highlightOptions(color = "white", weight = 3.5, bringToFront = TRUE), options = pathOptions(pane = "EBSAs_small"), group = "Costa Rica Dome <br>EBSA") %>% 
      addPolygons(data = PacificEEZ, weight = 1.5,  opacity = 0.6, fillOpacity = 0.4, label = ~geoname, color = "#3c4b57", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "EEZ"), group = "EEZs") %>%
      addPolygons(data = allmpas, weight = 1.5, opacity = 0.8, fillOpacity = 0.3, label=~LABEL, color = "goldenrod", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), options = pathOptions(pane = "MPAs"), group = "MPAs and Other <br>Protected Areas") %>%
      addPolygons(data = countriessubset, weight = 1.5, label = ~NAME, fillOpacity = 0.4, color = "#a2b03a", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3, bringToFront = TRUE), options = pathOptions(pane = "country"), group = "Countries") %>%
      addPolygons(data = maprisk, weight = 1.5, label = 'Fisheries Risk Analysis Coverage', fillOpacity = 0, color = "#495a69", highlightOptions = highlightOptions(color = "#3c4b57", weight = 3, bringToFront = TRUE), options = pathOptions(pane = "Relative Risk of Interaction"), group = HTML('<span style=\"color:rgb(253, 107, 49);\">Fisheries Risk Zone</span>')) %>%
      setView(-105, -8, zoom = 3) %>% 
      # Add layer controls
      addLayersControl(
        position = "bottomleft",
        overlayGroups = c("Intensity", "Residence Time","EBSAs", "Costa Rica Dome <br>EBSA", "MPAs and Other <br>Protected Areas", "EEZs", "Countries", HTML('<span style=\"color:rgb(253, 107, 49);\">Fisheries Risk Zone</span>')),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Residence Time","EBSAs", "MPAs and Other <br>Protected Areas", HTML('<span style=\"color:rgb(253, 107, 49);\">Fisheries Risk Zone</span>'))) %>%
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center;font-weight:bold;font-size:110%;margin-bottom:-2px;margin-right:-2px;\">Areas of Interest<br/></label>');
        }
    ") %>% 
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "rgb(253, 107, 49)",
        completedColor = "#A2B03A"
      )
  })

  filteredmap <- reactive({

    fisheries[[as.numeric(input$FisheriesYear)]]

  })
  
  filteredgear <- reactive({

     gear2020[[as.numeric(input$Fishinggear)]]

   })
  
  filteredriskgear <- reactive({
    
    filteredrisk = raster::subset(riskstack, grep(paste0('g', input$Riskfishinggear), names(riskstack), value = T))
    
    filteredrisk[[as.numeric(input$PlotRisk)]]
    
  })
  
  legendfilteredrisk <- reactive({
  
  filteredriskname = data.frame(names = c('Drifting longlines', 'Other fishing gears', 'Purse seines', 'Pole and line', 'Set gillnets', 'Set longlines', 'Squid jiggers', 'Trawlers', 'Tuna purse seines'), id = seq(9))
 
  filteredriskname[[input$Riskfishinggear, 1]]
  
  })

  proxy <- leafletProxy("prediction")
  
  observeEvent(input$StaticMap, {
    
    showModal(modalDialog(
      title = p('Eastern Pacific leatherback predictions for ', predictmonth, predictyear),
      div(img(src = paste0("Prediction_", month.abb[as.numeric(month)], year, "_0.1deg.jpg"),  height = 'auto', width = '100%')),
      br(),
      downloadButton("downloadstatic","Download Static Map"),
      easyClose = TRUE,
      fade = TRUE,
      footer = NULL
    ))
    
    })
  
  observeEvent(input$StaticMap2, {
    
    showModal(modalDialog(
      title = p('Eastern Pacific leatherback intensity predictions for ', predictmonth, predictyear),
      div(img(src = paste0("Intensity_prediction_", month.abb[as.numeric(month)], year, ".jpg"),  height = 'auto', width = '100%')),
      br(),
      downloadButton("downloadstatic","Download Static Map"),
      easyClose = TRUE,
      fade = TRUE,
      footer = NULL
    ))
    
  })
  
  output$downloadstatic <- downloadHandler(
    filename = paste0("Leatherback_Residence_", predictmonth, year, ".jpg"),
    content = function(file) {
      file.copy(paste0("www/Prediction_", month.abb[as.numeric(month)], year, "_0.1deg.jpg"), file)
    })   

  observeEvent(input$opacity, {
    
    proxy %>% clearGroup(group = "Intensity") %>%
      addRasterImage(predictintensity, colors = palpredictint, opacity = input$opacity, maxBytes=40 * 1024 * 1024, group = "Intensity")
    
  })
  
  observeEvent(input$opacity_residence, {
    
    proxy %>% clearGroup(group = "Residence Time") %>%
      addRasterImage(predictspat, colors = palpredict, opacity = input$opacity_residence, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "Residence Time")
    
  })
  
  observeEvent(input$GFWPlots, {
    
    colorpal <- reactive({colorBin(input$colors, terra::values(filteredmap()), bins = c(0, 0.1, 0.5, 2.5, 10, 20, 40, 80, 120, ceiling(max(terra::values(filteredmap()),na.rm=T))), na.color = "transparent")})
    
    updateCheckboxInput(session = session, inputId = "PlotGear", value = is.null(input$PlotGear))
    
    #Always clear the group first on the observed event
    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend") %>% clearGroup(group = "Risk.group") %>% removeControl("Risklegend") # Removes legend on click

    if(input$GFWPlots == 1){
      
      updateRadioButtons(session = session, "PlotRisk", HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom:10px;\">Select Behavior of Interest</label>'), choices = c('Transiting' = 1, 'Residential/ Foraging' = 2, 'Deep diving/ Exploratory' = 3), selected = character(0))
      observe({
        pal <- colorpal()
        proxy %>%
          removeControl(legend) %>% # Removes legend on year change
          addRasterImage(filteredmap(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group", project = FALSE) %>%
          addLegend(pal = pal, values = terra::values(filteredmap()), title = paste("Fishing Effort <br> ", parse_number(names(filteredmap()))), group = "Fisheries.group", layer = "Fisherieslegend")})
      
    }
  })
  
  observeEvent(input$GFWPlots, {
    
    colorpal2 <- reactive({colorBin(input$colors, terra::values(filteredmap()),  bins = c(0.1, 0.5, 2.5, 10, 20, 40, 80, 120, ceiling(max(terra::values(filteredmap()), na.rm=T))), na.color = "transparent")})
    
    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend") %>% clearGroup(group = "Risk.group") %>% removeControl("Risklegend") 
    
    updateCheckboxInput(session = session, inputId = "PlotGear", value = is.null(input$PlotGear))
    
    if(input$GFWPlots == 2){
      
      updateRadioButtons(session = session, "PlotRisk", HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom:10px;\">Select Behavior of Interest</label>'), choices = c('Transiting' = 1, 'Residential/ Foraging' = 2, 'Deep diving/ Exploratory' = 3), selected = character(0))
      observe({
        pal <- colorpal2()
        proxy %>%
          removeControl(legend) %>% 
          addRasterImage(filteredmap(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group", project = FALSE) %>%
          addLegend(pal = pal, values = terra::values(filteredmap()),title = paste("Fishing Effort >= 0.1 <br> ", parse_number(names(filteredmap()))), group = "Fisheries.group", layer = "Fisherieslegend")})
      
    }
  })
  
  observeEvent(input$PlotGear, {
    
    paletterev <- rev(viridis(6))
    colorpal <- reactive({colorBin(paletterev, terra::values(filteredgear()), bins = c(0, 0.1, 0.5, 2.5, 10, 20, 40, 120, ceiling(max(terra::values(filteredgear()),na.rm=T))), na.color = "transparent")})
    
    proxy %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend") %>% clearGroup(group = "Risk.group") %>% removeControl("Risklegend")
    
    if(input$PlotGear){
      updateRadioButtons(session = session, "GFWPlots", strong("Plot:"), choices = c("Fishing Effort" = 1, "Fishing Effort > 0.1" = 2), selected = character(0))
      updateRadioButtons(session = session, "PlotRisk", HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom:10px;\">Select Behavior of Interest</label>'), choices = c('Transiting' = 1, 'Residential/ Foraging' = 2, 'Deep diving/ Exploratory' = 3), selected = character(0))
      
      observe({
        
        pal <- colorpal()
        proxy %>%
          removeControl(legend) %>% 
          clearGroup(group = "Risk.group") %>% removeControl("Risklegend") %>% 
          addRasterImage(filteredgear(), color = pal, opacity = 0.9, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Fisheries.group", project = FALSE) %>%
          addLegend(pal = pal, values = terra::values(filteredgear()), title = paste("2020 Fishing Effort <br> ", sub("_"," ", names(filteredgear()))), group = "Fisheries.group", layer = "Fisherieslegend")})
      
    }
  })

  observeEvent(input$hideFisheries, {
    
    leafletProxy("prediction") %>% removeShape("foo") %>% clearGroup(group = "Fisheries.group") %>% removeControl("Fisherieslegend")

    updateCheckboxInput(session = session, inputId = "PlotGear", value = is.null(input$PlotGear))
    
    updateRadioButtons(session = session, "GFWPlots", strong("Plot:"), choices = c("Fishing Effort" = 1, "Fishing Effort > 0.1" = 2), selected = character(0))
    
  })
  
  observeEvent(input$usermap, {
    
    screenshot(id = "prediction", filename = paste0("South_Pacific_TurtleWatch_", predictmonth, year))
    
  })
  
  observeEvent(input$moveToInteractive, {
    
    updateTabsetPanel(session = session, inputId = "maptabs", selected = "Interactive Map")
    
  })
  
  observeEvent(input$PlotRisk, {
    
    proxy %>% clearGroup(group = "Risk.group") %>% removeControl("Risklegend") %>% removeControl("Fisherieslegend") %>% clearGroup("Intensity") %>% removeControl("Predictionlegend") %>% clearGroup("Residence Time") %>% removeControl("Predictionlegend_tel") # %>% clearGroup(group = "Fisheries.group") 
    
    
    updateRadioButtons(session = session, "GFWPlots", strong("Plot:"), choices = c("Fishing Effort" = 1, "Fishing Effort > 0.1" = 2), selected = character(0))
    
    observe({
      
      colorpalrisk <- reactive({colorNumeric(sptw_brewer_pal(n = 5, input$colorsRisk), c(min(terra::values(filteredriskgear()), na.rm=T), max(terra::values(filteredriskgear()), na.rm=T)), na.color = "#abd2e1")}) # na.rm=F was crashing shiny
        
      pal <- colorpalrisk()
      
      proxy %>% clearGroup("Intensity") %>% removeControl("Predictionlegend")  %>% clearGroup("Residence Time") %>% removeControl("Predictionlegend_tel") %>% 
        removeControl(legend) %>% # Removes legend on gear change
        addRasterImage(filteredriskgear(), color = pal, opacity = 1, maxBytes = 40 * 1024 * 1024, layerId = "foo", group = "Risk.group", project = FALSE) %>%
        addLegend(pal = pal, values = terra::values(filteredriskgear()), title = paste(HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom: 0px;\">Relative Risk <br>of Interaction <br></label>'),'<br>', p(legendfilteredrisk(), style = "text-align:center;color:#FD6B31")), group = "Risk.group", layer = "Risklegend", position = "bottomright", labFormat =  function(type, cuts, p) {n = length(cuts);cuts[n] = paste("Higher Risk"); for(i in 2:(n-1)){cuts[i] = ""};cuts[1] = paste("Lower Risk");cuts[2]=cuts[2]; paste0(cuts[-n], cuts[-1])}) %>% 
        hideGroup(c('Intensity','Residence Time')) %>% 
        showGroup(HTML('<span style=\"color:rgb(253, 107, 49);\">Fisheries Risk Zone</span>')) })
    
  })
  
  observeEvent(input$hideFisheriesRisk, {
    
    leafletProxy("prediction") %>% removeShape("foo") %>% clearGroup("Intensity") %>% removeControl("Predictionlegend") %>% clearGroup("Residence Time") %>% removeControl("Predictionlegend_tel") %>% clearGroup(group = "Risk.group") %>% removeControl("Risklegend") %>%
      addRasterImage(predictintensity, colors = palpredictint, opacity = 0.7, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "Intensity") %>% # Use FALSE if error in palette occurs
      addLegend(pal = palpredictint, values = terra::values(predictintensity), title = "Intensity \u03BB ", layer = "Predictionlegend", labFormat = labeller, group = "Intensity") %>% 
      addRasterImage(predictspat, colors = palpredict, opacity = 0.7, maxBytes = 40 * 1024 * 1024, project = TRUE, group = "Residence Time") %>%
      hideGroup(c(HTML('<span style=\"color:rgb(253, 107, 49);\">Fisheries Risk Zone</span>'), 'Residence Time')) %>% showGroup('Intensity') # Replot the prediction raster; clear and replot if button is double-clicked 
    
    updateSelectInput(session, "Riskfishinggear", label = "Fishing Gear", choices = c("Drifting longlines" = 1, "Purse seines" = 3, "Pole and line" = 4, "Set gillnets" = 5, "Set longlines" = 6, "Squid jiggers" = 7, "Trawlers" = 8, "Tuna purse seines" = 9, "Other fishing gears" = 2), selected = "Drifting longlines") 
                      
    updateRadioButtons(session = session, "PlotRisk", HTML('<label style=\"color:rgb(253, 107, 49);margin-bottom:10px;\">Select Behavior of Interest</label>'), choices = c('Transiting' = 1, 'Residential/ Foraging' = 2, 'Deep diving/ Exploratory' = 3), selected = character(0))
    
  })
  
  # track_usage(storage_mode = store_json(path = "logs/"))
  
  # output[["slickr"]] <- renderSlickR({

  #   slickR(imgs)

  # })

  # output[["slickrImgName"]] <- renderText({

    # paste0("CURRENT IMAGE: ", basename(imgs[input$slickr_output_current$.center]))

  # })
  
})

########


########
# Run Application

shinyApp(ui = ui, server = server)

########END
