# PURPOSE: Shiny app to create an Iraq climate monitoring dashboard integrating climate data pulled 
#          from Google Earth Engine 
# AUTHOR: Mehedi Khan, initial author | Data Specialist / Cody Adelson | Data Specialist
# DATE CREATED: August 22, 2022
# NOTES: Adapted by new data specialist beginning in Feb 2023 - 1) writing for methodology updated
#       2) new indicators added 3) reading in of files from join_data, updated to one df 4) data 
#       updated with 2023 5) year filter added

rm(list = ls())
# library -----------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinybrowser)
library(leaflet)
library(sf)
library(dplyr)
library(leaflet.extras)
library(htmltools)
library(leafgl)
#library(readr)
library(rmapshaper)
library(feather)
#library(lubridate)
#library(data.table)
# read_data ---------------------------------------------------------------
#grid <- st_read("01_inputs/01_hexagon/cluster_20_km.shp") %>% st_transform(4326) 

# indicator list ----------------------------------------------------------
indicator_list <- data.frame(
  name = c("Precipitation", "Precipitation deficit","3-months SPI","9-months SPI","12-months SPI","NDVI anomaly","NDWI anomaly", "Surface Temperature", "Temperature anomaly", "Evaporation anomaly"),
  group = c("Precipitation", "Precipitation","Precipitation","Precipitation","Precipitation","Vegetation","Vegetation", "Temperature","Temperature", "Evaporation"))

climate_data <- readRDS(file = "05_outputs/processed_indicators/climate_data.rds")


#subdistrict_map <- read_csv("05_outputs/sub_district_values.csv")

##### admin boundaries 
admin_boundary_path <-  "01_inputs/03_admin_boundary/"
#admin_zero <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm0_cso_itos_20190603.shp")) %>% ms_simplify(keep = .3)
admin1_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm1_cso_20190603.shp")) #%>% ms_simplify(keep = .02)
#admin2_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm2_cso_20190603.shp"))%>% ms_simplify(keep = .05)
# leaflet base map --------------------------------------------------------
base_map <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  #leaflet::addPolygons(data = admin_zero,color = "#EE5859", fillColor = "transparent") %>%
  leaflet::addPolygons(data = admin1_boundary, color = "#58585A",
                       weight = 2, dashArray = "12", fillColor = "transparent")  #%>% 
  # leaflet::addPolygons(data = admin2_boundary, color = "#58585A",
  #                      label = ~htmlEscape(ADM2_EN),
  #                      labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = "9px"),
  #                      weight = .5, dashArray = "12", fillColor = "transparent")
  # 

# ui ---------------------------------------------------------------------
ui <-fluidPage(
  # Styling -----------------------------------------------------------------
  tags$head(
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")),
  navbarPage(
    windowTitle = "IRAQ CLIMATE MONITORING DASHBOARD",
    HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"><a style="padding-left:0px;" class = "navbar-brand" href = "https://www.actionagainsthunger.org" target="_blank"><img src = "aah_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
    #HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.actionagainsthunger.org" target="_blank"><img src = "aah_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
    tabPanel("Introduction",
             mainPanel(width = 12,
                       br(),
                       h4(strong("Background: ")),
                       p(style="text-align: justify; font-size: 15px;", 
                         "Globally, Iraq has been classified by the UN Global Environment Outlook as the world’s fifth most vulnerable country to the effects of climate change. This is primarily due to changing factors affecting vulnerable populations such as decreasing rainfall, increasing temperatures and water scarcity. Development organizations and agencies operating in Iraq are beginning to work to address these impacts and are leading research to better understand and prepare in their areas of intervention. This climate monitoring and prediction modelling dashboard has been developed by REACH Initiative, Action Contre La Faim (ACF) and The University of Mosul with the purpose of supporting partners and stakeholders to monitor, prioritize and predict the impacts of various climate indicators throughout the country."),
                       hr(),
                       h4(strong("Methodology:")),
                       p(style="text-align: justify; font-size: 15px;",
                         "Data from remote satellite sensors have been has been used to monitor trends in six climate indicators. These data are integrated into the dashboard using a Google Earth Engine API. Each indicator is aggregated into a 20x20 km hexagon, a granular level that minimizes variation. Further details and background regarding each of the indicators can be found below:"),
                       tags$ol(
                         tags$li(em(strong("Precipitation/Precipitation Deficit:")), 
                                 "Precipitation measures the total amount of monthly rainfall in millimeters. Precipitation deficit measures the change in mean daily precipitation levels from historical levels; negative values indicate less than average precipitations levels, which may signify drought. Historical monthly average precipitation was calculated from 1981 to 2021. The monthly averages were then subtracted from the historical monthly figures to indicate the extent to which the month was wetter or drier than the historical norm. Source: Climate Hazards Group InfraRed Precipitation with Station (CHIRPS) dataset"),
                         p(style="text-align: justify; font-size: 15px;",
                           tags$li(em(strong("SPI:")), 
                                   "The Standardized Precipitation Index (SPI) is an indicator used to detect and characterize meteorological droughts – a period with an abnormal precipitation defecit compared to average historical conditions for the area. The SPI indicator indicates anomalies (deviations from the mean) of the total precipitation. The magnitude of the departure from the mean is a probabilistic measure of the severity of a wet or dry event. Increasingly severe rainfall deficits that can result in metereological droughts are indicated by a SPI measure below ‒1.0, while increasingly severe excess rainfall is indicated by an SPI above 1.0. SPI is computed into time bands as follows. Source: Climate Hazards Group InfraRed Precipitation with Station (CHIRPS) dataset",
                                   tags$ul( # list order of different spi
                                     tags$li(em("3-months SPI:"), 
                                             "SPI calculations for shorter accumulation periods (~ 1 to 3 months) can be used to indicate immediate effects including reduced soil moisture, snowpack, and flow in smaller creeks."), 
                                     # end 3 month spi   
                                     tags$li(em("9-months SPI:"), 
                                             "SPI calculations for medium accumulation periods (~ 3 to 12 months) can be used to indicate reduced stream flow and reservoir storage."), 
                                     # end 6 month spi 
                                     tags$li(em("12-months SPI:"), 
                                             "SPI calculations for longer accumulation periods (~ 12 to 48 months), can be used to indicate reduced reservoir and groundwater recharge.")
                                   ) # end SPIs ul list
                           ) # end tag$li for SPI
                         ), # end p()
                         p(style="text-align: justify; font-size: 15px;",
                           tags$li(em(strong("NDVI Anomaly: ")), 
                                   "The Normalized Difference Vegetative Index (NDVI) measures the density and color of vegetation, which gives insight to overall vegetative health. Historical NDVI monthly data were used and compared with current values using a z-score calculation to determine if the areas are more or less healthy than the average. This comparison is called the NDVI Anomaly. MODIS data has been used to calculate the NDVI Anomaly, using 2000-2015 as the base years. Source: MODIS Terra Vegetation Indices dataset"
                           )),
                         p(style="text-align: justify; font-size: 15px;",
                           tags$li(em(strong("NDWI Anomaly: ")), 
                                   "The Normalized Difference Water Index (NDWI) measures changes in water content of leaves, serving as a proxy for plant water stress. Historical NDWI monthly data were used and compared with current values using a z-score calculation, using 2000-2015 as the base years. Source: MODIS Terra Daily NDWI dataset"      
                           )),
                         p(style="text-align: justify; font-size: 15px;", 
                           tags$li(em(strong("Surface Temperature/Temperature Anomaly: ")), 
                                   "Surface temperature measures the average monthly temperature. Temperature anomaly measures differences in surface temperature, positive or negative, from historical levels using a z-score calculation. MODIS 16 days temperature data has been used to calculate temperature, using 2000-2015 as the base period for temperature anomaly. Source: MODIS Terra Land Surface Temperature and Emissivity dataset."      
                           )),
                         p(style="text-align: justify; font-size: 15px;", 
                           tags$li(em(strong("Evaporation Anomaly: ")), 
                                   "Evaporation anomaly measures the changes in total evaporation compared to historical levels using a zscore calculation. Evaporation can be effected by factors including heat, humidity, wind speed and water availability.  It was calculated using the monthly average of the total meters of water evaporation from bare soil, open water surfaces, the top of canopy and from vegetation. The monthly average was then subtracted from 2000-2015 historical monthly figures. Source: ERA5-Land - ECMWF Climate Reanalysis dataset"      
                           ))
                       ), # tag$ol
                       hr(),
                       h4(strong("Limitation:")),
                       p(style="text-align: justify; font-size: 14px;", 
                         "Crop masking is currently not integrated into NDVI and NDWI calculations"),
                       hr(),
                       h4(strong("Contact:"),tags$br(),
                          p("Cody Adelson",br(),
                            "Data Specialist",br(),
                            "Email:", tags$a(href="mailto:cody.adelson@impact-initiatives.org","cody.adelson@impact-initiatives.org"))
                       )
             ) # end main panel 0
    ), # end tab panel 0 
    tabPanel("Climate Indicators",
             mainPanel(width = 12,
                       br(),
                       h5(strong("Climate change is adversely affecting Iraq. REACH has developed this climate monitoring dashboard to support the humanitarian community and implementing agencies to quickly analyze and respond to hotspots. Note the most recent available month will differ due to the variance in publishing frequency of the satellite data.")),
                       hr(),
                       ##################### input ###############################
                       tags$div(pickerInput("select_climate_indicator",
                                            label = "Select Climate Indicator:",
                                            choices = unique(sub("_.*", "", names(climate_data)[-ncol(climate_data)])),
                                            selected = "Temperature anomaly",
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
                       tags$div(pickerInput("month",
                                            label = "Select Month:",
                                            choices = unique(sub(".*[_]([^.]+)[_].*", "\\1", names(climate_data)[-ncol(climate_data)])),
                                            selected = NULL,
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
                       tags$div(pickerInput("year",
                                            label = "Select Year:",
                                            choices = unique(sub(".*_", "", names(climate_data)[-ncol(climate_data)])),
                                            selected = NULL,
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
                       # tags$div(pickerInput("select_date",
                       #                      label = "Select Date:",
                       #                      choices = NULL,
                       #                      selected = NULL,
                       #                      multiple = F,
                       #                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       # ),style="display:inline-block"),
                       actionButton("run", "Show result"),
                       div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 200px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                           leafglOutput("map", width ="100%", height = "100%"))
             ) # end main panel  
    ), # tab 1 end 
    tabPanel("Surface Water",
             mainPanel(
               htmlOutput("frame"),
               em("Source: Joint Research Centre (JRC)"),
             ) # end mainpanel 2   
    ), # End table 2
    # tabPanel("Water Scarcity Anticipatory Model",
    #          mainPanel(
    #            htmlOutput("frame_acf_model"),
    #            shiny::em("Source: ACF/UoM"),
    #          ) # end mainpanel 3   
    # ),
    # tabPanel("Test Google Sheets",
    #          mainPanel(
    #            htmlOutput("test_sheet"),
    #            shiny::em("Source: ACF/UoM"),
    #          ) # end mainpanel 3
    # )
    # tabPanel("Climate Indicator Subdistrict",
    #          mainPanel(width = 12,
    #                    br(),
    #                    h5(strong("Climate change is adversely affecting Iraq. REACH has developed this climate monitoring dashboard to support the humanitarian community and implementing agencies to quickly analyze and respond to hotspots. Note the most recent available month will differ due to the variance in publishing frequency of the satellite data.")),
    #                    hr(),
    #                    ##################### input ###############################
    #                    tags$div(pickerInput("select_climate_indicator_subdistrict",
    #                                         label = "Select Climate Indicator:",
    #                                         choices = lapply(split(indicator_list$name, indicator_list$group), as.list),
    #                                         selected = "Temperature anomaly",
    #                                         multiple = F,
    #                                         options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
    #                    ), style="display:inline-block"),
    #                    tags$div(pickerInput("select_date_subdistrict",
    #                                         label = "Select Date:",
    #                                         choices = NULL,
    #                                         selected = NULL,
    #                                         multiple = F,
    #                                         options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
    #                    ),style="display:inline-block"),
    #                    actionButton("run_subdistrict", "Show result"),
                       # div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 200px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                       #     leafglOutput("map",width ="100%", height = "100%"))
             
     # end main panel  
    # End table 3
  ) # end navar page
) # end fluid page
# server ------------------------------------------------------------------
server <- function(input, output,session){
  # date_selected <-  reactive({input$select_date}) 
  # 
  # indicator <- reactive(input$select_climate_indicator)
  # 
  # #indicator_df1 <- reactive({get(indicator())}) 
  # 
  # indicator_df1 <- reactive({select(contains(indicator()))})
  # 
  # 
  # available_date <- reactive({colnames(indicator_df1())})
  # 
  # observe({updatePickerInput(session, "select_date", choices = paste0(c(available_date())))})

  
  filtered_data <- reactive({
    climate_data %>%
      filter(sub("_.*", "", names(climate_data)[-ncol(climate_data)]) == input$indicator,
             sub(".*[_]([^.]+)[_].*", "\\1", names(climate_data)[-ncol(climate_data)]) == input$month,
             sub(".*_", "", names(climate_data)[-ncol(climate_data)]) == input$year)
  })
  ########################## FILTER AND JOIN ###########################
  #df <- eventReactive(input$run,{filtered_data() %>% filter(date == date_selected())})

  #grid_with_df <- eventReactive(input$run,{grid %>% left_join(df(),by = "FID") %>% filter(!is.na(value))})

  #values for coloring ------------------------------------------------------
  column_names <- c("Precipitation deficit" = "value",
                    "12-months SPI" = "value",
                    "9-months SPI" = "value",
                    "3-months SPI" = "value",
                    "Evaporation anomaly" = "value",
                    "Temperature anomaly" = "value",
                    "NDWI anomaly" = "value",
                    "NDVI anomaly" = "value",
                    "Surface Temperature" = "value",
                    "Precipitation" = "value")
  

  # Use the lookup table to get the column name based on the selected climate indicator
  
 df1 <- eventReactive(input$run,{
    df1 <- filtered_data() %>%
      mutate(value2 = .data[[column_names[input$select_climate_indicator]]])
    return(df1)
  })
  
  # grid_with_df1 <- eventReactive(input$run,{
  #   grid_with_df1 <- grid_with_df() %>%
  #     mutate(value2 = .data[[column_names[input$select_climate_indicator]]])
  #   return(grid_with_df1)
  # })

  # 
  # color -------------------------------------------------------------------
  clr <- eventReactive(input$run,{
    if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
      bin <- c(-Inf, -2, -1.5, -1, -.5, .5, 1, 1.5, 2, Inf)
      clr <- colorBin(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in%  c("Evaporation anomaly")) {
      bin <- c(-Inf, -1.5, -1, -.5, .5, 1, 1.5, Inf)
      clr <- colorBin(c("#762a83", "#af8dc3" ,"#e7d4e8", "#FFFFE7","#d9f0d3", "#7fbf7b","#1b7837"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Temperature anomaly")) {
      bin <- c(-Inf,-2,-1,-.5,.5,1,2,Inf)
      clr <- colorBin(c("#367BB3", "#729DC6" , "#A1BCD7", "#FFFFE7","#eac435","#f3b700","#dd7230"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {
      bin <- c(-Inf,-2,-1,1,2,Inf)
      clr <- colorBin(c("#f2022a", "#fc4765", "#FFFFE7", "#71f575","#00971B"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Surface Temperature")) {
      bin <- c(-Inf,20,30,40,50,Inf)
      clr <- colorBin(c("#367BB3", "#A1BCD7", "#FFFFE7","#eac435", "#dd7230"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Precipitation")) {
      bin <- c(-Inf, .0001, 10, 20, 30, 40, 50, 60, 70, Inf)
      clr <- colorBin(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                      domain =  df1()$value2, bins = bin)
      return(clr)}
    })
  # legend ------------------------------------------------------------------
  add_legend_df <- eventReactive(input$run,{
    if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
      add_legend_df <- list(
        color = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
        label = c("-2 and less (drier)", "-2 to -1.5","-1.5 to -1","-1 to -.5", "-.5 to +.5 (normal)", "+0.5 to +1", "+1 to +1.5", "+1.5 to +2", "+2 and above (wetter)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in%  c("Evaporation anomaly")) {
      add_legend_df <- list(
        color = c("#762a83", "#af8dc3" ,"#e7d4e8", "#FFFFE7","#d9f0d3", "#7fbf7b","#1b7837"),
        label = c("-1.5 and less (less evaporation)","-1 to -1.5,","-.5 to -1", "-.5 to +0.5 (normal)", "+.5 to +1","+1 to +1.5","+1.5 and above (more evaporation)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Temperature anomaly")) {
      add_legend_df <- list(
        color = c( "#367BB3", "#729DC6", "#A1BCD7", "#FFFFE7", "#eac435", "#f3b700", "#dd7230"),
        label = c("Less than -2°C (cooler)","-2 to -1","-1 to -.5", "-.5 to .5 (normal)",".5 to 1","1 to 2","Greater than 2°C (warmer)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {
      add_legend_df <- list(
        color = c("#f2022a", "#fc4765", "#FFFFE7", "#71f575", "#00971B"),
        label = c("Less than -2 (drier)","-2 to -1","-1 to 1 (normal)", "1 to 2", "Greater than 2 (wetter)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Surface Temperature")) {
      add_legend_df <- list(
        color = c("#367BB3", "#A1BCD7", "#FFFFE7","#eac435", "#dd7230"),
        label = c("<20°C","20-30°C","30-40°C", "40-50°C", "50+°C"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Precipitation")) {
      add_legend_df <- list(
        color = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
        label = c("0mm","0-10mm","10-20mm", "20-30mm", "30-40mm", "40-50mm", "50-60mm", "60-70mm", "70+mm"))
      return(add_legend_df)}
  })
  ##############################################################################
  # output$map <-  renderLeaflet({
  #   base_map %>%  leafgl::addGlPolygons(data = df1(),
  #                                       fillOpacity = 1,
  #                                       color =   ~clr()(df1()$value2),
  #                                       stroke = F) %>% 
  #                                       # popup = paste("Hexagon ID:", grid_with_df1()$FID, "<br>",
  #                                       #               "Date:", input$select_month, input$select_date, "<br>",
  #                                       #               #"Governorate:", admin1_boundary$ADM1_EN, "<br>",
  #                                       #               #"District:", admin2_boundary$ADM2_EN, "<br>",
  #                                       #               "Indicator:", input$select_climate_indicator, "<br>",
  #                                       #               "Value:",  grid_with_df1()$value))  %>%
  #     setView(lat = 33.312805,lng = 44.361488,zoom = 6) %>%
  #     addMiniMap() %>%
  #     #addSearchOSM()%>%
  #     # addLegend("topright",pal = clr(), values = grid_with_df()$value)
  #     addLegend(
  #       colors = add_legend_df()$color,
  #       labels =add_legend_df()$label,
  #       opacity = 1,
  #       title = add_legend_df()$unit)
    
    output$map <- renderLeaflet({
      base_map %>%
        addTiles() %>%
        addPolygons(data = filtered_data(),
                    fillColor = "red",
                    color = "black",
                    fillOpacity = 0.5)
    
    
  })
  

  frame <- tags$iframe(src="https://global-surface-water.appspot.com/map", style="height: 100vh;",scrolling = 'no',width="150%", frameborder = "0")
  output$frame <- renderUI({
    frame
  })
  # frame_acf_model <- tags$iframe(src="https://docs.google.com/spreadsheets/d/e/2PACX-1vTrpOkXJSINvpomxOgQOlvmmkdVy29uL5KPn_r7fGN2FU4nPo4VTCI_IbuBaUfAX735RVjyeC0cWkzI/pubhtml", style="height: 100vh;",scrolling = 'no',width="150%", frameborder = "0")
  # output$frame_acf_model <- renderUI({
  #   frame_acf_model
  # })
  # test_sheet <- tags$iframe(src="https://docs.google.com/spreadsheets/d/1WeZOSyhzvRPgpJ163DQE6rIJ1rWHinluXsvqWE4XYgg/edit#gid=0", style="height: 100vh;",scrolling = 'yes',width="150%", frameborder = "0")
  # output$test_sheet <- renderUI({
  #   test_sheet
  # })
}
## App 
shinyApp(ui = ui, server = server)

