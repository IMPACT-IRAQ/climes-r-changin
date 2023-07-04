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
library(rmapshaper)
library(ggplot2)
library(glitr)
library(plotly)
library(ggh4x)
library(openxlsx)
library(zoo)
library(lubridate)
library(forcats)
library(RColorBrewer)
library(leaflegend)
# read_data ---------------------------------------------------------------

# indicator list ----------------------------------------------------------
indicator_list <- data.frame(
  name = c("Precipitation", "Precipitation deficit","3-months SPI","9-months SPI","12-months SPI","NDVI anomaly","NDWI anomaly", "Surface Temperature", "Temperature anomaly", "Evaporation anomaly"),
  group = c("Precipitation", "Precipitation","Precipitation","Precipitation","Precipitation","Vegetation","Vegetation", "Temperature","Temperature", "Evaporation"))

climate_data <- readRDS(file = "05_outputs/processed_indicators/climate_data.rds") %>% 
  st_as_sf(crs = st_crs()) 

model_data <- read.xlsx("05_outputs/model_data_output.xlsx") %>%
  mutate(date = zoo::as.yearmon(my(date)),
         indicator = fct_relevel(indicator, c("Predicted Irrigation Consumptive Use (mm/month)", "Evapotranspiration (mm)",
                                              "Rainfall (mm)", "Surface Temperature °C")))

model_geography_list <- model_data %>% distinct(governorate, district)

irrigation_boundaries <- model_data %>% 
  filter(!is.na(mean_irrigation)) %>% 
  distinct(district, mean_irrigation) 

date_choices <- format(model_data$date)

##### admin boundaries 
admin_boundary_path <-  "01_inputs/03_admin_boundary/"
admin1_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm1_cso_20190603.shp")) %>% ms_simplify(keep = .75)
admin2_boundary  <-st_read(paste0(admin_boundary_path,"/irq_admbnda_adm2_cso_20190603.shp"))%>% left_join(irrigation_boundaries, by=join_by(ADM2_EN==district)) %>%  ms_simplify(keep = .75)
admin2_irrigation <- admin2_boundary %>% filter(!is.na(mean_irrigation))

# leaflet base map --------------------------------------------------------

base_map <- leaflet::leaflet() %>% leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addPolygons(data = admin1_boundary, color = "#58585A",
                       weight = 2, dashArray = "12", fillColor = "transparent") %>% 
  leaflet::addPolygons(data = admin2_boundary, color = "#58585A",
                       label = ~htmlEscape(ADM2_EN),
                       labelOptions = labelOptions(noHide = F, textOnly = T, textsize = "12px"),
                       weight = .5, dashArray = "12", fillColor = "transparent")

irrigation_pal <- colorNumeric(palette = "Reds", domain = admin2_irrigation$mean_irrigation)

model_base_map <- leaflet::leaflet() %>%
  leaflet::addPolygons(data = admin1_boundary, color = "black",
                       weight = 2,  fillColor = "white", opacity = 1) %>% 
  leaflet::addPolygons(data = admin2_irrigation, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                       color = "black", weight = 2, fillColor = ~irrigation_pal(mean_irrigation),
                       popup = paste(
                         "District:", admin2_irrigation$ADM2_EN, "<br>",
                         "2023 Mean Shortfall:", round(admin2_irrigation$mean_irrigation, 1))
                       ) %>% 
  addLegendNumeric(data = admin2_irrigation, orientation = c("horizontal"), width = 180, height = 8, pal = irrigation_pal, values = ~mean_irrigation, title = "2023 Mean Irrigation Shortfall", position = 'bottomright')

# ui ---------------------------------------------------------------------
ui <-fluidPage(
  # Styling -----------------------------------------------------------------
  tags$head(
    HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")),
  navbarPage(
    windowTitle = "IRAQ CLIMATE MONITORING DASHBOARD",
    #HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"><a style="padding-left:0px;" class = "navbar-brand" href = "https://www.actionagainsthunger.org" target="_blank"><img src = "aah_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
    #HTML('<a style="padding-left:10px;" class = "navbar-brand" </a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
    HTML('<span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>IRAQ CLIMATE MONITORING DASHBOARD</strong></span>'),
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
                       h4(strong("Limitations:")),
                       p(style="text-align: justify; font-size: 14px;", 
                         "The grid size for the Climate Indicators map is 20x20km. Note that any variation within that area is not visible in the dashboard",
                         br(),
                         "Crop masking is not currently integrated into NDVI and NDWI calculations. The use of crop masking has been shown to be associated with improved crop yield predictions.",
                         br(),
                         "Surface temperature is recorded via remote sensing. Surface temperatures should be noted are generally hotter than ambient or air temperatures",),
                       hr(),
                       h4(strong("Contact:"),tags$br(),
                          p("Cody Adelson",br(),
                            "Data Specialist",br(),
                            "Email:", tags$a(href="mailto:cody.adelson@impact-initiatives.org","cody.adelson@impact-initiatives.org"))
                       ),
                       hr(
                       id = "logo", class = "card", bottom = 15, right = 20, fixed=TRUE, draggable = FALSE, height = "auto",
                       tags$a(href='https://www.reach-initiative.org', target = "_blank",
                              tags$img(src="reach_logo.png", height='40')),
                       id = "logo", class = "card", bottom = 15, right = 20, fixed=TRUE, draggable = FALSE, height = "auto",
                       tags$a(href='https://www.usaid.gov/iraq/humanitarian-assistance', target = "_blank",
                              tags$img(src="usaid_logo.png", height='80')),
                       id = "logo", class = "card", bottom = 15, right = 20, fixed=TRUE, draggable = FALSE, height = "auto",
                       tags$a(href='https://www.actionagainsthunger.org', target = "_blank",
                              tags$img(src="aah_logo2.png", height='50'))),
                       # End table 2
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
                                            choices = lapply(split(indicator_list$name, indicator_list$group), as.list),
                                            selected = "Temperature anomaly",
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
                       tags$div(pickerInput("select_year",
                                            label = "Select Year:",
                                            choices = unique(sub(".*_", "", names(climate_data)[-ncol(climate_data)])),
                                            selected = NULL,
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
                       tags$div(pickerInput("select_month",
                                            label = "Select Month:",
                                            choices = unique(sub(".*[_]([^.]+)[_].*", "\\1", names(climate_data)[-ncol(climate_data)])),
                                            selected = NULL,
                                            multiple = F,
                                            options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                       ), style="display:inline-block"),
  
                       actionButton("run", "Show result"),
                       div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 200px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                           leafglOutput("map",width ="100%", height = "100%")),
                       
                       
             ) # end main panel  
    ), # tab 1 end 
    tabPanel("Surface Water",
             mainPanel(
               htmlOutput("frame"),
               em("Source: Joint Research Centre (JRC)"),
             ) # end mainpanel 2   
    ), # End table 2
    tabPanel("University of Mosul Water Scarcity Model",
             sidebarPanel(
             #mainPanel(
               width = 5,
               tags$style(type='text/css', ".selectize-dropdown-content {max-height: 100px; }"), 
               tags$div(pickerInput("select_district",
                                    label = "Select District:",
                                    choices = lapply(split(model_geography_list$district, model_geography_list$governorate), as.list),
                                    selected = "Al-Baaj",
                                    width = 250,
                                    multiple = F,
                                    options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)),
                                    style="display:inline-block"),
               br(),
               
               tags$div(sliderTextInput("select_dates",
                                    label = "Select Date Range:",
                                    width = 250,
                                    choices = unique(model_data$date),
                                    selected = date_choices[c(1876, length(date_choices))]),
                                    style="display:inline-block"),
               tags$div(downloadButton("download_data", "Download Data")),
               leafletOutput("map_model", width = "105%", height =  500)),
             mainPanel(width = 7,
                       plotlyOutput("plot",  height = 575)
        
             ) # end mainpanel 2   
    ),
     # end main panel  
    # End table 3
  ) # end navar page
) # end fluid page
# server ------------------------------------------------------------------
server <- function(input, output,session){

  selected_col <- eventReactive(input$run, {
    col_name <- paste(input$select_climate_indicator, input$select_month, input$select_year, sep = "_")
    # check if the selected column exists in the data frame
    if (col_name %in% colnames(climate_data)) {
      return(col_name)
    } else {
      # show warning message if column does not exist
      showNotification("Indicator is not available for date selected", type = "warning")
      return(NULL)
    }
  })
  
  grid_with_df1 <- eventReactive(input$run, {
    req(selected_col())
    climate_data %>%
      select(selected_col(), geometry) %>%
      rename(value2 = !!selected_col())
  })
  
  indicator_selection_map <- eventReactive(input$run, {input$select_climate_indicator})
  month_selection_map <- eventReactive(input$run, {input$select_month})
  year_selection_map <- eventReactive(input$run, {input$select_year})

  model_data_filtered <- reactive({ 
    model_data %>%
      filter(district == input$select_district,
             date >= input$select_dates[1],
             date <= input$select_dates[2])})
  
  #values for coloring ------------------------------------------------------
  # color -------------------------------------------------------------------
  clr <- eventReactive(input$run,{
    if(input$select_climate_indicator %in%  c("Precipitation deficit","12-months SPI","9-months SPI","3-months SPI")) {
      bin <- c(-Inf, -2, -1.5, -1, -.5, .5, 1, 1.5, 2, Inf)
      clr <- colorBin(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in%  c("Evaporation anomaly")) {
      bin <- c(-Inf, -1.5, -1, -.5, .5, 1, 1.5, Inf)
      clr <- colorBin(c("#762a83", "#af8dc3" ,"#e7d4e8", "#FFFFE7","#d9f0d3", "#7fbf7b","#1b7837"),
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Temperature anomaly")) {
      bin <- c(-Inf,-2,-1,-.5,.5,1,2,Inf)
      clr <- colorBin(c("#367BB3", "#729DC6" , "#A1BCD7", "#FFFFE7","#eac435","#f3b700","#dd7230"),
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {
      bin <- c(-Inf,-2,-1,1,2,Inf)
      clr <- colorBin(c("#f2022a", "#fc4765", "#FFFFE7", "#71f575","#00971B"), 
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Surface Temperature")) {
      bin <- c(-Inf,20,30,40,50,Inf)
      clr <- colorBin(c("#367BB3", "#A1BCD7", "#FFFFE7","#eac435", "#dd7230"),
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    if(input$select_climate_indicator %in% c("Precipitation")) {
      bin <- c(-Inf, .0001, 15, 30, 45, 60, 75, 90, 105, Inf)
      clr <- colorBin(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
                      domain =  grid_with_df1()$value2, bins = bin)
      return(clr)}
    })
  # legend ------------------------------------------------------------------
  add_legend_df <- eventReactive(input$run,{
    if(input$select_climate_indicator %in%  c("Precipitation deficit")) {
      add_legend_df <- list(
        color = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
        label = c("-2 and less (drier conditions)", "-2 to -1.5","-1.5 to -1","-1 to -.5", "-.5 to +.5 (normal)", "+0.5 to +1", "+1 to +1.5", "+1.5 to +2", "+2 and above (wetter conditions)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in%  c("12-months SPI","9-months SPI","3-months SPI")) {
      add_legend_df <- list(
        color = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
        label = c("-2 and less (greater potential for drought)", "-2 to -1.5","-1.5 to -1","-1 to -.5", "-.5 to +.5 (normal)", "+0.5 to +1", "+1 to +1.5", "+1.5 to +2", "+2 and above (excess rainfall)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in%  c("Evaporation anomaly")) {
      add_legend_df <- list(
        color = c("#762a83", "#af8dc3" ,"#e7d4e8", "#FFFFE7","#d9f0d3", "#7fbf7b","#1b7837"),
        label = c("-1.5 and less (reduced evaporation)","-1 to -1.5,","-.5 to -1", "-.5 to +0.5 (normal)", "+.5 to +1","+1 to +1.5","+1.5 and above (increased evaporation)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Temperature anomaly")) {
      add_legend_df <- list(
        color = c( "#367BB3", "#729DC6", "#A1BCD7", "#FFFFE7", "#eac435", "#f3b700", "#dd7230"),
        label = c("Less than -2 (cooler conditions)","-2 to -1","-1 to -.5", "-.5 to .5 (normal)",".5 to 1","1 to 2","Greater than 2 (warmer conditions)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("NDWI anomaly","NDVI anomaly")) {
      add_legend_df <- list(
        color = c("#f2022a", "#fc4765", "#FFFFE7", "#71f575", "#00971B"),
        label = c("Less than -2 (reduced vegetative health)","-2 to -1","-1 to 1 (normal)", "1 to 2", "Greater than 2 (improved vegetative health)"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Surface Temperature")) {
      add_legend_df <- list(
        color = c("#367BB3", "#A1BCD7", "#FFFFE7","#eac435", "#dd7230"),
        label = c("<20°C","20-30°C","30-40°C", "40-50°C", "50+°C"))
      return(add_legend_df)}
    if(input$select_climate_indicator %in% c("Precipitation")) {
      add_legend_df <- list(
        color = c("#d73027", "#f46d43", "#fdae61", "#fee090", "#FFFFE7", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4"),
        label = c("0mm","0-15mm","15-30mm", "30-45mm", "45-60mm", "60-75mm", "75-90mm", "90-105mm", "105+mm"))
      return(add_legend_df)}
  })
  ##############################################################################
  output$map <-  renderLeaflet({
    base_map %>%  
      leafgl::addGlPolygons(data = grid_with_df1(),
                            fillOpacity = 1,
                            color =   ~clr()(grid_with_df1()$value2),
                            stroke = F,
                           popup = paste(
                             "Date:", month_selection_map(), year_selection_map(), "<br>",
                             #"Governorate:", admin1_boundary$ADM1_EN, "<br>",
                             #"District:", admin2_boundary$ADM2_EN, "<br>",
                             "Indicator:", indicator_selection_map(), "<br>",
                             "Value:",  grid_with_df1()$value2))  %>%
      setView(lat = 33.312805,lng = 44.361488,zoom = 6) %>% 
      addMiniMap() %>% 
      addLegend(
        colors = add_legend_df()$color,
        labels =add_legend_df()$label,
        opacity = 1, 
        title = add_legend_df()$unit) 
  }) 
  

irrigation_label <- reactive({
  admin2_irrigation %>% 
    filter(ADM2_EN == input$select_district)
    })
  
  output$map_model <- renderLeaflet({
    
    district_boundary <- admin2_boundary %>%
      filter(ADM2_EN == input$select_district)
    
    model_base_map %>% 
      setView(lat = 32.912805,lng = 43.811488,zoom = 6) %>% 
      addPolygons(data = district_boundary, color = "blue",
                  weight = 5, fillColor = "transparent",
                  popup = paste(
                    "District:", irrigation_label()$ADM2_EN, "<br>",
                    "2023 Mean Shortfall:", irrigation_label()$mean_irrigation)
      )
  })
  
  model_data_download <- reactive({
    model_data_filtered() %>%
      select(governorate, district, date, type, indicator, value)})
  
  
  # Downloadable csv of selected dataset ----
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$select_district, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(model_data_download(), file, row.names = FALSE)
    }
  )
  
  fig_title <- 'Water Scarcity Indicators — <span style = "color:#2057a7"><b>Historical</b></span> and <span style = "color:#e07653"><b>Model-Forecasted</b></span> Figures,'
  
  output$plot <- renderPlotly({
    plot_assigned <- 
      ggplot(model_data_filtered(), aes(x = date, y = value, group = indicator, color = type_color, fill = type_color, text = paste0("Date: ", date, "\n", value_label))) + 
      geom_area(alpha = .5, aes(x=date, y=ifelse(type %in% c("forecast", "data"), value, NA_real_)), fill = denim_light)+
      geom_area(alpha = .5, aes(x=date, y=ifelse(type=="forecast", value, NA_real_)), fill = burnt_sienna_light)+
      geom_line() +
      geom_point(fill = "white",
                 shape = 21,
                 size = .75,
                 stroke= .75) +
      facet_wrap(~indicator, ncol = 1, scales = "free_y")+
      facet_wrap(~indicator, ncol = 1, scales = "free_y")+
      labs(x = NULL, y = NULL) +
      si_style_ygrid()+
      scale_fill_identity()+
      scale_color_identity()+
      ggh4x::facetted_pos_scales(y = list(
        indicator == "Evapotranspiration (mm)" ~ scale_y_continuous(breaks = c(0, 50, 100, 150, 200)),
        indicator == "Rainfall (mm)" ~ scale_y_continuous(breaks = c(0, 50, 100, 150, 200)),
        indicator == "Temperature °C" ~ scale_y_continuous(breaks = c(0, 10, 20, 30, 40))))+
      theme(legend.position = "none", 
            panel.spacing = unit(.5, "lines"),
            plot.title = element_text(size = 14, family = "Calibri"),
            axis.text.x  = element_text(size = 11, family = "Calibri"),
            axis.text.y  = element_text(family = "Calibri" ),
            strip.text = element_text(size=12, family = "Calibri"))
    
    ggplotly(plot_assigned, 
             #tooltip = c("group", "x", "y", "irrigation_consumptive_use")
             tooltip = c("text", "text1", "text2")) %>% 
      plotly::layout(title = list(text = paste('<span style="font-size: 16px;">',
                                               " ", fig_title, input$select_district,
                                               '</span>',
                                               '<br>',
                                               '<span style="font-size: 10px;">', '<i>',
                                               "  Prediction model developed independently by The University of Mosul",
                                               '</i>', '</span>'),
                                  x = 0)) %>%
      plotly::layout(margin = list(l = 0, r = 0, t = 125, b = 0),
                     hovermode = 'closest') %>% 
      plotly::style(hoverinfo = "skip", traces = c(1,2,3, 4, 5, 6))
  })#, res = 96)
  
  
  frame <- tags$iframe(src="https://global-surface-water.appspot.com/map", style="height: 100vh;",scrolling = 'no',width="150%", frameborder = "0")
  output$frame <- renderUI({
    frame
  })
}
## App 
shinyApp(ui = ui, server = server)

