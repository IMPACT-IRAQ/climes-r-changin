#' ee_rolling_statistic
#'
#' @param x tidyee object
#' @param stat \code{character} rolling statistic e.g mean (default), median, min, max, sd
#' @param window \code{numeric} window size for rolling statistic
#' @param time_unit \code{character} time unit for statistic e.g "month" (default),"day"
#' @return tidyee containing ee$ImageCollection where each image represents the right aligned rolling statistics specified
#' @export
#'
#' @examples \dontrun{
#' library(tidyrgee)
#' library(rgee)
#' library(dplyr)
#' ee_Initialize()
#' chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
#' chirps <- ee$ImageCollection(chirps_link)
#' chirps_tidy <- as_tidyee(chirps)
#' rolling_tidyee_collection <- ee_rolling_statistic(x=chirps_tidy,stat="mean", window=3,time_unit="month")
#' }
ee_rolling_statistic <-  function(x,stat, window,time_unit){
  assertthat::assert_that(time_unit %in% c("day","month","year"))
  dates_to_map <- get_roll_dates(x, window,time_unit)
  # band_names_x <- tidyrgee:::vrt_band_names(x)
  ee_reducer <- tidyrgee:::stat_to_reducer_full(fun = stat)
  ic <- x$ee_ob
  summarised_composite_ic = ee$ImageCollection$fromImages(
    dates_to_map$map( rgee::ee_utils_pyfunc(function (monthly_sum){
      
      startTime <-  ee$Date(monthly_sum)$advance(ee$Number$parse(as.character(window))$multiply(-1), time_unit)
      endTime <- ee$Date(monthly_sum)
      filtered_ic <-  ic$filterDate(startTime, endTime)
      imageAmount = filtered_ic$size()
      summary_composite_img = ee_reducer(filtered_ic)$
        set('Used_Images', imageAmount)$
        set('Start_Date', ee$Date(filtered_ic$first()$get('system:time_start')))$
        set('End_Date', ee$Date(filtered_ic$limit(1, 'system:time_end', FALSE)$first()$get('system:time_end')))$
        set('system:time_start', filtered_ic$first()$get('system:time_start') )$
        set('system:time_end', filtered_ic$limit(1, 'system:time_end', FALSE)$first()$get('system:time_end'))$
        set('month_end',ee$Date(filtered_ic$limit(1, 'system:time_end', FALSE)$first()$get('system:time_end'),'month'))
      
      time <- ee$Date(summary_composite_img$get('system:time_end'))$
        difference(ee$Date(summary_composite_img$get('system:time_start')), time_unit)$
        round()
      
      summary_composite_img_w_props <-  summary_composite_img$set('Observed_Months', time)
      
      return(
        ee$Algorithms$If(
          time$gte(ee$Number$parse(as.character(window))),
          summary_composite_img_w_props)
      )
    }
    )
    )
  )
  
  
  summarised_composite_ic = ee$ImageCollection(summarised_composite_ic$copyProperties(ic))
  return(summarised_composite_ic |> tidyrgee::as_tidyee())
  
  
}


#' get_roll_dates
#'
#' @param x tidyee object
#' @param window \code{numeric} time window to calculate rolling statistic on. Currently can only roll right
#' @param time_unit \code{character} unit of temporal aggregation (default = month).
#'
#' @return `ee$List` composed of `ee$Date` ready for using with ee$List$map
#' @export
#'
#' @examples \dontrun{
#' library(tidyrgee)
#' library(rgee)
#' library(dplyr)
#' ee_Initialize()
#' chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
#' chirps <- ee$ImageCollection(chirps_link)
#' chirps_tidy <- as_tidyee(chirps)
#' dates_to_map <- get_roll_dates(x=chirps_tidy, window=3,time_unit="month")
#' }
get_roll_dates <-  function(x, window,time_unit){
  
  # get all unique dates floored to month
  date_vec <- x$vrt |>
    dplyr::arrange(time_start) |>
    dplyr::pull(time_start) |>
    lubridate::as_date() |>
    lubridate::floor_date(unit =time_unit) |>
    unique()
  
  # can only lag to the month_lag + 1 th earliest record
  roll_dates <- date_vec[-c(1:window)] |> as.character()
  roll_dates_ee <- ee$List(roll_dates)$map(
    rgee::ee_utils_pyfunc(
      function(date){
        ee$Date(date)
        
      }
    )
  )
  return(roll_dates_ee)
  
}




#' ee_rolling_statistic2
#'
#' @param x tidyee object
#' @param stat \code{character} rolling statistic e.g mean (default), median, min, max, sd
#' @param window \code{numeric} window size for rolling statistic
#' @param time_unit \code{character} time unit for statistic e.g "month" (default),"day"
#' @return tidyee containing ee$ImageCollection where each image represents the right aligned rolling statistics specified
#' @export
#'
#' @examples \dontrun{
#' library(tidyrgee)
#' library(rgee)
#' library(dplyr)
#' ee_Initialize()
#' chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
#' chirps <- ee$ImageCollection(chirps_link)
#' chirps_tidy <- as_tidyee(chirps)
#' rolling_tidyee_collection <- ee_rolling_statistic(x=chirps_tidy,stat="mean", window=3,time_unit="month")
#' }
ee_rolling_statistic2 <-  function(x=monthly_rainfall,
                                   stat="sum",
                                   window,
                                   time_unit="month",
                                   return_tidyee=T){
  assertthat::assert_that(time_unit %in% c("day","month","year"))
  dates_to_map <- get_roll_dates(x, window,time_unit)
  # band_names_x <- tidyrgee:::vrt_band_names(x)
  name_suffix <-  glue::glue("_roll{stat}{window}")
  ee_reducer <- tidyrgee:::stat_to_reducer_full(fun = stat)
  ic <- x$ee_ob
  summarised_composite_ic = ee$ImageCollection$fromImages(
    dates_to_map$map( rgee::ee_utils_pyfunc(function (monthly_sum){
      
      startTime <-  ee$Date(monthly_sum)$advance(ee$Number$parse(as.character(window))$multiply(-1), time_unit)
      endTime <- ee$Date(monthly_sum)$advance(1,"day")
      filtered_ic <-  ic$filterDate(startTime, endTime)
      imageAmount = filtered_ic$size()
      summary_composite_img = ee_reducer(filtered_ic)$
        set('Used_Images', imageAmount)$
        set('Start_Date', ee$Date(filtered_ic$first()$get('system:time_start')))$
        set('End_Date', ee$Date(filtered_ic$limit(1, 'system:time_end', FALSE)$first()$get('system:time_end')))$
        
        # get time_start of last image in IC
        set('system:time_start', filtered_ic$limit(1, 'system:time_start', FALSE)$first()$get('system:time_start'))$
        set('system:time_end', filtered_ic$limit(1, 'system:time_end', FALSE)$first()$get('system:time_end'))$
        set("month",ee$Date(filtered_ic$
                              limit(1, 'system:time_start', FALSE)$first()$get('system:time_start'))$
              getRelative("month","year")$add(1)
        )
      
      
      
      time <- ee$Date(summary_composite_img$get('End_Date'))$
        difference(ee$Date(summary_composite_img$get('Start_Date')), time_unit)$
        round()
      
      summary_composite_img_w_props <-  summary_composite_img$set('Observed_n', time)
      
      return(
        ee$Algorithms$If(
          time$gte(ee$Number$parse(as.character(window))),
          summary_composite_img_w_props)
      )
    }
    )
    )
  )
  
  
  summarised_composite_ic = ee$ImageCollection(summarised_composite_ic)
  
  if("ee.imagecollection.ImageCollection"%in%class(summarised_composite_ic) ){
    bnames <- summarised_composite_ic$first()$bandNames()$getInfo()
    replace_rgx <- glue::glue("_{stat}$")
    new_bnames <- str_replace_all(bnames,replace_rgx,name_suffix)
    ee_ob_renamed <- summarised_composite_ic$map(
      function(img){
        img$rename(new_bnames)
      }
    )
    if(return_tidyee)
      output <-  as_tidyee(ee_ob_renamed,time_end = T)
    
    if(!return_tidyee){
      output <- ee_ob_renamed
    }
  }
  
  return(output)
  
}






ee_spi <- function(x,
                   window=6,
                   window_unit="month",
                   ic_time_unit= "month",
                   band="precipitation_sum",
                   moi=c(3,4),use_tidyee=T
){
  
  assertthat::assert_that(window_unit %in% c("day","month"),
                          msg = "currently SPI must be formulated as in 'day' or 'month'" )
  
  assertthat::assert_that(ic_time_unit %in% c("day","month"),
                          msg = "imageCollection must be in 'day' or 'month' time steps for the SPI calculation ot take place" )
  new_spi_band_name<- glue::glue("{band}_{window}_{window_unit}_SPI")
  spi_mos <- glue::glue('2020-{moi}-01') |>
    lubridate::as_date() |>
    purrr::map(~glue::glue_collapse(lubridate::month(.x-months(1:window-1) |> rev(),label=T, abbr=T),sep="-"))
  if(length(spi_mos)>1){
    spi_mos <- glue::glue_collapse(spi_mos,sep = " and\n")
  }
  cat(glue::glue("calculating {window} month SPI with \n{spi_mos} \n"))
  
  rolling_ee<- ee_rolling_statistic2(x = x,
                                     stat = "sum",
                                     window = window,
                                     time_unit = window_unit,
                                     return_tidyee = F)
  
  # this could speed this up by skipping this step and using `rgee` + mapping on month
  # think I should doit both ways and compare?
  if(use_tidyee){
    rolling_tidyee <-  tidyrgee::as_tidyee(rolling_ee)
    
    baseline_rollling_tidyee <- rolling_tidyee |>
      group_by(month) |>
      summarise(stat= list("mean","sd"))
    
    current_and_historical_rolling <- rolling_tidyee |>
      filter(month%in%moi) |>
      inner_join(baseline_rollling_tidyee, by="month")
    tidyrgee:::vrt_band_names(current_and_historical_rolling)
    
    rolling_band_name <- glue::glue("{band}_rollsum{window}")
    
    ic_spi<- current_and_historical_rolling$ee_ob$map(
      function(img){
        zscore<- img$expression(
          "float((precip_current-precip_baseline_mean)/(precip_baseline_sd))",
          opt_map= list(precip_current= img$select(rolling_band_name),
                        
                        precip_baseline_mean= img$select(glue::glue("{rolling_band_name}_mean")),
                        precip_baseline_sd= img$select(glue::glue("{rolling_band_name}_sd"))
          )
        )$rename(new_spi_band_name)
        img$select(rolling_band_name)$addBands(zscore)
      }
      
    )
    ic_spi <- ic_spi$select(new_spi_band_name)
    
  }
  return(as_tidyee(ic_spi))
}



ee_chirps_spi <- function(x=NULL,
                          window,
                          window_unit,
                          moi,
                          .load_chirps=T,
                          band="precipitation"
){
  
  assertthat::assert_that(window_unit %in% c("day","month"),
                          msg = "currently SPI must be formulated as in 'day' or 'month'" )
  if(is.null(x)){
    cat("no tidyee object loaded, CHIRPS daily being loaded from GEE\n")
    .load_chirps <- T
  }
  if(.load_chirps){
    chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
    ic<- rgee::ee$ImageCollection(chirps_link)
    cat("loading tidyee object (takes a few seconds)\n")
    x <-  as_tidyee(ic)
  }
  
  x_year_month <-  x |>
    group_by(year, month) |>
    summarise(
      stat="sum"
    )
  new_band_name <-  glue::glue("{band}_sum")
  
  cat("beginning SPI func - expect 2-3 minutes\n")
  spi_tidyee <- ee_spi(x=x_year_month,
                       window=window,
                       window_unit=window_unit,
                       ic_time_unit= "month",
                       band=new_band_name,
                       moi=moi,
                       use_tidyee=T)
  
  return(spi_tidyee)
  
}
