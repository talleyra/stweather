#' @title Function for retrieving data from weather API
#' @param start_date a start_date
#' @param end_date an end_date
#' @param scode a sensor code see, sensors dataset
#' @param type a measurement specification, see sensors dataset
#' @importFrom lubridate today
#' @importFrom stringr str_remove_all
#' @importFrom readr read_csv
#' @importFrom glue glue
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd_hms
#' @importFrom glue glue
#' @export


get_measurement_data <- function(start_date, end_date, scode, type){

  date_from = start_date %>% str_remove_all("-")
  date_to   = end_date %>% str_remove_all("-")

  md <- get_metadata(type, scode)

  out <- read_csv(glue::glue("http://daten.buergernetz.bz.it/services/meteo/v1/timeseries?",
                "station_code={scode}&sensor_code={type}&date_from={date_from}&date_to={date_to}",
                "&output_format=CSV")) %>%
    rename_with(tolower) %>%
    mutate(date = ymd_hms(date)) %>%
    mutate(measurement = md$measurement,
           location = md$station)

  return(out)
}

