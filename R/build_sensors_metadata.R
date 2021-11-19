#' @title Get metadata for sensors
#' @description metadata is going to be used to make queries against the api. \cr
#' This function retrieves SCODE and TYPE for each sensor and thus indicates from \cr
#' what sensor we want to retrieve data and what datapoint we want to query.
#' @importFrom httr GET
#' @importFrom tibble tibble
#' @importFrom httr content
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @export


build_sensors_metadata <- function(){

   resp_stations <- GET("http://daten.buergernetz.bz.it/services/meteo/v1/stations")

   stations <- tibble(data = content(resp_stations)$features) %>%
     unnest_wider(data) %>%
     unnest_wider(properties) %>%
     select(-type) %>%
     unnest_wider(geometry) %>%
     select(-type) %>%
     select(-coordinates)

   resp_sensors <- GET("http://dati.retecivica.bz.it/services/meteo/v1/sensors")

   sensors <- tibble(data = content(resp_sensors)) %>%
     unnest_wider(data)


   out <- stations %>%
     left_join(sensors, by = "SCODE") %>%
     select(SCODE, TYPE, NAME_D, NAME_I, DESC_D, DESC_I, UNIT, everything())

   return(out)
}


