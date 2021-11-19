#'@title retrieve metadata for query
#'@export

get_metadata <- function(type, scode){

  descr_point <- stweather::sensor_measurement %>%
    filter(TYPE == type) %>%
    pull(DESC_D)

  station_descr <- stweather::station_codes %>%
    filter(SCODE == scode) %>%
    pull(NAME_D)

  out <- list(measurement = descr_point,
              station     = station_descr)

  return(out)
}


