#' GetLMLstatinfoAPI2
#'
#' Functie om van één bepaald luchtmeetnetstation de stationsinformatie op te halen
#' van de Luchtmeetnet API.
#' Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @param station : string met stationsnummer bijv. NL01908
#'
#' @return info: dataframe met de kolommen lon, lat, stattype, naam,
#' station_number, organisatie
#' @export
#'
#' @examples
#' TEST <- GetLMLstatinfoAPI2("NL01908")
GetLMLstatinfoAPI2 <- function(station){
  # Initialisatie
  # Dataframe waar alles mag worden opgeslagen
  stat_info_df <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                           c("station_number", "naam", "lat", "lon", "stattype",
                             "organisatie"))

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de stationsinformatie
  # URL van de specifieke LML station informatie
  url_stat_info <- paste0("https://api.luchtmeetnet.nl/open_api/stations/",
                         station, "/?page=1")
  # logging voor debugging
  logger::log_debug(paste0("Ophalen data van url_stat_info: ", url_stat_info,
                           "  ..."))

  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe2(httr2::request(url_stat_info))

  #  Check of er daar is, als er geen info is komt er een NULL terug
  if(is.null(content_stat_info)){
    # logging
    logger::log_info(paste0("Geen data van url_stat_info: ", url_stat_info))
    # Return dan NULL
    return(NULL)
  }else if(length(content_stat_info)>0){
    stat_info_df[1,"station_number"] <- station
    stat_coords <- content_stat_info$data$geometry$coordinates
    stat_info_df[1,"lon"] <- stat_coords[[1]]
    stat_info_df[1,"lat"] <- stat_coords[[2]]
    stat_info_df[1,"stattype"] <- content_stat_info$data$type
    stat_info_df[1,"naam"] <- content_stat_info$data$location
    stat_info_df[1,"organisatie"] <- content_stat_info$data$organisation

    # logging
    logger::log_debug(paste0("Data opgehaald van url_stat_info: ",
                             url_stat_info))
  }
  return(stat_info_df)
}


#' GetLMLstatinfoAPI
#'
#' Functie om van een bepaald luchtmeetnetstation de stationsinformatie op te halen
#' van de Luchtmeetnet API. Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @param station : string met stationsnummer bijv. NL01908
#'
#' @return info: dataframe met de kolommen lon, lat, stattype, naam, station_number
#' @export
#'
#' @examples
#' TEST <- GetLMLstatinfoAPI("NL01908")
GetLMLstatinfoAPI <- function(station){
  .Deprecated("GetLMLstatinfoAPI2")

  # Initialisatie

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de ststionsinformatie
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations/",station, sep="")
  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe(url_stat_info)
  # print(paste0("url gebruikt: ", url_stat_info))
  tryCatch({
    # Neem de info eruit die je nodig hebt:
    stat_componenten <- content_stat_info$data$components
    stat_coords <- content_stat_info$data$geometry$coordinates
    stat_type <- content_stat_info$data$type
    stat_naam <- content_stat_info$data$location
    stat_organ <- content_stat_info$data$organisation

    # Sla op in een dataframe
    stat_info_df <- data.frame(lon=stat_coords[1],lat=stat_coords[2], stattype=stat_type, naam=stat_naam, station_number=station, organisatie=stat_organ)
  }, error = function(e){
    stop("Error afkomstig van api luchtmeetnet.")
  })

  return(stat_info_df)
}
