#' GetLMLallstatinfoAPI2
#'
#' Functie om van alle stations hun stationsinformatie op te halen
#' van de Luchtmeetnet API.
#' Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @return : dataframe met de kolommen:
#' station_number: id van het station
#' naam: naam van het stations
#' lat
#' lon
#' stattype: stationstype
#' organisatie: de organisatie van wie het station is
#'
#' When no data from API, but there was connection an empty dataframe will be returned
#' If there was no api connection NULL will be returned
#' @export
#'
#' @examples
#' \dontrun{
#' TEST <-GetLMLallstatinfoAPI2()
#' }
#'
GetLMLallstatinfoAPI2 <- function(){
  # Initialisatie
  # Dataframe waar alles mag worden opgeslagen
  stat_info_compleet <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                           c("station_number", "naam", "lat", "lon", "stattype",
                             "organisatie"))

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de ststionsinformatie
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations")

  #logging
  logger::log_debug(paste0("Data ophalen van: ", url_stat_info))

  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe2(httr2::request(url_stat_info))

  #  Check of er daat is, als er geen info is komt er een NULL terug
  if(is.null(content_stat_info)){
    # logging
    logger::log_info(paste0("Geen data van url_stat_info: ", url_stat_info))
    # Return dan NULL
    return(NULL)
  }

  # Het werkt met pagina's.
  # Ga elke pagina af en haal de id en naam van de stations op
  stat_info <- GetDataPagination(content_stat_info, url_stat_info)

  # Als er geen data is opgehaald, maar wel connectie was met de api
  if(length(stat_info) == 0){
    # return empty dataframe
    return(stat_info_compleet)
  }

  # Ga voor elk station ook de coordinaten ophalen
  for(station in stat_info$number){
    stat_info_single <- GetLMLstatinfoAPI2(station)

    # Wanneer er geen coordinaten zijn opgehaald, ga dan door naar volgende
    # station. Bijv wanneer api niet bereikt
    if(is.null(stat_info_single)){
      # logging
      logger::log_info(paste0("Geen data van stat_info_single: ", station))
      next()
      }
    stat_info_compleet <- rbind(stat_info_compleet, stat_info_single)
  }

  # logging
  logger::log_debug(paste0("Data opgehaald van url_stat_info: ", url_stat_info))

  return(stat_info_compleet)
}


#' GetLMLallstatinfoAPI
#'
#' Functie om van alle stations hun stationsinformatie op te halen
#' van de Luchtmeetnet API. Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#' @return : dataframe met de kolommen:
#' station_number: id van het station
#' naam: naam van het stations
#' lat
#' lon
#' stattype: stationstype
#' organisatie: de organisatie van wie het station is
#' @export
#'
#' @examples
#' \dontrun{
#' TEST <-GetLMLallstatinfoAPI()
#' }
GetLMLallstatinfoAPI <- function(){
  .Deprecated("GetLMLallstatinfoAPI2")

  # Initialisatie
  # Dataframe waar alles mag worden opgeslagen
  stat_info <- data.frame('id'=NULL,'naam'=NULL)
  stat_info_compleet <- data.frame('station_number'=NULL,'naam'=NULL, 'lat'=NULL, 'lon'=NULL, 'stattype'=NULL, 'organisatie'=NULL)

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de ststionsinformatie ----
  # URL van de specifieke LML station informatie
  url_stat_info <- paste("https://api.luchtmeetnet.nl/open_api/stations")
  # Ophalen van de informatie in API : station info
  content_stat_info <- GetAPIDataframe(url_stat_info)
  # print(paste0("url gebruikt: ", url_stat_info))

  #  Als er iets mis is aan de serverkant, dan komt er een error string uit
  if(purrr::is_character(content_stat_info)){
    # Return dan de lege stat_info_compleet
    return(stat_info_compleet)
  }

  # Het werkt met pagina's.
  # Ga elke pagina af en haal de id en naam van de stations op
  verschillende_pages <- content_stat_info$pagination$page_list
  for(pagina in verschillende_pages){
    url_page <- paste0(url_stat_info, '/?page=', pagina)
    # haal de gegevens van de url op
    content_stat_info <- GetAPIDataframe(url_page)
    # print(paste0("url gebruikt: ", url_page))

    # Maak dataframe waar je de gegevens van deze pagina tijdelijk opslaat
    stat_info_new <- NULL
    stat_info_new$id <- content_stat_info$data$number
    stat_info_new$naam <- content_stat_info$data$location

    # Voeg alles samen tot 1 dataframe
    stat_info <- rbind(stat_info, stat_info_new)
  }

  # Ga voor elk station ook de coordinaten ophalen
  for(station in stat_info$id){
    stat_info_single <- GetLMLstatinfoAPI(station)
    stat_info_compleet <- rbind(stat_info_compleet, stat_info_single)
  }

  return(stat_info_compleet)
}
