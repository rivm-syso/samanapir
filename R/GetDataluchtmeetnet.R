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
#' TEST <-GetLMLallstatinfoAPI()
GetLMLallstatinfoAPI <- function(){
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
  # Initialisatie

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de ststionsinformatie ----
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

#' GetLMLstatdataAPI
#'
#'Functie om de meetwaardes van een luchtmeetstation op te halen voor een bepaalde periode
#'van de Luchtmeetnet API. Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @param station : string met stationsnummer bijv. NL01908
#' @param ymd_vanaf : string met de datum van het begin van de periode bijv.
#' @param ymd_tot : string met de datum van het eind van de periode bijv.
#'
#' @return metingen_df: dataframe met de kolommen:
#'       formula: het gemeten component, bijv. NO2
#'       value: de gemeten concentratie microgram per kubieke meter
#'       timestamp_measured: tijd in UTC (Eindtijd van het uurgemiddelde)
#'       station_number: het nummer/id van het station bijv. NL01908
#' @export
#'
#' @examples
#' TEST <- GetLMLstatdataAPI("NL01908", "20190505", "20190510")
GetLMLstatdataAPI <- function(station, ymd_vanaf, ymd_tot){
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de meetgegevens ----
  ymd_vanaf <- as.POSIXct(ymd_vanaf, format="%Y%m%d", tz="UTC")
  ymd_tot <- as.POSIXct(ymd_tot, format="%Y%m%d", tz="UTC")
  ymd_tot_extra <- ymd_tot + 60*60*24*7 # neem een week extra, voor de cut functie

  # Deel de tijdsperiode op in weken, de api kan maar 1 week data leveren per keer
  week_opdeling <- cut(c(ymd_vanaf, ymd_tot_extra),"weeks")

  # Ga elke week af en haal de gegevens op
  for(index_week in seq(1,length(levels(week_opdeling))-1)){
    # Stel de URL samen van de week en de stationnummer
    # print(levels(week_opdeling)[index_week])
    startdatum <- format(as.POSIXct(levels(week_opdeling)[index_week], format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    einddatum <- format(as.POSIXct(levels(week_opdeling)[index_week+1], format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    url_week <- paste("https://api.luchtmeetnet.nl/open_api/measurements?station_number=",station,"&start=",startdatum,"&end=",einddatum, sep="")
    url_week <- gsub(" ","T", url_week) # Spaties mogen niet in de api, dan krijg je geen resultaat terug.
    # print(url_week)
    # Haal de gegevens op
    content_measurements <- GetAPIDataframe(url_week)

    if (length(content_measurements) == 1 | ! is.data.frame(content_measurements$data)) {
      # Dan is er een error teruggekomen, bijvoorbeeld 502 OF
      # Het kan zijn dat een station voor de gekozen periode geen data levert.
      print(content_measurements)
      # stat_info_df$error <- "Error in gegevens ophalen. Check of alle gegevens er zijn."
      next
    } else{
      # substract het stuk data
      measurements_data <- content_measurements$data
      # Zet de tijd om naar POSTXct in de UTC tijdszone
      measurements_data$timestamp_measured <- as.POSIXct(sub('T',' ', measurements_data$timestamp_measured), tz='UTC')

      # Voeg de data aan de dataframe
      metingen_df <- rbind(metingen_df, measurements_data)
    }
  }

  # Filter de data dat alleen de gegevens van de gevraagde periode erbij zitten
  metingen_df <- dplyr::filter(metingen_df, timestamp_measured <= ymd_tot & timestamp_measured >= ymd_vanaf )

  print(paste0("Data van luchtmeetnet opgehaald van station: ", station))
  return(metingen_df)
}


#' GetLMLAPI
#'Functie om de gegevens van een luchtmeetnetstation
#' voor een bepaalde periode op te halen van de Luchtmeetnet API.
#' Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @param station : string met stationsnummer bijv. NL01908
#' @param ymd_vanaf : string met de datum van het begin van de periode bijv.
#' @param ymd_tot : string met de datum van het eind van de periode bijv.
#'
#' @return named list met:
#'   info: dataframe met de kolommen lon, lat, type, naam, id, error
#'   data: dataframe met de kolommen :
#'       formula: het gemeten component, bijv. NO2
#'       value: de gemeten concentratie microgram per kubieke meter
#'       timestamp_measured: tijd in UTC (Eindtijd van het uurgemiddelde)
#'       station_number: het nummer/id van het station bijv. NL01908
#' @export
#'
#' @examples
#' TEST <- GetLMLAPI("NL01908", "20190505", "20190510")
GetLMLAPI <- function(station, ymd_vanaf, ymd_tot){
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de stationsinformatie ----
  stat_info_df <- GetLMLstatinfoAPI(station)

  ## Ophalen van de meetgegevens ----
  metingen_df <- GetLMLstatdataAPI(station, ymd_vanaf, ymd_tot)

  # Maak een named list voor de output
  lml_info_data <- list(info=stat_info_df, data=metingen_df)

  print(paste0("Meetgegevens en informatie van luchtmeetnet opgehaald van station: ", station))
  return(lml_info_data)
}
