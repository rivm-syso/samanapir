#' GetLMLstatdataAPI2
#'
#'Functie om de meetwaardes van een luchtmeetstation op te halen voor een
#'bepaalde periode van de Luchtmeetnet API.
#'Meer info op: https://api-docs.luchtmeetnet.nl/?version=latest
#'
#' @param station : string met stationsnummer bijv. NL01908
#' @param ymd_vanaf : string met de datum van het begin van de periode
#' bijv."20260505"#'
#' @param ymd_tot : string met de datum van het eind van de periode
#' bijv. "20260510"
#'
#' @return metingen_df: dataframe met de kolommen:
#'       formula: het gemeten component, bijv. NO2
#'       value: de gemeten concentratie microgram per kubieke meter
#'       timestamp_measured: tijd in UTC (Eindtijd van het uurgemiddelde)
#'       station_number: het nummer/id van het station bijv. NL01908
#'       NB geeft geheel leeg df terug wanneer geen connectie of geen data
#'       dim(metingen_df) == c(0,0)
#' @export
#'
#' @examples
#' TEST <- GetLMLstatdataAPI2("NL01495", "20260505", "20260510")
GetLMLstatdataAPI2 <- function(station, ymd_vanaf, ymd_tot){
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
    startdatum <- format(as.POSIXct(levels(week_opdeling)[index_week],
                                    format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    einddatum <- format(as.POSIXct(levels(week_opdeling)[index_week+1],
                                   format='%Y-%m-%d'), '%Y-%m-%d %H:%M:%S')
    url_week <- paste("https://api.luchtmeetnet.nl/open_api/measurements?station_number=",
                      station,"&start=",startdatum,"&end=",einddatum,
                      "&page=1",sep="")
    url_week <- gsub(" ","T", url_week) # Spaties mogen niet in de api, dan krijg je geen resultaat terug.

    # logging voor debugging
    logger::log_debug(paste0("Ophalen data van url_week: ", url_week, "  ..."))

    # Haal de gegevens op
    content_measurements <- GetAPIDataframe2(httr2::request(url_week))

    # Check of succesvolle call
    if(!is.null(content_measurements)){
      #Ga eventueel de andere pagina's af
      content_measurements <- GetDataPagination(content_measurements, url_week)
    }

    if (is.null(content_measurements)) {
      # Dan is er een error teruggekomen, bijvoorbeeld 502 OF
      # Het kan zijn dat een station voor de gekozen periode geen data levert.
      # logging voor als er geen data is
      logger::log_info(paste0("Geen data beschikbaar: ", url_week))
      next
    } else{
      # substract het stuk data
      measurements_data <- content_measurements
      # Zet de tijd om naar POSTXct in de UTC tijdszone
      measurements_data$timestamp_measured <-
        as.POSIXct(sub('T',' ', measurements_data$timestamp_measured), tz='UTC')

      # Voeg de data aan de dataframe
      metingen_df <- rbind(metingen_df, measurements_data)

      # Filter de data dat alleen de gegevens van de gevraagde periode erbij zitten
      metingen_df <- dplyr::filter(metingen_df,
                                   timestamp_measured <= ymd_tot &
                                     timestamp_measured >= ymd_vanaf )

      # logging voor debugging
      logger::log_debug(paste0("Data opgehaald van url_week: ", url_week))
    }
  }

  # logging
  logger::log_info(paste0("Data van luchtmeetnet opgehaald van station: ",
                          station))
  return(metingen_df)
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
  .Deprecated("GetLMLstatdataAPI2")
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
    do.call(rbind, lapply(content_measurements$data, as.data.frame))

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

      # Filter de data dat alleen de gegevens van de gevraagde periode erbij zitten
      metingen_df <- dplyr::filter(metingen_df, timestamp_measured <= ymd_tot & timestamp_measured >= ymd_vanaf )

    }
  }

  print(paste0("Data van luchtmeetnet opgehaald van station: ", station))
  return(metingen_df)
}
