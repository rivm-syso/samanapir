#' GetLMLAPI2
#' Functie om de gegevens van een luchtmeetnetstation
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
#' TEST <- GetLMLAPI2("NL01908", "20260505", "20260510")
GetLMLAPI2 <- function(station, ymd_vanaf, ymd_tot){
  # Initialisatie
  # Maak een dataframe om de meetgegevens in op te slaan
  # Dit is een longformat
  metingen_df <- data.frame()

  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  ## Ophalen van de stationsinformatie
  stat_info_df <- GetLMLstatinfoAPI2(station)

  ## Ophalen van de meetgegevens
  metingen_df <- GetLMLstatdataAPI2(station, ymd_vanaf, ymd_tot)

  # Maak een named list voor de output
  lml_info_data <- list(info=stat_info_df, data=metingen_df)

  # Logging
  logger::log_debug(paste0(
    "Meetgegevens en informatie van luchtmeetnet opgehaald van station: ",
    station))

  return(lml_info_data)
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
  .Deprecated("GetLMLAPI2")
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
