#' GetAPIDataframe
#'
#' Deze functie roept de API aan met input url
#' haalt de inhoud op als text, dit is een JSON
#' pakt de Json uit
#' en returnd vervolgens de opgehaalde gegevens in een dataframe
#'
#' @param url_api : string met een url van een API
#'
#' @return dataframe met de content van de url
#'
#' @examples
#' TEST <- GetAPIDataframe("https://api-samenmeten.rivm.nl/v1.0/Things?")
GetAPIDataframe <- function(url_api){
  # Parameters voor de check op errors
  nog_eens_opvragen <- TRUE
  counter <- 1

  # While voor de check op errors, als de server iets heeft, wil dat niet zeggen
  # dat de data er niet is. Dan gewoon nog eens aan server vragen
  while (nog_eens_opvragen){
    # Vraag de URL op via API
      # log_trace("GetAPIDataframe: Getting url {url_api}")
    raw_result <- httr::GET(url=url_api)
      # log_trace("GetAPIDataframe: Getting url, got result {raw_result$status_code}")

    # Check de status: is de api correct?
    # Er is iets met de server, gewoon nog eens proberen
    # Tenzij er iets met de data (bijv. je vraag een project op dat niet bestaat)
    # Daarvoor zit een counter dat je niet eeuwig in de while loop kunt blijven
    if (raw_result$status_code != 200){
      # log_trace("GetAPIDataframe: got error status code {raw_result$status_code}")
      if (counter < 10){
        counter <- counter + 1
        nog_eens_opvragen <- TRUE}
      # Als er na 100 keer nog geen antwoord gevonden is, return de error
      else{
          # log_error("GetApiDataframe ERROR: server responded with error code {raw_result$stats_code}")
          return(paste('Error', raw_result$status_code, sep=":"))
      }
    }
    # Het is gelukt, verwerk de data
    else{
      # Uitpakken van de content en omzetten naar list met dataframe
      raw_content <- rawToChar(raw_result$content)
      content <- jsonlite::fromJSON(raw_content)

      # Check of er wel content is:
      if(length(content[[1]])<1){
        # log_error("GetApiDataframe ERROR: JSON content is empty")
        return('Error: Er is geen content gevonden.')
      }
      else{return(content)}
    }
  }
}
