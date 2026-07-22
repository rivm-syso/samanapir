#' Get observations from Samen Meten API
#'
#' Get from the Samen Meten API the observations from a given sensor and
#' measured parameter.
#'
#' @param datastream_id string, id from the datastream
#' @param kit_id string, id/name from the sensor
#' @param ymd_from string, date from which data will be obtained in format yyyymmdd
#' @param ymd_to string, date to which data will be obtained in format yyyymmdd
#'
#' @return dataframe with the columns: kit_id (string), timestamp(posixct UTC),
#' parameter (string), value(numeric)
#'
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIobs2("31508","LTD_55101","20220101","20220103")
GetSamenMetenAPIobs2 <- function(datastream_id, kit_id, ymd_from, ymd_to){
  # start from the function
  start_time <- Sys.time()
  # Create url for the measuered parameter
  url_property <- paste("https://api-samenmeten.rivm.nl/v1.0/Datastreams(",
                        datastream_id,")/ObservedProperty", sep='')

  # Get the name of the datastream measured parameter
  logger::log_debug(paste0("GetSamenMetenAPIobs: requesting property data from url: ",
                           url_property))

  # Get observations (info) from API
  content_prop <- GetAPIDataframe2(httr2::request(url_property))

  # Check if succesfull
  if(is.null(content_prop)){
    logger::log_info(paste0("No data received from: ", url_property))
    return(NULL)
  }

  logger::log_debug(paste0("Data received from: ", url_property))

  # Get the observations (data) from API
  url_obs <- paste("https://api-samenmeten.rivm.nl/v1.0/Datastreams(",
                   datastream_id,")/Observations?$filter=phenomenonTime+gt+%27",
                   ymd_from,"%27+and+phenomenonTime+lt+%27",ymd_to,
                   "%27&$orderby=phenomenonTime",sep='')

  # Ophalen van de informatie in API
  content_obs <- GetAPIDataframe2(httr2::request(url_obs))

  #  Check of er daat is, als er geen info is komt er een NULL terug
  if(is.null(content_obs)){
    # logging
    logger::log_info(paste0("Geen data van url_obs: ", url_obs))
    # Return dan NULL
    return(NULL)
  }

  # Het werkt vaak met pagina's.
  # Ga elke pagina af en haal observaties op
  obs_data <- GetDataPaginationSamenMeten(content_obs)

  # Check if there are observations
  if(length(obs_data)<1){
    logger::log_info(paste0("GetDataPaginationSamenMeten, no data from: ",
                     url_obs))
    return(NULL)
  }

  logger::log_debug(paste0("GetAPIDatframe: data received from: ",
                           url_obs))

  # Store the observations in dataframe
  obs_data <- do.call(
    rbind,
    lapply(obs_data, function(x) {
      # vervang NULL door NA zodat alles dezelfde lengte heeft
      x[sapply(x, is.null)] <- NA
      as.data.frame(as.list(x), stringsAsFactors = FALSE)
    })
  )

  # Add kit_id and parameter, keep timestamp and value
  obs_data <- obs_data |>
    dplyr::mutate(
      timestamp = as.POSIXct(phenomenonTime,
                             format='%Y-%m-%dT%H:%M:%S',
                             tz='UTC'),
      value     = result,
      kit_id = kit_id,
      parameter = content_prop$name
    ) |> dplyr::select(c(timestamp, kit_id, value, parameter))

  # end from the function
  end_time <- Sys.time()
  logger::log_info("The download of the data took {end_time - start_time} seconds")

  # return the data
  return(obs_data)

}

#' Get observations from Samen Meten API
#'
#' Get from the Samen Meten API the observations from a given sensor and
#' measured parameter.
#'
#' @param datastream_id string, id from the datastream
#' @param kit_id string, id/name from the sensor
#' @param ymd_from string, date from which data will be obtained in format yyyymmdd
#' @param ymd_to string, date to which data will be obtained in format yyyymmdd
#'
#' @return dataframe with the columns: kit_id (string), timestamp(posixct UTC),
#' parameter (string), value(numeric)
#'
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIobs("31508","LTD_55101","20220101","20220103")
GetSamenMetenAPIobs <- function(datastream_id, kit_id, ymd_from, ymd_to){
  .Deprecated("GetSamenMetenAPIobs2")
  # start from the function
  start_time <- Sys.time()
  # Create url for the measuered parameter
  url_property <- paste("https://api-samenmeten.rivm.nl/v1.0/Datastreams(",
                        datastream_id,")/ObservedProperty", sep='')

  # Get the name of the datastream measured parameter
  logger::log_trace("GetSamenMetenAPIobs: requesting property data from url {url_property}")
  # Get from API
  tryCatch({
    content_prop <- GetAPIDataframe(url_property)
  }, error = function(e){
    # There could be a overload of the API server
    # Try again after 30 seconds
    Sys.sleep(3)
    # Get from API
    tryCatch({
      content_prop <- GetAPIDataframe(url_property)
    }, error = function(e){
      logger::log_error("GetSamenMetenAPIobs ERROR: GetAPIDataFrame returned error.")
      stop("GetSamenMetenAPIobs ERROR in URL")
    })
  })

  logger::log_debug(paste0("Data received from: ", url_property))

  # Create a dataframe to store the observations
  obs_data <- data.frame()

  # Get the observations
  url_obs <- paste("https://api-samenmeten.rivm.nl/v1.0/Datastreams(",
                   datastream_id,")/Observations?$filter=phenomenonTime+gt+%27",
                   ymd_from,"%27+and+phenomenonTime+lt+%27",ymd_to,
                   "%27&$orderby=phenomenonTime",sep='')

  # The API uses multiple pages, if there are, then get them all
  multiple_pages_obs <- TRUE

  # Get all sensors and there properties and further urls to the datastream properties and observations
  while(multiple_pages_obs){

    logger::log_info(paste0("Get data from url: ", url_obs))
    # Get from API
    tryCatch({
      content_obs <- GetAPIDataframe(url_obs)
      content_obs_df <- content_obs$value
    }, error = function(e){
      # There could be a overload of the API server
      # Try again after 30 seconds
      Sys.sleep(3)
      # Get from API
      tryCatch({
        content_obs <- GetAPIDataframe(url_obs)
        content_obs_df <- content_obs$value
      }, error = function(e){
        logger::log_error("GetSamenMetenAPIobs ERROR: GetAPIDAtaframe returned error")
        stop("GetSamenMetenAPIobs ERROR")
      })
    })

    logger::log_debug("GetAPIDatframe: data received from: {url_obs}, {nrow(content_obs_df)} records")

    # Store the observations, add kit_id and parameter
    obs_data <- obs_data |>
      dplyr::bind_rows(data.frame(
        kit_id = kit_id,
        parameter = content_prop$name,
        timestamp = as.POSIXct(content_obs_df$phenomenonTime, format='%Y-%m-%dT%H:%M:%S', tz='UTC'),
        value = content_obs_df$result
      ))

    # Check if there is another page to read
    if (length(content_obs)>1){
      url_obs <- content_obs[[1]]
      logger::log_trace("GetAPIDataframe, get next page")
    } else{
      multiple_pages_obs <- FALSE
      logger::log_trace("GetAPIDataframe, got last page")
    }
  }

  # end from the function
  end_time <- Sys.time()
  logger::log_info("The download of the data took {end_time - start_time} seconds")

  # return the data
  return(obs_data)

}
