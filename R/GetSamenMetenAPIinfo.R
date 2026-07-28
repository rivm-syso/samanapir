#' GetSamenMetenAPIinfo2
#'
#' This function will obtain the information of each sensor in a particular
#' municipality or project. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param url_part string with the name of the project or municipality where
#'   you are interested in, in the format as the api can read.
#'   For project: project eq'Amersfoort'
#'   For municipality:  codegemeente eq '310'
#'
#' @return named list:
#' "sensor_data" =
#'  c("things_id", "kit_id", "project", "lat", "lon","knmicode","pm10closecode",
#'   "pm10regiocode", "pm10stadcode","pm25closecode","pm25regiocode",
#'   "pm25stadcode")
#'  "datastream_data" =
#'  c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs"))
#'
#'  NB When no data from API but connection: return empty df
#'  NB When no data from API becuase no connection: return NULL
#' @export
#'
#' @examples
#' TEST <- GetSamenMetenAPIinfo2("project eq'Amersfoort'")
GetSamenMetenAPIinfo2 <- function(url_part){
  # Create url
  url_things <- paste("https://api-samenmeten.rivm.nl/v1.0/Things?$filter=(properties/",
                      url_part,")&$expand=Locations,Datastreams", sep='')
  url_things <- gsub(' ','%20', url_things)

  # Get the things from API
  content_things <- GetAPIDataframe2(httr2::request(url_things))

  # Check if succesfull
  if(is.null(content_things)){
    logger::log_info(paste0("No connectio; no data received from: ", url_things))
    return(NULL)
  }

  # Check if there is any data
  if(length(content_things) == 0){
    logger::log_info(paste0("No data received from: ", url_things))
    return(data.frame())
  }

  # Het werkt vaak met pagina's.
  # Ga elke pagina af en haal observaties op
  things_data <- GetDataPaginationSamenMeten(content_things)

  # Check if succesfull
  if(is.null(things_data)){
    logger::log_info(paste0("No data received from: ", url_things))
    return(NULL)
  }

  # Create df to store the sensor info data
  sensor_data <- setNames(data.frame(matrix(ncol = 12, nrow = 0)),
                          c("things_id", "kit_id", "project", "lat", "lon",
                            "knmicode",
                            "pm10closecode", "pm10regiocode", "pm10stadcode",
                            "pm25closecode",
                            "pm25regiocode", "pm25stadcode"))

  # Create an empty dataframe to store the urls
  datastream_data <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
    c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs"))

  # Get the data of interest
  for(part in seq(1:length(things_data))){
    data_deel <- things_data[part]

    # Check if all the needed info is there
    info_checked <- try(chk::check_names(data_deel[[1]], names = c("@iot.id", "@iot.selfLink", "name", "description", "properties",
      "HistoricalLocations@iot.navigationLink", "Locations", "Datastreams"
    ), order = FALSE))

    if(class(info_checked) == "try-error"){
      logger::log_info(paste0("No complete data avalaible for: ",
                              data_deel[[1]]$name))
      next()
    }

    data_deel_df <- data.frame(
      'things_id' = data_deel[[1]]$'@iot.id',
      'kit_id' = data_deel[[1]]$'name',
      'project' = data_deel[[1]]$properties$project,
      'lat' = data_deel[[1]]$Locations[[1]]$location$coordinates[[1]],
      'lon' = data_deel[[1]]$Locations[[1]]$location$coordinates[[2]],
      'knmicode' = data_deel[[1]]$properties$knmicode,
      'pm10closecode' = data_deel[[1]]$properties$pm10closecode,
      'pm10regiocode' = data_deel[[1]]$properties$pm10regiocode,
      'pm10stadcode' = data_deel[[1]]$properties$pm10stadcode,
      'pm25closecode' = data_deel[[1]]$properties$pm25closecode,
      'pm25regiocode' = data_deel[[1]]$properties$pm25regiocode,
      'pm25stadcode' = data_deel[[1]]$properties$pm25stadcode)

    # Store the info about the sensor (meta-data)
    sensor_data <- rbind(sensor_data, data_deel_df)

    # Datastream info
    datastream_deel_df <- extract_datastream2(data_deel[[1]]$Datastreams) |>
      dplyr::mutate(kit_id = data_deel[[1]]$'name',)

    # Store the datastream info
    datastream_data <- rbind(datastream_data, datastream_deel_df)
  }

  logger::log_debug(paste0("All data obtained from: ", url_things))

  # Combine the sensordata and the datastream
  all_data_list <- list('sensor_data' = sensor_data,
                        'datastream_data'= datastream_data)
  return(all_data_list)
}

#' GetSamenMetenAPIinfo
#'
#' This function will obtain the information of each sensor in a particular
#' municipality or project. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param url_part string with the name of the project or municipality where
#'   you are interested in, in the format as the api can read.
#'   For project: project eq'Amersfoort'
#'   For municipality:  codegemeente eq '310'
#'
#' @return list with the info for each sensor in the url_part
#' @export
#'
#' @examples
#' TEST <- GetSamenMetenAPIinfo("project eq'Amersfoort'")
GetSamenMetenAPIinfo <- function(url_part){
  .Deprecated("GetSamenMetenAPIinfo2")

  url_things <- paste("https://api-samenmeten.rivm.nl/v1.0/Things?$filter=(properties/",url_part,")&$expand=Locations,Datastreams", sep='')
  url_things <- gsub(' ','%20', url_things)

  # Create an empty dataframe to store sensordata
  sensor_data <- data.frame()

  # Create an empty dataframe to store the urls
  datastream_data <- data.frame()

  # The API uses multiple pages, if there are, then get them all
  multiple_pages_things <- TRUE

  # Get all sensors and there properties and further urls to the datastream properties and observations
  while(multiple_pages_things){

    logger::log_info(paste0("GetSamenMetenAPIinfo: Get data from url: ", url_things))
    # Get from API
    tryCatch({
      content_things <- GetAPIDataframe(url_things)
      content_things_df <- content_things$value
    }, error = function(e){
      # There could be a overload of the API server
      # Try again after 30 seconds
      logger::log_trace("GetSamenMetenAPIinfo: GetAPIDataframe returned error, trying again ...")
      Sys.sleep(3)
      # Get from API
      tryCatch({
        content_things <- GetAPIDataframe(url_things)
        content_things_df <- content_things$value
      }, error = function(e){
        logger::log_error("GetSamenMetenAPIinfo: GetAPIDataframe returned error")
        stop("GetSamenMetenAPIinfo ERROR in URL things")
      })
    })


    logger::log_debug("GetSamenMetenAPIinfo: Data received from {url_things}")

    # Extract the coordinates: The coordinates are listed in the dataframe
    location_df <- content_things_df$Locations
    coordinates <-   lapply(location_df, extract_coord) |> dplyr::bind_rows()

    # Store the info about the sensor (meta-data)
    sensor_data <- rbind(sensor_data, data.frame('things_id' = content_things_df[,'@iot.id'],
                                                 'kit_id' = content_things_df[,'name'],
                                                 'project' = content_things_df$properties['project'],
                                                 'lat' = coordinates$lat,
                                                 'lon' = coordinates$lon,
                                                 'knmicode' = content_things_df$properties['knmicode'],
                                                 'pm10closecode' = content_things_df$properties['pm10closecode'],
                                                 'pm10regiocode' = content_things_df$properties['pm10regiocode'],
                                                 'pm10stadcode' = content_things_df$properties['pm10stadcode'],
                                                 'pm25closecode' = content_things_df$properties['pm25closecode'],
                                                 'pm25regiocode' = content_things_df$properties['pm25regiocode'],
                                                 'pm25stadcode' = content_things_df$properties['pm25stadcode']
    ))

    # Store the info about the parameters measured (datastreams)
    # Extract the datastream
    datastream_list <- content_things_df |> dplyr::select(Datastreams) |> dplyr::pull() |> lapply( extract_datastream)

    # Add the kit_id to the datastream
    kit_id_overview <- content_things_df[,'name']
    names(datastream_list) <- kit_id_overview

    # Convert to dataframe
    datastream_df <- datastream_list |> dplyr::bind_rows( .id = "kit_id")

    # Store the info about the datastreams (meta-data)
    datastream_data <- rbind(datastream_data, datastream_df)

    # Check if there is another page to read
    if (length(content_things)>1){
      url_things <- content_things[[1]]
      logger::log_trace("GetSamenMetenAPIinfo: getting next page ...")
    } else{
      logger::log_trace("GetSamenMetenAPIinfo: got final page")
      multiple_pages_things <- FALSE
    }
  }

  all_data_list <- list('sensor_data' = sensor_data, 'datastream_data'= datastream_data)
  return(all_data_list)
}
