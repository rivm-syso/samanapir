#### Helper functions ----

check_parameter_info <- function(parameter_column){

  # Not all parameters have the same columns with the same info, check if the specific
  # information is available for this parameter.
  ifelse(is.null(parameter_column), " ", parameter_column)

}


replace_NULL <- function(list){

  # Some values return as NULL, this needs to be replaced with NA to keep the
  # amount of values consistent.
  NULL_values <- which(sapply(list, is.null))

  for (i in NULL_values) {
    list[[i]] <- NA
  }

  return(list)

}

#### side functions ----
#' Get KNMI parameters
#'
#' Get a dataframe with the parameters which could be available for the
#' knmi stations.
#'
#' @param token string, token to get acces to the KNMI API EDR
#'
#' @returns dataframe with the names and information of the parameters
#' which are available from the knmi stations;
#' returns Error if something is wrong in apu connection
#' @export
#'
getKNMIparameters <- function(token) {

  # Set the correct variables for the API
  api_version <- "v1"
  collection <- "10-minute-in-situ-meteorological-observations"
  base_url <- paste0("https://api.dataplatform.knmi.nl/edr/", api_version,
                     "/collections/", collection)

  # Set the coords and datetime
  # We're not interested in data of a specific time and place, we're only interested in the
  # accompanying parameter info. These location_id and datetime values are the ones from the
  # example script of the API.
  location_id <- "0-20000-0-06260"
  datetime <- "2022-07-19T06:00:00Z/2022-07-19T18:00:00Z"

  # Set the request with the API key
  req <- httr2::request(paste0(base_url, "/locations/", location_id)) |>
    httr2::req_headers("Authorization" = token) |>
    httr2::req_url_query("datetime" = datetime)

  # Get the response from the request
  resp <- httr2::req_perform(req)

  # Check if the response is correct (200)
  if(resp$status_code == 200){

    # Extract the data as a json
    resp <- resp |> httr2::resp_body_json()

    # Select the parameter names
    parameters <- resp$parameters
    parameter_names <- names(parameters)

    # Select the parameter names and the accompanying information and collect this
    # in a dataframe (not all parameters have the same information available).
    parameter_info <- data.frame()
    for (i in parameter_names){

      parameter_info <- rbind(parameter_info,
                              data.frame(parameter_name = i,
                                         label = check_parameter_info(resp$parameters[[i]]$label$en),
                                         description = check_parameter_info(resp$parameters[[i]]$description$en),
                                         observed_proberty_label = check_parameter_info(resp$parameters[[i]]$observedProperty$label$en),
                                         observed_proberty_description = check_parameter_info(resp$parameters[[i]]$observedProperty$description$en),
                                         unit = check_parameter_info(resp$parameters[[i]]$unit$label$en),
                                         measurement_method = check_parameter_info(resp$parameters[[i]]$measurementType$method),
                                         measurement_duration = check_parameter_info(resp$parameters[[i]]$measurementType$duration)))
    }

    return(parameter_info)

  }else{

    stop()

  }

}

#### main function ----
#' Get KNMI data via EDR api
#'
#' Get the 10-minute unvalidated data from KNMI dataplatform.
#' More information visit: https://dataplatform.knmi.nl/
#'
#' @param date_start start date of the returned data, string in the
#' format "%Y%m%d"
#' @param date_end end date of the returned data, string in the format
#' "%Y%m%d"
#' @param parameter string, options:
#' "wind" for the wind speed and direction at sensor height (default)
#' "temp" for the temperature (not yet implemented)
#' "rain" for the precipitation of last hour (not yet implemented)
#' @param token string, token to get acces to the KNMI API EDR
#' @param location_id string, in format c("356"), one location ids
#' from location you want the knmi data of.
#' @param data_result string, default == "raw", the data is as given by the api,
#' "hourly", the data is average per hour using the TimeAverage funtion form
#' openAir.
#'
#' @returns dataframe with the KNMI data in columns c(values (numeric),
#' date_time (posixct), id_nr(character), parameter_name (character),
#' result_type (character), lat (numeric), lon(numeric). If not succesful call then NULL is returned
#' @export
#'
GetKNMIAPIEDR <- function(date_start, date_end, token,
                          location_id, parameter = "wind",
                          data_result = "raw")
{

  # Set the correct variables for the API
  api_version <- "v1"
  collection <- "10-minute-in-situ-meteorological-observations"
  base_url <- paste0("https://api.dataplatform.knmi.nl/edr/", api_version,
                     "/collections/", collection)

  # Get the dates and transform to the right format
  date_start_new <- as.Date(date_start, format("%Y%m%d")) |> format("%Y-%m-%dT%TZ")
  date_end_new <- as.Date(date_end, format("%Y%m%d")) |> format("%Y-%m-%dT%TZ")


  # Change location id to correct format
  location_id_new <- paste0("0-20000-0-06", location_id)

  # Change parameters to correct format
  if(parameter == "wind"){
    parameter_name_new <- "dd,ffs"
    parameter_name <- c("dd", "ffs")
  }else{
    #TODO: add  temp and rain options
    stop()
  }

  # Set the request with the API key
  req <- httr2::request(paste0(base_url, "/locations/", location_id_new)) |>
    httr2::req_headers("Authorization" = token) |>
    httr2::req_url_query("datetime" = sprintf("%s/%s", date_start_new, date_end_new),
                  "parameter-name" = parameter_name_new)

  # Get the response from the request
  resp <- GetAPIRespKNMI(req)

  # Check if the response is correct
  if(is.null(resp)){
    return(NULL)
  }else{
    # Extract the data as a json
    resp_json <- resp |> httr2::resp_body_json()

    # Set empty dataframe
    result <- data.frame()

      # Get the results for each parameter
      for (j in parameter_name) {
        print(j)

        # Check if there is data available for this parameter at this station
        skip_to_next <- FALSE
        tryCatch(resp_json$coverages[[1]]$ranges[[j]]$values,
                 error = function(e) {
                   skip_to_next <<- TRUE
                 })
        if(skip_to_next) {
          print(paste0("There is no data for parameter ", j, " at station ",
                       location_id))
          next
        }

        # Extract the measurement values from the response
        values <- resp_json$coverages[[1]]$ranges[[j]]$values

        # Check if values is empty
        if (is.null(values))
        {
          print(paste0("There is no data for parameter ", j, " at station ",
                       location_id))
          next
        }

        # Check if list contains NULLs
        values <- replace_NULL(values)

        # Extract the measurement times
        times <- resp_json$coverages[[1]]$domain$axes$t$values
        # Check if list contains NULLs
        times <- replace_NULL(times)

        # Extract id number
        id_nr <- resp_json$coverages[[1]]$`eumetnet:locationId`
        id_nr <- unlist(id_nr) |> stringr::str_replace("0-20000-0-06", "")

        # Combine the measurement values and times in a dataframe
        df <- data.frame(values = unlist(values),
                         date_time = unlist(times),
                         id_nr = id_nr,
                         parameter_name = j)

        result <- rbind(result, df)

        } # parameters

    # Set correct datetime format
    result <- result |>
      dplyr::mutate(date_time = as.POSIXct(date_time,
                                           format = "%Y-%m-%dT%TZ"))

    # If hourly average is requested
    if(data_result == "hourly"){
      if(parameter == "wind"){
        #Add columns for openair function
        result_prep <- result |> dplyr::mutate(
          date = date_time
        ) |> tidyr::pivot_wider(names_from = parameter_name,
                                values_from = values) |>
          dplyr::mutate(ws = ffs,
                        wd = dd)

        # Calculate the hourly windspeed and direction
        average_wind <- openair::timeAverage(result_prep,
                                             avg.time = "hour",
                                             type = "id_nr")

        result <- average_wind |>
          dplyr::mutate(dd = wd,
                        ffs = ws,
                        date_time = date,
                        id_nr = as.character(id_nr)) |>
          dplyr::select(c(date_time, dd, ffs, id_nr)) |>
          tidyr::pivot_longer(cols = c(ffs, dd),
                              names_to = "parameter_name",
                              values_to = "values")
      }
      #TODO: add average temp and rain
    }

    # Extract coordinations
    coord_lon <- resp_json$coverages[[1]]$domain$axes$x$values |> unlist()
    coord_lat <- resp_json$coverages[[1]]$domain$axes$y$values |> unlist()
    # add coordinates to measurements
    result <- result |> dplyr::mutate(
      lat = coord_lat,
      lon = coord_lon
    )

    # Set data_result as type
    result <- result |>
      dplyr::mutate(result_type = data_result)

    return(result)

  }
}
