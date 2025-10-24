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
  req <- request(paste0(base_url, "/locations/", location_id)) |>
    req_headers("Authorization" = token) |>
    req_url_query("datetime" = datetime)

  # Get the response from the request
  resp <- req_perform(req)

  # Check if the response is correct (200)
  if(resp$status_code == 200){

    # Extract the data as a json
    resp <- resp %>% resp_body_json()

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
#' @param date_start start date of the returned data, string in the format %Y-%m-%d
#' @param date_end end date of the returned data, string in the format %Y-%m-%d
#' @param parameter_name string, in the format c("dd", ...)
#' @param token string, token to get acces to the KNMI API EDR
#' @param location_id string, in format c("356", ...), one or more location ids
#' from location you want the knmi data of.
#'
#' @returns dataframe with the KNMI data in columns c(values (numeric),
#' date_time (posixct), id_nr(character)). If not succesful call then an error
#' is returned.
#' @export
#'
GetKNMIAPIEDR <- function(date_start, date_end, parameter_name, token, location_id)
{

  # Set the correct variables for the API
  api_version <- "v1"
  collection <- "10-minute-in-situ-meteorological-observations"
  base_url <- paste0("https://api.dataplatform.knmi.nl/edr/", api_version,
                     "/collections/", collection)

  # Get the dates and transform to the right format
  date_start_new <- as.Date(date_start) |> format("%Y-%m-%dT%TZ")
  date_end_new <- as.Date(date_end) |> format("%Y-%m-%dT%TZ")

  # Change location id to correct format
  location_id_new <- paste0("0-20000-0-06", location_id)
  location_id_new <- paste(location_id_new, collapse = ",")

  # Change parameters to correct format
  parameter_name_new <- paste(parameter_name, collapse = ",")

  # Set the request with the API key
  req <- request(paste0(base_url, "/locations/", location_id_new)) |>
    req_headers("Authorization" = token) |>
    req_url_query("datetime" = sprintf("%s/%s", date_start_new, date_end_new),
                  "parameter-name" = parameter_name_new)

  # Get the response from the request
  resp <- req_perform(req)

  # Check if the response is correct (200)
  if(resp$status_code == 200){
    # Extract the data as a json
    resp_json <- resp %>% resp_body_json()

    # Set empty dataframe
    result <- data.frame()

    # Get the results for each location id
    for (i in 1:length(location_id)) {

      # Get the results for each parameter
      for (j in parameter_name) {

        # Check if there is data available for this paramater at this station
        skip_to_next <- FALSE
        tryCatch(resp_json$coverages[[i]]$ranges[[j]]$values,
                 error = function(e) {
                   skip_to_next <<- TRUE
                 })
        if(skip_to_next) {
          print(paste0("There is no data for parameter ", j, " at station ", location_id[[i]]))
          next
        }

        # Extract the measurement values from the response
        values <- resp_json$coverages[[i]]$ranges[[j]]$values

        # Check if values is empty
        if (is.null(values))
        {
          print(paste0("There is no data for parameter ", j, " at station ", location_id[[i]]))
          next
        }

        # Check if list contains NULLs
        values <- replace_NULL(values)

        # Extract the measurement times
        times <- resp_json$coverages[[i]]$domain$axes$t$values
        # Check if list contains NULLs
        times <- replace_NULL(times)

        # Extract id number
        id_nr <- resp_json$coverages[[i]]$`eumetnet:locationId`
        id_nr <- unlist(id_nr) |> str_replace("0-20000-0-06", "")

        # Combine the measurement values and times in a dataframe
        df <- data.frame(values = unlist(values),
                         date_time = unlist(times),
                         id_nr = id_nr,
                         parameter_name = j)

        result <- rbind(result, df)

      }

    }

    # Set correct datetime format
    result <- result |>
      dplyr::mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%dT%TZ"))

    return(result)

  }else{

    stop()

  }
}
