#' Extract datastream from API result
#'
#' Helper function (using lapply) of the function: GetSamenMetenAPIinfo
#'
#' In the return of the SamenMeten API the information of the dataastreams are
#' nested in lists, this function extract the url to the observedproperties and
#' the observations and checks if there is information at all. If no information
#' is available, then "no data" is set.
#'
#' @param x, list with lists with the data (`@iot_id`, unitOfMeasurement,
#' `ObservedProperty@iot.navigationLink`, `Observations@iot.navigationLink`,
#' name)
#'
#'
#' @return dataframe with the kit_id_ext, unit, url_prop, url_obs
#'
extract_datastream2 <- function(x){
  # Check if there is data, if not reutrn df with default
  if(is.null(x)){
    return(data.frame(kit_id_ext = "no data",
                      unit = "no data",
                      datastream_id = -999,
                      url_prop = "no data",
                      url_obs = "no data"
    )
    )
  }

  # Set to df and extract info of interest
  x_df <- do.call(rbind, x) |> as.data.frame()

  unit <- do.call(rbind, x_df$unitOfMeasurement) |> as.data.frame() |>
    dplyr::select(symbol) |> unlist()

  url_properties <- do.call(rbind, x_df$`ObservedProperty@iot.navigationLink`) |>
    as.data.frame() |> dplyr::pull()

  url_observations <- do.call(rbind, x_df$`Observations@iot.navigationLink`) |>
    as.data.frame() |> dplyr::pull()

  kit_id_ext <- x_df$name |> unlist()

  datastream_id <- x_df$`@iot.id` |> unlist()

  # combine info of interest and return df
  return(data.frame(kit_id_ext = kit_id_ext,
                    unit = unit,
                    datastream_id = datastream_id,
                    url_prop = url_properties,
                    url_obs = url_observations))
}


#' Extract datastream from API result
#'
#' Helper function (using lapply) of the function: GetSamenMetenAPIinfo
#'
#' In the return of the SamenMeten API the information of the dataastreams are
#' nested in lists, this function extract the url to the observedproperties and
#' the observations and checks if there is information at all. If no information
#' is available, then "no data" is set.
#'
#' @param x
#'
#' @return dataframe with the kit_id_ext, unit, url_properties, url_observations
#'
extract_datastream <- function(x){
  if(is.null(x)){
    return(data.frame(kit_id_ext = "no data",
                      unit = "no data",
                      datastream_id = -999
    )
    )
  }
  unit <- x |> dplyr::select("unitOfMeasurement") |>
    dplyr::pull() |>
    dplyr::select("symbol") |>
    dplyr::pull()
  url_properties <- x |> dplyr::select("ObservedProperty@iot.navigationLink") |>
    dplyr::pull()
  url_observations <- x |> dplyr::select("Observations@iot.navigationLink") |>
    dplyr::pull()
  kit_id_ext <- x |> dplyr::select("name") |> dplyr::pull()
  datastream_id <- x |> dplyr::select("@iot.id") |> dplyr::pull()

  return(data.frame(kit_id_ext = kit_id_ext,
                    unit = unit,
                    datastream_id = datastream_id))
}
