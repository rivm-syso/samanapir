#' Get data Samen Meten API per Municipality
#'
#' This function will obtain the information of each sensor in a particular
#' municipality from the Samen Meten API. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param muni_number string with the code of the municipality, for example '310'
#'
#' @return list with the info for each sensor in the municipality
#'  When no data from API but connection: return empty df
#'  When no data from API becuase no connection: return NULL
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIinfoMuni2("330")
GetSamenMetenAPIinfoMuni2 <- function(muni_code){
  # check if input is character
  if(!is.character(muni_code)){
    logger::log_error("Input 'muni_code' should be a character")
    return(NULL)
  }

  # Create part for in the url of the API
  url_part <- paste("codegemeente eq'",muni_code,"'", sep='')

  # Get the data from the API
  data_out <- GetSamenMetenAPIinfo2(url_part)

  return(data_out)
}

#' Get data Samen Meten API per Municipality
#'
#' This function will obtain the information of each sensor in a particular
#' municipality from the Samen Meten API. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param muni_number string with the code of the municipality, for example '310'
#'
#' @return list with the info for each sensor in the municipality
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIinfoMuni("330")
GetSamenMetenAPIinfoMuni <- function(muni_code){
  .Deprecated("GetSamenMetenAPIinfoMuni2")
  # check if input is character
  if(!is.character(muni_code)){
    logger::log_error("Input 'muni_code' should be a character")
    return(NULL)
  }

  # Create part for in the url of the API
  url_part <- paste("codegemeente eq'",muni_code,"'", sep='')

  # Get the data from the API
  data_out <- GetSamenMetenAPIinfo2(url_part)

  return(data_out)
}
