#' Get data Samen Meten API per Project
#'
#' This function will obtain the information of each sensor in a particular
#' project from the Samen Meten API. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param project_name string with name of the project for example "HEI"
#'
#' @return list with the info for each sensor in the project
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIinfoProject2("HEI")
GetSamenMetenAPIinfoProject2 <- function(project_name){
  # check if input is character
  if(!is.character(project_name)){
    logger::log_error("Input 'project_name' should be a character")
    return(NULL)
  }

  # Create part for in the url of the API
  url_part <- paste("project eq'",project_name,"'", sep='')

  # Get the data from the API
  data_out <- GetSamenMetenAPIinfo2(url_part)

  return(data_out)
}

#' Get data Samen Meten API per Project
#'
#' This function will obtain the information of each sensor in a particular
#' project from the Samen Meten API. The name, location, closest reference stations,
#' measured components and the urls to the observations of each datastream.
#'
#' @param project_name string with name of the project for example "HEI"
#'
#' @return list with the info for each sensor in the project
#' @export
#'
#' @examples TEST <- GetSamenMetenAPIinfoProject("HEI")
GetSamenMetenAPIinfoProject <- function(project_name){
  .Deprecated("GetSamenMetenAPIinfo2")
  # check if input is character
  if(!is.character(project_name)){
    logger::log_error("Input 'project_name' should be a character")
    return(NULL)
  }

  # Create part for in the url of the API
  url_part <- paste("project eq'",project_name,"'", sep='')

  # Get the data from the API
  data_out <- GetSamenMetenAPIinfo(url_part)

  return(data_out)
}

