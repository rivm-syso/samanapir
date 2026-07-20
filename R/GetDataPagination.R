#' GetDataPagination
#'
#' Get the data from the API from all the pages, see GetAPIDataframe
#' for the details from getting the data
#'
#' @param content resp of first data request using GetAPIDataframe
#' @param url_page string, url from the data used in GetAPIDataframe
#'
#' @returns df with all the data from the different pages, columns
#' depending on data itself
GetDataPagination <- function(content, url_page){
  # Haalt van alle pagina's de gegevens op en returned 1 df
  # Neemt van de eerste pagina de data
  data_totaal <- do.call(rbind,lapply(content$data, as.data.frame))

  #logging
  logger::log_formatter(formatter_pander)
  logger::log_debug("Pagination, column names start df: ")
  logger::log_debug(names(data_totaal))

  # Haalt de volgende pagina's op
  current_page <- content$pagination$current_page
  last_page <- content$pagination$last_page

  # logging
  logger::log_debug(paste0("Pagination:  ", current_page, " of ",
                           last_page))

  while(current_page < last_page){
    # Maak de url voor de volgende pagina en haal data op
    url_next <- httr2::request(url_page) |>
      httr2::req_url_query(page = current_page + 1)

    # logging debug
    logger::log_debug(paste0("Data ophalen van: ", url_next$url))

    # Get the data from API
    content_next <- GetAPIDataframe2(url_next)

    # Check if api call is succesful
    if(is.null(content_next)){
      logger::log_info(paste0("Not succesful: ", url_next$url))
      return(data_totaal)
    }

    # Check of er nog een nieuwe pagina is
    current_page <- content_next$pagination$current_page

    if(current_page > 0){
      #Voeg de nieuwe data toe aan het totaal
      data_new <- do.call(rbind,lapply(content_next$data, as.data.frame))
      data_totaal <- rbind(data_totaal, data_new)
    }
  }

  # logging
  logger::log_debug(paste0("All pages done:  ", current_page, " of ",
                           last_page))

  return(data_totaal)
}

#' GetDataPaginationSamenMeten
#'
#' Get the data from the API from all the pages, see GetAPIDataframe
#' for the details from getting the data
#'
#' @param content resp of first data request using GetAPIDataframe
#'
#' @returns list with all the data from the different pages, depending on return api
GetDataPaginationSamenMeten <- function(content){
  data_totaal <- content$value

  # Check if there is a new page
  if("@iot.nextLink" %in% names(content)){
    check_next <- TRUE
    }else{
      check_next <- FALSE
      return(content$value)
    }

  # Haalt de volgende pagina's op
  while(check_next){
    # Get the url string for the next page
    next_page <- content$`@iot.nextLink`

    # Maak de url request voor de volgende pagina en haal data op
    url_next <- httr2::request(next_page)

    # logging debug
    logger::log_debug(paste0("Data ophalen van: ", url_next$url))

    # Get the data from API
    content_next <- GetAPIDataframe2(url_next)

    # Check if api call is succesful
    if(is.null(content_next)){
      logger::log_info(paste0("Not succesful: ", url_next$url))
      return(data_totaal)
    }
    # Get the data of the new page
    data_new <- content_next$value

    # Add the data to the total
    data_totaal <- append(data_totaal, data_new)

    # Check of er nog een nieuwe pagina is
    if("@iot.nextLink" %in% names(content_next)){
      check_next <- TRUE
    }else{
      # logging
      logger::log_debug(paste0("All pages done"))

      check_next <- FALSE
      return(data_totaal)
    }
  }

  # logging
  logger::log_debug(paste0("All pages done"))

  return(data_totaal)
}
