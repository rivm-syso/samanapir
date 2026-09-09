GetAPIRespKNMI <- function(req){
  # check if valid input
  if(!inherits(req, "httr2_request")){
    logger::log_info("KNMI request not a httr2_request")
    return(NULL)
  }

  # Excecute the API request
  resp <- try(httr2::req_perform(req), silent = TRUE)

  # Check if the request was succesful
  if(inherits(resp, "try-error")){
    logger::log_info(paste0("Error in API KNMI: ", req$url))
    return(NULL)
  }else if(resp$status_code == 200){
    return(resp)
  }else{
    return(NULL)
  }

}
