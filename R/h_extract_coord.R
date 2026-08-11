#' Extract coordinates from API result
#'
#' Helper function (using lapply) of the function: GetSamenMetenAPIinfo
#'
#' In the return of the SamenMeten API the coordinates are nested in lists,
#' this function extract the coordinates and checks if there are coordinates.
#' If no coordinates are available 0,0 is returned as coordinates.
#'
#' @param x list including coordinates
#'
#' @return dataframe with lat and lon as columns
#'
extract_coord <- function(x){
  if(is.null(x)){
    return(data.frame(lat = 0, lon=0)
    )
  }
  coordinates_list <- x |> dplyr::select("location") |> dplyr::pull() |> dplyr::select("coordinates")
  coordinates_num <- coordinates_list[[1]] |> unlist()
  return(data.frame(lon = coordinates_num[[1]],
                    lat = coordinates_num[[2]]))
}
