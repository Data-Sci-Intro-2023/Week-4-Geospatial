# code to pull national park boundary shapefiles from IRMA
getParkBoundary <- function(park, save = FALSE, path = NULL){
  
  call <- "https://irmaservices.nps.gov/datastore/v4/rest"
  
  #pull resource ID using reference ID of the park boundaries landing page
  downloadLink <- httr::GET(paste0(call, "/Reference/2296705/DigitalFiles")) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(.,flatten = TRUE) %>% 
    dplyr::as_tibble() %>% 
    filter(str_detect(fileName, "nps_boundary")) %>% 
    pull(downloadLink)
  
  #download boundary 
  temp1 <- tempfile()
  download.file(downloadLink, destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)
  
  parks <- sf::st_read(dsn = temp2)
  
  poi <- parks %>% 
    dplyr::filter(UNIT_CODE %in% park) %>%
    dplyr::group_by(UNIT_CODE,REGION,STATE) %>%
    dplyr::summarize()
  
  if(save == TRUE){
    sf::st_write(poi, paste0(path, "/park_boundary.shp"))
    
  }
  
  return(poi)
  
}