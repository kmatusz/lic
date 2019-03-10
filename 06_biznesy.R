get_geocode<- function(adress){
  base_url <- 'https://www.mapquestapi.com/geocoding/v1/address?key=QGyAe21hkN5jUYH3oHtn6lWQBNooCyIt&inFormat=kvp&outFormat=json&thumbMaps=false&delimiter=%2C'
  
  #url <- httr::modify_url(base_url)
  resp <- httr::GET(
    url = base_url, 
    #config = httr::add_headers("user-key" = api_key),
    query = list(location=adress)#list(entity_id = 109, entity_type='city')
  )
  
  a<-jsonlite::fromJSON(
    httr::content(
      resp, as = "text", encoding = "UTF-8"
    ), flatten = T)
  
  a$results$locations[[1]]%>%select(latLng.lat, latLng.lng)%>%
    rename(lat=latLng.lat,
           lng=latLng.lng)%>%
    head(1)%>%return()
}


biznesy <- read_delim("warszawa_business.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)



biznesy %>% 
  select(8, 9, 18) %>%
  mutate(full_adress = paste0(Address, ", Warsaw, ", Postcode)) -> biznesy_adress 


biznesy_adress$lat<-NA
biznesy_adress$lng<-NA
timer<-Sys.time()
for (i in 3480:8000){
  temp<-get_geocode(biznesy_adress$full_adress[i])
  biznesy_adress$lat[i]<-temp$lat[1]
  biznesy_adress$lng[i]<-temp$lng[1]
  Sys.sleep(1)
  if (i%%5==0) print(c(i, temp$lat[1], Sys.time()-timer))
}

biznesy_adress %>% write.csv(file="biznesy_geocode_temp.csv")



