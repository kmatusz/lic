#analiza
library(readr)

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

srodmiescie_polnocne <- read.csv("srodmiescie-polnocne.csv", stringsAsFactors = F)

View(srodmiescie_polnocne)

df<-list.files('data-geocode')%>%
  map(~read.csv(paste0('data-geocode/',.), stringsAsFactors = F))%>%bind_rows()


df1<-df
df1$lat<-NA
df1$lng<-NA
timer<-Sys.time()
for (i in 367:755){
  temp<-get_geocode(df1$adress[i])
  df1$lat[i]<-temp$lat[1]
  df1$lng[i]<-temp$lng[1]
  Sys.sleep(1)
  if (i%%5==0) print(c(i, temp$lat[1], Sys.time()-timer))
}

#if_correct- sprawdzenie czy dany rekord należy do bbox Warszawy
df1%>%mutate(if_correct_lng= ifelse(between(lng, 20,22) , T, F),
            if_correct_lat= ifelse(between(lat,52, 53), T, F),
            if_correct= ifelse(if_correct_lat&if_correct_lng, T, F))->df1

write_csv(df1, 'ver4_z_coords.csv')


df2<-df1%>%filter(if_correct==T)%>%select(-if_correct_lng, -if_correct_lat, -if_correct)

df<-data_zomato

library(leaflet)

leaflet(data_zomato)%>%
  addTiles()%>%
  addCircleMarkers()

