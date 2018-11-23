library(rvest)
library(stringr)
library(tidyverse)
setwd("C:/rwiz/lic/zomato-http")

get_df<-function(adress){
  #funkcja pobiera data frama ze strony za pomocą htmla
  zomato<-read_html(adress)

  res<-zomato%>%
    html_nodes('.card.search-snippet-card.search-card')
  
  #linki
  links<-res%>%
    html_nodes(".result-title.hover_feedback.zred.bold.ln24.fontsize0 ")%>%
    html_attr('href')
  
  #nazwa z dzielnicą
  name<-res%>%
    html_node('.result-title.hover_feedback.zred.bold.ln24.fontsize0')%>%
    html_attr('title')
  
  #rating liczbowy
  rating<-res%>%
    html_nodes('.rating-popup.res-rating-nf')%>%
    html_text()%>%
    str_trim()%>%
    as.numeric()
  
  #id restauracji
  res_id<-res%>%
    html_nodes('.rating-popup.res-rating-nf')%>%
    html_attr('data-res-id')
  
  #ilość głosów
  votes_cnt<-res%>%
    html_nodes('.ta-right.floating.search_result_rating.col-s-4.clearfix')%>%
    map(function(x) x%>%html_nodes('span')%>%
          html_text())%>%
    str_extract('[0-9]{1,5}')%>%
    as.numeric()
  
  
  #adres (trzeba wyczyścić)
  adress<-res%>%
    html_nodes('.col-m-16.search-result-address.grey-text.nowrap.ln22')%>%
    html_text()
  
  
  #typ kuchni 
  cuisines<-res%>%
    html_nodes('.col-s-11.col-m-12.nowrap.pl0')%>%
    html_text()%>%
    str_trim()
  
  
  #cena
  price<-res%>%
    html_nodes('.search-page-text.clearfix.row')%>%
    map(function(x) x%>%
          html_nodes( '.res-cost.clearfix')%>%
          html_text()%>%
          str_extract('[0-9]{1,5}')%>%
          as.numeric())%>%
    map_dbl(function(x) ifelse(x%>%is_empty(), NA, x))
  
  results<-data.frame(name, adress, links, price, res_id, rating, votes_cnt, cuisines)
  results<-results%>%mutate_if(is.factor, function(x) as.character(levels(x))[x] )
  return(results)
}

#srodmiescie polnocne----
html_core<-'https://www.zomato.com/pl/warszawa/restauracje-r%C3%B3dmie-cie-p%C3%B3%C5%82nocne?ref_page=subzone&nearby=0&page='
htmls<-paste0(html_core, 1:21)

list_results<-vector('list', 21)

for (i in seq_along(htmls)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sr_pln<-list_results%>%bind_rows()%>%
  mutate(subzone='Śródmieście północne')
write.csv(df_sr_pln, 'srodmiescie-polnocne.csv')


#srodmiescie poludniowe-----
html_core<-'https://www.zomato.com/pl/warszawa/%C5%9Br%C3%B3dmie%C5%9Bcie-po%C5%82udniowe-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:29)

list_results<-vector('list', 29)

for (i in 29){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sr_pld<-list_results%>%bind_rows()%>%
  mutate(subzone='Śródmieście południowe')
write.csv(df_sr_pld, 'srodmiescie-poludniowe.csv')


#mokotów----
html_core<-'https://www.zomato.com/pl/warszawa/mokot%C3%B3w-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:7)

list_results<-vector('list', 7)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_mokotow<-list_results%>%bind_rows()%>%
  mutate(subzone='Mokotów')
write.csv(df_mokotow, 'mokotow.csv')


#powiśle----
html_core<-'https://www.zomato.com/pl/warszawa/powi%C5%9Ble-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:7)

list_results<-vector('list', 7)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Powiśle')
write.csv(df_powisle, 'powisle.csv')


#ochota----
html_core<-'https://www.zomato.com/pl/warszawa/ochota-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:7)

list_results<-vector('list', 7)

for (i in 7){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Ochota')
write.csv(df_powisle, 'ochota.csv')


#ursynów----
html_core<-'https://www.zomato.com/pl/warszawa/ursyn%C3%B3w-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:9)

list_results<-vector('list', 9)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Ursynów')
write.csv(df_powisle, 'ursynów.csv')


#wola----
html_core<-'https://www.zomato.com/pl/warszawa/wola-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:16)

list_results<-vector('list', 16)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Wola')
write.csv(df_powisle, 'wola.csv')


#wilanów----
html_core<-'https://www.zomato.com/pl/warszawa/wilan%C3%B3w-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:6)

list_results<-vector('list', 6)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Wilanów')
write.csv(df_powisle, 'wilanów.csv')


#bielany----
html_core<-'https://www.zomato.com/pl/warszawa/bielany-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:5)

list_results<-vector('list', 5)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Bielany')
write.csv(df_powisle, 'bielany.csv')



#żoliborz----
html_core<-'https://www.zomato.com/pl/warszawa/żoliborz-restauracje?ref_page=subzone&nearby=0&page='

htmls<-paste0(html_core, 1:4)

list_results<-vector('list', 4)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_powisle<-list_results%>%bind_rows()%>%
  mutate(subzone='Żoliborz')
write.csv(df_powisle, 'żoliborz.csv')




#włochy----
subzone_nazwa<-'włochy'
str_cnt<-2
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#bemowo----
subzone_nazwa<-'bemowo'

html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)
html_core
str_cnt<-7

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#ursus----
subzone_nazwa<-'ursus'
str_cnt<-4
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#białołęka----
subzone_nazwa<-'białołęka'
str_cnt<-4
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#targówek----
subzone_nazwa<-'targówek'
str_cnt<-2
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#Praga-Północ----
subzone_nazwa<-'praga-północ'
str_cnt<-7
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))


#PRAGA POŁUDNIe----

subzone_nazwa<-'praga-południe'
str_cnt<-19
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))


#REMBERTÓW----
subzone_nazwa<-'rembertów'
str_cnt<-1
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#WAWER----

subzone_nazwa<-'wawer'
str_cnt<-5
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))

#WESOŁA----

subzone_nazwa<-'wesoła'
str_cnt<-2
html_core<-sprintf('https://www.zomato.com/pl/warszawa/%s-restauracje?ref_page=subzone&nearby=0&page=',
                   subzone_nazwa)

htmls<-paste0(html_core, 1:str_cnt)

list_results<-vector('list', str_cnt)

for (i in seq_along(list_results)){
  list_results[[i]]<-get_df(htmls[i])
  Sys.sleep(2)
}
df_sub<-list_results%>%bind_rows()%>%
  mutate(subzone=subzone_nazwa)
write.csv(df_sub, paste0(subzone_nazwa,'.csv'))



#DODAĆ SŁUŻEW