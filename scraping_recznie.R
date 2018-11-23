
#https://www.zomato.com/pl/warszawa/restauracje-r%C3%B3dmie-cie-p%C3%B3%C5%82nocne?ref_page=subzone&nearby=0&category=6
zomato<-read_html('https://www.zomato.com/pl/warszawa/restauracje-r%C3%B3dmie-cie-p%C3%B3%C5%82nocne?ref_page=subzone&nearby=0')
zomato<-read_html(htmls[1])
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
  
  
