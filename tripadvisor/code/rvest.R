# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

pacman::p_load(rvest, dplyr, tidyverse, tidyr, stringr, xltools, lubridate)

url_meridien_maritius <-
  'https://www.tripadvisor.co.uk/Hotel_Review-g298344-d302629-Reviews-Le_Meridien_Ile_Maurice-Pointe_Aux_Piments.html'
url_base1 <- 'https://www.tripadvisor.co.uk/Hotel_Review-g298344-d302629-Reviews'
url_base2 <- '-Le_Meridien_Ile_Maurice-Pointe_Aux_Piments.html'
url_sheraton <- 'https://www.tripadvisor.co.uk/Hotel_Review-g6695203-d478381-Reviews-Sheraton_Maldives_Full_Moon_Resort_Spa-Furanafushi_Island.html'

getHotelData <- function(url, max_page = NULL) {
  
  if (is.null(max_page)) {
    n_pages <- url %>%
      read_html %>%
      html_node('.pageNumbers') %>%
      html_children %>%
      html_attr('data-page-number') %>%
      tail(.,1) %>%
      as.numeric()
  } else {
    n_pages <- max_page
  }
  
  url_base <- str_match(url, '(^.+-Reviews)(-.+$)')[, 2:3]
  page_idx <- c('', paste0('-or', seq(2, n_pages - 1), '0'))
  urls <- paste0(url_base[1], page_idx, url_base[2])
  
  node_hotel_name <- '#HEADING'
  node_street <- 'span.street-address'
  node_locality <- 'span.locality'
  
  out <- data.frame()
  
  for (u in urls) {
    page <- read_html(u)
    
    hotel_name <- page %>%
      html_node(node_hotel_name) %>%
      html_text(trim = T)
    
    street <- page %>%
      html_node(node_street) %>%
      html_text(trim = T)
    
    locality <- page %>%
      html_node(node_locality) %>%
      html_text(trim = T)
    
    
    reviews <- page %>%
      html_nodes("#REVIEWS .innerBubble")
    
    # reviews <- page %>%
    #   html_node('#taplc_hr_reviews_list_0') %>%
    #   html_children %>%
    
    
    id <- reviews %>%
      html_node('.quote a') %>%
      html_attr("id")
    
    quote <- reviews %>%
      # html_node(".quote span") %>%
      # html_node(" span.noQuotes") %>%
      # html_node(".noQuotes") %>%
      # html_text()
      html_node('.quote .noQuotes') %>%
      html_text()
    
    rating <- reviews %>%
      html_node(".reviewItemInline > span") %>%
      html_attr("class") %>%
      str_match(., pattern = '_(\\d)') %>%
      '['(, 2) %>%
      as.integer()
    
    date <- reviews %>%
      html_node(".rating .ratingDate") %>%
      html_attr("title") %>%
      strptime("%d %b %Y") %>%
      as.POSIXct()
    
    review <- reviews %>%
      html_node(".wrap .prw_rup.prw_common_html") %>%
      html_text()
    
    out <- rbind(
      out,
      data.frame(hotel_name, locality, street, id, quote, rating, date, review, stringsAsFactors = FALSE)
    )
  }
  
  return(out)
}

test <- getHotelData(url_sheraton)

test$wc <- xlWC(test$date)
test$year_month <- ymd(paste(year(test$date), month(test$date), 1, sep = '-'))
test$month <- month(test$date)
test %>%
  group_by(year_month) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(data = ., aes(x = year_month, y = avg_rating)) +
  geom_line()

test %>%
  filter(year(date) >= 2014) %>%
  group_by(month) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(data = ., aes(x = month, y = avg_rating)) +
  geom_col()
