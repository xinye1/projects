# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
pacman::p_load(rvest, dplyr, tidyverse, tidyr, stringr, xltools, lubridate)

url_meridien_maritius <-
  'https://www.tripadvisor.co.uk/Hotel_Review-g298344-d302629-Reviews-Le_Meridien_Ile_Maurice-Pointe_Aux_Piments.html'
url_sheraton <- 'https://www.tripadvisor.co.uk/Hotel_Review-g6695203-d478381-Reviews-Sheraton_Maldives_Full_Moon_Resort_Spa-Furanafushi_Island.html'
url_shanti <- "https://www.tripadvisor.co.uk/Hotel_Review-g1182899-d630437-Reviews-Shanti_Maurice_A_Nira_Resort-St_Felix.html"
hotel_urls <- c(url_meridien_maritius, url_sheraton, url_shanti)

getHotelData <- function(url, max_page = NULL, include_hotel_name = T, selenium_port = 4444L, ...) {
  
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
  
  # Setup Selenium
  driver <- rsDriver(port = selenium_port, browser = 'phantomjs')
  rmDr <- driver[['client']]
  
  
  for (u in urls) {
    
    rmDr$navigate(u)
    
    expand_links <- rmDr$findElements(using = 'css selector', '.expandLink')
    resHeaders <- unlist(lapply(expand_links, function(x){x$getElementText()}))
    expand_click <- expand_links[which(str_trim(resHeaders) == 'More')]
    expand_click[[1]]$findChildElement(using = 'css selector', 'span')$clickElement()
    
    src <- rmDr$getPageSource()
    page <- src[[1]] %>% read_html()
    
    if (include_hotel_name) {
      hotel_name <- page %>%
        html_node(node_hotel_name) %>%
        html_text(trim = T)
      
      street <- page %>%
        html_node(node_street) %>%
        html_text(trim = T)
      
      locality <- page %>%
        html_node(node_locality) %>%
        html_text(trim = T)
    }
    
    reviews <- page %>%
      html_nodes("#REVIEWS .innerBubble")

    id <- reviews %>%
      html_node(".quote a") %>%
      html_attr("id")
      
    quote <- reviews %>%
      html_node(".quote .noQuotes") %>%
      html_text()
    
    rating <- reviews %>%
      html_node(".reviewItemInline > span") %>%
      html_attr("class") %>%
      str_match(., pattern = "_(\\d)") %>%
      "["(, 2) %>%
      as.integer()
    
    review_date <- reviews %>%
      html_node(".rating .ratingDate") %>%
      html_attr("title") %>%
      strptime("%d %b %Y") %>%
      as.POSIXct()
    
    review <- reviews %>%
      html_node(".wrap .prw_rup.prw_common_html") %>%
      html_text()
    
    visit <- reviews %>%
      html_node("span.recommend-titleInline") %>%
      html_text() %>%
      str_match("^Stayed (.+), travelled (.+)$")
    
    visit_year_month <- visit[, 2] %>%
      paste0('1 ', .) %>%
      strptime("%d %b %Y") %>%
      as.POSIXct()
    visit_year <- year(visit_year_month)
    visit_month <- month(visit_year_month)
    
    visit_company <- visit[, 3]
    
    tips <- reviews %>%
      html_node("div.reviewItem.inlineRoomTip") %>%
      html_text() %>%
      str_match("^Room Tip: (.+)See more room tips$") %>%
      "["(, 2)
    
    if (include_hotel_name) {
      tb <- data.frame(
          hotel_name, locality, street,
          id, quote, rating, review_date,
          review, visit_year_month,
          visit_company, tips,
          stringsAsFactors = FALSE)
    } else {
      tb <- data.frame(
          id, quote, rating, review_date,
          review, visit_year_month,
          visit_company, tips,
          stringsAsFactors = FALSE)
    }
    
    out <- rbind(out, tb)
  }
  
  rmDr$close()
  return(out)
}

test <- getHotelData(hotel_urls[1], max_page = 1, include_hotel_name = T, selenium_port = 4445L)

hotel_data <- data.frame()
for (h in hotel_urls) {
  hotel_data <- cbind(
    hotel_data,
    getHotelData(h, max_page = 5, include_hotel_name = T, selenium_port = 4445L))
}

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
