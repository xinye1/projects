# Inspired by
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

library(rvest)

url_meridien_maritius <-
  'https://www.tripadvisor.co.uk/Hotel_Review-g298344-d302629-Reviews-Le_Meridien_Ile_Maurice-Pointe_Aux_Piments.html'
url_base1 <- 'https://www.tripadvisor.co.uk/Hotel_Review-g298344-d302629-Reviews'
url_base2 <- '-Le_Meridien_Ile_Maurice-Pointe_Aux_Piments.html'
n_pages <- 201
page_idx <- c('', paste0('-or', seq(2:(n_pages - 1)), '0'))
urls_meridien_maritius <- paste0(url_base1, page_idx, url_base2)

node_hotel_name <- '#HEADING'
node_street <- '#HEADING_GROUP > div > div.header_contact_info.tr_cta > address > div > div.header_address.fl.blLinks.contact_item > span > span.street-address'
node_locality <- '#HEADING_GROUP > div > div.header_contact_info.tr_cta > address > div > div.header_address.fl.blLinks.contact_item > span > span.locality'

page <- read_html(urls_meridien_maritius[2])

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
  html_node(".noQuotes") %>%
  html_text()

rating <- reviews %>%
  html_node(".reviewItemInline > span") %>%
  html_attr("class") %>%
  gsub(" of 5 stars", "", .) %>%
  as.integer()

date <- reviews %>%
  html_node(".rating .ratingDate") %>%
  html_attr("title") %>%
  strptime("%b %d, %Y") %>%
  as.POSIXct()

review <- reviews %>%
  html_node("div.prw_rup.prw_common_html") %>%
  html_text()

data.frame(id, quote, rating, date, review, stringsAsFactors = FALSE) %>% View()