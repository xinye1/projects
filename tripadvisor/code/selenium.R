pacman::p_load(RSelenium, stringr, rvest, lubridate)
driver <- rsDriver(port = 4444L, browser = 'phantomjs')
rmDr <- driver[['client']]
rmDr$navigate(url_meridien_maritius)
# rmDr$navigate(url_sheraton)
# rmDr$getStatus()


expand_links <- rmDr$findElements(using = 'css selector', '.expandLink')
resHeaders <- unlist(lapply(expand_links, function(x){x$getElementText()}))
expand_click <- expand_links[which(str_trim(resHeaders) == 'More')]
expand_click[[1]]$findChildElement(using = 'css selector', 'span')$clickElement()

node_hotel_name <- '#HEADING'
node_street <- 'span.street-address'
node_locality <- 'span.locality'

    src <- rmdr$getpagesource()
    page <- src[[1]] %>% read_html()
    
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

    id <- reviews %>%
      html_node('.quote a') %>%
      html_attr("id")
      
    quote <- reviews %>%
      html_node('.quote .noQuotes') %>%
      html_text()
    
    rating <- reviews %>%
      html_node(".reviewItemInline > span") %>%
      html_attr("class") %>%
      str_match(., pattern = '_(\\d)') %>%
      '['(, 2) %>%
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
      '['(, 2)
    
    # Haven't found a way to extract the sub ratings
    # sub_rating <- reviews %>%
    #   lapply(function (x) {
    #     sub_nodes <- html_nodes("li.recommend-answer")
    #     if (length(sub_nodes) == 0) next
    #     else {
    #       sub_nodes %>%
    #         html_attr("class") %>%
    #         str_match("_(\\d)") %>%
    #         "["(, 2) %>%
    #         as.integer()}
    #   })
          
    
out <- data.frame(
  # hotel_name, locality, street,
  id, quote, rating, review_date,
  review, visit_year_month,
  visit_company, tips,
  stringsAsFactors = FALSE)
      
rmDr$close()
