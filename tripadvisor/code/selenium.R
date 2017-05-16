pacman::p_load(RSelenium, stringr, rvest)
driver <- rsDriver(port = 4444L, browser = 'chrome')
rmDr <- driver[['client']]
rmDr$navigate(url_meridien_maritius)
rmDr$navigate(url_sheraton)
rmDr$getStatus()


expand_links <- rmDr$findElements(using = 'css selector', '.expandLink')
resHeaders <- unlist(lapply(expand_links, function(x){x$getElementText()}))
expand_click <- expand_links[which(str_trim(resHeaders) == 'More')]
expand_click[[1]]$findChildElement(using = 'css selector', 'span')$clickElement()




src <- rmDr$getPageSource()
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
    
      
    quote <- page %>%
      html_nodes('.quote .noQuotes') %>%
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
    
out <- data.frame(hotel_name, locality, street, id, quote, rating, date, review, stringsAsFactors = FALSE)
      
rmDr$close()
