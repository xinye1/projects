pacman::p_load(rvest, stringr, RPushbullet, jsonlite)

packt_url <- 'https://www.packtpub.com/packt/offers/free-learning'
selector <- '#deal-of-the-day > div > div > div.dotd-main-book-summary.float-left > div.dotd-title > h2'
book_title <- packt_url %>%
  read_html %>%
  html_node(selector) %>%
  html_text %>%
  str_trim

# pbGetDevices(apikey = 'o.JasHi2zbLFB366EdYtdClQZAM7WoKPEJ')
fromJSON(pbGetDevices())$devices[,c("iden", "nickname")]
pbPost("link", title="Today's Packt free book",
       url = packt_url, body = book_title) 

user_key <- Sys.getenv('PUSHOVER_USER_KEY')

send_push(user_key, message = book_title, title = "Today's Packt free book", url = packt_url)
