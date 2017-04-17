# This is where the hexagon map idea came from
# However it is irrelevant because the tilegrams package only has maps of the US
# It is left here as a future reference - anyone who's interested in plotting hexagon maps for the US, feel free to reference the sources below

# setup and packages
pacman::p_load(
  'devtools', 'purrr', 'dplyr', 'htmlwidgets',
'stringr', 'rvest', 'xml2', 'htmltools', 'leaflet')
# devtools::install_github('bhaskarvk/tilegramsR')
# devtools::install_github('bhaskarvk/usgazetteer')
pacman::p_load(tilegramsR, usgazetteer)

url <- 'http://projects.fivethirtyeight.com/2016-election-forecast/?ex_cid=2016-senate-forecast'

g <- xml2::read_html(url)
str(g)

# These divs hold our data
state.winprobs <- g %>% rvest::html_nodes('.cards')
state.winprobs <- state.winprobs[2:52] # Select only state data

# How many electoral votes per state
electoral.votes <- purrr::map_chr(
  state.winprobs,
  function(winprob) {
    winprob %>%
      rvest::html_node('p.top-powerbar') %>%
      rvest::html_text()
  }) %>%
  stringr::str_extract('[0-9]+') %>% as.numeric()