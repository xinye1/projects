# Data source: The Electoral Commssion
# URL: http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/eu-referendum/electorate-and-count-information
# CSV at: http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv

csv_url <- 'http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv'

# For reproducibility the CSV is also downloaded in the data/ folder
# download.file(url = csv_url, destfile = paste0('./data/', regmatches(csv_url, regexpr('[^/]+$', csv_url))))

raw_votes <- read.csv(csv_url, stringsAsFactors = F)

# Create names_votes to match against the shape file later
names_votes <- raw_votes$Area