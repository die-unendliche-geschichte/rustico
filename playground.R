library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "rustico_properties.sqlite")

dbGetQuery(
  con,
  "SELECT * FROM raw_properties WHERE property_link = 'https://...'"
)
# or by id:
dbGetQuery(con, "SELECT raw_text FROM raw_properties WHERE id = 42")

dbDisconnect(con)
# The most useful column is `raw_text` — it's the full `html_text()` of the listing card as scraped. If you want to see how it was parsed:

source("web_scraper.R")
row <- dbGetQuery(con, "SELECT * FROM raw_properties WHERE id = 42")
parse_property_text(row$raw_text)
