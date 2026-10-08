library(DBI)
library(RSQLite)
con <- dbConnect(SQLite(), "rustico_properties.sqlite")

# ── Browse raw data ───────────────────────────────────────────────────────────

# All 426 rows — card text + detail page text
dbGetQuery(
  con,
  "SELECT id, property_link, length(raw_text), length(detail_text) FROM raw_properties"
)

# Read the detail page text for a specific listing
row <- dbGetQuery(con, "SELECT * FROM raw_properties WHERE id = 1")
cat(row$detail_text)

# Search detail text across all listings
dbGetQuery(
  con,
  "SELECT id, property_link FROM raw_properties WHERE detail_text LIKE '%parzelle%'"
)

# ── Structured data ──────────────────────────────────────────────────────────

# All parsed properties (59 columns)
props <- dbGetQuery(con, "SELECT * FROM properties")

# Quick look at a single row
props[1, ]

colnames(props)

props$secondary_home

# ── Disconnect ────────────────────────────────────────────────────────────────
dbDisconnect(con)
