library(rvest)
library(dplyr)
library(httr)
library(DBI)
library(RSQLite)

# ── Keyword configuration ──────────────────────────────────────────────────────
# Matching is case-insensitive and substring-based.

KEYWORDS_BLACKLIST <- tolower(c(
  "dorfrustico"
  # add more terms to exclude here
))

KEYWORDS_INTERESTING <- tolower(c(
  "alleinlage",
  "ruhelage",
  "panorama",
  "aussicht",
  "vista",
  "quiete",
  "nucleo"
  # add more terms of interest here
))

# ── Image download ─────────────────────────────────────────────────────────────

download_image <- function(image_url, filename) {
  tryCatch({
    if (!dir.exists("images")) dir.create("images")
    clean_filename <- gsub("[^A-Za-z0-9._-]", "_", filename)
    full_path <- file.path("images", clean_filename)
    response <- GET(image_url, write_disk(full_path, overwrite = TRUE))
    if (status_code(response) == 200) {
      full_path
    } else {
      cat("Failed to download image:", image_url, "\n")
      NA
    }
  }, error = function(e) {
    cat("Error downloading image:", e$message, "\n")
    NA
  })
}

# ── HTML scraping helpers ──────────────────────────────────────────────────────

scrape_website <- function(url, css_selector = NULL) {
  tryCatch({
    response <- GET(url, config(ssl_verifypeer = FALSE))
    if (http_error(response)) {
      cat("HTTP error:", status_code(response), "\n")
      return(NULL)
    }
    page <- read_html(content(response, "text", encoding = "UTF-8"))
    if (is.null(css_selector)) return(page)
    page %>% html_elements(css_selector) %>% html_text(trim = TRUE)
  }, error = function(e) {
    cat("Error scraping:", e$message, "\n")
    NULL
  })
}

scrape_with_retry <- function(url, max_retries = 3, base_delay = 2) {
  for (attempt in seq_len(max_retries)) {
    result <- scrape_website(url)
    if (!is.null(result)) return(result)
    if (attempt < max_retries) {
      cat(" Retry", attempt, "of", max_retries - 1, "...")
      Sys.sleep(base_delay * attempt)
    }
  }
  NULL
}

scrape_table <- function(url, table_index = 1) {
  tryCatch({
    page <- read_html(url)
    tables <- page %>% html_table()
    if (length(tables) >= table_index) {
      tables[[table_index]]
    } else {
      cat("Table index", table_index, "not found\n")
      NULL
    }
  }, error = function(e) {
    cat("Error scraping table:", e$message, "\n")
    NULL
  })
}

scrape_links <- function(url, link_selector = "a") {
  tryCatch({
    page <- read_html(url)
    page %>% html_elements(link_selector) %>% html_attr("href")
  }, error = function(e) {
    cat("Error scraping links:", e$message, "\n")
    NULL
  })
}

# ── Property text parsing ──────────────────────────────────────────────────────

# Return a comma-separated string of keywords found in text, or NA if none match
match_keywords <- function(text, keywords) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return(NA_character_)
  text_lower <- tolower(text)
  matched <- keywords[vapply(keywords, function(kw) grepl(kw, text_lower, fixed = TRUE), logical(1))]
  if (length(matched) == 0) NA_character_ else paste(matched, collapse = ", ")
}

# Extract a numeric area value from strings like "ca. 100,00 m²" or "59,00 m²"
extract_m2 <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_real_)
  m <- regmatches(x, regexpr("[0-9]+(?:[.,][0-9]+)?", x, perl = TRUE))
  if (length(m) == 0) return(NA_real_)
  as.numeric(gsub(",", ".", m))
}

# Parse all structured fields out of a property's raw text content.
#
# Each listing card has two parts:
#   Line 1 — inline summary: "PLZ, ORT : XXXX CityKaufpreis: CHF X,-Wohnfläche:Ym²[extras]"
#   Lines 2+ — structured label:value fields (Region, Zustand, Etagen, …)
#
# The inline summary is processed separately so that Kaufpreis/Wohnfläche are
# read from it directly, avoiding double-matching when Wohnfläche also appears
# in the structured section.  Labels cover both listing-card fields and future
# detail-page fields (Objekt Nummer, Adresse, …).
parse_property_text <- function(text) {
  text <- gsub("\u00a0", " ", text)   # non-breaking space → regular space

  # ── Split on newlines to separate the inline summary (line 1) ───────────────
  lines <- trimws(strsplit(text, "\n")[[1]])
  lines <- lines[nzchar(lines)]
  first_line <- if (length(lines) >= 1) lines[1] else ""

  # ── PLZ / ORT from the inline summary ───────────────────────────────────────
  postal_code <- NA_character_
  city        <- NA_character_
  header <- sub("(?i)(?:Kaufpreis|Wohnfl\u00e4che|Nutzfl\u00e4che).*$", "", first_line, perl = TRUE)
  plz_m  <- regmatches(header, regexec("PLZ, ORT : ([0-9]{4}) (.+)$", trimws(header)))[[1]]
  if (length(plz_m) >= 3) {
    postal_code <- trimws(plz_m[2])
    city        <- trimws(plz_m[3])
  }

  # ── Kaufpreis from the inline summary (never appears in structured section) ──
  m_price <- regmatches(first_line, regexpr(
    "(?i)Kaufpreis:?\\s*(CHF\\s*[\\d.,]+[,-]+)", first_line, perl = TRUE))
  inline_purchase_price <- if (length(m_price) > 0)
    trimws(sub("(?i)Kaufpreis:?\\s*", "", m_price, perl = TRUE))
  else
    NA_character_
  inline_price_chf <- if (!is.na(inline_purchase_price))
    as.numeric(gsub("[^0-9]", "", sub("CHF\\s*", "", inline_purchase_price)))
  else
    NA_real_

  # ── Build structured-section text ───────────────────────────────────────────
  # Strip the "Kaufpreis:…Wohnfläche:Xm²" prefix from line 1 so it cannot
  # interfere with field extraction.  Any trailing content on line 1 (e.g.
  # neighbourhood note, "Standort | Umgebung") is kept as it may be useful.
  after_summary <- trimws(sub(
    "(?i).*(?:Wohnfl\u00e4che|Nutzfl\u00e4che):?\\s*[\\d.,]+\\s*m(?:\u00b2|2)\\s*",
    "", first_line, perl = TRUE
  ))
  struct_lines <- c(
    if (nzchar(after_summary)) after_summary,
    if (length(lines) > 1) lines[-1]
  )
  # Remove site-navigation artefacts before label extraction
  text <- gsub("(?i)\\bDetails\\s+Merken\\b", "", paste(struct_lines, collapse = " "), perl = TRUE)
  text <- gsub("\\s+", " ", trimws(text))

  # ── Label patterns ───────────────────────────────────────────────────────────
  labels <- c(
    # ── Listing card fields ──────────────────────────────────────────────────
    living_area    = "(?:Wohn|Nutz)fl\u00e4che",
    plot_area      = "Grundst\u00fccks?(?:gr\u00f6(?:sse|\u00dfe)|groesse|fl\u00e4che)",
    built_area     = "verbaute\\s*Fl\u00e4che",
    region         = "Region",
    condition      = "Zustand",
    bathrooms      = "(?:Dusche|Badezimmer|Bad)/WC",
    basement       = "Keller",
    secondary_home = "Zweitwohnsitz",
    parking        = "Parkpl(?:\u00e4tze?|atz)",
    lage           = "Lage",
    ausblick       = "(?:Ausblick|Aussicht)",
    floors         = "(?:Etagen|Geschosszahl)",
    heating        = "Heizung",
    # ── Detail page fields (populated when scraping individual listings) ─────
    object_number  = "Objekt\\s*Nummer",
    object_type    = "Objekt\\s*Typ",
    state          = "Bundesland",
    address        = "Adresse",
    rooms          = "Zimmer",
    noise_level    = "L\u00e4rmbelastung"
  )

  all_labels_pat <- paste(labels, collapse = "|")
  result <- setNames(rep(list(NA_character_), length(labels)), names(labels))

  for (col_name in names(labels)) {
    pat <- paste0(
      "(?i)(?:", labels[[col_name]], ")\\s*:?\\s*(.*?)(?=\\s*(?:",
      all_labels_pat, ")\\s*:|$)"
    )
    m <- regmatches(text, regexpr(pat, text, perl = TRUE))
    if (length(m) > 0 && nchar(m) > 0) {
      strip_pat <- paste0("^(?i)(?:", labels[[col_name]], ")\\s*:?\\s*")
      val <- trimws(sub(strip_pat, "", m, perl = TRUE))
      # Strip "mehr... Details Merken" trailing artefacts
      val <- trimws(sub("\\s*mehr\\.\\.\\..*$", "", val, perl = TRUE))
      result[[col_name]] <- if (nzchar(val)) val else NA_character_
    }
  }

  # ── Free-text description ────────────────────────────────────────────────────
  first_label_pos <- regexpr(paste0("(?i)(?:", all_labels_pat, ")\\s*:"), text, perl = TRUE)
  pre_label <- if (first_label_pos[1] > 1) trimws(substr(text, 1, first_label_pos[1] - 1)) else ""
  result$description <- if (nzchar(pre_label)) pre_label else NA_character_

  # ── Postal code fallback: derive from Adresse field on detail pages ──────────
  if (is.na(postal_code) && !is.na(result$address)) {
    addr_m <- regmatches(result$address,
      regexec("^([0-9]{4})\\s+(.+)$", trimws(result$address)))[[1]]
    if (length(addr_m) >= 3) {
      postal_code <- addr_m[2]
      city        <- addr_m[3]
    }
  }
  result$postal_code <- postal_code
  result$city        <- city

  # ── Price — always from the inline summary ───────────────────────────────────
  result$purchase_price <- inline_purchase_price
  result$price_chf      <- inline_price_chf

  # ── Numeric conversions ──────────────────────────────────────────────────────
  result$living_area_m2 <- extract_m2(result$living_area)
  result$plot_area_m2   <- extract_m2(result$plot_area)
  result$built_area_m2  <- extract_m2(result$built_area)
  result$rooms_n        <- suppressWarnings(as.numeric(trimws(result$rooms)))
  result$floors_n       <- suppressWarnings(as.numeric(trimws(result$floors)))

  as.data.frame(result, stringsAsFactors = FALSE)
}

# ── Database ───────────────────────────────────────────────────────────────────

init_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)

  # Raw scraped data — scraped_at = first seen, last_seen = most recently seen
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_properties (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      page          INTEGER,
      position      INTEGER,
      property_link TEXT UNIQUE,
      image_url     TEXT,
      raw_text      TEXT,
      scraped_at    DATETIME DEFAULT CURRENT_TIMESTAMP,
      last_seen     DATETIME DEFAULT CURRENT_TIMESTAMP,
      is_active     INTEGER  DEFAULT 1
    )
  ")

  # Migrate existing databases
  existing_raw_cols <- dbListFields(con, "raw_properties")
  if (!"last_seen"  %in% existing_raw_cols)
    dbExecute(con, "ALTER TABLE raw_properties ADD COLUMN last_seen DATETIME DEFAULT CURRENT_TIMESTAMP")
  if (!"is_active"  %in% existing_raw_cols)
    dbExecute(con, "ALTER TABLE raw_properties ADD COLUMN is_active INTEGER DEFAULT 1")

  # Structured data derived from raw_properties — can be dropped and rebuilt
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS properties (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      raw_id         INTEGER REFERENCES raw_properties(id),
      property_link  TEXT UNIQUE,
      object_number  TEXT,
      object_type    TEXT,
      state          TEXT,
      address        TEXT,
      postal_code    TEXT,
      city           TEXT,
      purchase_price TEXT,
      price_chf      REAL,
      living_area    TEXT,
      living_area_m2 REAL,
      plot_area      TEXT,
      plot_area_m2   REAL,
      built_area     TEXT,
      built_area_m2  REAL,
      rooms          TEXT,
      rooms_n        REAL,
      heating        TEXT,
      floors         TEXT,
      floors_n       REAL,
      noise_level          TEXT,
      region               TEXT,
      condition            TEXT,
      lage                 TEXT,
      ausblick             TEXT,
      bathrooms            TEXT,
      basement             TEXT,
      secondary_home       TEXT,
      parking              TEXT,
      description          TEXT,
      blacklist_keywords   TEXT,
      interesting_keywords TEXT,
      is_blacklisted       INTEGER DEFAULT 0,
      image_path           TEXT,
      lat                  REAL,
      lon                  REAL,
      parsed_at            DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Migrate existing properties tables
  existing_prop_cols <- dbListFields(con, "properties")
  new_prop_cols <- list(
    region = "TEXT", condition = "TEXT", lage = "TEXT", ausblick = "TEXT",
    bathrooms = "TEXT", basement = "TEXT", secondary_home = "TEXT", parking = "TEXT",
    lat = "REAL", lon = "REAL"
  )
  for (col in names(new_prop_cols)) {
    if (!col %in% existing_prop_cols)
      dbExecute(con, sprintf("ALTER TABLE properties ADD COLUMN %s %s", col, new_prop_cols[[col]]))
  }

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS scraped_pages (
      page_idx    INTEGER PRIMARY KEY,
      items_found INTEGER,
      scraped_at  DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Convenience view: parsed properties joined with active/last_seen from raw
  dbExecute(con, "
    CREATE VIEW IF NOT EXISTS active_properties AS
    SELECT p.*, r.last_seen, r.is_active
    FROM properties p
    JOIN raw_properties r ON r.id = p.raw_id
    WHERE r.is_active = 1
  ")

  con
}

# ── Step 1: Scrape ─────────────────────────────────────────────────────────────

# Hit the listing pages and store raw text + resolved URLs in raw_properties.
# Resumable: pages already recorded in scraped_pages are skipped.
# No parsing happens here — this step only touches the website.
# max_age_days: if set, pages scraped more than this many days ago are removed
# from scraped_pages so they will be re-fetched on this run.
scrape_raw <- function(max_pages = 233, items_per_page = 10,
                       db_path = "rustico_properties.sqlite",
                       max_age_days = NULL) {
  base_url <- paste0(
    "https://immobiliensuche.edireal.com/tiimmobili.ch/search",
    "?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL",
    "&country=CH&category=8",
    "&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo"
  )

  con <- init_db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!is.null(max_age_days)) {
    n_stale <- dbExecute(con,
      "DELETE FROM scraped_pages WHERE scraped_at < datetime('now', ?)",
      list(paste0("-", max_age_days, " days"))
    )
    if (n_stale > 0) cat("Cleared", n_stale, "stale pages for re-scraping\n")
  }

  done_pages <- dbGetQuery(con, "SELECT page_idx FROM scraped_pages")$page_idx

  cat("=== SCRAPING RAW DATA ===\n")
  cat("Max pages:", max_pages, "| Items per page:", items_per_page, "\n")
  cat("Already scraped:", length(done_pages), "pages\n\n")

  for (page_idx in 0:(max_pages - 1)) {
    if (page_idx %in% done_pages) {
      cat("Skipping page", page_idx + 1, "(already scraped)\n")
      next
    }

    url <- paste0(base_url, "&itemsPerPage=", items_per_page, "&pageIdx=", page_idx)
    cat("Scraping page", page_idx + 1, "...")

    page <- scrape_with_retry(url)

    if (!is.null(page)) {
      estate_items <- page %>% html_elements(".estate-item")

      if (length(estate_items) == 0) {
        cat(" No more properties found. Stopping.\n")
        break
      }

      cat(" Found", length(estate_items), "properties\n")

      for (i in seq_along(estate_items)) {
        property <- estate_items[i]
        raw_text <- property %>% html_text(trim = TRUE)

        # Resolve property link to absolute URL
        link_nodes    <- property %>% html_elements("a")
        property_link <- NA_character_
        if (length(link_nodes) > 0) {
          rel <- link_nodes[1] %>% html_attr("href")
          if (!is.na(rel)) {
            property_link <- if (startsWith(rel, "http")) rel
            else if (startsWith(rel, "/")) paste0("https://immobiliensuche.edireal.com", rel)
            else paste0("https://immobiliensuche.edireal.com/tiimmobili.ch/", rel)
            # Normalise ./  in path (e.g. /tiimmobili.ch/./immobilien/ → /tiimmobili.ch/immobilien/)
            property_link <- gsub("/\\./", "/", property_link)
          }
        }

        # Resolve image URL to absolute URL (download happens in step 2)
        image_nodes <- property %>% html_elements("img")
        image_url   <- NA_character_
        if (length(image_nodes) > 0) {
          src <- image_nodes[1] %>% html_attr("src")
          if (!is.na(src)) {
            image_url <- if (startsWith(src, "//")) paste0("https:", src)
            else if (startsWith(src, "/")) paste0("https://immobiliensuche.edireal.com", src)
            else if (!startsWith(src, "http")) paste0("https://immobiliensuche.edireal.com/", src)
            else src
          }
        }

        tryCatch(
          dbExecute(con,
            "INSERT INTO raw_properties (page, position, property_link, image_url, raw_text)
             VALUES (?, ?, ?, ?, ?)
             ON CONFLICT(property_link) DO UPDATE SET
               image_url = excluded.image_url,
               raw_text  = excluded.raw_text,
               last_seen = CURRENT_TIMESTAMP,
               is_active = 1",
            list(page_idx + 1, i, property_link, image_url, raw_text)
          ),
          error = function(e) cat("  DB write error:", e$message, "\n")
        )
      }

      dbExecute(con,
        "INSERT OR REPLACE INTO scraped_pages (page_idx, items_found) VALUES (?, ?)",
        list(page_idx, length(estate_items))
      )
    } else {
      cat(" Failed to scrape page", page_idx + 1, "\n")
    }

    Sys.sleep(2)
  }

  total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM raw_properties")$n
  cat("\n=== SCRAPING COMPLETE ===\n")
  cat("Total raw properties stored:", total, "\n")
}

# ── Step 2: Parse and enrich ───────────────────────────────────────────────────

# Parse structured fields from raw_text and download images for all rows in
# raw_properties that do not yet have a corresponding entry in properties.
# Safe to re-run: already-parsed rows are skipped.
# To re-parse everything: dbExecute(con, "DELETE FROM properties"), then re-run.
parse_and_enrich <- function(db_path = "rustico_properties.sqlite") {
  con <- init_db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  unparsed <- dbGetQuery(con, "
    SELECT r.id, r.page, r.position, r.property_link, r.image_url, r.raw_text
    FROM raw_properties r
    LEFT JOIN properties p ON p.raw_id = r.id
    WHERE p.id IS NULL               -- never parsed
       OR r.scraped_at > p.parsed_at -- re-scraped since last parse
    ORDER BY r.page, r.position
  ")

  cat("=== PARSING PROPERTIES ===\n")
  cat("Rows to parse:", nrow(unparsed), "\n\n")

  for (row_idx in seq_len(nrow(unparsed))) {
    row    <- unparsed[row_idx, ]
    parsed <- parse_property_text(row$raw_text)

    # Download image
    image_path <- NA_character_
    if (!is.na(row$image_url)) {
      clean_url      <- sub("\\?.*$", "", row$image_url)
      file_extension <- tools::file_ext(basename(clean_url))
      if (!nzchar(file_extension)) file_extension <- "jpg"
      slug     <- if (!is.na(row$property_link))
        basename(sub("\\?.*$", "", row$property_link))
      else
        as.character(row$id)
      filename  <- paste0(slug, ".", file_extension)
      full_path <- file.path("images", gsub("[^A-Za-z0-9._-]", "_", filename))

      if (file.exists(full_path)) {
        image_path <- full_path
        cat("[", row_idx, "/", nrow(unparsed), "] Image already exists, skipping\n")
      } else {
        cat("[", row_idx, "/", nrow(unparsed), "] Downloading image...")
        downloaded_path <- download_image(row$image_url, filename)
        if (!is.na(downloaded_path)) {
          image_path <- downloaded_path
          cat(" OK\n")
        } else {
          cat(" Failed\n")
        }
        Sys.sleep(0.5)
      }
    }

    tryCatch({
      # Remove stale parsed row if this is a re-scrape
      dbExecute(con, "DELETE FROM properties WHERE raw_id = ?", list(row$id))
      dbExecute(con,
        "INSERT INTO properties
         (raw_id, property_link,
          object_number, object_type, state, address, postal_code, city,
          purchase_price, price_chf,
          living_area, living_area_m2, plot_area, plot_area_m2,
          built_area, built_area_m2, rooms, rooms_n,
          heating, floors, floors_n, noise_level,
          region, condition, lage, ausblick,
          bathrooms, basement, secondary_home, parking,
          description, image_path)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        list(
          row$id, row$property_link,
          parsed$object_number, parsed$object_type, parsed$state,
          parsed$address, parsed$postal_code, parsed$city,
          parsed$purchase_price, parsed$price_chf,
          parsed$living_area,    parsed$living_area_m2,
          parsed$plot_area,      parsed$plot_area_m2,
          parsed$built_area,     parsed$built_area_m2,
          parsed$rooms,          parsed$rooms_n,
          parsed$heating, parsed$floors, parsed$floors_n,
          parsed$noise_level,
          parsed$region, parsed$condition, parsed$lage, parsed$ausblick,
          parsed$bathrooms, parsed$basement, parsed$secondary_home, parsed$parking,
          parsed$description, image_path
        )
      )
    }, error = function(e) cat("  DB write error:", e$message, "\n"))
  }

  total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM properties")$n
  cat("\n=== PARSING COMPLETE ===\n")
  cat("Total properties parsed:", total, "\n")
}

# ── Step 3: Apply keywords ─────────────────────────────────────────────────────

# Update keyword flags on already-parsed properties. Run this any time the
# keyword lists change — no re-scraping or re-parsing needed.
apply_keywords <- function(db_path   = "rustico_properties.sqlite",
                           blacklist   = KEYWORDS_BLACKLIST,
                           interesting = KEYWORDS_INTERESTING) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  rows <- dbGetQuery(con, "
    SELECT p.id, p.description, r.raw_text
    FROM properties p
    JOIN raw_properties r ON r.id = p.raw_id
  ")

  cat("Applying keywords to", nrow(rows), "properties...\n")

  for (i in seq_len(nrow(rows))) {
    row         <- rows[i, ]
    search_text <- paste(row$description, row$raw_text)
    bl_kw  <- match_keywords(search_text, blacklist)
    int_kw <- match_keywords(search_text, interesting)
    dbExecute(con,
      "UPDATE properties
       SET blacklist_keywords = ?, interesting_keywords = ?, is_blacklisted = ?
       WHERE id = ?",
      list(bl_kw, int_kw, as.integer(!is.na(bl_kw)), row$id)
    )
  }

  n_bl  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM properties WHERE is_blacklisted = 1")$n
  n_int <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM properties WHERE interesting_keywords IS NOT NULL")$n
  cat("Done.", n_bl, "blacklisted,", n_int, "flagged as interesting.\n")
}

# ── Active status ──────────────────────────────────────────────────────────────

# Mark properties as inactive if they have not been seen within max_age_days.
# Run after scrape_raw() once a full scrape cycle has completed.
# Properties absent from the site simply stop getting their last_seen updated,
# so after one full cycle they will fall outside the window and be marked inactive.
update_active_status <- function(db_path = "rustico_properties.sqlite",
                                 max_age_days = 30) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con,
    "UPDATE raw_properties
     SET is_active = CASE
       WHEN last_seen >= datetime('now', ?) THEN 1
       ELSE 0
     END",
    list(paste0("-", max_age_days, " days"))
  )

  counts <- dbGetQuery(con, "
    SELECT is_active, COUNT(*) AS n FROM raw_properties GROUP BY is_active
  ")
  n_active   <- counts$n[counts$is_active == 1]
  n_inactive <- counts$n[counts$is_active == 0]
  if (length(n_active)   == 0) n_active   <- 0L
  if (length(n_inactive) == 0) n_inactive <- 0L
  cat("Active:", n_active, "  Inactive:", n_inactive,
      " (threshold:", max_age_days, "days)\n")
}

# ── Trial run helper ───────────────────────────────────────────────────────────

# Scrape n_pages pages (stored in DB; skipped if already scraped), then show
# the parsed output for every property without writing to the properties table.
# Use this to verify parsing before committing to a full run.
test_parse <- function(n_pages = 1, items_per_page = 2,
                       db_path = "rustico_properties.sqlite") {
  scrape_raw(max_pages = n_pages, items_per_page = items_per_page, db_path = db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  rows <- dbGetQuery(con, paste0(
    "SELECT * FROM raw_properties ORDER BY page, position LIMIT ",
    n_pages * items_per_page
  ))

  cat("=== PARSE TEST (", nrow(rows), "properties) ===\n\n")

  for (i in seq_len(nrow(rows))) {
    row    <- rows[i, ]
    parsed <- parse_property_text(row$raw_text)

    cat(sprintf("--- Property %d  (page %d, pos %d) ---\n", i, row$page, row$position))
    cat("Location    :", parsed$postal_code, parsed$city, "\n")
    cat("Price       : CHF", parsed$price_chf, "  (raw:", parsed$purchase_price, ")\n")
    cat("Living area :", parsed$living_area_m2, "m²  Plot:", parsed$plot_area_m2,
        "m²  Floors:", parsed$floors_n, "\n")
    cat("Region      :", parsed$region, "\n")
    cat("Condition   :", parsed$condition, "\n")
    cat("Lage        :", parsed$lage, "\n")
    cat("Ausblick    :", parsed$ausblick, "\n")
    cat("Heating     :", parsed$heating, "\n")
    cat("Parking     :", parsed$parking, "\n")
    cat("Description :", parsed$description, "\n")
    cat("Link        :", row$property_link, "\n")
    cat("\n")
  }

  invisible(rows)
}

# ── Diagnostic helpers ─────────────────────────────────────────────────────────

test_pagination <- function() {
  base_url <- paste0(
    "https://immobiliensuche.edireal.com/tiimmobili.ch/search",
    "?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL",
    "&country=CH&category=8",
    "&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo",
    "&itemsPerPage=10"
  )
  cat("=== TESTING PAGINATION ===\n\n")
  for (page_idx in 0:2) {
    url <- paste0(base_url, "&pageIdx=", page_idx)
    cat("Testing page", page_idx + 1, "(pageIdx=", page_idx, ")\n")
    cat("URL:", url, "\n")
    page <- scrape_website(url)
    if (!is.null(page)) {
      estate_items <- page %>% html_elements(".estate-item")
      cat("Found", length(estate_items), "properties on this page\n")
      if (length(estate_items) > 0) {
        first_property <- estate_items[1] %>% html_text(trim = TRUE)
        cat("First property preview:", substr(first_property, 1, 100), "...\n")
      }
    } else {
      cat("Failed to scrape page", page_idx + 1, "\n")
    }
    cat("\n")
    Sys.sleep(1)
  }
}

analyze_page_structure <- function() {
  url <- paste0(
    "https://immobiliensuche.edireal.com/tiimmobili.ch/search",
    "?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL",
    "&country=CH&category=8",
    "&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo",
    "&itemsPerPage=10&pageIdx=0"
  )
  page <- scrape_website(url)
  if (is.null(page)) { cat("Failed to scrape the page\n"); return(NULL) }

  cat("=== PAGE STRUCTURE ANALYSIS ===\n\n")

  selectors_to_try <- c(
    ".estate", ".estate-item", ".property", ".listing", ".result-item",
    ".search-result", "[data-estate]", ".item", ".card", ".result",
    "article", ".property-card", ".listing-item", ".real-estate-item"
  )
  for (selector in selectors_to_try) {
    elements <- page %>% html_elements(selector)
    if (length(elements) > 0)
      cat("Found", length(elements), "elements with selector:", selector, "\n")
  }

  scripts <- page %>% html_elements("script") %>% html_text()
  if (any(grepl("ajax|fetch|XMLHttpRequest|dynamically", scripts, ignore.case = TRUE)))
    cat("\n*** NOTICE: This page may load content dynamically with JavaScript ***\n")

  divs_with_classes <- page %>% html_elements("div[class]")
  class_names       <- divs_with_classes %>% html_attr("class")
  unique_classes    <- unique(unlist(strsplit(class_names, " ")))
  property_related  <- unique_classes[grepl("estate|property|listing|result|item|card",
                                            unique_classes, ignore.case = TRUE)]
  if (length(property_related) > 0) {
    cat("\nPotential property-related CSS classes found:\n")
    for (cls in property_related) {
      elements <- page %>% html_elements(paste0(".", cls))
      cat("-", cls, "(", length(elements), "elements )\n")
    }
  }

  estate_items <- page %>% html_elements(".estate-item")
  if (length(estate_items) > 0) {
    cat("\n=== PROPERTY LISTINGS FOUND ===\n")
    cat("Number of properties:", length(estate_items), "\n\n")
    for (i in seq_len(min(3, length(estate_items)))) {
      cat("--- Property", i, "---\n")
      property     <- estate_items[i]
      text_content <- property %>% html_text(trim = TRUE)
      cat("Text content:", substr(text_content, 1, 200), "...\n")
      links <- property %>% html_elements("a") %>% html_attr("href")
      if (length(links) > 0) { cat("Links found:", length(links), "\n"); cat("First link:", links[1], "\n") }
      images <- property %>% html_elements("img") %>% html_attr("src")
      if (length(images) > 0) cat("Images found:", length(images), "\n")
      cat("\n")
    }
  }

  return(page)
}

# ── Geolocation ────────────────────────────────────────────────────────────────

# Look up the centroid (WGS84 lat/lon) of each property's ZIP code polygon from
# the Swiss official localities geodatabase and write lat/lon back to properties.
# Re-running is safe: coordinates are updated in place for all matched rows.
geolocate <- function(db_path  = "rustico_properties.sqlite",
                      gdb_path = "data/AMTOVZ_GDB_LV95.gdb") {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required: install.packages('sf')")

  zip <- sf::st_read(gdb_path, layer = "AMTOVZ_ZIP", quiet = TRUE)

  # Compute centroid of each ZIP polygon and transform to WGS84
  centroids     <- sf::st_centroid(sf::st_geometry(zip))
  centroids_wgs <- sf::st_transform(centroids, 4326)
  coords        <- sf::st_coordinates(centroids_wgs)

  # Average coordinates for PLZs that span multiple polygons
  lookup <- aggregate(
    cbind(lon = coords[, 1], lat = coords[, 2]) ~ postal_code,
    data = data.frame(postal_code = as.character(zip$ZIP4), coords,
                      stringsAsFactors = FALSE),
    FUN  = mean
  )

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  init_db(db_path)   # ensure lat/lon columns exist

  props <- dbGetQuery(con, "SELECT id, postal_code FROM properties WHERE postal_code IS NOT NULL AND postal_code != ''")
  joined <- merge(props, lookup, by = "postal_code", all.x = TRUE)

  n_ok <- 0L
  for (i in seq_len(nrow(joined))) {
    if (!is.na(joined$lat[i])) {
      dbExecute(con, "UPDATE properties SET lat = ?, lon = ? WHERE id = ?",
                list(joined$lat[i], joined$lon[i], joined$id[i]))
      n_ok <- n_ok + 1L
    }
  }

  cat("Geolocated", n_ok, "of", nrow(props), "properties\n")
}

# ── Export ─────────────────────────────────────────────────────────────────────

export_to_csv <- function(db_path = "rustico_properties.sqlite",
                          filename = "rustico_properties.csv") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  properties <- dbReadTable(con, "properties")
  write.csv(properties, filename, row.names = FALSE)
  cat("Exported", nrow(properties), "properties to", filename, "\n")
}

# ── Entry point ────────────────────────────────────────────────────────────────

# Only run when executed directly (Rscript web_scraper.R), not when sourced
if (sys.nframe() == 0) {
  scrape_raw(max_pages = 233, items_per_page = 10)
  update_active_status()
  parse_and_enrich()
  apply_keywords()
  geolocate()
  export_to_csv()
}
