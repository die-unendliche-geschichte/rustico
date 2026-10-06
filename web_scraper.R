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
    page <- read_html(url)
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
# Handles both the compact format ("Zimmer:4Heizung:...") and the
# spaced-out key/value format ("Kaufpreis:\nCHF 59.000,-\n...") by
# normalising whitespace first, then using all known label names as
# look-ahead anchors so each field value stops at the next label.
parse_property_text <- function(text,
                               blacklist   = KEYWORDS_BLACKLIST,
                               interesting = KEYWORDS_INTERESTING) {
  text <- gsub("\\s+", " ", trimws(text))

  labels <- c(
    object_number  = "Objekt\\s*Nummer",
    object_type    = "Objekt\\s*Typ",
    state          = "Bundesland",
    address        = "Adresse",
    purchase_price = "Kaufpreis",
    living_area    = "Wohnfl\u00e4che",
    plot_area      = "Grundst\u00fccks(?:gr\u00f6sse|gr\u00f6\u00dfe|groesse)",
    built_area     = "verbaute\\s*Fl\u00e4che",
    rooms          = "Zimmer",
    heating        = "Heizung",
    floors         = "Geschosszahl",
    noise_level    = "L\u00e4rmbelastung"
  )

  all_labels_pat <- paste(labels, collapse = "|")
  result <- setNames(rep(list(NA_character_), length(labels)), names(labels))

  for (col_name in names(labels)) {
    pat <- paste0(
      "(?i)(?:", labels[[col_name]], ")\\s*:?\\s*(.*?)(?=\\s*(?:",
      all_labels_pat, ")|$)"
    )
    m <- regmatches(text, regexpr(pat, text, perl = TRUE))
    if (length(m) > 0 && nchar(m) > 0) {
      strip_pat <- paste0("^(?i)(?:", labels[[col_name]], ")\\s*:?\\s*")
      result[[col_name]] <- trimws(sub(strip_pat, "", m, perl = TRUE))
    }
  }

  # Extract free-text description: everything before the first known label
  first_label_pos <- regexpr(paste0("(?i)(?:", all_labels_pat, ")"), text, perl = TRUE)
  result$description <- if (first_label_pos[1] > 1) {
    trimws(substr(text, 1, first_label_pos[1] - 1))
  } else {
    NA_character_
  }

  # Keyword matching against description + full text
  search_text <- paste(result$description, text, sep = " ")
  result$blacklist_keywords   <- match_keywords(search_text, blacklist)
  result$interesting_keywords <- match_keywords(search_text, interesting)
  result$is_blacklisted       <- as.integer(!is.na(result$blacklist_keywords))

  # Derive postal code and city from address field (e.g. "6658 Borgnone")
  addr <- result$address
  if (!is.na(addr)) {
    addr_m <- regmatches(addr, regexec("^([0-9]{4})\\s+(.+)$", trimws(addr)))[[1]]
    result$postal_code <- if (length(addr_m) >= 3) addr_m[2] else NA_character_
    result$city        <- if (length(addr_m) >= 3) addr_m[3] else NA_character_
  } else {
    result$postal_code <- NA_character_
    result$city        <- NA_character_
  }

  # Parse numeric forms of the key fields
  result$price_chf      <- as.numeric(gsub("[^0-9]", "", sub("CHF\\s*", "", result$purchase_price)))
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

  # Raw scraped data — append-only, never mutated after insert
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS raw_properties (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      page          INTEGER,
      position      INTEGER,
      property_link TEXT UNIQUE,
      image_url     TEXT,
      raw_text      TEXT,
      scraped_at    DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

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
      noise_level         TEXT,
      description         TEXT,
      blacklist_keywords  TEXT,
      interesting_keywords TEXT,
      is_blacklisted      INTEGER DEFAULT 0,
      image_path          TEXT,
      parsed_at           DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS scraped_pages (
      page_idx    INTEGER PRIMARY KEY,
      items_found INTEGER,
      scraped_at  DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  con
}

# ── Step 1: Scrape ─────────────────────────────────────────────────────────────

# Hit the listing pages and store raw text + resolved URLs in raw_properties.
# Resumable: pages already recorded in scraped_pages are skipped.
# No parsing happens here — this step only touches the website.
scrape_raw <- function(max_pages = 233, items_per_page = 2,
                       db_path = "rustico_properties.sqlite") {
  base_url <- paste0(
    "https://immobiliensuche.edireal.com/tiimmobili.ch/search",
    "?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL",
    "&country=CH&category=8",
    "&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo"
  )

  con <- init_db(db_path)
  on.exit(dbDisconnect(con), add = TRUE)

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
            "INSERT OR IGNORE INTO raw_properties (page, position, property_link, image_url, raw_text)
             VALUES (?, ?, ?, ?, ?)",
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
    WHERE r.id NOT IN (SELECT raw_id FROM properties WHERE raw_id IS NOT NULL)
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
      filename <- paste0("property_", row$page, "_", row$position, ".", file_extension)

      cat("[", row_idx, "/", nrow(unparsed), "] Downloading image...")
      downloaded_path <- download_image(row$image_url, filename)
      if (!is.na(downloaded_path)) {
        image_path <- downloaded_path
        cat(" OK\n")
      } else {
        cat(" Failed\n")
      }
    }

    tryCatch(
      dbExecute(con,
        "INSERT OR IGNORE INTO properties
         (raw_id, property_link,
          object_number, object_type, state, address, postal_code, city,
          purchase_price, price_chf,
          living_area, living_area_m2, plot_area, plot_area_m2,
          built_area, built_area_m2, rooms, rooms_n,
          heating, floors, floors_n, noise_level,
          description, blacklist_keywords, interesting_keywords, is_blacklisted,
          image_path)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
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
          parsed$description,
          parsed$blacklist_keywords, parsed$interesting_keywords, parsed$is_blacklisted,
          image_path
        )
      ),
      error = function(e) cat("  DB write error:", e$message, "\n")
    )
  }

  total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM properties")$n
  cat("\n=== PARSING COMPLETE ===\n")
  cat("Total properties parsed:", total, "\n")
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
  scrape_raw(max_pages = 233, items_per_page = 2)
  parse_and_enrich()
  export_to_csv()
}
