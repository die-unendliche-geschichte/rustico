library(rvest)
library(dplyr)
library(httr)
library(DBI)
library(RSQLite)

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
parse_property_text <- function(text) {
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
    # Match: label + optional colon + optional space + value (non-greedy)
    # Stop at the next label or end of string
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

  # Derive postal code and city from the address field (e.g. "6658 Borgnone")
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

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS properties (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      page           INTEGER,
      position       INTEGER,
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
      noise_level    TEXT,
      property_link  TEXT UNIQUE,
      image_path     TEXT,
      full_text      TEXT,
      scraped_at     DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS scraped_pages (
      page_idx    INTEGER PRIMARY KEY,
      items_found INTEGER,
      scraped_at  DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Migrate existing databases: add any columns introduced since first creation
  existing_cols <- dbListFields(con, "properties")
  new_cols <- list(
    object_number  = "TEXT", object_type    = "TEXT", state          = "TEXT",
    address        = "TEXT", postal_code    = "TEXT", city           = "TEXT",
    purchase_price = "TEXT", price_chf      = "REAL",
    living_area    = "TEXT", living_area_m2 = "REAL",
    plot_area      = "TEXT", plot_area_m2   = "REAL",
    built_area     = "TEXT", built_area_m2  = "REAL",
    rooms          = "TEXT", rooms_n        = "REAL",
    heating        = "TEXT", floors         = "TEXT", floors_n = "REAL",
    noise_level    = "TEXT"
  )
  for (col in names(new_cols)) {
    if (!col %in% existing_cols)
      dbExecute(con, sprintf("ALTER TABLE properties ADD COLUMN %s %s", col, new_cols[[col]]))
  }

  con
}

# ── Main scraper ───────────────────────────────────────────────────────────────

scrape_all_properties <- function(max_pages = 233, items_per_page = 2,
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

  cat("=== SCRAPING ALL PROPERTIES ===\n")
  cat("Max pages to scrape:", max_pages, "\n")
  cat("Items per page:", items_per_page, "\n")
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
        property     <- estate_items[i]
        text_content <- property %>% html_text(trim = TRUE)
        parsed       <- parse_property_text(text_content)

        # Property link
        link_nodes    <- property %>% html_elements("a")
        property_link <- NA_character_
        if (length(link_nodes) > 0) {
          rel <- link_nodes[1] %>% html_attr("href")
          if (!is.na(rel)) {
            property_link <- if (startsWith(rel, "http")) {
              rel
            } else if (startsWith(rel, "/")) {
              paste0("https://immobiliensuche.edireal.com", rel)
            } else {
              paste0("https://immobiliensuche.edireal.com/tiimmobili.ch/", rel)
            }
          }
        }

        # Image
        image_nodes <- property %>% html_elements("img")
        image_path  <- NA_character_
        if (length(image_nodes) > 0) {
          image_src <- image_nodes[1] %>% html_attr("src")
          if (!is.na(image_src)) {
            image_url <- if (startsWith(image_src, "//")) {
              paste0("https:", image_src)
            } else if (startsWith(image_src, "/")) {
              paste0("https://immobiliensuche.edireal.com", image_src)
            } else if (!startsWith(image_src, "http")) {
              paste0("https://immobiliensuche.edireal.com/", image_src)
            } else {
              image_src
            }

            clean_url      <- sub("\\?.*$", "", image_url)
            file_extension <- tools::file_ext(basename(clean_url))
            if (!nzchar(file_extension)) file_extension <- "jpg"
            filename <- paste0("property_", page_idx + 1, "_", i, ".", file_extension)

            cat("  Downloading image for property", i, "...")
            downloaded_path <- download_image(image_url, filename)
            if (!is.na(downloaded_path)) {
              image_path <- downloaded_path
              cat(" OK\n")
            } else {
              cat(" Failed\n")
            }
          }
        }

        # Write row immediately; IGNORE silently skips duplicate property_link
        tryCatch(
          dbExecute(con,
            "INSERT OR IGNORE INTO properties
             (page, position,
              object_number, object_type, state, address, postal_code, city,
              purchase_price, price_chf,
              living_area, living_area_m2, plot_area, plot_area_m2,
              built_area, built_area_m2, rooms, rooms_n,
              heating, floors, floors_n, noise_level,
              property_link, image_path, full_text)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            list(
              page_idx + 1, i,
              parsed$object_number, parsed$object_type, parsed$state,
              parsed$address, parsed$postal_code, parsed$city,
              parsed$purchase_price, parsed$price_chf,
              parsed$living_area,    parsed$living_area_m2,
              parsed$plot_area,      parsed$plot_area_m2,
              parsed$built_area,     parsed$built_area_m2,
              parsed$rooms,          parsed$rooms_n,
              parsed$heating, parsed$floors, parsed$floors_n,
              parsed$noise_level,
              property_link, image_path,
              substr(text_content, 1, 500)
            )
          ),
          error = function(e) cat("  DB write error:", e$message, "\n")
        )
      }

      # Mark this page done so it is skipped on resume
      dbExecute(con,
        "INSERT OR REPLACE INTO scraped_pages (page_idx, items_found) VALUES (?, ?)",
        list(page_idx, length(estate_items))
      )
    } else {
      cat(" Failed to scrape page", page_idx + 1, "\n")
    }

    Sys.sleep(2)
  }

  total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM properties")$n
  cat("\n=== SCRAPING COMPLETE ===\n")
  cat("Total properties in database:", total, "\n")
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
  scrape_all_properties(max_pages = 233, items_per_page = 2)
  export_to_csv()
}
