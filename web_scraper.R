library(rvest)
library(dplyr)
library(httr)
library(DBI)
library(RSQLite)

# Function to download image
download_image <- function(image_url, filename) {
  tryCatch({
    # Create images directory if it doesn't exist
    if (!dir.exists("images")) {
      dir.create("images")
    }
    
    # Clean filename to avoid issues
    clean_filename <- gsub("[^A-Za-z0-9._-]", "_", filename)
    full_path <- file.path("images", clean_filename)
    
    # Download the image
    response <- GET(image_url, write_disk(full_path, overwrite = TRUE))
    
    if (status_code(response) == 200) {
      return(full_path)
    } else {
      cat("Failed to download image:", image_url, "\n")
      return(NA)
    }
  }, error = function(e) {
    cat("Error downloading image:", e$message, "\n")
    return(NA)
  })
}

scrape_website <- function(url, css_selector = NULL) {
  tryCatch({
    page <- read_html(url)
    
    if (is.null(css_selector)) {
      return(page)
    } else {
      data <- page %>%
        html_elements(css_selector) %>%
        html_text(trim = TRUE)
      return(data)
    }
  }, error = function(e) {
    cat("Error scraping:", e$message, "\n")
    return(NULL)
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
  return(NULL)
}

scrape_table <- function(url, table_index = 1) {
  tryCatch({
    page <- read_html(url)
    tables <- page %>% html_table()
    
    if (length(tables) >= table_index) {
      return(tables[[table_index]])
    } else {
      cat("Table index", table_index, "not found\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error scraping table:", e$message, "\n")
    return(NULL)
  })
}

scrape_links <- function(url, link_selector = "a") {
  tryCatch({
    page <- read_html(url)
    links <- page %>%
      html_elements(link_selector) %>%
      html_attr("href")
    return(links)
  }, error = function(e) {
    cat("Error scraping links:", e$message, "\n")
    return(NULL)
  })
}

test_pagination <- function() {
  base_url <- "https://immobiliensuche.edireal.com/tiimmobili.ch/search?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL&country=CH&category=8&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo&itemsPerPage=10"
  
  cat("=== TESTING PAGINATION ===\n\n")
  
  # Test first 3 pages
  for (page_idx in 0:2) {
    url <- paste0(base_url, "&pageIdx=", page_idx)
    cat("Testing page", page_idx + 1, "(pageIdx=", page_idx, ")\n")
    cat("URL:", url, "\n")
    
    page <- scrape_website(url)
    
    if (!is.null(page)) {
      estate_items <- page %>% html_elements(".estate-item")
      cat("Found", length(estate_items), "properties on this page\n")
      
      if (length(estate_items) > 0) {
        # Show first property from each page as example
        first_property <- estate_items[1] %>% html_text(trim = TRUE)
        cat("First property preview:", substr(first_property, 1, 100), "...\n")
      }
    } else {
      cat("Failed to scrape page", page_idx + 1, "\n")
    }
    
    cat("\n")
    
    # Add small delay to be respectful to the server
    Sys.sleep(1)
  }
}

analyze_page_structure <- function() {
  url <- "https://immobiliensuche.edireal.com/tiimmobili.ch/search?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL&country=CH&category=8&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo&itemsPerPage=10&pageIdx=0"
  
  page <- scrape_website(url)
  
  if (!is.null(page)) {
    cat("=== PAGE STRUCTURE ANALYSIS ===\n\n")
    
    # Look for common real estate listing patterns
    selectors_to_try <- c(
      ".estate", ".estate-item", ".property", ".listing", ".result-item", 
      ".search-result", "[data-estate]", ".item", ".card", ".result",
      "article", ".property-card", ".listing-item", ".real-estate-item"
    )
    
    for (selector in selectors_to_try) {
      elements <- page %>% html_elements(selector)
      if (length(elements) > 0) {
        cat("Found", length(elements), "elements with selector:", selector, "\n")
      }
    }
    
    # Check for JavaScript-loaded content indicators
    scripts <- page %>% html_elements("script") %>% html_text()
    if (any(grepl("ajax|fetch|XMLHttpRequest|dynamically", scripts, ignore.case = TRUE))) {
      cat("\n*** NOTICE: This page may load content dynamically with JavaScript ***\n")
    }
    
    # Look for div elements with class attributes that might contain listings
    divs_with_classes <- page %>% html_elements("div[class]")
    class_names <- divs_with_classes %>% html_attr("class")
    unique_classes <- unique(unlist(strsplit(class_names, " ")))
    
    property_related_classes <- unique_classes[grepl("estate|property|listing|result|item|card", unique_classes, ignore.case = TRUE)]
    if (length(property_related_classes) > 0) {
      cat("\nPotential property-related CSS classes found:\n")
      for (cls in property_related_classes) {
        elements <- page %>% html_elements(paste0(".", cls))
        cat("-", cls, "(", length(elements), "elements )\n")
      }
    }
    
    # Get detailed info about the estate items
    estate_items <- page %>% html_elements(".estate-item")
    if (length(estate_items) > 0) {
      cat("\n=== PROPERTY LISTINGS FOUND ===\n")
      cat("Number of properties:", length(estate_items), "\n\n")
      
      # Extract details from first few properties as examples
      for (i in 1:min(3, length(estate_items))) {
        cat("--- Property", i, "---\n")
        property <- estate_items[i]
        
        # Try to extract text content
        text_content <- property %>% html_text(trim = TRUE)
        cat("Text content:", substr(text_content, 1, 200), "...\n")
        
        # Look for links
        links <- property %>% html_elements("a") %>% html_attr("href")
        if (length(links) > 0) {
          cat("Links found:", length(links), "\n")
          cat("First link:", links[1], "\n")
        }
        
        # Look for images
        images <- property %>% html_elements("img") %>% html_attr("src")
        if (length(images) > 0) {
          cat("Images found:", length(images), "\n")
        }
        
        cat("\n")
      }
    }
    
    return(page)
  } else {
    cat("Failed to scrape the page\n")
    return(NULL)
  }
}

init_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS properties (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      page           INTEGER,
      position       INTEGER,
      postal_code    TEXT,
      city           TEXT,
      price          TEXT,
      living_area    TEXT,
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

  return(con)
}

scrape_all_properties <- function(max_pages = 233, items_per_page = 2, db_path = "rustico_properties.sqlite") {
  base_url <- "https://immobiliensuche.edireal.com/tiimmobili.ch/search?sorting=MODIFICATION_DATE&estates-view=list-view&offerType=SELL&country=CH&category=8&priceFrom&priceTo&areaFrom&areaTo&groundAreaFrom&groundAreaTo&roomNumberFrom&roomNumberTo"

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
        property <- estate_items[i]

        text_content <- property %>% html_text(trim = TRUE)

        # Extract postal code and city using capture groups
        postal_code <- ""
        city <- ""
        plz_match <- regmatches(text_content, regexec("PLZ, ORT : ([0-9]{4}) ([^\n\r]+)", text_content))[[1]]
        if (length(plz_match) >= 3) {
          postal_code <- plz_match[2]
          city <- trimws(gsub("\\s+[A-ZÜÄÖ]{2,}.*$", "", plz_match[3]))
        }

        if (postal_code == "" || city == "") {
          alt_match <- regmatches(text_content, regexec("([0-9]{4}) ([A-Za-züäöÄÖÜ][A-Za-züäöÄÖÜ -]*[A-Za-züäöÄÖÜ])", text_content))[[1]]
          if (length(alt_match) >= 3) {
            postal_code <- alt_match[2]
            city <- alt_match[3]
          }
        }

        # Extract price
        price_match <- regmatches(text_content, regexpr("CHF [0-9.,]+,-", text_content))
        price <- ifelse(length(price_match) > 0, price_match[1], "")

        # Extract living area
        area_match <- regmatches(text_content, regexpr("Wohnfläche:[0-9.,]+ m²", text_content))
        living_area <- ifelse(length(area_match) > 0, area_match[1], "")

        # Extract property link
        link_nodes <- property %>% html_elements("a")
        property_link <- ""
        if (length(link_nodes) > 0) {
          relative_link <- link_nodes[1] %>% html_attr("href")
          if (!is.na(relative_link)) {
            if (startsWith(relative_link, "http")) {
              property_link <- relative_link
            } else if (startsWith(relative_link, "/")) {
              property_link <- paste0("https://immobiliensuche.edireal.com", relative_link)
            } else {
              property_link <- paste0("https://immobiliensuche.edireal.com/tiimmobili.ch/", relative_link)
            }
          }
        }

        # Extract and download images
        image_nodes <- property %>% html_elements("img")
        image_path <- ""
        if (length(image_nodes) > 0) {
          image_src <- image_nodes[1] %>% html_attr("src")
          if (!is.na(image_src)) {
            if (startsWith(image_src, "//")) {
              image_url <- paste0("https:", image_src)
            } else if (startsWith(image_src, "/")) {
              image_url <- paste0("https://immobiliensuche.edireal.com", image_src)
            } else if (!startsWith(image_src, "http")) {
              image_url <- paste0("https://immobiliensuche.edireal.com/", image_src)
            } else {
              image_url <- image_src
            }

            clean_url <- sub("\\?.*$", "", image_url)
            file_extension <- tools::file_ext(basename(clean_url))
            if (file_extension == "") file_extension <- "jpg"
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

        # Write row directly to database; IGNORE silently skips duplicate property_link
        tryCatch(
          dbExecute(con,
            "INSERT OR IGNORE INTO properties
             (page, position, postal_code, city, price, living_area, property_link, image_path, full_text)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            list(page_idx + 1, i, postal_code, city, price, living_area,
                 property_link, image_path, substr(text_content, 1, 500))
          ),
          error = function(e) cat("  DB write error:", e$message, "\n")
        )
      }

      # Mark this page as done so it is skipped on resume
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

# Export the properties table from the database to a CSV file
export_to_csv <- function(db_path = "rustico_properties.sqlite", filename = "rustico_properties.csv") {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  properties <- dbReadTable(con, "properties")
  write.csv(properties, filename, row.names = FALSE)
  cat("Exported", nrow(properties), "properties to", filename, "\n")
}

# Only run when executed directly (Rscript web_scraper.R), not when sourced
if (sys.nframe() == 0) {
  scrape_all_properties(max_pages = 233, items_per_page = 2)
  export_to_csv()
}