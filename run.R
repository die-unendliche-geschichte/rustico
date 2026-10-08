source("web_scraper.R")

# ── Full pipeline (uncomment steps as needed) ─────────────────────────────────
#
# Steps are independent and resumable — already-done work is skipped.
# Run them top-to-bottom for a fresh start; skip steps that haven't changed.

# ── 1. Scrape listing cards ───────────────────────────────────────────────────
# Fetches all listing pages and stores raw card text. Skips already-scraped pages.
# Set max_age_days to force re-scraping of pages older than N days.
#
# scrape_raw()
# scrape_raw(max_age_days = 30)    # re-scrape pages older than 30 days

# ── 2. Mark inactive listings ─────────────────────────────────────────────────
# Listings absent from the last full scrape cycle are marked is_active = 0.
# Run once after scrape_raw() completes a full cycle.
#
# update_active_status()

# ── 3. Parse card text → structured fields ────────────────────────────────────
# Parses raw card text into the properties table. Downloads thumbnail images.
# Only processes new/updated rows.
#
parse_and_enrich()

# ── 4. Scrape + parse detail pages ────────────────────────────────────────────
# Fetches each listing's detail page and re-parses for richer field coverage
# (rooms, build year, distances, etc.). Stores raw detail text in raw_properties.
# parse_detail_pages() alone re-parses already-fetched pages without hitting the network
# (use this when parsing logic changes).
#
# scrape_detail_pages() # network: fetch + parse all detail pages
parse_detail_pages() # local only: re-parse stored detail text (no network)

# ── 5. Apply keyword flags ────────────────────────────────────────────────────
# Tags each listing with matching blacklist / interesting keywords.
# Edit KEYWORDS_BLACKLIST and KEYWORDS_INTERESTING at the top of web_scraper.R,
# then re-run this step — no re-scraping needed.
#
# apply_keywords()

# ── 6. Geolocate (PLZ centroid) ───────────────────────────────────────────────
# Looks up the centroid of each property's ZIP polygon from AMTOVZ_GDB_LV95.gdb
# and writes lat/lon to properties. Re-run if postal codes have changed.
#
geolocate()

# ── 7. Enrich Gemeinde / Bezirk ───────────────────────────────────────────────
# Spatial join: PLZ centroid → Gemeinde polygon → writes gemeinde + bezirk
# to the properties table. Requires swissBOUNDARIES3D .gpkg.
# Once computed the data lives in SQLite — no need to re-run unless boundaries change.
#
enrich_gemeinde_bezirk()


set_precise_location("1003733924", 717668.8125, 140499.90625)
set_precise_location("1004530440", 719975.4375, 142667.281)
set_precise_location("1004542013", 2709806.35, 1118373.09, crs = 2056)


export_for_web()

# ── 8. Export ─────────────────────────────────────────────────────────────────
# export_to_csv()     #— full properties table → rustico_properties.csv
# export_for_web()    #— active properties + PLZ polygons → data/properties.js + data/plz.js
#                       (commit data/ to make changes live on GitHub Pages)
#
export_to_csv()
export_for_web()
