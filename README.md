# Rustico Suche — Property Scraper

Scrapes Swiss real-estate listings (category: houses/rustici for sale in Ticino) from
`immobiliensuche.edireal.com` into a local SQLite database, with structured field
extraction, keyword tagging, and active/inactive tracking.

---

## Requirements

```r
# Scraper
install.packages(c("rvest", "httr", "dplyr", "DBI", "RSQLite"))

# Dashboard
install.packages(c("DT", "crosstalk", "leaflet", "sf"))
# Also requires Quarto: https://quarto.org/docs/get-started/
```

---

## Pipeline overview

The workflow is split into four independent steps. Each step can be re-run in
isolation without repeating the others.

```
scrape_raw()          — fetch listing pages, store raw text (network-heavy)
    |
update_active_status() — mark properties no longer on the site as inactive
    |
parse_and_enrich()    — parse fields from raw text, download images (local + images)
    |
apply_keywords()      — tag blacklist / interesting keywords (local, instant)
    |
export_to_csv()       — dump properties table to CSV
```

Run everything at once:

```r
source("web_scraper.R")   # entry point guard: runs only when executed directly
# or interactively:
scrape_raw()
update_active_status()
parse_and_enrich()
apply_keywords()
export_to_csv()
```

---

## Step-by-step reference

### Trial run

Before a full scrape, test parsing on a small sample:

```r
source("web_scraper.R")
test_parse(n_pages = 1)   # scrapes 1 page, prints parsed output, writes nothing to properties
```

### Step 1 — `scrape_raw()`

```r
scrape_raw(
  max_pages    = 233,                        # upper limit on pages to fetch
  items_per_page = 2,                        # listings per page (site default)
  db_path      = "rustico_properties.sqlite",
  max_age_days = NULL                        # set e.g. 30 to re-scrape stale pages
)
```

- Resumable: pages already recorded in `scraped_pages` are skipped.
- Re-scraping: `max_age_days = 30` drops pages older than 30 days from
  `scraped_pages` before the loop, forcing them to be re-fetched.
- On re-scrape, raw text is updated in-place (UPSERT preserves the row id, so the
  foreign key from `properties` stays valid). `last_seen` is refreshed; `scraped_at`
  keeps the original first-seen timestamp.
- Retries failed requests up to 3 times with exponential back-off.

### Step 2 — `update_active_status()`

```r
update_active_status(
  db_path      = "rustico_properties.sqlite",
  max_age_days = 30   # properties not seen within this window are marked inactive
)
```

Properties that have disappeared from the site simply stop getting their `last_seen`
updated. After one full scrape cycle, `update_active_status()` sets `is_active = 0`
for anything older than the threshold. The `active_properties` view always reflects
the current active set.

### Step 3 — `parse_and_enrich()`

```r
parse_and_enrich(db_path = "rustico_properties.sqlite")
```

- Processes only rows in `raw_properties` that have no entry in `properties`, or
  where `raw_properties.scraped_at > properties.parsed_at` (re-scraped since last parse).
- Downloads the thumbnail image for each property into `images/`. Images are named
  after the last path segment of `property_link` (e.g. `tiimmobili-12345.jpg`),
  so filenames are stable across re-runs. Already-downloaded images are skipped.
- Safe to re-run: already up-to-date rows are skipped.
- To re-parse everything from scratch: `DELETE FROM properties`, delete `images/`, then re-run.

### Step 4 — `apply_keywords()`

```r
apply_keywords(
  db_path     = "rustico_properties.sqlite",
  blacklist   = KEYWORDS_BLACKLIST,    # defaults to the lists at top of file
  interesting = KEYWORDS_INTERESTING
)
```

Updates `blacklist_keywords`, `interesting_keywords`, and `is_blacklisted` on every
row in `properties`. Re-run any time you edit the keyword lists — no re-scraping
or re-parsing needed.

### Export

```r
export_to_csv(
  db_path  = "rustico_properties.sqlite",
  filename = "rustico_properties.csv"
)
```

---

## Keyword configuration

Edit the two vectors near the top of `web_scraper.R`:

```r
KEYWORDS_BLACKLIST <- tolower(c(
  "dorfrustico"
  # add terms to exclude here
))

KEYWORDS_INTERESTING <- tolower(c(
  "alleinlage", "ruhelage", "panorama", "aussicht",
  "vista", "quiete", "nucleo"
  # add terms of interest here
))
```

Matching is case-insensitive and substring-based against both the free-text
description and the full raw text. After editing, just re-run `apply_keywords()`.

---

## Database schema

### `raw_properties`

| Column | Type | Description |
|---|---|---|
| `id` | INTEGER PK | |
| `page` | INTEGER | Listing page number |
| `position` | INTEGER | Position on that page |
| `property_link` | TEXT UNIQUE | Absolute URL to the detail page |
| `image_url` | TEXT | Resolved image URL |
| `raw_text` | TEXT | Full `html_text()` of the listing element |
| `scraped_at` | DATETIME | When this property was **first** seen |
| `last_seen` | DATETIME | When it was **most recently** seen on the site |
| `is_active` | INTEGER | 1 = currently listed, 0 = no longer visible |

### `properties`

Derived from `raw_properties`. Can be dropped and rebuilt at any time.

| Column | Type | Description |
|---|---|---|
| `id` | INTEGER PK | |
| `raw_id` | INTEGER FK | References `raw_properties.id` |
| `property_link` | TEXT UNIQUE | |
| `object_number` | TEXT | Objekt Nummer |
| `object_type` | TEXT | Objekt Typ (e.g. Haus) |
| `state` | TEXT | Bundesland (e.g. Tessin) |
| `address` | TEXT | Full address string |
| `postal_code` | TEXT | Extracted from address |
| `city` | TEXT | Extracted from address |
| `purchase_price` | TEXT | Raw price string (e.g. "CHF 59.000,-") |
| `price_chf` | REAL | Numeric price in CHF |
| `living_area` | TEXT | Raw Wohnfläche string |
| `living_area_m2` | REAL | Numeric living area |
| `plot_area` | TEXT | Raw Grundstücksgrösse string |
| `plot_area_m2` | REAL | Numeric plot area |
| `built_area` | TEXT | Raw verbaute Fläche string |
| `built_area_m2` | REAL | Numeric built area |
| `rooms` | TEXT | Raw Zimmer string |
| `rooms_n` | REAL | Numeric room count |
| `heating` | TEXT | Heizung |
| `floors` | TEXT | Raw Geschosszahl string |
| `floors_n` | REAL | Numeric floor count |
| `noise_level` | TEXT | Lärmbelastung |
| `description` | TEXT | Free-text description (before structured fields) |
| `blacklist_keywords` | TEXT | Comma-separated matched blacklist terms |
| `interesting_keywords` | TEXT | Comma-separated matched interesting terms |
| `region` | TEXT | Geographical region (e.g. Sottoceneri) |
| `condition` | TEXT | Property condition |
| `lage` | TEXT | Location quality description |
| `ausblick` | TEXT | View description |
| `bathrooms` | TEXT | Badezimmer |
| `basement` | TEXT | Keller |
| `secondary_home` | TEXT | Zweitwohnung flag |
| `parking` | TEXT | Parking info |
| `is_blacklisted` | INTEGER | 1 if any blacklist keyword matched |
| `image_path` | TEXT | Local path to downloaded thumbnail |
| `lat` | REAL | Latitude (reserved) |
| `lon` | REAL | Longitude (reserved) |
| `parsed_at` | DATETIME | When this row was last parsed |

### `scraped_pages`

Tracks which listing pages have been fetched. Used by `scrape_raw()` to skip
already-scraped pages and by `max_age_days` to trigger re-scraping.

### `active_properties` (view)

```sql
SELECT p.*, r.last_seen, r.is_active
FROM properties p
JOIN raw_properties r ON r.id = p.raw_id
WHERE r.is_active = 1
```

Use this for day-to-day queries to automatically exclude sold/withdrawn listings.

---

## Typical refresh workflow

```r
source("web_scraper.R")

# Re-scrape pages older than 14 days
scrape_raw(max_age_days = 14)

# Mark anything not seen in 14 days as inactive
update_active_status(max_age_days = 14)

# Parse any new or updated listings
parse_and_enrich()

# Re-apply keywords (fast, always safe to re-run)
apply_keywords()
```

---

## Dashboard

`index.qmd` is a Quarto dashboard that reads from the SQLite database and renders
an interactive HTML report.

```bash
quarto preview index.qmd   # live preview
quarto render  index.qmd   # build index.html
```

The dashboard shows:
- **Value boxes** — total listings, active count, average and range of prices
- **Properties tab** — filterable DT table with per-column filters
- **Map tab** — leaflet map with one circle marker per property, placed at the PLZ
  centroid; marker colour encodes price. Table filters drive the map via crosstalk —
  filtering by region or price range highlights only the matching markers.

Requires `data/AMTOVZ_GDB_LV95.gdb` (Swiss official ZIP polygon dataset) for the map.

---

## TODOs

- **Tag system for listings** — allow adding/removing tags per listing (e.g. "alleinlage", "dorfrustico") directly in the dashboard, persisted to the database, so items can be quickly included or excluded from view.
- **Checkbox filter for Gemeinde / Bezirk** — replace the text column filter with a proper multi-select checkbox dropdown. AG Grid Community doesn't have `agSetColumnFilter` (Enterprise only); needs a custom external filter implementation.
- **Mobile-friendly layout** — the current split-pane design doesn't work on small screens. Needs a responsive layout (e.g. stacked table/map with a toggle, or a drawer).

---

## Known data gaps

### PLZ codes not matched to Gemeinde/Bezirk

The following postal codes are present in the scraped data but missing from
`AMTOVZ_GDB_LV95.gdb` (the Swiss official ZIP polygon dataset), so no
Gemeinde/Bezirk can be assigned to listings with these PLZ:

| PLZ  | Notes |
|------|-------|
| 6663 | |
| 6664 | |
| 6911 | Campione d'Italia (Italian exclave within CH) |

To investigate: check whether these PLZ have been reassigned, merged, or are
otherwise present under a different code in a newer AMTOVZ edition.

---

## File structure

```
web_scraper.R               — all scraping, parsing, and DB logic
index.qmd                   — Quarto dashboard
rustico_properties.sqlite   — SQLite database (created on first run)
rustico_properties.csv      — CSV export (created by export_to_csv())
images/                     — downloaded property thumbnails (named by property_link slug)
data/AMTOVZ_GDB_LV95.gdb   — Swiss ZIP polygons (for map centroids)
```
