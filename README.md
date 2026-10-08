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

Each step is independent and resumable — already-done work is skipped.
See `run.R` for a commented menu of all steps.

```
1. scrape_raw()              fetch listing cards → raw_properties (network)
2. update_active_status()    mark delisted properties inactive
3. parse_and_enrich()        card text → structured fields, download images
4. scrape_detail_pages()     fetch detail pages → richer fields  (network)
   parse_detail_pages()        re-parse stored detail text (no network)
5. apply_keywords()          tag blacklist / interesting keywords
6. geolocate()               PLZ centroid → lat/lon  (needs AMTOVZ .gdb)
7. enrich_gemeinde_bezirk()  PLZ → Gemeinde/Bezirk   (needs swissBOUNDARIES3D .gpkg)
8. export_to_csv()           dump properties table to CSV
   export_for_web()          write data/properties.js + data/plz.js for dashboard
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

### Step 3b — `scrape_detail_pages()` / `parse_detail_pages()`

```r
scrape_detail_pages()   # fetch each listing's detail page + immediately re-parse
parse_detail_pages()    # re-parse already-fetched detail text (no network calls)
```

`scrape_detail_pages()` fetches the full detail page for each listing, stores the raw
text in `raw_properties.detail_text`, then calls `parse_detail_pages()` automatically.
Use `parse_detail_pages()` alone whenever the parsing logic changes — no network needed.

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

### Step 5 — `geolocate()`

```r
geolocate(
  db_path  = "rustico_properties.sqlite",
  gdb_path = "data/AMTOVZ_GDB_LV95.gdb"
)
```

Computes the centroid of each property's ZIP polygon and writes `lat`/`lon` to
`properties`. Requires the AMTOVZ geodatabase (gitignored — too large).

### Step 6 — `enrich_gemeinde_bezirk()`

```r
enrich_gemeinde_bezirk(
  db_path   = "rustico_properties.sqlite",
  gpkg_path = "data/swissBOUNDARIES3D_1_5_LV95_LN02.gpkg",  # default path
  gdb_path  = "data/AMTOVZ_GDB_LV95.gdb"
)

# If the .gpkg is elsewhere on your system:
enrich_gemeinde_bezirk(
  gpkg_path = "/path/to/swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"
)
```

Spatial join: PLZ centroid → Gemeinde polygon → writes `gemeinde` + `bezirk` to
`properties`. Requires `swissBOUNDARIES3D_1_5_LV95_LN02.gpkg` (gitignored).
Once computed the data lives in SQLite — no need to re-run unless boundaries change.

### Step 7 — Export

```r
export_to_csv()    # full properties table → rustico_properties.csv

export_for_web()   # active properties + PLZ polygons → data/properties.js + data/plz.js
                   # commit data/ afterwards to update GitHub Pages
```

### Precise geolocation (manual)

When you know the exact location of a property (e.g. from the cadastral map), set it with:

```r
source("web_scraper.R")

# listing_id: the numeric ID from the listing URL (e.g. "1003733924"),
#             the full URL, or the integer DB id
# easting/northing: Swiss coordinates in LV95 (~2.7M) or LV03 (~700K) — auto-detected
set_precise_location("1003733924", 719738.875, 140367.281)

# Then re-export so the dashboard picks it up
export_for_web()
```

Coordinates are converted to WGS84 locally via the `sf` package (LV95/EPSG:2056 or LV03/EPSG:21781, auto-detected from magnitude).
`export_for_web()` will use the precise coords instead of the PLZ centroid.
On the map, precisely located properties show as solid markers; PLZ-centroid ones are semi-transparent with a dashed border.

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

- **Tag system for listings** — allow adding/removing tags per listing (e.g. "alleinlage", "dorfrustico", "favorite") directly in the dashboard, persisted to the database, so items can be quickly included or excluded from view. 
- **Precise geolocation per listing** — implemented via `set_precise_location()`. See below.

- **PLZ-polygon map with precise-location point overlay** — the current clustered circle markers are a holdover from the Quarto/Crosstalk prototype. With pure JS this can be done better: use filter-aware coloured PLZ polygons as the primary map layer (one polygon per PLZ, colour/opacity encoding e.g. count or median price of visible listings), and only show individual point markers for listings with `location_precise = true`. Clicking a polygon would highlight its listings in the grid; precise-location markers keep their existing popup. This removes the need for marker clustering entirely.

- **6911 Campione d'Italia** — Italian exclave with a Swiss postal code. Listings there are geographically in Italy and don't fit the current Ticino-focused setup (wrong canton assignment, no Gemeinde/Bezirk match). Options: filter them out during export, flag them with a dedicated tag, or handle the PLZ separately in the geolocation pipeline. See also the Known data gaps section.

- **Bezirk/Gemeinde hierarchy in filters** — currently Bezirk and Gemeinde are independent flat filters. Consider linking them: selecting a Bezirk should narrow the Gemeinde options to only those within it, and vice versa. Requires the filters to share state and re-render each other on change.

- **Parcel polygon overlay** — listings often include a screenshot of the cantonal cadastral map showing the parcel boundary. Pipeline:
  1. Identify which scraped image(s) show a cadastral/parcel map screenshot (vs. photos of the building) — requires a capable vision model, preferably local (e.g. LLaVA, Qwen-VL, or similar).
  2. Extract the parcel number from that image via OCR/vision inference.
  3. Resolve commune name + parcel number → EGRID + polygon via the swisstopo API (see below).
  4. Overlay the polygon on the Leaflet map per listing.

---

## Swiss cadastral parcel API

All calls are free, no authentication required.

### Commune name + parcel number → EGRID + feature ID

```
GET https://api3.geo.admin.ch/rest/services/ech/SearchServer?searchText={commune}+{parcel_number}&type=locations&origins=parcel
```

Example: `searchText=Aranno+357`

Returns (relevant fields from `results[0].attrs`):

| Field | Example | Notes |
|---|---|---|
| `detail` | `357 aranno 5143 ch130292077506` | commune, BFS nr, EGRID |
| `egris_egrid` | `CH130292077506` | national parcel identifier |
| `id` | `1575559` | internal feature ID, used in step 2 |
| `lat` / `lon` | `46.0117 / 8.8735` | centroid in WGS84 |
| `geom_st_box2d` | `BOX(711094 96542, ...)` | bbox in LV95 |

If the commune name is ambiguous, filter by BFS number in the `detail` string (format: `{parcel} {commune} {bfs_nr} {egrid}`).

### Feature ID → parcel polygon (WGS84)

```
GET https://api3.geo.admin.ch/rest/services/all/MapServer/
    ch.swisstopo-vd.amtliche-vermessung/{feature_id}
    ?returnGeometry=true&sr=4326
```

Returns a polygon ring as `feature.geometry.rings[0]` — array of `[lon, lat]` pairs, ready for Leaflet/GeoJSON.

### BFS Gemeindenummer → commune name (optional)

Only needed if you have the BFS number but not the name:

```
GET https://api3.geo.admin.ch/rest/services/all/MapServer/
    ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill/{bfs_nr}
    ?returnGeometry=false
```

Returns `feature.attributes.gemname`.

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
