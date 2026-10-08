#!/usr/bin/env python3
"""
Enrich properties.js with gemeinde and bezirk.

PLZ → Gemeinde pipeline:
  AMTOVZ_ZIP.ZIP4 → AMTOVZ_ZIP.FK_LOCALITY
                  → AMTOVZ_LOCALITY geometry (centroid)
                  → point-in-polygon into tlm_hoheitsgebiet → Gemeinde
                  → bezirksnummer → tlm_bezirksgebiet → Bezirk name
"""

import json, re, sqlite3
from pathlib import Path

import geopandas as gpd

ROOT    = Path(__file__).parent.parent
GPKG    = ROOT / "data" / "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"
AMTOVZ  = ROOT / "data" / "AMTOVZ_GDB_LV95.gdb"
PROP_JS = ROOT / "data" / "properties.js"
SQLITE  = ROOT / "rustico_properties.sqlite"

# ── Load Gemeinde polygons ────────────────────────────────────────────────────

print("Loading Gemeinde polygons …")
gemeinden = (
    gpd.read_file(GPKG, layer="tlm_hoheitsgebiet")
    .pipe(lambda df: df[df["objektart"] == "Gemeindegebiet"])
    [["name", "bezirksnummer", "geometry"]]
)  # stays in LV95 (EPSG:2056) — same CRS as AMTOVZ

print("Loading Bezirk name table …")
bezirk_map = dict(zip(
    *gpd.read_file(GPKG, layer="tlm_bezirksgebiet")[["bezirksnummer", "name"]].values.T
))

# ── Build PLZ centroid GeoDataFrame from AMTOVZ ───────────────────────────────

print("Loading AMTOVZ localities …")
localities = gpd.read_file(AMTOVZ, layer="AMTOVZ_LOCALITY")[["LOCALITYID", "geometry"]]
localities = localities.assign(
    geometry=localities.to_crs(epsg=2056).geometry.centroid
).to_crs(epsg=2056)

print("Loading AMTOVZ ZIP table …")
zips = gpd.read_file(AMTOVZ, layer="AMTOVZ_ZIP")[["ZIP4", "FK_LOCALITY"]]

# join ZIP → locality centroid
plz_centroids = (
    zips.merge(localities, left_on="FK_LOCALITY", right_on="LOCALITYID", how="left")
    .dropna(subset=["geometry"])
    .drop_duplicates("ZIP4")
)
plz_centroids = gpd.GeoDataFrame(plz_centroids, geometry="geometry", crs=2056)

# ── Spatial join: PLZ centroid → Gemeinde ─────────────────────────────────────

print(f"Joining {len(plz_centroids)} PLZ centroids to Gemeinden …")
joined = gpd.sjoin(plz_centroids, gemeinden, how="left", predicate="within")

# fallback for any that missed (right on a border)
missed_mask = joined["name"].isna()
if missed_mask.any():
    missed_pts = plz_centroids[plz_centroids["ZIP4"].isin(joined.loc[missed_mask, "ZIP4"])]
    fallback = gpd.sjoin_nearest(missed_pts, gemeinden, how="left")
    joined.loc[missed_mask, ["name", "bezirksnummer"]] = fallback[["name", "bezirksnummer"]].values

plz_lookup = {}
for _, row in joined.iterrows():
    bezirk = bezirk_map.get(row["bezirksnummer"]) if row["bezirksnummer"] else None
    plz_lookup[str(int(row["ZIP4"]))] = {"gemeinde": row["name"] or None, "bezirk": bezirk}

# ── Load properties from SQLite ───────────────────────────────────────────────

print("Loading properties from SQLite …")
con = sqlite3.connect(SQLITE)
con.row_factory = sqlite3.Row
cur = con.cursor()
cur.execute("""
    SELECT p.id, p.postal_code, p.city, p.region,
           p.price_chf, p.living_area_m2, p.plot_area_m2,
           p.rooms_n, p.wc_n, p.bathrooms_n, p.floors_n,
           p.build_year_n,
           p.condition, p.condition_cat,
           p.secondary_home, p.secondary_home_yn,
           p.noise_level_cat, p.basement_yn, p.wasser_yn,
           p.parking, p.lage, p.ausblick,
           p.public_transport_m, p.dist_highway_km, p.dist_city_km,
           p.schools, p.garden,
           p.interesting_keywords, p.blacklist_keywords,
           COALESCE(p.lat_precise, p.lat) AS lat,
           COALESCE(p.lon_precise, p.lon) AS lon,
           CASE WHEN p.lat_precise IS NOT NULL THEN 1 ELSE 0 END AS location_precise,
           r.property_link
    FROM properties p
    JOIN raw_properties r ON r.id = p.raw_id
    WHERE r.is_active = 1
    ORDER BY p.price_chf ASC NULLS LAST
""")
properties = [dict(row) for row in cur.fetchall()]
con.close()
print(f"Loaded {len(properties)} properties.")

# ── Enrich properties ─────────────────────────────────────────────────────────

print("Enriching properties …")

for p in properties:
    info = plz_lookup.get(str(p.get("postal_code") or ""), {})
    p["gemeinde"] = info.get("gemeinde")
    p["bezirk"]   = info.get("bezirk")

covered = sum(1 for p in properties if p["gemeinde"])
print(f"Enriched {covered}/{len(properties)} properties with Gemeinde.")

PROP_JS.write_text("const PROPERTIES = " + json.dumps(properties, ensure_ascii=False) + ";\n")
print("Written →", PROP_JS)
