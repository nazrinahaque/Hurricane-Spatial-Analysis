# ============================================================
# HURRICANE MICHAEL — SPATIAL INTERSECTION
# Author: Nazrina Haque
# Description: Intersects parcel buffers with Michael severity
#              zones and windswath to assign treatment status
# ============================================================

library(sf)
library(dplyr)
library(openxlsx)
conflicted::conflicts_prefer(dplyr::filter)

# ============================================================
# LOAD DATA
# ============================================================
parcel_buffers  <- readRDS("parcel_buffers.rds")
michael_severity <- st_read("hurricane_michael_severity_zones.shp")
michael_windswath <- st_read("AL142018_windswath.shp")

# ============================================================
# MATCH CRS
# ============================================================
michael_severity  <- st_transform(michael_severity,  crs = st_crs(parcel_buffers))
michael_windswath <- st_transform(michael_windswath, crs = st_crs(parcel_buffers))

# Make geometries valid
michael_severity  <- st_make_valid(michael_severity)
michael_windswath <- st_make_valid(michael_windswath)
parcel_buffers    <- st_make_valid(parcel_buffers)

# ============================================================
# INTERSECT WITH SEVERITY ZONES
# ============================================================
cat("Running intersection with severity zones...\n")

bbox            <- st_bbox(parcel_buffers)
severity_crop   <- st_crop(michael_severity, bbox)

buffers_in_zones <- st_join(
  parcel_buffers,
  severity_crop,
  join = st_intersects,
  left = FALSE
)

buffers_in_zones <- st_simplify(buffers_in_zones, dTolerance = 0.01)

cat("✅ Parcels intersecting severity zones:", nrow(buffers_in_zones), "\n")

# ============================================================
# ASSIGN SEVERITY LEVELS (Catastrophic / Severe / Moderate)
# ============================================================
library(stringr)

buffers_in_zones <- buffers_in_zones %>%
  mutate(
    saleid       = str_trim(tolower(saleid)),
    situs_county = str_trim(tolower(situs_county)),
    match_id     = paste(saleid, situs_county, sep = "_")
  )

catastrophic_ids <- buffers_in_zones %>% filter(ident == "Catastrophic") %>% pull(match_id) %>% unique()
severe_ids       <- buffers_in_zones %>% filter(ident == "Severe")       %>% pull(match_id) %>% unique()
moderate_ids     <- buffers_in_zones %>% filter(ident == "Moderate")     %>% pull(match_id) %>% unique()

# ============================================================
# EXPORT RESULTS
# ============================================================
intersection_values <- st_drop_geometry(buffers_in_zones)

write.xlsx(intersection_values, "michael_intersection_values.xlsx")
write.csv(intersection_values,  "buffers_in_zones.csv", row.names = FALSE)

cat("✅ Intersection results saved\n")
