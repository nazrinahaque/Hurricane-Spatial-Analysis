# ============================================================
# HURRICANE MICHAEL — CONTROL VARIABLES
# Author: Nazrina Haque
# Description: Extracts raster-based climate controls and
#              calculates distance to roads and urban areas
# ============================================================

library(sf)
library(raster)
library(dplyr)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(raster::extract)

# ============================================================
# LOAD PARCEL DATA
# ============================================================
points_df <- read.csv("michael_treatment_assigned.csv")

points_sf <- st_as_sf(
  points_df,
  coords = c("parcel_level_longitude", "parcel_level_latitude"),
  crs    = 4326
)

# Convert to sp for raster extraction
points_sp           <- as(points_sf, "Spatial")
proj4string(points_sp) <- CRS("+proj=longlat +datum=WGS84")

# ============================================================
# EXTRACT CLIMATE RASTERS
# ============================================================
raster_files <- list(
  vpdmin    = "PRISM_vpdmin_30yr_normal_4kmM5_annual_bil.bil",
  vpdmax    = "PRISM_vpdmax_30yr_normal_4kmM5_annual_bil.bil",
  precip    = "PRISM_precip_30yr_normal_4kmM5_annual_bil.bil",
  mtemp     = "PRISM_tmean_30yr_normal_4kmM5_annual_bil.bil"
)

for (var_name in names(raster_files)) {
  rfile <- raster_files[[var_name]]

  if (!file.exists(rfile)) {
    cat("⚠️  Missing raster:", rfile, "\n")
    next
  }

  r                  <- raster(rfile)
  points_df[[var_name]] <- raster::extract(r, points_sp)
  cat("✅ Extracted:", var_name, "\n")
}

# ============================================================
# DISTANCE TO HIGHWAYS
# ============================================================
if (file.exists("National_Highway_System_TDA.shp")) {

  highways    <- st_read("National_Highway_System_TDA.shp")
  highways    <- st_transform(highways, crs = st_crs(points_sf))
  highways    <- st_make_valid(highways)

  road_dist   <- st_distance(points_sf, highways)
  points_df$near_dist_roads <- as.numeric(apply(road_dist, 1, min))

  cat("✅ Distance to highways calculated\n")

} else {
  cat("⚠️  Highway shapefile not found\n")
}

# ============================================================
# DISTANCE TO URBAN BOUNDARIES
# ============================================================
if (file.exists("Urban_Boundaries.shp")) {

  urban       <- st_read("Urban_Boundaries.shp")
  urban       <- st_transform(urban, crs = st_crs(points_sf))
  urban       <- st_make_valid(urban)

  urban_dist  <- st_distance(points_sf, urban)
  points_df$near_dist_urban <- as.numeric(apply(urban_dist, 1, min))

  cat("✅ Distance to urban boundaries calculated\n")

} else {
  cat("⚠️  Urban boundaries shapefile not found\n")
}

# ============================================================
# SAVE FINAL FILE WITH ALL CONTROLS
# ============================================================
write.csv(points_df, "michael_final_with_controls.csv", row.names = FALSE)
cat("✅ Saved: michael_final_with_controls.csv\n")
cat("   Columns:", paste(names(points_df), collapse = ", "), "\n")
