# ============================================================
# HURRICANE MICHAEL — TREATMENT ASSIGNMENT
# Author: Nazrina Haque
# Description: Assigns hurricane hit, severity level, and
#              storm treatment variables to parcel dataset
# ============================================================

library(dplyr)
library(stringr)
conflicted::conflicts_prefer(dplyr::filter)

# ============================================================
# LOAD DATA
# ============================================================
final_file       <- read.csv("combined_data.csv")
buffers_in_zones <- read.csv("buffers_in_zones.csv")

# ============================================================
# STANDARDIZE IDs
# ============================================================
final_file <- final_file %>%
  mutate(
    saleid       = str_trim(tolower(saleid)),
    situs_county = str_trim(tolower(situs_county)),
    match_id     = paste(saleid, situs_county, sep = "_")
  )

buffers_in_zones <- buffers_in_zones %>%
  mutate(
    saleid       = str_trim(tolower(saleid)),
    situs_county = str_trim(tolower(situs_county)),
    match_id     = paste(saleid, situs_county, sep = "_")
  )

# ============================================================
# TREATMENT FLAGS
# ============================================================

# Michael hit (STORMID = AL142018)
michael_ids <- buffers_in_zones %>%
  filter(STORMID == "AL142018") %>%
  pull(match_id) %>% unique()

# Other hurricanes (RADII = 64, not Michael)
other_hurricane_ids <- buffers_in_zones %>%
  filter(STORMID != "AL142018" & RADII == 64) %>%
  pull(match_id) %>% unique()

# All storms (any RADII)
all_storm_ids <- buffers_in_zones %>%
  pull(match_id) %>% unique()

# Severity levels
catastrophic_ids <- buffers_in_zones %>% filter(ident == "Catastrophic") %>% pull(match_id) %>% unique()
severe_ids       <- buffers_in_zones %>% filter(ident == "Severe")       %>% pull(match_id) %>% unique()
moderate_ids     <- buffers_in_zones %>% filter(ident == "Moderate")     %>% pull(match_id) %>% unique()

# ============================================================
# ASSIGN TO FINAL FILE
# ============================================================
final_file <- final_file %>%
  mutate(
    hurricane_hit       = ifelse(match_id %in% michael_ids,         1, 0),
    other_hurricane_hit = ifelse(match_id %in% other_hurricane_ids,  1, 0),
    all_storms          = ifelse(match_id %in% all_storm_ids,        1, 0),
    catastrophic        = ifelse(match_id %in% catastrophic_ids,     1, 0),
    severe              = ifelse(match_id %in% severe_ids,           1, 0),
    moderate            = ifelse(match_id %in% moderate_ids,         1, 0)
  ) %>%
  select(-match_id)

# ============================================================
# SUMMARY
# ============================================================
cat("✅ Treatment assignment complete\n")
cat("  Michael hit:       ", sum(final_file$hurricane_hit),       "\n")
cat("  Other hurricanes:  ", sum(final_file$other_hurricane_hit),  "\n")
cat("  Catastrophic:      ", sum(final_file$catastrophic),         "\n")
cat("  Severe:            ", sum(final_file$severe),               "\n")
cat("  Moderate:          ", sum(final_file$moderate),             "\n")

# ============================================================
# SAVE
# ============================================================
write.csv(final_file, "michael_treatment_assigned.csv", row.names = FALSE)
cat("✅ Saved: michael_treatment_assigned.csv\n")
