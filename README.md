# Hurricane Michael & Other Big Hurricanes — Spatial GIS Analysis
**Author:** Nazrina Haque | PhD Candidate, Oregon State University  
**Funding:** USDA Forest Service  

---

## Overview
This repository contains the spatial GIS pipeline used to assign hurricane
treatment variables to timberland parcels across the Southeastern U.S.

The scripts process NOAA hurricane windswath shapefiles and USDA forest
damage severity zones, create acre-based parcel buffers, perform spatial
intersections, and export regression-ready datasets used in my
job market paper on Hurricane Michael's impact on forestland values.

---

## Research Purpose
- Map Hurricane Michael's windswath and severity zones across 4 states
- Create circular parcel buffers proportional to parcel acreage
- Spatially intersect parcels with hurricane zones to assign treatment
- Assign severity levels: Catastrophic, Severe, and Moderate
- Calculate distances to roads and urban boundaries as control variables
- Extract climate raster values at parcel locations

---

## Study Area
Georgia, Alabama, South Carolina, and Florida.  
Hurricane Michael made landfall on **October 10, 2018** as a Category 5 storm.

---

## Repository Structure

| File | Description |
|------|-------------|
| `01_load_shapefiles.R` | Loads and maps Michael severity zones and windswath shapefiles |
| `02_parcel_buffers.R` | Converts parcel points to acre-based circular spatial buffers |
| `03_spatial_intersection.R` | Intersects buffers with hurricane zones, assigns severity levels |
| `04_treatment_assignment.R` | Assigns Michael hit, other hurricane, and severity flags to parcels |
| `05_control_variables.R` | Extracts PRISM climate rasters and road/urban distance controls |

---

## Spatial Workflow
```
NOAA windswath shapefiles + FIA severity zones
              ↓
        CRS alignment (WGS84)
              ↓
     Parcel points (CSV) → sf object
              ↓
  Acre-based circular buffer per parcel
  radius = sqrt(acres × 4046.86 / π)
              ↓
  Spatial intersection with hurricane zones
              ↓
  Treatment flags: hurricane_hit, severity level
              ↓
  Regression-ready CSV export
```

---

## Hurricane Shapefiles Used
| Storm ID | Storm Name | Year |
|----------|------------|------|
| AL142018 | Hurricane Michael | 2018 |
| AL052019 | Hurricane Dorian | 2019 |
| AL052021 | Hurricane Elsa | 2021 |
| AL062018 | Hurricane Florence | 2018 |
| AL092016 | Hurricane Hermine | 2016 |
| AL092020 | Hurricane Sally | 2020 |
| AL292020 | Hurricane Zeta | 2020 |

---

## Key Output Variables
| Variable | Definition |
|----------|------------|
| `hurricane_hit` | Parcel intersects Michael windswath post-2018 |
| `other_hurricane_hit` | Parcel intersects any other hurricane windswath |
| `catastrophic` | Parcel in FIA catastrophic damage zone |
| `severe` | Parcel in FIA severe damage zone |
| `moderate` | Parcel in FIA moderate damage zone |
| `near_dist_roads` | Distance to nearest highway (meters) |
| `near_dist_urban` | Distance to nearest urban boundary (meters) |

---

## R Packages Required
```r
install.packages(c("sf", "maps", "ggplot2", "dplyr",
                   "raster", "openxlsx", "conflicted"))
```

---

## Tools
![R](https://img.shields.io/badge/-R-276DC3?style=flat&logo=r&logoColor=white)
![sf](https://img.shields.io/badge/-sf-4CAF50?style=flat)
![ggplot2](https://img.shields.io/badge/-ggplot2-E91E63?style=flat)
![ArcGIS](https://img.shields.io/badge/-ArcGIS%20Pro-2C7AC3?style=flat)

---

## Contact
Nazrina Haque | haquen@oregonstate.edu  
Department of Applied Economics, Oregon State University
