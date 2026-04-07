#' =============================================================================
#' CONSTRUCT LA ENFORCEMENT DISTRICTS MAP
#' =============================================================================
#' Creates a map of LADOT parking enforcement districts with office locations.
#'
#' Input:  20170803_payworkers_comp/anonymized_data_073117.txt (division codes)
#'         20250320_division_names_offices/list_complete.csv (division names + office locations)
#'         20250311_ladot_enforcement_districts/ (shapefiles)
#' Output: out/figures/01_07_la_street_map.png
#' =============================================================================

library('data.table')
library('sf')
library('osmdata')
library('ggplot2')
library('tidygeocoder')

source('config.R')
source('utils/logging.R')

log_init("01_07_mk_map.R")
log_message("Building LA enforcement districts map")

#' -----------------------------------------------------------------------------
#' LOAD DIVISION CODES
#' -----------------------------------------------------------------------------

raw_path <- file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")
list_path <- file.path(CONFIG$raw_office_dir, "list_complete.csv")
districts_path <- CONFIG$raw_districts_dir
district_shape_path <- file.path(districts_path, paste0(CONFIG$districts_layer, ".shp"))

assert_required_files(c(raw_path, list_path, district_shape_path))

working <- fread(raw_path)
working <- unique(working[!is.na(`Div Cd`), c("Div Cd", "Assigned Div")])
setkey(working, `Div Cd`)

ensure_directory(CONFIG$data_dir)
write.csv(working, file.path(CONFIG$data_dir, "01_07_list_tofill.csv"), row.names = FALSE)

#' -----------------------------------------------------------------------------
#' LOAD SHAPEFILES AND OFFICE LOCATIONS
#' -----------------------------------------------------------------------------

shape <- read_sf(dsn = districts_path, layer = CONFIG$districts_layer)

major_roads <- NULL
log_message("Fetching OpenStreetMap road data for district extent (optional)")
tryCatch({
  shape_bbox <- st_bbox(st_transform(shape, 4326))
  la_query <- add_osm_feature(
    opq(
      bbox = unname(shape_bbox[c("xmin", "ymin", "xmax", "ymax")]),
      timeout = 120L
    ),
    key = "highway",
    value = c("motorway", "primary", "secondary")
  )
  la_major <- osmdata_sf(la_query)

  if (!is.null(la_major$osm_lines) && nrow(la_major$osm_lines) > 0) {
    major_roads <- st_transform(la_major$osm_lines, st_crs(shape))
    log_message(paste("Loaded", nrow(major_roads), "major road segments"))
  } else {
    log_message(
      "OpenStreetMap query returned no road segments; continuing without road overlay",
      level = "WARN"
    )
  }
}, error = function(e) {
  log_message(
    paste(
      "OpenStreetMap road fetch failed; continuing without road overlay.",
      "Reason:", conditionMessage(e)
    ),
    level = "WARN"
  )
})

#' -----------------------------------------------------------------------------
#' GEOCODE OFFICE LOCATIONS
#' -----------------------------------------------------------------------------

hqs <- fread(list_path)
setnames(hqs, "clean_name", "Offices")
hqs <- unique(hqs[`Div Cd` > CONFIG$map_div_code_min & Offices != "Habitual Parking",
                  c("Offices", "location_parking_enforcement_office")])

office_lookup <- data.table(
  Offices = c(
    "Southern",
    "Central",
    "Hollywood-Wilshire",
    "Western",
    "Valley",
    "Harbor Traffic Control"
  ),
  location_parking_enforcement_office = c(
    "7510 S. Figueroa St., Los Angeles, CA 90007",
    "1016 N. Mission Rd., Los Angeles, CA 90033",
    "888 S. Vermont Avenue, Los Angeles, CA 90005",
    "11214 W. Exposition Blvd., Los Angeles, CA 90064",
    "12544 Saticoy St. S., North Hollywood, CA 91605",
    "425 South Palos Verdes Street, San Pedro, CA, 90731"
  ),
  lat = c(
    33.9715313,
    34.0580198,
    34.0560498,
    34.0349391,
    34.2050540,
    33.7406180
  ),
  long = c(
    -118.2825814,
    -118.2175035,
    -118.2913733,
    -118.4351514,
    -118.4069220,
    -118.2819310
  )
)

hqs[office_lookup, on = .(Offices, location_parking_enforcement_office),
    `:=`(lat = i.lat, long = i.long)]

geocode_safe <- function(dt, context) {
  if (nrow(dt) == 0) {
    return(copy(dt))
  }

  tryCatch(
    geocode(copy(dt), location_parking_enforcement_office),
    error = function(e) {
      log_message(paste(context, "failed:", conditionMessage(e)), level = "WARN")
      out <- copy(dt)
      out[, `:=`(lat = NA_real_, long = NA_real_)]
      out
    }
  )
}

hqs[, row_id := .I]
missing_coords <- is.na(hqs$lat) | is.na(hqs$long)
if (any(missing_coords)) {
  log_message(
    paste(
      "Geocoding", sum(missing_coords),
      "office location(s) not covered by the local lookup"
    )
  )
  geocoded <- geocode_safe(
    hqs[missing_coords, .(row_id, Offices, location_parking_enforcement_office)],
    "Primary geocoding"
  )
  hqs[geocoded$row_id, `:=`(lat = geocoded$lat, long = geocoded$long)]
}

# Retry failed addresses with cleaned format (remove directional prefixes like "W.")
failed <- is.na(hqs$lat) | is.na(hqs$long)
if (any(failed)) {
  log_message(paste("Geocoding failed for", sum(failed), "addresses, retrying with cleaned format"))
  retry <- hqs[failed, .(row_id, Offices, location_parking_enforcement_office)]
  retry[, location_parking_enforcement_office := gsub("\\b[NSEW]\\.\\s*", "",
                                                      location_parking_enforcement_office)]
  retry <- geocode_safe(retry, "Retry geocoding")
  hqs[retry$row_id, `:=`(lat = retry$lat, long = retry$long)]
}

still_failed <- is.na(hqs$lat) | is.na(hqs$long)
if (any(still_failed)) {
  warning("Could not geocode: ", paste(hqs$Offices[still_failed], collapse = ", "))
  hqs <- hqs[!still_failed, ]
}
hqs[, row_id := NULL]
hqs[, Offices := as.factor(Offices)]

hqs <- st_as_sf(hqs, coords = c("long", "lat"), remove = FALSE)
st_crs(hqs) <- "EPSG:4326"
hqs <- st_transform(hqs, st_crs(shape))

log_message(paste("Geocoded", nrow(hqs), "office locations"))

#' -----------------------------------------------------------------------------
#' BUILD AND SAVE MAP
#' -----------------------------------------------------------------------------

setnames(shape, "District", "Parking Enforcement Districts")

street_plot <- ggplot() +
  geom_sf(data = shape, aes(fill = `Parking Enforcement Districts`), color = NA, alpha = 0.5)

if (!is.null(major_roads) && nrow(major_roads) > 0) {
  street_plot <- street_plot +
    geom_sf(data = major_roads, color = "black", alpha = 0.3, size = 0.2)
}

street_plot <- street_plot +
  geom_point(data = hqs,
             aes(shape = Offices, geometry = geometry),
             stat = "sf_coordinates", size = 4, fill = "black") +
  scale_shape_manual(values = c("diamond", "circle", "asterisk", "square",
                                "triangle", "triangle down filled")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

ensure_directory(dirname(CONFIG$map_output))
ggsave(CONFIG$map_output, plot = street_plot,
       width = CONFIG$map_width, height = CONFIG$map_height, units = "in")

log_message(paste("Map saved:", CONFIG$map_output))
log_complete(success = TRUE)
message("01_07_mk_map complete")
