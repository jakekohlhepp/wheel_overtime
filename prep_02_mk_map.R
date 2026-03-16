#' =============================================================================
#' CONSTRUCT LA ENFORCEMENT DISTRICTS MAP
#' =============================================================================
#' Creates a map of LADOT parking enforcement districts with office locations.
#'
#' Input:  20170803_payworkers_comp/anonymized_data_073117.txt (division codes)
#'         data/01_05_list_complete.csv (division names + office locations)
#'         20250311_ladot_enforcement_districts/ (shapefiles)
#' Output: out/figures/prep_02_la_street_map.png
#' =============================================================================

library('data.table')
library('sf')
library('osmdata')
library('ggplot2')
library('tidygeocoder')

source('config.R')
source('utils/logging.R')

log_init("prep_02_mk_map.R")
log_message("Building LA enforcement districts map")

#' -----------------------------------------------------------------------------
#' LOAD DIVISION CODES
#' -----------------------------------------------------------------------------

raw_path <- file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")
list_path <- file.path(CONFIG$data_dir, "01_05_list_complete.csv")
districts_path <- CONFIG$raw_districts_dir

assert_required_files(c(raw_path, list_path))

working <- fread(raw_path)
working <- unique(working[!is.na(`Div Cd`), c("Div Cd", "Assigned Div")])
setkey(working, `Div Cd`)

write.csv(working, file.path(CONFIG$data_dir, "prep_02_list_tofill.csv"))

#' -----------------------------------------------------------------------------
#' LOAD SHAPEFILES AND OFFICE LOCATIONS
#' -----------------------------------------------------------------------------

shape <- read_sf(dsn = districts_path, layer = CONFIG$districts_layer)

log_message("Fetching OpenStreetMap road data for Los Angeles")
la_major <- getbb(place_name = "Los Angeles") |>
  opq() |>
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary")) |>
  osmdata_sf()

#' -----------------------------------------------------------------------------
#' GEOCODE OFFICE LOCATIONS
#' -----------------------------------------------------------------------------

hqs <- fread(list_path)
setnames(hqs, "clean_name", "Offices")
hqs[, Offices := as.factor(Offices)]
hqs <- unique(hqs[`Div Cd` > CONFIG$map_div_code_min & Offices != "Habitual Parking",
                  c("Offices", "location_parking_enforcement_office")])
hqs <- geocode(hqs, location_parking_enforcement_office)

## Retry failed addresses with cleaned format (remove directional prefixes like "W.")
failed <- is.na(hqs$lat)
if (any(failed)) {
  log_message(paste("Geocoding failed for", sum(failed), "addresses, retrying with cleaned format"))
  retry <- hqs[failed, ]
  retry$lat <- NULL
  retry$long <- NULL
  retry$location_parking_enforcement_office <- gsub("\\b[NSEW]\\. ", "",
                                                     retry$location_parking_enforcement_office)
  retry <- geocode(retry, location_parking_enforcement_office)
  hqs$lat[failed] <- retry$lat
  hqs$long[failed] <- retry$long
}

still_failed <- is.na(hqs$lat)
if (any(still_failed)) {
  warning("Could not geocode: ", paste(hqs$Offices[still_failed], collapse = ", "))
  hqs <- hqs[!still_failed, ]
}

hqs <- st_as_sf(hqs, coords = c("long", "lat"), remove = FALSE)
st_crs(hqs) <- "EPSG:4326"
hqs <- st_transform(hqs, st_crs(shape))

log_message(paste("Geocoded", nrow(hqs), "office locations"))

#' -----------------------------------------------------------------------------
#' BUILD AND SAVE MAP
#' -----------------------------------------------------------------------------

setnames(shape, "District", "Parking Enforcement Districts")

street_plot <- ggplot() +
  geom_sf(data = shape, aes(fill = `Parking Enforcement Districts`), color = NA, alpha = 0.5) +
  geom_sf(data = la_major$osm_lines, color = "black", alpha = 0.3, size = 0.2) +
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
message("prep_02_mk_map complete")
