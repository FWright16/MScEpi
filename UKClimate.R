# UKClimate.R
# Open-Meteo fallback workflow (no CDS key required):
# Fetch 16-Oct-2003 daily mean temperature at each UK Adm2 centroid,
# join back to polygons, map, and export.

# ---------------------------
# 0) Package setup
# ---------------------------
needed <- c("sf", "terra", "dplyr", "ggplot2", "geodata", "httr2", "jsonlite")
missing <- needed[!vapply(needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing) > 0) install.packages(missing)
invisible(lapply(needed, library, character.only = TRUE))

# ---------------------------
# 1) Settings
# ---------------------------
target_date <- as.Date("2003-10-16")
out_csv <- "uk_adm2_tmean_20031016_openmeteo.csv"
out_gpkg <- "uk_adm2_tmean_20031016_openmeteo.gpkg"
out_png <- "uk_adm2_tmean_20031016_openmeteo.png"

# Open-Meteo Archive endpoint (historical daily)
base_url <- "https://archive-api.open-meteo.com/v1/archive"

# ---------------------------
# 2) UK GADM Adm2 boundaries
# ---------------------------
UK_adm2_vect <- geodata::gadm(country = "GBR", level = 2, path = tempdir())
UK_adm2_sf <- sf::st_as_sf(UK_adm2_vect)

# Ensure lon/lat CRS for API coordinates
if (sf::st_crs(UK_adm2_sf)$epsg != 4326) {
  UK_adm2_sf <- sf::st_transform(UK_adm2_sf, 4326)
}

# ---------------------------
# 3) Build one centroid per Adm2
# ---------------------------
# st_point_on_surface avoids centroids falling outside concave polygons.
pts <- sf::st_point_on_surface(UK_adm2_sf)
coords <- sf::st_coordinates(pts)

adm2_points <- UK_adm2_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    lon = coords[, "X"],
    lat = coords[, "Y"]
  ) %>%
  dplyr::select(dplyr::any_of(c("GID_2", "NAME_2")), lon, lat)

# ---------------------------
# 4) Function: fetch daily mean from Open-Meteo
# ---------------------------
get_openmeteo_daily_mean <- function(lat, lon, date_value) {
  date_str <- format(date_value, "%Y-%m-%d")

  req <- httr2::request(base_url) %>%
    httr2::req_url_query(
      latitude = sprintf("%.6f", lat),
      longitude = sprintf("%.6f", lon),
      start_date = date_str,
      end_date = date_str,
      daily = "temperature_2m_mean",
      timezone = "UTC"
    ) %>%
    httr2::req_timeout(30)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # Defensive parsing for occasional missing fields
  if (is.null(body$daily) || is.null(body$daily$temperature_2m_mean)) {
    return(NA_real_)
  }

  value <- body$daily$temperature_2m_mean
  if (length(value) == 0) return(NA_real_)
  as.numeric(value[1])
}

# ---------------------------
# 5) Query API for each Adm2 centroid
# ---------------------------
# Note: UK Adm2 has many polygons; this can take several minutes.
results <- vector("list", nrow(adm2_points))

for (i in seq_len(nrow(adm2_points))) {
  lat_i <- adm2_points$lat[i]
  lon_i <- adm2_points$lon[i]

  temp_i <- tryCatch(
    get_openmeteo_daily_mean(lat = lat_i, lon = lon_i, date_value = target_date),
    error = function(e) NA_real_
  )

  results[[i]] <- data.frame(
    GID_2 = adm2_points$GID_2[i],
    tmean_c = round(temp_i, 2)
  )

  if (i %% 25 == 0 || i == nrow(adm2_points)) {
    message("Fetched ", i, " / ", nrow(adm2_points), " Adm2 centroids")
  }

  # Gentle pacing to avoid rate-limit spikes
  Sys.sleep(0.1)
}

adm2_temp_df <- dplyr::bind_rows(results)

# ---------------------------
# 6) Join back to polygons
# ---------------------------
UK_adm2_tmean_sf <- UK_adm2_sf %>%
  dplyr::left_join(adm2_temp_df, by = "GID_2")

# ---------------------------
# 7) Plot
# ---------------------------
p <- ggplot(UK_adm2_tmean_sf) +
  geom_sf(aes(fill = tmean_c), color = NA) +
  scale_fill_viridis_c(option = "rocket", direction = 1, na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "UK Adm2 mean 2m temperature (Open-Meteo centroids) - 16 October 2003",
    fill = "\u00B0C"
  )

print(p)

# ---------------------------
# 8) Save outputs
# ---------------------------
write.csv(adm2_temp_df, out_csv, row.names = FALSE)
sf::st_write(UK_adm2_tmean_sf, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
ggsave(out_png, plot = p, width = 9, height = 10, dpi = 300)

message("Done. Outputs:")
message(" - ", out_csv)
message(" - ", out_gpkg)
message(" - ", out_png)
message("Method note: centroid sampling (not full polygon raster zonal mean).")
