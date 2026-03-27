required_packages <- c("httr2", "jsonlite", "dplyr", "purrr", "tidyr", "readr", "stringr", "tibble")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Please install these packages before running the script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

api_base <- "https://api.ukhsa-dashboard.data.gov.uk"

safe_api_get <- function(url) {
  message("Requesting: ", url)

  req <- request(url) |>
    req_user_agent("EcoliCiproR/1.0") |>
    req_headers(Accept = "application/json") |>
    req_timeout(60)

  resp <- req_perform(req)
  resp_check_status(resp)
  resp_body_json(resp, simplifyVector = TRUE)
}

fetch_paginated_results <- function(url, pause_seconds = 0.1) {
  all_results <- list()
  if (!str_detect(url, "page_size=")) {
    separator <- if (str_detect(url, fixed("?"))) "&" else "?"
    url <- paste0(url, separator, "page_size=5000")
  }

  next_url <- url
  page_number <- 1
  total_rows <- 0
  expected_count <- NULL

  while (!is.null(next_url) && !identical(next_url, "")) {
    payload <- safe_api_get(next_url)

    if (!is.null(payload$results)) {
      page_results <- payload$results
      expected_count <- payload$count %||% expected_count

      if (length(page_results) > 0) {
        page_tbl <- as_tibble(page_results)
        all_results[[length(all_results) + 1]] <- page_tbl
        total_rows <- total_rows + nrow(page_tbl)
      }

      next_url <- payload[["next"]] %||% NULL

      if (!is.null(expected_count) && total_rows >= expected_count) {
        next_url <- NULL
      }
    } else {
      if (length(payload) == 0) {
        return(tibble())
      }

      return(as_tibble(payload))
    }

    message("Finished page ", page_number)
    page_number <- page_number + 1

    if (!is.null(next_url) && pause_seconds > 0) {
      Sys.sleep(pause_seconds)
    }
  }

  if (length(all_results) == 0) {
    return(tibble())
  }

  bind_rows(all_results)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

discover_amr_metadata <- function(base_url = api_base) {
  themes_url <- paste0(base_url, "/themes")
  themes_tbl <- fetch_paginated_results(themes_url)

  theme_name <- themes_tbl |>
    filter(name == "infectious_disease") |>
    pull(name)

  if (length(theme_name) == 0) {
    stop("Could not find the infectious_disease theme.", call. = FALSE)
  }

  subthemes_url <- paste0(base_url, "/themes/", theme_name, "/sub_themes")
  subthemes_tbl <- fetch_paginated_results(subthemes_url)

  sub_theme_name <- subthemes_tbl |>
    filter(name == "antimicrobial_resistance") |>
    pull(name)

  if (length(sub_theme_name) == 0) {
    stop("Could not find the antimicrobial_resistance sub-theme.", call. = FALSE)
  }

  topics_url <- paste0(
    base_url,
    "/themes/", theme_name,
    "/sub_themes/", sub_theme_name,
    "/topics"
  )
  topics_tbl <- fetch_paginated_results(topics_url)

  topic_name <- topics_tbl |>
    filter(str_to_lower(name) == "e-coli") |>
    pull(name)

  if (length(topic_name) == 0) {
    stop("Could not find the E-coli topic.", call. = FALSE)
  }

  geography_types_url <- paste0(
    base_url,
    "/themes/", theme_name,
    "/sub_themes/", sub_theme_name,
    "/topics/", URLencode(topic_name, reserved = TRUE),
    "/geography_types"
  )
  geography_types_tbl <- fetch_paginated_results(geography_types_url)

  geography_type_name <- geography_types_tbl |>
    filter(name == "Integrated Care Board") |>
    pull(name)

  if (length(geography_type_name) == 0) {
    stop("Integrated Care Board geography type is not available for E-coli.", call. = FALSE)
  }

  geographies_url <- paste0(
    base_url,
    "/themes/", theme_name,
    "/sub_themes/", sub_theme_name,
    "/topics/", URLencode(topic_name, reserved = TRUE),
    "/geography_types/", URLencode(geography_type_name, reserved = TRUE),
    "/geographies"
  )
  geographies_tbl <- fetch_paginated_results(geographies_url)

  if (nrow(geographies_tbl) == 0) {
    stop("No ICB geographies were returned for the E-coli topic.", call. = FALSE)
  }

  list(
    theme = theme_name[[1]],
    sub_theme = sub_theme_name[[1]],
    topic = topic_name[[1]],
    geography_type = geography_type_name[[1]],
    geographies = geographies_tbl
  )
}

discover_metric_name <- function(metadata, antibiotic = "Ciprofloxacin") {
  sample_geography <- metadata$geographies$name[[1]]

  metrics_url <- paste0(
    api_base,
    "/themes/", metadata$theme,
    "/sub_themes/", metadata$sub_theme,
    "/topics/", URLencode(metadata$topic, reserved = TRUE),
    "/geography_types/", URLencode(metadata$geography_type, reserved = TRUE),
    "/geographies/", URLencode(sample_geography, reserved = TRUE),
    "/metrics"
  )

  metrics_tbl <- fetch_paginated_results(metrics_url)

  metric_name <- metrics_tbl |>
    filter(
      str_detect(name, regex("bacteraemia", ignore_case = TRUE)),
      str_detect(name, regex("PercentResistant", ignore_case = TRUE)),
      str_detect(name, regex("RollingMonth", ignore_case = TRUE))
    ) |>
    pull(name)

  if (length(metric_name) == 0) {
    stop("Could not discover the rolling-month bacteraemia percent resistant metric.", call. = FALSE)
  }

  metric_name <- metric_name[[1]]

  sample_metric_url <- paste0(
    api_base,
    "/themes/", metadata$theme,
    "/sub_themes/", metadata$sub_theme,
    "/topics/", URLencode(metadata$topic, reserved = TRUE),
    "/geography_types/", URLencode(metadata$geography_type, reserved = TRUE),
    "/geographies/", URLencode(sample_geography, reserved = TRUE),
    "/metrics/", metric_name
  )

  sample_metric_data <- fetch_paginated_results(sample_metric_url)

  if (nrow(sample_metric_data) == 0) {
    stop("The discovered metric returned no rows for the sample ICB.", call. = FALSE)
  }

  available_strata <- sample_metric_data |>
    distinct(stratum) |>
    arrange(stratum) |>
    pull(stratum)

  if (!(antibiotic %in% available_strata)) {
    stop(
      "The discovered metric does not contain the requested antibiotic stratum: ",
      antibiotic,
      call. = FALSE
    )
  }

  list(
    sample_geography = sample_geography,
    metric_name = metric_name,
    antibiotic = antibiotic,
    available_strata = available_strata
  )
}

build_metric_url <- function(metadata, geography_name, metric_name) {
  paste0(
    api_base,
    "/themes/", metadata$theme,
    "/sub_themes/", metadata$sub_theme,
    "/topics/", URLencode(metadata$topic, reserved = TRUE),
    "/geography_types/", URLencode(metadata$geography_type, reserved = TRUE),
    "/geographies/", URLencode(geography_name, reserved = TRUE),
    "/metrics/", metric_name
  )
}

pull_one_icb <- function(metadata, geography_name, metric_name, antibiotic = "Ciprofloxacin") {
  metric_url <- build_metric_url(metadata, geography_name, metric_name)
  metric_data <- fetch_paginated_results(metric_url)

  if (nrow(metric_data) == 0) {
    warning("No rows returned for geography: ", geography_name, call. = FALSE)
    return(tibble())
  }

  metric_data |>
    filter(stratum == antibiotic)
}

clean_modelling_dataset <- function(raw_data) {
  raw_data |>
    transmute(
      date = as.Date(date),
      geography = geography,
      geography_code = geography_code,
      metric = metric,
      stratum = stratum,
      metric_value = as.numeric(metric_value),
      theme = theme,
      sub_theme = sub_theme,
      topic = topic,
      geography_type = geography_type,
      metric_group = metric_group,
      sex = sex,
      age = age,
      year = year,
      month = month,
      epiweek = epiweek,
      in_reporting_delay_period = in_reporting_delay_period
    ) |>
    arrange(geography, date)
}

summarise_missing_months <- function(clean_data) {
  if (nrow(clean_data) == 0) {
    return(tibble())
  }

  global_months <- tibble(
    date = seq.Date(
      from = min(clean_data$date, na.rm = TRUE),
      to = max(clean_data$date, na.rm = TRUE),
      by = "month"
    )
  )

  missing_rows <- clean_data |>
    distinct(geography, geography_code) |>
    crossing(global_months) |>
    anti_join(
      clean_data |>
        distinct(geography, geography_code, date),
      by = c("geography", "geography_code", "date")
    ) |>
    rename(missing_month = date)

  if (nrow(missing_rows) == 0) {
    return(
      list(
        missing_rows = missing_rows,
        missing_summary = tibble()
      )
    )
  }

  missing_summary <- missing_rows |>
    group_by(geography, geography_code) |>
    summarise(
      missing_months = n(),
      first_missing_month = min(missing_month),
      last_missing_month = max(missing_month),
      .groups = "drop"
    ) |>
    arrange(desc(missing_months), geography)

  list(
    missing_rows = missing_rows,
    missing_summary = missing_summary
  )
}

run_ukhsa_ecoli_cipro <- function(
  output_dir = ".",
  antibiotic = "Ciprofloxacin",
  test_geography = NULL
) {
  metadata <- discover_amr_metadata()

  message("Confirmed hierarchy:")
  message("theme = ", metadata$theme)
  message("sub_theme = ", metadata$sub_theme)
  message("topic = ", metadata$topic)
  message("geography_type = ", metadata$geography_type)
  message("Number of ICB geographies found: ", nrow(metadata$geographies))

  metric_info <- discover_metric_name(metadata, antibiotic = antibiotic)

  message("Sample ICB used for metric discovery: ", metric_info$sample_geography)
  message("Metric selected: ", metric_info$metric_name)
  message("Antibiotic stratum confirmed: ", metric_info$antibiotic)
  message("Available strata in sample data: ", paste(metric_info$available_strata, collapse = ", "))

  selected_geographies <- metadata$geographies

  if (!is.null(test_geography)) {
    selected_geographies <- selected_geographies |>
      filter(name == test_geography)

    if (nrow(selected_geographies) == 0) {
      stop("The requested test geography was not found in the ICB list.", call. = FALSE)
    }
  }

  raw_combined <- map_dfr(
    selected_geographies$name,
    \(geo_name) {
      message("Pulling data for: ", geo_name)
      pull_one_icb(
        metadata = metadata,
        geography_name = geo_name,
        metric_name = metric_info$metric_name,
        antibiotic = antibiotic
      )
    }
  )

  if (nrow(raw_combined) == 0) {
    stop("The API pull completed but returned no rows after filtering to the requested antibiotic.", call. = FALSE)
  }

  clean_data <- clean_modelling_dataset(raw_combined)
  missing_check <- summarise_missing_months(clean_data)

  raw_path <- file.path(output_dir, "ukhsa_ecoli_ciprofloxacin_icb_raw.csv")
  clean_path <- file.path(output_dir, "ukhsa_ecoli_ciprofloxacin_icb_model.csv")

  write_csv(raw_combined, raw_path, na = "")
  write_csv(clean_data, clean_path, na = "")

  message("Saved raw dataset to: ", normalizePath(raw_path, winslash = "/"))
  message("Saved cleaned dataset to: ", normalizePath(clean_path, winslash = "/"))

  message("Final cleaned column names:")
  print(names(clean_data))

  message("Preview of cleaned data:")
  print(utils::head(clean_data, 10))

  message("Missing month summary by ICB:")
  if (nrow(missing_check$missing_summary) == 0) {
    message("No missing months found across the observed date range.")
  } else {
    print(missing_check$missing_summary)
  }

  invisible(
    list(
      metadata = metadata,
      metric_info = metric_info,
      raw_data = raw_combined,
      clean_data = clean_data,
      missing_rows = missing_check$missing_rows,
      missing_summary = missing_check$missing_summary,
      raw_path = raw_path,
      clean_path = clean_path
    )
  )
}

# Run this first to discover the hierarchy and confirm the metric on one sample ICB.
# metadata <- discover_amr_metadata()
# metric_info <- discover_metric_name(metadata)

# Run this next to test the full workflow on one ICB only.
# test_result <- run_ukhsa_ecoli_cipro(
#   output_dir = ".",
#   test_geography = "NHS Birmingham and Solihull Integrated Care Board"
# )

# Run this when you are ready to pull all ICBs.
 full_result <- run_ukhsa_ecoli_cipro(output_dir = ".")
 view(full_result$clean_data)
 readr::write_csv(full_result$clean_data, "EcoliCipro.csv")
 getwd()
 
