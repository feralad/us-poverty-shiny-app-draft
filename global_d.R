# ──────────────────────────────────────────────────────────────────────────────
# ENVIRONMENT ------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

## -- Load Packages ------------------------------------------------------------
library(arrow)
library(sfarrow)
library(dplyr)
library(purrr)
library(memoise)
library(sf)

# ──────────────────────────────────────────────────────────────────────────────
# DATA -------------------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# -- Parquet URLs --------------------------------------------------------------
county_poverty_parquet_url <- "https://storage.googleapis.com/us-poverty-organizations-shiny/county_poverty_data.parquet"
state_poverty_parquet_url  <- "https://storage.googleapis.com/us-poverty-organizations-shiny/state_poverty_data.parquet"
us_poverty_parquet_url     <- "https://storage.googleapis.com/us-poverty-organizations-shiny/us_poverty_data.parquet"
county_geometry_url        <- "https://storage.googleapis.com/us-poverty-organizations-shiny/county_geometry.parquet"

# -- Persistent Data Directory -------------------------------------------------
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir)

# -- Persistent File Paths -----------------------------------------------------
county_poverty_parquet_path  <- file.path(data_dir, "county_poverty_data.parquet")
state_poverty_parquet_path   <- file.path(data_dir, "state_poverty_data.parquet")
us_poverty_parquet_path      <- file.path(data_dir, "us_poverty_data.parquet")
county_geometry_parquet_path <- file.path(data_dir, "county_geometry.parquet")

# -- Download Parquet Files (if not present) -----------------------------------
if (!file.exists(county_poverty_parquet_path)) {
  download.file(county_poverty_parquet_url, destfile = county_poverty_parquet_path, mode = "wb")
}
if (!file.exists(state_poverty_parquet_path)) {
  download.file(state_poverty_parquet_url, destfile = state_poverty_parquet_path, mode = "wb")
}
if (!file.exists(us_poverty_parquet_path)) {
  download.file(us_poverty_parquet_url, destfile = us_poverty_parquet_path, mode = "wb")
}
if (!file.exists(county_geometry_parquet_path)) {
  download.file(county_geometry_url, destfile = county_geometry_parquet_path, mode = "wb")
}

# -- Open Arrow Datasets -------------------------------------------------------
county_poverty_data <- open_dataset(county_poverty_parquet_path)
state_poverty_data  <- open_dataset(state_poverty_parquet_path)
us_poverty_data     <- open_dataset(us_poverty_parquet_path)

# -- Load County Geometry Data -------------------------------------------------
county_geometry <- st_read_parquet(county_geometry_parquet_path)

# -- Load Small Datasets to Memory ---------------------------------------------
state_poverty_data <- state_poverty_data %>% collect()
us_poverty_data    <- us_poverty_data    %>% collect()

# ──────────────────────────────────────────────────────────────────────────────
# PRECOMPUTE CHOICES FOR UI ----------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# These vectors are used for dropdowns and other UI elements
## Ensures compatibility with Arrow datasets (can't use $ or [ directly)
## Compute once at startup for maximum efficiency and clarity.

# Race/Ethnicity choices (arranged by sore)
race_eth_choices <- county_poverty_data %>%
  select(race_eth_f, sore) %>%
  distinct() %>%
  arrange(sore) %>%
  collect() %>%
  pull(race_eth_f)

# State choices (arranged alphabetically)
state_choices <- county_poverty_data %>%
  select(state_n) %>%
  distinct() %>%
  collect() %>%
  pull(state_n) %>%
  sort()

# County choices (arranged alphabetically)
county_choices <- county_poverty_data %>%
  select(county_f) %>%
  distinct() %>%
  collect() %>%
  pull(county_f) %>%
  sort()

# Population in Poverty (US) choices (arranged by sognsmpbpE)
n_poverty_total_choices <- county_poverty_data %>%
  select(gnsmpbpE, sognsmpbpE) %>%
  distinct() %>%
  arrange(sognsmpbpE) %>%
  collect() %>%
  pull(gnsmpbpE)

# Population in Poverty (State) choices (arranged by sogssmpbpE)
s_poverty_total_choices <- county_poverty_data %>%
  select(gssmpbpE, sogssmpbpE) %>%
  distinct() %>%
  arrange(sogssmpbpE) %>%
  collect() %>%
  pull(gssmpbpE)

# Poverty Rate choices (arranged by sogppbpE)
poverty_rate_choices <- county_poverty_data %>%
  select(gppbpE, sogppbpE) %>%
  distinct() %>%
  arrange(sogppbpE) %>%
  collect() %>%
  pull(gppbpE)

# Poverty Reliability choices (arranged by sorppbpE)
poverty_reliability_choices <- county_poverty_data %>%
  select(rppbpE, sorppbpE) %>%
  distinct() %>%
  arrange(sorppbpE) %>%
  collect() %>%
  pull(rppbpE)

# Margin of Error choices (arranged by sogppbpM)
poverty_moe_choices <- county_poverty_data %>%
  select(gppbpM, sogppbpM) %>%
  distinct() %>%
  arrange(sogppbpM) %>%
  collect() %>%
  pull(gppbpM)

# GEOID choices (for bounding box or filtering by geography, if needed)
geoid_choices <- county_poverty_data %>%
  select(GEOID) %>%
  distinct() %>%
  collect() %>%
  pull(GEOID)

# ──────────────────────────────────────────────────────────────────────────────
# CREATE CACHE -----------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Create cache to store memoised function results across sessions/users
memoise_cache <- cache_filesystem("memoise_cache")

# ──────────────────────────────────────────────────────────────────────────────
# MEMOISED FUNCTIONS -----------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

# Memoised function to get filtered county poverty data
get_county_poverty_data <- memoise(function(
    race_eth_f = NULL,
    state_n = NULL,
    county_f = NULL,
    gnsmpbpE = NULL,
    gssmpbpE = NULL,
    gppbpE = NULL,
    rppbpE = NULL,
    gppbpM = NULL
) {
  dat <- county_poverty_data %>% collect()
  
  if (!is.null(race_eth_f)) dat <- dat %>% filter(race_eth_f %in% !!race_eth_f)
  if (!is.null(state_n)) dat <- dat %>% filter(state_n %in% !!state_n)
  if (!is.null(county_f)) dat <- dat %>% filter(county_f %in% !!county_f)
  if (!is.null(gnsmpbpE)) dat <- dat %>% filter(gnsmpbpE %in% !!gnsmpbpE)
  if (!is.null(gssmpbpE)) dat <- dat %>% filter(gssmpbpE %in% !!gssmpbpE)
  if (!is.null(gppbpE)) dat <- dat %>% filter(gppbpE %in% !!gppbpE)
  if (!is.null(rppbpE)) dat <- dat %>% filter(rppbpE %in% !!rppbpE)
  if (!is.null(gppbpM)) dat <- dat %>% filter(gppbpM %in% !!gppbpM)
  
  result <- dat
  
  # Join with county_geometry on GEOID
  joined <- left_join(result, county_geometry, by = "GEOID") %>% st_as_sf(crs = 4326)

  return(joined)
}, cache = memoise_cache)

# ──────────────────────────────────────────────────────────────────────────────
# WARM THE CACHE ---------------------------------------------------------------
# ──────────────────────────────────────────────────────────────────────────────

## -- Poverty Data -------------------------------------------------------------

# Warm the cache for all counties for each race/ethnicity
walk(
  race_eth_choices,
  ~get_county_poverty_data(race_eth_f = .x)
)
