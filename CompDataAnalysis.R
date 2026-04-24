library(tidyverse)
library(rnpn)

#### Cover Data ####

# ── 1. Load data ──────────────────────────────────────────────────────────────
dat <- read_csv("NUT011.csv")

# ── 2. Comp subplot groups ────────────────────────────────────────────────────
comp_subplots <- c("A", "B", "C", "D",
                   "A.1", "B.1", "C.1", "D.1",
                   "A1",  "B1",  "C1",  "D1")

# ── 3. Grasses & sedges to exclude ───────────────────────────────────────────
grasses_sedges <- c(
  "andropogon gerardii", "schizachyrium scoparium", "bouteloua curtipendula",
  "sorghastrum nutans", "sporobolus compositus", "dichanthelium oligosanthes",
  "muhlenbergia cuspidata", "muhlenbergia racemosa", "muhlenbergia reverchonii",
  "panicum virgatum", "elymus canadensis", "bromus inermis", "poa pratensis",
  "carex inops", "carex meadii", "carex gravida", "carex bicknellii",
  "carex annectens", "carex brevior", "carex oklahomensis", "carex pellita",
  "juncus interior", "juncus tenuis", "cyperus lupulinus",
  "bouteloua gracilis", "bouteloua hirsuta", "digitaria cognata",
  "aristida purpurea", "sphenopholis obtusata", "koeleria macrantha",
  "nassella viridula", "stipa spartea", "hesperostipa spartea",
  "dichanthelium acuminatum", "dichanthelium leibergii", "dichanthelium depauperatum",
  "paspalum setaceum", "tripsacum dactyloides", "distichlis spicata",
  "muhlenbergia frondosa", "muhlenbergia schreberi", "sporobolus heterolepis",
  "dichanthelium ovale", "eragrostis spectabilis", "bare_ground",
  "cyperus spp.", "schedonnardus paniculatus", "panicum capillare"
)

# ── 4. Wind-pollinated forbs to exclude ──────────────────────────────────────
wind_pollinated <- c(
  "ambrosia artemisiifolia",
  "ambrosia psilostachya",
  "artemisia ludoviciana"
)

# ── 5. Filter to comp subplots, both seasons, insect-pollinated forbs only ────
comp_both <- dat %>%
  filter(Subplot %in% comp_subplots) %>%
  filter(
    !is.na(Taxa), Taxa != "",
    !str_to_lower(Taxa) %in% grasses_sedges,
    !str_to_lower(Taxa) %in% wind_pollinated
  ) %>%
  mutate(Taxa = str_to_title(str_to_lower(Taxa)))

# ── 6. Mean cover per species per year and season ─────────────────────────────
cover_by_year_season <- comp_both %>%
  group_by(RecYear, Season, Taxa) %>%
  summarise(
    n_obs      = n(),
    mean_cover = round(mean(Cover), 2),
    max_cover  = max(Cover),
    .groups = "drop"
  ) %>%
  arrange(RecYear, Season, desc(mean_cover))

# ── 6b. Mean cover per species per season (aggregated across all years) ───────
cover_by_season <- comp_both %>%
  group_by(Season, Taxa) %>%
  summarise(
    n_obs         = n(),
    years_present = n_distinct(RecYear),
    mean_cover    = round(mean(Cover), 2),
    max_cover     = max(Cover),
    .groups = "drop"
  ) %>%
  arrange(Season, desc(mean_cover))

write_csv(cover_by_season, "forb_cover_by_species_season_comp.csv")
write_csv(cover_by_year_season, "forb_cover_by_species_year_season_comp.csv")
print(cover_by_season, n = 20)

# ── 7. Literature-based flowering dates for species missing from PhenoBase ────
# Sources: wildflower.org, kswildflower.org, Wikipedia, Chicago Botanic Garden
literature_dates <- tribble(
  ~Taxa,                ~mean_doy, ~mean_date, ~median_doy, ~median_date, ~earliest_doy, ~earliest_date, ~latest_doy, ~latest_date, ~source,
  "Physalis Pumila",         196,   "Jul 15",        196,      "Jul 15",           121,       "May 01",        243,    "Aug 31",  "literature",
  "Asclepias Viridis",       152,   "Jun 01",        152,      "Jun 01",           121,       "May 01",        212,    "Jul 31",  "literature",
  "Asclepias Viridiflora",   196,   "Jul 15",        196,      "Jul 15",           152,       "Jun 01",        274,    "Oct 01",  "literature",
  "Baptisia Bracteata",      135,   "May 15",        135,      "May 15",            91,       "Apr 01",        166,    "Jun 15",  "literature",
  "Salvia Azurea",           248,   "Sep 05",        248,      "Sep 05",           209,       "Jul 28",        274,    "Oct 01",  "literature",
  "Conyza Canadensis",       235,   "Aug 23",        235,      "Aug 23",           188,       "Jul 07",        274,    "Oct 01",  "literature"
)

#### Avg Emergence Date ####

# ── 8. Clean species names for NPN query ─────────────────────────────────────
taxa_list <- cover_by_season$Taxa %>%
  unique() %>%
  discard(~ str_detect(.x, "Unk_"))

# ── 9. Look up NPN species IDs ───────────────────────────────────────────────
npn_species_list <- npn_species()

taxa_df <- tibble(full_name = taxa_list) %>%
  separate(full_name, into = c("genus", "species"), sep = " ", extra = "drop") %>%
  mutate(genus   = str_to_lower(genus),
         species = str_to_lower(species))

npn_matched <- npn_species_list %>%
  mutate(genus   = str_to_lower(genus),
         species = str_to_lower(species)) %>%
  inner_join(taxa_df, by = c("genus", "species"))

unmatched <- taxa_df %>%
  anti_join(npn_species_list %>%
              mutate(genus   = str_to_lower(genus),
                     species = str_to_lower(species)),
            by = c("genus", "species"))

cat("Unmatched species:\n")
print(unmatched)

species_ids <- npn_matched$species_id

# ── 10. Download open flower status data ──────────────────────────────────────
flowering_raw <- npn_download_status_data(
  request_source = "your_name",
  species_ids    = species_ids,
  phenophase_ids = c(501),
  years          = c(2007:2023)
)

# ── 11. Filter to positive observations only ──────────────────────────────────
flowering_yes <- flowering_raw %>%
  filter(phenophase_status == 1)

# ── 12. Summarise DOY per species ─────────────────────────────────────────────
flowering_summary <- flowering_yes %>%
  group_by(genus, species) %>%
  summarise(
    n_obs        = n(),
    mean_doy     = round(mean(day_of_year, na.rm = TRUE)),
    sd_doy       = round(sd(day_of_year, na.rm = TRUE), 1),
    median_doy   = median(day_of_year, na.rm = TRUE),
    earliest_doy = min(day_of_year, na.rm = TRUE),
    latest_doy   = max(day_of_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    full_name     = str_to_title(paste(genus, species)),
    mean_date     = format(as.Date(mean_doy - 1,     origin = "2023-01-01"), "%b %d"),
    median_date   = format(as.Date(median_doy - 1,   origin = "2023-01-01"), "%b %d"),
    earliest_date = format(as.Date(earliest_doy - 1, origin = "2023-01-01"), "%b %d"),
    latest_date   = format(as.Date(latest_doy - 1,   origin = "2023-01-01"), "%b %d")
  ) %>%
  select(full_name, n_obs,
         mean_doy, mean_date,
         median_doy, median_date,
         sd_doy,
         earliest_doy, earliest_date,
         latest_doy, latest_date) %>%
  arrange(mean_doy)

write_csv(flowering_summary, "flowering_doy_by_species.csv")
print(flowering_summary, n = 61)

#### Determine Sampling Dates ####

# ── 13. Top 5 insect-pollinated species per season ────────────────────────────
cover <- read_csv("forb_cover_by_species_season_comp.csv")
dates <- read_csv("flowering_doy_by_species.csv")

top5_by_season <- cover %>%
  group_by(Season) %>%
  slice_max(order_by = mean_cover, n = 5) %>%
  ungroup()

# ── 14. Join with NPN flowering dates ────────────────────────────────────────
top5_with_dates <- top5_by_season %>%
  left_join(
    dates %>% select(full_name, n_obs, mean_doy, mean_date, median_doy, median_date,
                     sd_doy, earliest_doy, earliest_date, latest_doy, latest_date),
    by = c("Taxa" = "full_name")
  ) %>%
  arrange(Season, desc(mean_cover))

# ── 15. Fill remaining NAs from literature ────────────────────────────────────
top5_filled <- top5_with_dates %>%
  left_join(literature_dates, by = "Taxa", suffix = c("", "_lit")) %>%
  mutate(
    mean_doy      = coalesce(mean_doy,      mean_doy_lit),
    mean_date     = coalesce(mean_date,     mean_date_lit),
    median_doy    = coalesce(median_doy,    median_doy_lit),
    median_date   = coalesce(median_date,   median_date_lit),
    earliest_doy  = coalesce(earliest_doy,  earliest_doy_lit),
    earliest_date = coalesce(earliest_date, earliest_date_lit),
    latest_doy    = coalesce(latest_doy,    latest_doy_lit),
    latest_date   = coalesce(latest_date,   latest_date_lit),
    source        = if_else(is.na(source),  "PhenoBase/NPN", source)
  ) %>%
  select(-ends_with("_lit"))

print(top5_filled)
write_csv(top5_filled, "top5_cover_with_dates_filled.csv")
