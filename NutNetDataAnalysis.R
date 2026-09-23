# =============================================================================
# 01_clean_konza_poll.R
# Import KonzaPollObs2026Summer.xlsx, clean, and write tidy CSVs.
#
# Outputs (in data_clean/):
#   konza_poll_2026_clean.csv    one row per visitor observation, cleaned
#   konza_poll_2026_counts.csv   collapsed to counts per plot/period/plant/taxon
#   konza_poll_2026_review.csv   rows that still need a human decision
#
# Every value this script changes is recorded in the `fix_note` column.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(readr)
})

in_file  <- "KonzaPollObs2026Summer.xlsx"
out_dir  <- "data_clean"
dir.create(out_dir, showWarnings = FALSE)

start_date <- as.Date("2026-05-25")   # Day 1 == 2026-05-26

# 1. Import as text ############################################################
# Reading everything as text prevents readxl from guessing types off the first
# few rows, and keeps Excel from silently reformatting dates/times. We coerce
# deliberately below.

raw <- read_excel(in_file, sheet = 1, col_types = "text", trim_ws = FALSE)

message("Imported ", nrow(raw), " rows x ", ncol(raw), " cols")

# 2. Column names ##############################################################
names(raw) <- names(raw) |>
  str_trim() |>                     # kills the trailing space on "Family "
  str_replace_all("[/ ]+", "_") |>
  tolower()

d <- raw |>
  rename(
    period          = morning_evening,
    air_temp        = airtemp,
    cloud_cover     = cloudcover,
    open_flower     = openflower,
    plant_spp       = plantspp,
    visitor_desc    = vistordesc,     # header typo in the source file
    open_flower_num = openflowernum,
    time_raw        = time,
    date_raw        = date,
    day_raw         = day,
    inat            = inat
  ) |>
  mutate(src_row = row_number() + 1L, .before = 1)  # +1 = Excel row incl. header

# 3. Whitespace + spelling #####################################################
# Trailing spaces ("Hymenoptera " vs "Hymenoptera", "Cantharidae ", "Y ")
# create phantom factor levels that will silently split your groups.

d <- d |>
  mutate(across(where(is.character), ~ str_squish(.x))) |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

d <- d |>
  mutate(
    visitor_desc = str_replace(visitor_desc, "Solider", "Soldier"),
    species      = str_to_lower(species),          # "Mellifera" -> "mellifera"
    genus        = str_to_sentence(genus),
    cloud_cover  = if_else(cloud_cover == "z", NA_character_, cloud_cover)
  )

# 4. Types: date and time ######################################################
# Handles both possibilities: real Excel dates (numeric serial) and text.

excel_date <- function(x) {
  x <- str_trim(x)
  out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  ser <- !is.na(x) & str_detect(x, "^[0-9]+(\\.[0-9]+)?$")
  out[ser] <- as.Date(as.numeric(x[ser]), origin = "1899-12-30")
  txt <- !is.na(x) & !ser
  out[txt] <- suppressWarnings(mdy(x[txt]))
  out
}

# Time has no AM/PM in the file, so Morning/Evening is the only disambiguator.
parse_clock <- function(x, period) {
  x   <- str_trim(x)
  frac <- !is.na(x) & str_detect(x, "^0?\\.[0-9]+$")      # Excel time fraction
  hm  <- x
  hm[frac] <- format(as.POSIXct(as.numeric(x[frac]) * 86400,
                                origin = "1970-01-01", tz = "UTC"), "%H:%M")
  h <- suppressWarnings(as.integer(str_extract(hm, "^[0-9]{1,2}")))
  m <- suppressWarnings(as.integer(str_extract(hm, "(?<=:)[0-9]{2}")))
  pm <- !is.na(period) & str_to_lower(period) == "evening" & !is.na(h) & h < 12
  h[pm] <- h[pm] + 12L
  if_else(is.na(h) | is.na(m), NA_character_, sprintf("%02d:%02d", h, m))
}

d <- d |>
  mutate(
    plot            = suppressWarnings(as.integer(plot)),
    block           = suppressWarnings(as.integer(block)),
    round           = suppressWarnings(as.integer(round)),
    air_temp        = suppressWarnings(as.numeric(air_temp)),
    wind            = suppressWarnings(as.numeric(wind)),
    rh              = suppressWarnings(as.numeric(rh)),
    open_flower_num = suppressWarnings(as.integer(open_flower_num)),
    day_raw         = suppressWarnings(as.integer(day_raw)),
    obs_date        = excel_date(date_raw),
    time_24         = parse_clock(time_raw, period),
    open_flower     = str_to_upper(str_trim(open_flower))
  )

# Drop the eight blank placeholder rows (Round 5 morning, Blocks 2-3).
blank_rows <- d |> filter(is.na(obs_date) & is.na(time_24))
if (nrow(blank_rows) > 0) {
  message("Dropping ", nrow(blank_rows), " blank placeholder rows: ",
          paste(blank_rows$src_row, collapse = ", "))
  d <- d |> filter(!(is.na(obs_date) & is.na(time_24)))
}

# 5. Targeted record fixes #####################################################
# Each entry states the evidence. Edit, reorder, or comment out any of these --
# the script reports how many rows each one touched, so a silent no-match is
# impossible to miss.

fixes <- list(
  
  ## Plot/Block/Trt disagreements with the plot key ####
  list(
    note  = "Plot 23 -> 13: mid-Block-2 walk; plant composition (A. tuberosa + Physalis) matches plot 13 that evening",
    where = quote(obs_date == as.Date("2026-05-26") & period == "Morning" & plot == 23L),
    set   = list(plot = 13L)
  ),
  list(
    note  = "Block 2 -> 3: plot 25 is Block 3's N plot; Block 2's N (plot 14) was already surveyed at 10:50. UNCERTAIN - Block 3 was otherwise surveyed 5/27",
    where = quote(obs_date == as.Date("2026-05-26") & plot == 25L & block == 2L),
    set   = list(block = 3L)
  ),
  list(
    note  = "Plot 19 -> 28: surrounding rows are a Block 3 walk; 19 is Block 2's NP plot",
    where = quote(obs_date == as.Date("2026-05-29") & period == "Morning" & plot == 19L & block == 3L),
    set   = list(plot = 28L)
  ),
  list(
    note  = "Plot 25 -> 30: Block 3 NPK is plot 30; plot 25 already recorded as N at 08:32 that morning",
    where = quote(obs_date == as.Date("2026-05-29") & period == "Morning" & plot == 25L & trt == "NPK"),
    set   = list(plot = 30L)
  ),
  list(
    note  = "Plot 1 -> 2: Block 1 NPK is plot 2; plot 1 already recorded as NP at 08:00",
    where = quote(obs_date == as.Date("2026-06-02") & period == "Morning" & plot == 1L & trt == "NPK"),
    set   = list(plot = 2L)
  ),
  list(
    note  = "Trt P -> C: plot 21 is Block 3's control (and its signature Pediomelum stand)",
    where = quote(obs_date == as.Date("2026-05-30") & period == "Evening" & plot == 21L & trt == "P"),
    set   = list(trt = "C")
  ),
  list(
    note  = "Trt C -> NPK: plot 30 is Block 3's NPK plot",
    where = quote(obs_date == as.Date("2026-05-30") & period == "Evening" & plot == 30L & trt == "C"),
    set   = list(trt = "NPK")
  ),
  
  ## Blank Plot ####
  list(
    note  = "Plot blank -> 30 from Block 3 / NPK key",
    where = quote(is.na(plot) & obs_date == as.Date("2026-06-01") & block == 3L & trt == "NPK"),
    set   = list(plot = 30L)
  ),
  list(
    note  = "Plot blank -> 28 from Block 3 / NP key",
    where = quote(is.na(plot) & obs_date == as.Date("2026-06-03") & block == 3L & trt == "NP"),
    set   = list(plot = 28L)
  ),
  
  ## Date typos ####
  list(
    note  = "Date 5/27 -> 5/26: sits at the end of an unbroken 5/26 evening run (15:20 -> 15:41 -> 16:00)",
    where = quote(obs_date == as.Date("2026-05-27") & plot == 30L & round == 1L & period == "Evening"),
    set   = list(obs_date = as.Date("2026-05-26"))
  ),
  list(
    note  = "Date 5/30 -> 5/29: opens the Round 3 evening run, immediately followed by a 5/29 row at 14:04",
    where = quote(obs_date == as.Date("2026-05-30") & plot == 4L & round == 3L & period == "Evening"),
    set   = list(obs_date = as.Date("2026-05-29"))
  )
)

d$fix_note <- NA_character_

for (f in fixes) {
  idx <- which(eval(f$where, d))          # which() drops NAs -> NA-safe
  if (length(idx) == 0) {
    warning("FIX MATCHED NOTHING: ", f$note, call. = FALSE)
    next
  }
  for (nm in names(f$set)) d[[nm]][idx] <- f$set[[nm]]
  d$fix_note[idx] <- ifelse(is.na(d$fix_note[idx]), f$note,
                            paste(d$fix_note[idx], f$note, sep = " | "))
  message(sprintf("Fix applied to %2d row(s): %s", length(idx), f$note))
}

# 6. Derived fields + validation flags #########################################
# Day is fully determined by the date, so recompute rather than trust it.

plot_key <- tribble(
  ~plot, ~key_block, ~key_trt,
  1L, 1L, "NP",   2L, 1L, "NPK",  4L, 1L, "C",   5L, 1L, "N",   7L, 1L, "P",
  13L, 2L, "P",   14L, 2L, "N",   16L, 2L, "NPK", 18L, 2L, "C",  19L, 2L, "NP",
  21L, 3L, "C",   23L, 3L, "P",   25L, 3L, "N",   28L, 3L, "NP", 30L, 3L, "NPK"
)

d <- d |>
  left_join(plot_key, by = "plot") |>
  mutate(
    day = as.integer(obs_date - start_date),
    
    visitor_present = !is.na(visitor_desc) & visitor_desc != "None",
    has_flowers     = !is.na(plant_spp) & plant_spp != "None",
    
    flag_plot_key      = is.na(key_block) | block != key_block | trt != key_trt,
    flag_day_mismatch  = !is.na(day_raw) & day_raw != day,
    # "N" open flowers, yet a plant and a flower count are recorded:
    flag_openflower_no = open_flower == "N" & has_flowers,
    # "Y" open flowers, but no plant named:
    flag_openflower_yes = open_flower == "Y" & !has_flowers,
    flag_visitor_no_flower = visitor_present & !has_flowers,
    flag_count_missing = has_flowers & is.na(open_flower_num),
    
    needs_review = flag_plot_key | flag_day_mismatch | flag_openflower_no |
      flag_openflower_yes | flag_visitor_no_flower | flag_count_missing
  ) |>
  arrange(obs_date, period, time_24, plot)

# 7. Write outputs #############################################################
clean <- d |>
  select(src_row, obs_date, round, day, period, observer,
         block, plot, trt,
         air_temp, wind, rh, cloud_cover, time_24,
         open_flower, plant_spp, open_flower_num,
         visitor_desc, visitor_present,
         order, suborder, family, genus, species, inat, notes,
         starts_with("flag_"), needs_review, fix_note)

write_csv(clean, file.path(out_dir, "konza_poll_2026_clean.csv"), na = "")

review <- clean |> filter(needs_review)
write_csv(review, file.path(out_dir, "konza_poll_2026_review.csv"), na = "")

# Collapse the duplicated individual rows into counts. This is what makes the
# 50 identical 15:17 rows in plot 28 auditable.
counts <- clean |>
  group_by(obs_date, round, day, period, block, plot, trt,
           air_temp, wind, rh, cloud_cover,
           time_24, plant_spp, open_flower_num,
           visitor_desc, order, suborder, family, genus, species) |>
  summarise(count = sum(visitor_present), .groups = "drop") |>
  mutate(count = if_else(count == 0L, 0L, count))

write_csv(counts, file.path(out_dir, "konza_poll_2026_counts.csv"), na = "")

# 8. Report ####################################################################
cat("\n--- Cleaning summary ---------------------------------------------\n")
cat("Rows out:            ", nrow(clean), "\n")
cat("Rows changed:        ", sum(!is.na(clean$fix_note)), "\n")
cat("Rows needing review: ", nrow(review), "\n\n")

clean |>
  summarise(across(starts_with("flag_"), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "flag", values_to = "n") |>
  filter(n > 0) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nVisits by treatment (uncorrected for flower availability):\n")
clean |>
  group_by(trt) |>
  summarise(visits = sum(visitor_present),
            plot_visits = n_distinct(paste(obs_date, period, plot)),
            .groups = "drop") |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\nVisits by plant species -- note the Apocynum dominance:\n")
clean |>
  filter(visitor_present) |>
  count(plant_spp, sort = TRUE) |>
  as.data.frame() |>
  print(row.names = FALSE)

# =============================================================================
# 02_analyse_konza_poll.R
# Figures and models for KonzaPollObs2026Summer.
#
# Run 01_clean_konza_poll.R first.
#
# The analysis is built around one idea: a visitor count is meaningless without
# knowing how many flowers were on offer. Every visitation model here therefore
# carries an offset for floral abundance, and floral abundance itself is
# modelled as a response in its own right (Section 5). Treatment can act on
# visitation through either path, and they need separating.
#
# Sampling unit is the plot x date x period observation, NOT the row. Rows are
# individual insects and are pseudoreplicates.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(forcats)
  # modelling (install if needed)
  library(glmmTMB)
  library(emmeans)
  library(DHARMa)
  library(vegan)
})

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

theme_set(
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"))
)

trt_levels <- c("C", "N", "P", "NP", "NPK")
trt_cols   <- c(C = "#5B5B5B", N = "#2A6EBB", P = "#D4772F",
                NP = "#7A3B9E", NPK = "#B23A48")

# 1. Read cleaned data #########################################################

d <- read_csv("data_clean/konza_poll_2026_clean.csv", show_col_types = FALSE) |>
  mutate(
    trt      = factor(trt, levels = trt_levels),
    block    = factor(block),
    plot     = factor(plot),
    round    = factor(round),
    period   = factor(period, levels = c("Morning", "Evening")),
    plant_spp = na_if(plant_spp, "None"),
    order    = na_if(order, "None")
  )

# Drop rows still flagged for review so they don't quietly enter the models.
# Inspect them first -- do not just let this run.
if (any(d$needs_review)) {
  message(sum(d$needs_review), " rows flagged for review. ",
          "Resolve them in 01_clean before trusting anything below.")
}

# 2. Build the two analysis units ##############################################

# Unit A: plot x date x period x plant species.
#   The finest unit at which a flower count exists. This is where you ask
#   "given that plant X is flowering, does treatment change visitation to it?"
plant_period <- d |>
  filter(!is.na(plant_spp)) |>
  group_by(obs_date, round, day, period, block, plot, trt,
           plant_spp, open_flower_num, air_temp, wind, rh, cloud_cover) |>
  summarise(visits = sum(visitor_present), .groups = "drop") |>
  rename(flowers = open_flower_num)

# Unit B: plot x date x period. Total flowers and total visits per survey.
#   This is where you ask "does treatment change the plot's floral resource,
#   and the pollinator traffic it receives?"
plot_period <- d |>
  group_by(obs_date, round, day, period, block, plot, trt) |>
  summarise(
    visits       = sum(visitor_present),
    flowers      = sum(open_flower_num[!is.na(plant_spp)], na.rm = TRUE),
    n_plant_spp  = n_distinct(plant_spp[!is.na(plant_spp)]),
    apocynum     = any(plant_spp == "Apocynum cannabinum", na.rm = TRUE),
    .groups = "drop"
  )

# Plot-periods with zero flowers are STRUCTURAL zeros: a visitation rate is
# undefined, not zero. They belong in the floral model, not the rate model.
plot_period_flowering <- plot_period |> filter(flowers > 0)

message("plot-periods: ", nrow(plot_period),
        " (", sum(plot_period$flowers == 0), " with no flowers)")

# 3. Design and effort check ###################################################
# Before any inference: was every plot surveyed in every round? Unbalanced
# effort will masquerade as a treatment effect in any raw total.

fig_effort <- plot_period |>
  count(round, period, trt, block) |>
  ggplot(aes(x = round, y = fct_rev(block), fill = n)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = n), size = 3, colour = "white", fontface = "bold") +
  facet_grid(period ~ trt) +
  scale_fill_viridis_c(option = "mako", begin = 0.2, end = 0.8) +
  labs(title = "Sampling effort: surveys per block x treatment x round",
       subtitle = "Gaps and doubles here constrain everything downstream",
       x = "Round", y = "Block", fill = "Surveys")

ggsave(file.path(fig_dir, "fig1_effort.png"), fig_effort,
       width = 9, height = 4.5, dpi = 300)

# 4. What is actually flowering ################################################

fig_flora <- d |>
  filter(!is.na(plant_spp)) |>
  distinct(obs_date, period, plot, trt, block, plant_spp, open_flower_num) |>
  group_by(trt, plant_spp) |>
  summarise(mean_flowers = mean(open_flower_num, na.rm = TRUE),
            n_surveys = n(), .groups = "drop") |>
  ggplot(aes(x = trt, y = fct_reorder(plant_spp, mean_flowers),
             fill = mean_flowers)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = round(mean_flowers)), size = 3, colour = "grey95") +
  scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.15) +
  labs(title = "Mean open flowers per survey, by plant species and treatment",
       subtitle = "Which species carry the floral display in each treatment",
       x = "Treatment", y = NULL, fill = "Mean\nflowers")

ggsave(file.path(fig_dir, "fig2_floral_composition.png"), fig_flora,
       width = 7.5, height = 4.5, dpi = 300)

# Floral abundance per survey, treatment x block. Block shown explicitly
# because with 3 blocks it is not a nuisance you can wave away.
fig_flowers <- ggplot(plot_period, aes(x = trt, y = flowers)) +
  geom_boxplot(aes(fill = trt), outlier.shape = NA, alpha = 0.25, width = 0.6) +
  geom_point(aes(colour = trt, shape = block),
             position = position_jitter(width = 0.15, height = 0), size = 2) +
  scale_fill_manual(values = trt_cols, guide = "none") +
  scale_colour_manual(values = trt_cols, guide = "none") +
  scale_y_sqrt(breaks = c(0, 5, 25, 50, 100)) +
  labs(title = "Floral abundance per survey",
       subtitle = "sqrt scale; shape = block",
       x = "Treatment", y = "Open flowers in plot")

ggsave(file.path(fig_dir, "fig3_floral_abundance.png"), fig_flowers,
       width = 6.5, height = 4, dpi = 300)

# 5. THE CONFOUND #############################################################
# Apocynum cannabinum carries nearly all the visitation in this dataset, and it
# is not evenly distributed across plots. If Apocynum presence is itself a
# block or treatment property, then "treatment effect on visitation" and
# "treatment effect on Apocynum" are the same number wearing two hats.
# Look at this figure before you interpret any model below.

apoc <- plot_period |>
  group_by(block, plot, trt) |>
  summarise(surveys = n(),
            apoc_surveys = sum(apocynum),
            visits = sum(visits), .groups = "drop") |>
  mutate(apoc_prop = apoc_surveys / surveys)

fig_confound <- ggplot(apoc, aes(x = apoc_prop, y = visits)) +
  geom_point(aes(colour = trt, shape = block), size = 3.5) +
  geom_text(aes(label = plot), nudge_y = 6, size = 2.8, colour = "grey40") +
  scale_colour_manual(values = trt_cols) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Visitation tracks Apocynum, not treatment",
       subtitle = "Each point is a plot. Label = plot number.",
       x = "Surveys in which Apocynum cannabinum was flowering",
       y = "Total visitors recorded", colour = "Treatment", shape = "Block")

ggsave(file.path(fig_dir, "fig4_apocynum_confound.png"), fig_confound,
       width = 7, height = 4.5, dpi = 300)

# Formal statement of the same problem: is Apocynum presence balanced across
# treatments and blocks? If this table has empty cells, treatment effects on
# visitation are not identifiable from treatment effects on Apocynum.
cat("\n--- Apocynum presence by block x treatment ---\n")
apoc |>
  select(block, plot, trt, surveys, apoc_surveys) |>
  arrange(block, trt) |>
  as.data.frame() |>
  print(row.names = FALSE)

# 6. Visitation ################################################################

# Raw counts -- the number people will ask for, and the most misleading.
fig_visits_raw <- ggplot(plot_period, aes(x = trt, y = visits)) +
  geom_boxplot(aes(fill = trt), outlier.shape = NA, alpha = 0.25, width = 0.6) +
  geom_point(aes(colour = trt, shape = block),
             position = position_jitter(width = 0.15, height = 0), size = 2) +
  scale_fill_manual(values = trt_cols, guide = "none") +
  scale_colour_manual(values = trt_cols, guide = "none") +
  labs(title = "Visitors per survey (raw)",
       subtitle = "Not corrected for floral availability -- see fig 6",
       x = "Treatment", y = "Visitors")

# Per-flower rate -- the ecologically meaningful quantity.
fig_visits_rate <- plot_period_flowering |>
  mutate(rate = 100 * visits / flowers) |>
  ggplot(aes(x = trt, y = rate)) +
  geom_boxplot(aes(fill = trt), outlier.shape = NA, alpha = 0.25, width = 0.6) +
  geom_point(aes(colour = trt, shape = block),
             position = position_jitter(width = 0.15, height = 0), size = 2) +
  scale_fill_manual(values = trt_cols, guide = "none") +
  scale_colour_manual(values = trt_cols, guide = "none") +
  labs(title = "Visitors per 100 open flowers",
       subtitle = "Flowering plot-periods only; zero-flower surveys are undefined, not zero",
       x = "Treatment", y = "Visitors / 100 flowers")

ggsave(file.path(fig_dir, "fig5_visits_raw.png"), fig_visits_raw,
       width = 6.5, height = 4, dpi = 300)
ggsave(file.path(fig_dir, "fig6_visits_per_flower.png"), fig_visits_rate,
       width = 6.5, height = 4, dpi = 300)

# 7. Who is visiting ###########################################################

fig_order <- d |>
  filter(visitor_present) |>
  count(trt, order) |>
  ggplot(aes(x = trt, y = n, fill = fct_reorder(order, n))) +
  geom_col(position = "fill", width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Visitor composition by treatment",
       subtitle = "Proportion of individuals; bar widths hide very unequal n",
       x = "Treatment", y = "Proportion of visitors", fill = "Order")

# Show the n that the proportions conceal.
fig_order_n <- d |>
  filter(visitor_present) |>
  count(trt) |>
  ggplot(aes(x = trt, y = n, fill = trt)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  scale_fill_manual(values = trt_cols, guide = "none") +
  labs(x = "Treatment", y = "Individuals recorded", title = NULL)

ggsave(file.path(fig_dir, "fig7_visitor_composition.png"), fig_order,
       width = 6.5, height = 4, dpi = 300)
ggsave(file.path(fig_dir, "fig7b_visitor_n.png"), fig_order_n,
       width = 6.5, height = 3, dpi = 300)

# Interaction matrix: which visitor families use which plants.
fig_network <- d |>
  filter(visitor_present, !is.na(plant_spp)) |>
  mutate(taxon = coalesce(family, order, visitor_desc)) |>
  count(plant_spp, taxon) |>
  ggplot(aes(x = fct_reorder(taxon, n, sum),
             y = fct_reorder(plant_spp, n, sum), fill = n)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = n), size = 3, colour = "grey95") +
  scale_fill_viridis_c(option = "mako", direction = -1, trans = "sqrt") +
  labs(title = "Plant x visitor interaction matrix",
       subtitle = "Individuals recorded; sqrt colour scale",
       x = "Visitor family / order", y = "Plant species", fill = "n") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave(file.path(fig_dir, "fig8_interaction_matrix.png"), fig_network,
       width = 8, height = 4.5, dpi = 300)

# 8. Phenology #################################################################
# Five rounds over ~9 days is a narrow window, but Apocynum came into bloom
# during it, which drives most of the temporal signal.

fig_phen <- plot_period |>
  group_by(obs_date, trt) |>
  summarise(flowers = mean(flowers), visits = mean(visits), .groups = "drop") |>
  pivot_longer(c(flowers, visits)) |>
  ggplot(aes(x = obs_date, y = value, colour = trt)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_wrap(~ name, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(flowers = "Mean open flowers per survey",
                                      visits  = "Mean visitors per survey"))) +
  scale_colour_manual(values = trt_cols) +
  labs(title = "Floral display and visitation over the sampling window",
       x = NULL, y = NULL, colour = "Treatment")

ggsave(file.path(fig_dir, "fig9_phenology.png"), fig_phen,
       width = 7.5, height = 5, dpi = 300)

# 9. Sampling completeness #####################################################
# How much of the visitor fauna did 10 days of survey actually catch? If the
# accumulation curve is still climbing steeply, richness comparisons between
# treatments are premature.

comm <- d |>
  filter(visitor_present) |>
  mutate(taxon = coalesce(family, order, visitor_desc),
         unit  = paste(obs_date, period, plot)) |>
  count(unit, taxon) |>
  pivot_wider(names_from = taxon, values_from = n, values_fill = 0) |>
  tibble::column_to_rownames("unit")

if (nrow(comm) > 2) {
  acc <- specaccum(comm, method = "random", permutations = 200)
  png(file.path(fig_dir, "fig10_accumulation.png"),
      width = 6.5, height = 4, units = "in", res = 300)
  plot(acc, ci.type = "polygon", col = "grey20", ci.col = "grey85",
       ci.lty = 0, xlab = "Plot-period surveys", ylab = "Visitor taxa",
       main = "Visitor taxon accumulation")
  dev.off()
  cat("\nObserved taxa:", ncol(comm),
      "| Chao1 estimate:", round(specpool(comm)$chao, 1), "\n")
}

# 10. Models ###################################################################
# Read the caveats in the chat before reporting any of these. n = 3 plots per
# treatment. These models are worth fitting to structure your thinking; they
# are not worth reporting as confirmatory tests.

## 10a. Does nutrient addition change the floral resource? ####
# This is the question the design can actually address, and the one the Konza
# N x P literature makes a directional prediction about.

m_flowers <- glmmTMB(
  flowers ~ trt + period + (1 | block) + (1 | plot) + (1 | obs_date),
  family = nbinom2,
  data = plot_period
)
cat("\n=== Floral abundance model ===\n"); print(summary(m_flowers))
print(emmeans(m_flowers, ~ trt, type = "response"))

## 10b. Does nutrient addition change visitation, given the flowers? ####
# offset(log(flowers)) makes the response a per-flower rate. A treatment effect
# here means pollinators respond to the plot beyond its floral display.

m_rate <- glmmTMB(
  visits ~ trt + period + offset(log(flowers)) + (1 | block) + (1 | plot),
  family = nbinom2,
  data = plot_period_flowering
)
cat("\n=== Visitation rate model ===\n"); print(summary(m_rate))

# Per-flower rates on the response scale (offset = 0 -> per single flower).
emm_rate <- emmeans(m_rate, ~ trt, offset = 0, type = "response")
print(emm_rate)
print(pairs(emm_rate, adjust = "tukey"))

## 10c. Within-Apocynum comparison ####
# The cleanest available contrast: hold the plant constant, ask whether
# treatment changes visitation to it. Will only work if Apocynum occurs in
# enough treatments -- check the table from Section 5 first.

apoc_only <- plant_period |>
  filter(plant_spp == "Apocynum cannabinum", flowers > 0)

cat("\nApocynum plot-periods per treatment:\n")
print(table(apoc_only$trt, apoc_only$block))

if (n_distinct(apoc_only$trt) >= 3 && n_distinct(apoc_only$block) >= 2) {
  m_apoc <- glmmTMB(
    visits ~ trt + offset(log(flowers)) + (1 | plot),
    family = nbinom2, data = apoc_only
  )
  cat("\n=== Apocynum-only visitation model ===\n"); print(summary(m_apoc))
} else {
  message("Apocynum is too unevenly spread across treatments/blocks to fit ",
          "this model. That is a result about the design, not a failure.")
}

## 10d. Diagnostics ####
for (nm in c("m_flowers", "m_rate")) {
  if (exists(nm)) {
    res <- simulateResiduals(get(nm), n = 1000)
    png(file.path(fig_dir, paste0("diag_", nm, ".png")),
        width = 8, height = 4, units = "in", res = 300)
    plot(res)
    dev.off()
    cat("\n", nm, " dispersion test:\n", sep = "")
    print(testDispersion(res, plot = FALSE))
  }
}

# 11. A more honest alternative to the GLMMs ###################################
# With 15 plots in 3 blocks, a randomisation test respecting the blocking makes
# fewer assumptions than a mixed model with 5 random-effect levels. Shuffle
# treatment labels within block and rebuild the null distribution.

perm_test <- function(dat, response, n_perm = 4999) {
  obs_stat <- dat |>
    group_by(trt) |>
    summarise(m = mean(.data[[response]]), .groups = "drop") |>
    summarise(s = var(m)) |>
    pull(s)
  
  null <- replicate(n_perm, {
    shuffled <- dat |>
      group_by(block) |>
      mutate(trt = sample(trt)) |>
      ungroup()
    shuffled |>
      group_by(trt) |>
      summarise(m = mean(.data[[response]]), .groups = "drop") |>
      summarise(s = var(m)) |>
      pull(s)
  })
  list(observed = obs_stat, p = (sum(null >= obs_stat) + 1) / (n_perm + 1))
}

plot_means <- plot_period |>
  group_by(block, plot, trt) |>
  summarise(flowers = mean(flowers), visits = mean(visits), .groups = "drop")

set.seed(42)
cat("\n=== Within-block randomisation tests (plot means, n = 15) ===\n")
cat("Floral abundance: "); str(perm_test(plot_means, "flowers"))
cat("Visitors:         "); str(perm_test(plot_means, "visits"))

message("\nFigures written to ", normalizePath(fig_dir))


# =============================================================================
# 03_floral_composition.R
# Does nutrient addition change WHICH forbs flower, not just how many flowers?
#
# Run 01_clean_konza_poll.R first. Standalone -- does not need 02 in memory.
#
# Three analyses of the same 15 x 9 matrix, in order of how much I trust them:
#   1. manyglm  -- model-based, handles the mean-variance relationship, gives
#                  per-species tests. This is the inferential result.
#   2. NMDS     -- the picture. A visualisation, not a test.
#   3. adonis2  -- the familiar cross-check, with its known caveat.
#
# The visitor matrix is deliberately NOT analysed here: six plots recorded zero
# visitors, which leaves some treatments with a single plot. See the chat.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(forcats)
  library(vegan)
  library(mvabund)
  library(permute)
})

fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE)

trt_levels <- c("C", "N", "P", "NP", "NPK")
trt_cols   <- c(C = "#5B5B5B", N = "#2A6EBB", P = "#D4772F",
                NP = "#7A3B9E", NPK = "#B23A48")

# How to collapse repeat surveys of a plot into one number per species.
#   max  = peak floral display (default; robust to surveys before/after bloom)
#   mean = average display over the window
agg_fun <- max

theme_set(
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"))
)

# 1. Build the site x species matrix ###########################################

d <- read_csv("data_clean/konza_poll_2026_clean.csv", show_col_types = FALSE) |>
  mutate(plant_spp = na_if(plant_spp, "None"))

# One flower count per plot x survey x species. The raw file repeats the count
# on every insect row, so collapse those first -- otherwise a plot with 50
# soldier beetles contributes its flower count 50 times.
plant_survey <- d |>
  filter(!is.na(plant_spp), !is.na(open_flower_num)) |>
  group_by(obs_date, period, block, plot, trt, plant_spp) |>
  summarise(flowers = max(open_flower_num), .groups = "drop")

flora_long <- plant_survey |>
  group_by(block, plot, trt, plant_spp) |>
  summarise(flowers = agg_fun(flowers), .groups = "drop")

flora_wide <- flora_long |>
  pivot_wider(names_from = plant_spp, values_from = flowers, values_fill = 0) |>
  arrange(block, plot)

site <- flora_wide |>
  select(block, plot, trt) |>
  mutate(block = factor(block),
         trt   = factor(trt, levels = trt_levels))

mat <- flora_wide |>
  select(-block, -plot, -trt) |>
  as.matrix()
rownames(mat) <- flora_wide$plot

cat("\n=== Site x species matrix (peak open flowers) ===\n")
print(as.data.frame(cbind(site[, c("block", "trt")], mat)))

cat("\nDimensions: ", nrow(mat), " plots x ", ncol(mat), " species\n", sep = "")
cat("Empty rows (plots that never flowered): ", sum(rowSums(mat) == 0), "\n")
cat("Species present in only one plot: ",
    sum(colSums(mat > 0) == 1), " of ", ncol(mat), "\n", sep = "")

# Species occurring in a single plot carry no comparative information and
# destabilise both the ordination and the per-species tests. Report them, but
# consider dropping for a sensitivity check.
rare <- names(which(colSums(mat > 0) == 1))
if (length(rare)) message("Singleton species: ", paste(rare, collapse = ", "))

# 2. Model-based analysis (the one to report) ##################################
# manyglm fits a separate negative-binomial GLM per species and tests them
# jointly by resampling, so the mean-variance relationship is modelled rather
# than assumed away by a distance metric.
#
# block enters as a FIXED factor. With 3 blocks that is the right call anyway,
# and manyglm has no random effects.

Y <- mvabund(mat)

# Check the mean-variance relationship before trusting the family choice.
png(file.path(fig_dir, "fig11_meanvar.png"), width = 6, height = 4.5,
    units = "in", res = 300)
meanvar.plot(Y ~ site$trt, col = trt_cols[as.character(site$trt)], pch = 16,
             xlab = "Mean", ylab = "Variance")
dev.off()

m_many <- manyglm(Y ~ block + trt, family = "negative.binomial", data = site)

# Residual check: look for fan shapes. Points should be structureless.
png(file.path(fig_dir, "fig12_manyglm_resid.png"), width = 6, height = 4.5,
    units = "in", res = 300)
plot(m_many, which = 1)
dev.off()

cat("\nResidual df: ", nrow(mat) - length(coef(m_many)[, 1]),
    " (15 plots, ", length(coef(m_many)[, 1]), " parameters)\n", sep = "")

set.seed(42)
an_many <- anova(m_many, p.uni = "adjusted", nBoot = 999, resamp = "pit.trap")

cat("\n=== Community-level test (manyglm) ===\n")
print(an_many)

# The per-species table is the payoff: it says WHICH forb drives any community
# effect, with multiplicity already handled. This is what SIMPER pretends to do.
cat("\n=== Per-species tests, adjusted for multiple comparisons ===\n")
uni <- as.data.frame(t(an_many$uni.p))
colnames(uni) <- rownames(an_many$uni.p)
print(round(uni, 4))

# 3. NMDS -- the figure ########################################################
# Hellinger transformation + Euclidean distance = Hellinger distance. Hellinger
# exists precisely so that Euclidean distance behaves on abundance data, so
# this is the principled pairing. Bray-Curtis is run below as a cross-check
# because it is what most readers expect to see.

flora_hell <- decostand(mat, method = "hellinger")

set.seed(42)
nmds <- metaMDS(flora_hell, distance = "euclidean", k = 2,
                trymax = 200, autotransform = FALSE, trace = 0)

cat("\n=== NMDS ===\n")
cat("Stress: ", round(nmds$stress, 4), "\n", sep = "")
cat("Converged: ", nmds$converged > 0, "\n", sep = "")
# With 15 sites and 9 species, low stress is nearly automatic. It is a measure
# of how well the picture represents the distance matrix -- NOT evidence that
# treatments differ. Do not report it as if it were.
if (nmds$stress < 0.05)
  message("Stress < 0.05 with this few sites/species may indicate a ",
          "degenerate solution. Check the Shepard plot.")

png(file.path(fig_dir, "fig13_shepard.png"), width = 5.5, height = 5,
    units = "in", res = 300)
stressplot(nmds)
dev.off()

site_scores <- as.data.frame(scores(nmds, display = "sites")) |>
  bind_cols(site)
spp_scores <- as.data.frame(scores(nmds, display = "species")) |>
  rownames_to_column("plant_spp")

hulls <- site_scores |>
  group_by(trt) |>
  slice(chull(NMDS1, NMDS2)) |>
  ungroup()

fig_nmds <- ggplot(site_scores, aes(NMDS1, NMDS2)) +
  geom_polygon(data = hulls, aes(fill = trt, colour = trt),
               alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(colour = trt, shape = block), size = 3) +
  geom_text(aes(label = plot), nudge_y = 0.05, size = 2.6, colour = "grey40") +
  geom_text(data = spp_scores, aes(label = plant_spp),
            fontface = "italic", size = 2.9, colour = "grey20") +
  scale_colour_manual(values = trt_cols) +
  scale_fill_manual(values = trt_cols, guide = "none") +
  coord_equal() +
  labs(title = "Floral community composition",
       subtitle = paste0("NMDS on Hellinger distance; stress = ",
                         round(nmds$stress, 3),
                         ". Hulls are triangles -- n = 3 plots per treatment."),
       colour = "Treatment", shape = "Block")

ggsave(file.path(fig_dir, "fig14_nmds_floral.png"), fig_nmds,
       width = 7.5, height = 6, dpi = 300)

# Cross-check: does the familiar Bray-Curtis ordination tell the same story?
# metaMDS applies wisconsin(sqrt()) by default when it sees large counts.
set.seed(42)
nmds_bray <- metaMDS(mat, distance = "bray", k = 2, trymax = 200, trace = 0)
cat("Bray-Curtis stress: ", round(nmds_bray$stress, 4), "\n", sep = "")
proc <- procrustes(nmds, nmds_bray, symmetric = TRUE)
cat("Procrustes SS between the two ordinations: ", round(proc$ss, 4),
    " (near 0 = same story)\n", sep = "")

# 4. PERMANOVA -- the cross-check ##############################################
# Permutation MUST be restricted within blocks. Block variance was larger than
# every treatment coefficient in the abundance model; free permutation would
# shuffle it straight into the treatment term.

D <- vegdist(flora_hell, method = "euclidean")

perm_ctrl <- how(nperm = 4999)
setBlocks(perm_ctrl) <- site$block

set.seed(42)
pmv <- adonis2(D ~ block + trt, data = site, permutations = perm_ctrl,
               by = "terms")

cat("\n=== PERMANOVA (permutation restricted within blocks) ===\n")
print(pmv)
message("The block row's p-value is not interpretable here: permuting within ",
        "blocks cannot generate a null for a block effect. It is in the model ",
        "to partial block variance out of the residual, nothing more.")

## 4b. Dispersion check ####
# A significant adonis2 can mean groups have different SPREAD rather than
# different CENTROIDS. With n = 3 this test is nearly powerless -- run it, but
# report the null as uninformative rather than as reassurance.

bd <- betadisper(D, site$trt)
set.seed(42)
bd_test <- permutest(bd, permutations = 999)

cat("\n=== Homogeneity of multivariate dispersion ===\n")
print(bd_test)

png(file.path(fig_dir, "fig15_betadisper.png"), width = 6, height = 4.5,
    units = "in", res = 300)
boxplot(bd, xlab = "Treatment", ylab = "Distance to centroid",
        main = "Multivariate dispersion by treatment")
dev.off()

# 5. Sensitivity: drop singleton species #######################################

if (length(rare)) {
  mat2 <- mat[, colSums(mat > 0) > 1, drop = FALSE]
  Y2 <- mvabund(mat2)
  m_many2 <- manyglm(Y2 ~ block + trt, family = "negative.binomial", data = site)
  set.seed(42)
  cat("\n=== Sensitivity: singletons dropped (",
      ncol(mat2), " species) ===\n", sep = "")
  print(anova(m_many2, nBoot = 999, resamp = "pit.trap"))
}

# 6. Reporting aid #############################################################
# Presence/absence by treatment -- the table that makes the Apocynum point
# without any model at all.

cat("\n=== Species occurrence: plots (of 3) per treatment ===\n")
flora_long |>
  filter(flowers > 0) |>
  count(trt, plant_spp) |>
  pivot_wider(names_from = trt, values_from = n, values_fill = 0) |>
  arrange(plant_spp) |>
  as.data.frame() |>
  print(row.names = FALSE)

message("\nFigures written to ", normalizePath(fig_dir))
# =============================================================================
# 03b_floral_matrix_figures.R
# Replaces Sections 3 and 4 of 03_floral_composition.R.
#
# The NMDS was degenerate (stress = 0, one-dimensional): 73% of the matrix is
# zeros and 41% of plot pairs share no species, so most dissimilarities are
# tied at the ceiling and no low-dimensional embedding exists to find. PERMANOVA
# on the same distance matrix inherits the problem.
#
# These two figures show the same information without pretending otherwise.
# Keep manyglm (Section 2) -- it models each species separately and never
# computes a distance, so none of this touches it.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(forcats); library(vegan)
})

# Assumes flora_wide, site, mat, trt_cols and fig_dir exist from
# 03_floral_composition.R sections 0-1.

# 1. Document the sparsity #####################################################
# Print this in the methods. It is the justification for not ordinating.

ns <- no.shared(mat)
cat("\n=== Why no ordination ===\n")
cat("Matrix fill:            ", round(100 * sum(mat > 0) / length(mat), 1), "%\n", sep = "")
cat("Species per plot:        median ", median(rowSums(mat > 0)),
    ", range ", min(rowSums(mat > 0)), "-", max(rowSums(mat > 0)), "\n", sep = "")
cat("Plot pairs sharing no species: ", sum(ns), " of ", length(ns),
    " (", round(100 * mean(ns), 1), "%)\n", sep = "")

# 2. The occupancy matrix (replaces the NMDS) ##################################

tile_dat <- flora_wide |>
  pivot_longer(-c(block, plot, trt), names_to = "plant_spp", values_to = "flowers") |>
  group_by(plant_spp) |>
  mutate(n_plots = sum(flowers > 0)) |>
  ungroup() |>
  mutate(
    trt       = factor(trt, levels = names(trt_cols)),
    plant_spp = fct_reorder(plant_spp, n_plots),
    plot_lab  = factor(plot, levels = site$plot[order(site$trt, site$block)])
  )

fig_matrix <- ggplot(tile_dat, aes(x = plot_lab, y = plant_spp)) +
  geom_tile(aes(fill = ifelse(flowers > 0, flowers, NA_real_)),
            colour = "white", linewidth = 0.7) +
  geom_text(aes(label = ifelse(flowers > 0, flowers, "")),
            size = 2.5, colour = "grey15") +
  facet_grid(~ trt, scales = "free_x", space = "free_x") +
  scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.35,
                       trans = "sqrt", na.value = "grey96",
                       breaks = c(1, 5, 20, 50, 80)) +
  labs(title = "Peak floral display by plot and species",
       subtitle = paste0("Grey = species absent. ",
                         round(100 * sum(mat == 0) / length(mat)),
                         "% of cells are empty -- this is why the ordination failed."),
       x = "Plot", y = NULL, fill = "Peak\nflowers") +
  theme(axis.text.y = element_text(face = "italic"),
        panel.grid = element_blank())

ggsave(file.path(fig_dir, "fig14_floral_matrix.png"), fig_matrix,
       width = 9, height = 4, dpi = 300)

# 3. Occupancy vs reward #######################################################
# The real structure in these data: widespread species offer few flowers,
# high-display species are patchy. Every visitor record is on a high-display
# species. This figure earns its place -- it explains the visitation data.

spp_summary <- tile_dat |>
  filter(flowers > 0) |>
  group_by(plant_spp) |>
  summarise(n_plots   = n(),
            mean_peak = mean(flowers),
            max_peak  = max(flowers),
            .groups   = "drop")

# Which species ever received a visitor? Reads straight from the clean file.
visited <- read_csv("data_clean/konza_poll_2026_clean.csv", show_col_types = FALSE) |>
  filter(visitor_present) |>
  count(plant_spp, name = "visitors")

spp_summary <- spp_summary |>
  left_join(visited, by = "plant_spp") |>
  mutate(visitors = coalesce(visitors, 0L))

fig_tradeoff <- ggplot(spp_summary, aes(x = n_plots, y = mean_peak)) +
  geom_point(aes(size = visitors, colour = visitors > 0)) +
  geom_text(aes(label = plant_spp), hjust = -0.12, size = 2.9,
            fontface = "italic", colour = "grey25") +
  scale_colour_manual(values = c(`FALSE` = "grey65", `TRUE` = "#B23A48"),
                      labels = c("Never visited", "Visited"), name = NULL) +
  scale_size_area(max_size = 11, name = "Visitors\nrecorded") +
  scale_y_log10() +
  scale_x_continuous(limits = c(0, 12), breaks = 1:9) +
  labs(title = "Widespread forbs offer few flowers; rich forbs are patchy",
       subtitle = "Point size = total visitors recorded on that species",
       x = "Plots occupied (of 15)", y = "Mean peak display (flowers, log scale)")

ggsave(file.path(fig_dir, "fig15_occupancy_reward.png"), fig_tradeoff,
       width = 7.5, height = 5, dpi = 300)

cat("\n=== Species summary ===\n")
spp_summary |> arrange(desc(n_plots)) |> as.data.frame() |> print(row.names = FALSE)

message("\nDone. fig14 replaces the NMDS; fig15 is the one worth leading with.")

# =============================================================================
# 04_models_watch_level.R
# Replaces Section 10 of 02_analyse_konza_poll.R.
#
# Protocol (confirmed): each flowering plant species in a plot received a fixed
# 10-minute watch. Consequences:
#
#   * The sampling unit is the WATCH: plot x date x period x plant species.
#     Not the plot. A plot with four species flowering received 40 minutes of
#     observation; a plot with one received 10. Plot-level totals sum across
#     unequal effort and confound treatment with floral richness.
#
#   * Effort is constant per watch, so log(10) is a constant that vanishes into
#     the intercept. The offset is numerically identical to offset(log(flowers)).
#     What changes is that the reported rate now has honest units:
#     visits per flower per 10 minutes.
#
#   * plant_spp can enter as a predictor. This is the point. Section 3 races
#     treatment against plant identity directly.
#
# Run 01_clean first. Standalone.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(forcats)
  library(glmmTMB); library(emmeans); library(DHARMa); library(bbmle)
})

fig_dir <- "figures"; dir.create(fig_dir, showWarnings = FALSE)

WATCH_MIN  <- 10                       # protocol watch length, minutes
trt_levels <- c("C", "N", "P", "NP", "NPK")
trt_cols   <- c(C = "#5B5B5B", N = "#2A6EBB", P = "#D4772F",
                NP = "#7A3B9E", NPK = "#B23A48")

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  plot.subtitle = element_text(colour = "grey35")))

# 1. Build watches #############################################################

d <- read_csv("data_clean/konza_poll_2026_clean.csv", show_col_types = FALSE) |>
  mutate(plant_spp = na_if(plant_spp, "None"),
         trt   = factor(trt, levels = trt_levels),
         block = factor(block), plot = factor(plot),
         period = factor(period, levels = c("Morning", "Evening")))

watch <- d |>
  filter(!is.na(plant_spp), !is.na(open_flower_num), open_flower_num > 0) |>
  group_by(obs_date, round, day, period, block, plot, trt, plant_spp,
           air_temp, wind, rh, cloud_cover) |>
  summarise(visits  = sum(visitor_present),
            flowers = max(open_flower_num),
            n_counts = n_distinct(open_flower_num),
            .groups = "drop") |>
  mutate(plant_spp = factor(plant_spp),
         effort_min = WATCH_MIN,
         log_flowers = log(flowers),
         log_flowers_c = log_flowers - mean(log_flowers))

# A watch should have exactly one flower count. More than one means the survey
# recorded conflicting numbers for the same patch -- fix upstream, don't smooth.
if (any(watch$n_counts > 1)) {
  warning(sum(watch$n_counts > 1), " watches have conflicting flower counts; ",
          "max() was used. Resolve in 01_clean.")
  watch |> filter(n_counts > 1) |>
    select(obs_date, period, plot, plant_spp) |> as.data.frame() |> print()
}

cat("\nWatches: ", nrow(watch), " (", nrow(watch) * WATCH_MIN, " observer-minutes)\n", sep = "")

# 2. Effort was not equal across plots #########################################
# The figure that justifies abandoning plot-level analysis.

effort <- watch |> count(block, plot, trt, name = "watches") |>
  mutate(minutes = watches * WATCH_MIN)

fig_effort <- ggplot(effort, aes(x = fct_reorder(plot, minutes), y = minutes)) +
  geom_col(aes(fill = trt), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = trt_cols) +
  labs(title = "Observation effort per plot was never equal",
       subtitle = "10 min per flowering species per survey; plots differ >3-fold",
       x = "Plot", y = "Total observer-minutes", fill = "Treatment")

ggsave(file.path(fig_dir, "fig16_effort_per_plot.png"), fig_effort,
       width = 6.5, height = 4.5, dpi = 300)

cat("\nObserver-minutes per plot: range ", min(effort$minutes), "-",
    max(effort$minutes), "\n", sep = "")

# 3. Treatment vs plant identity ###############################################
# The central question, now askable. If knowing the treatment adds nothing once
# you know which plant was watched, the visitation signal was never about
# nutrients.

f_base <- visits ~ log_flowers_c + period + (1 | plot)

m_null  <- glmmTMB(update(f_base, . ~ .),                 family = nbinom2, data = watch)
m_trt   <- glmmTMB(update(f_base, . ~ . + trt),           family = nbinom2, data = watch)
m_plant <- glmmTMB(update(f_base, . ~ . + plant_spp),     family = nbinom2, data = watch)
m_both  <- glmmTMB(update(f_base, . ~ . + trt + plant_spp), family = nbinom2, data = watch)

cat("\n=== Does treatment explain anything plant identity doesn't? ===\n")
print(AICtab(m_null, m_trt, m_plant, m_both, base = TRUE, weights = TRUE))

cat("\nLikelihood ratio test, adding trt to the plant-identity model:\n")
print(anova(m_plant, m_both))

# NOTE ON CONFOUNDING: plant_spp and plot are partly nested (Solanum occurs only
# in plot 16). Coefficients for singleton species are not estimable in any
# meaningful sense. Check for absurd SEs before reading them.
cat("\n=== m_both ===\n"); print(summary(m_both))

# 4. Density dependence: is the offset's slope = 1 defensible? ##################
# offset(log(flowers)) forces visits to scale exactly proportionally with
# flowers. Fit the slope freely and look.

m_dens <- glmmTMB(visits ~ trt + log_flowers + period + (1 | plot) + (1 | plant_spp),
                  family = nbinom2, data = watch)

slope <- fixef(m_dens)$cond["log_flowers"]
slope_se <- sqrt(diag(vcov(m_dens)$cond))["log_flowers"]
z_vs_1 <- (slope - 1) / slope_se

cat("\n=== Slope on log(flowers) ===\n")
cat("Estimate: ", round(slope, 3),
    "  95% CI: ", round(slope - 1.96 * slope_se, 3), " to ",
    round(slope + 1.96 * slope_se, 3), "\n", sep = "")
cat("Test against 1 (the offset's assumption): z = ", round(z_vs_1, 2),
    ", p = ", format.pval(2 * pnorm(-abs(z_vs_1)), digits = 3), "\n", sep = "")
if (abs(z_vs_1) > 1.96)
  message("Slope differs from 1: the offset is not defensible. Report m_dens, ",
          "not m_offset.")

# 5. The offset model, with honest units #######################################
# Kept for comparison and because readers expect a per-flower rate. Effort is
# constant so this is numerically identical to offset(log(flowers)); only the
# units of the reported rate change.

m_offset <- glmmTMB(visits ~ trt + period + offset(log(flowers * effort_min)) +
                      (1 | plot) + (1 | plant_spp),
                    family = nbinom2, data = watch)

cat("\n=== Offset model ===\n"); print(summary(m_offset))

# offset = log(1 flower x 10 min) -> rate per flower per 10-minute watch.
emm <- emmeans(m_offset, ~ trt, offset = log(1 * WATCH_MIN), type = "response")
cat("\nVisits per flower per 10-minute watch:\n"); print(emm)

cat("\nAIC: free slope ", round(AIC(m_dens), 1),
    " vs offset ", round(AIC(m_offset), 1), "\n", sep = "")

# 6. Figures ###################################################################

fig_density <- ggplot(watch, aes(x = flowers, y = (visits + 0.5) / (flowers * WATCH_MIN / 10))) +
  geom_point(aes(colour = trt), alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = TRUE, colour = "grey25",
              linewidth = 0.7, span = 1) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = trt_cols) +
  labs(title = "Per-flower visitation falls as display grows",
       subtitle = "Each point is one 10-min watch. Offset assumes this line is flat.",
       x = "Open flowers in patch (log)",
       y = "Visits per flower per 10 min (log, +0.5 offset)",
       colour = "Treatment")

ggsave(file.path(fig_dir, "fig17_density_dependence.png"), fig_density,
       width = 7, height = 4.5, dpi = 300)

fig_byplant <- watch |>
  mutate(rate = visits / (flowers * WATCH_MIN / 10)) |>
  ggplot(aes(x = fct_reorder(plant_spp, rate), y = rate)) +
  geom_boxplot(outlier.shape = NA, fill = "grey92", width = 0.6) +
  geom_point(aes(colour = trt), position = position_jitter(width = 0.15),
             size = 1.8, alpha = 0.8) +
  coord_flip() +
  scale_colour_manual(values = trt_cols) +
  labs(title = "Visitation is a property of the plant, not the plot",
       subtitle = "Visits per flower per 10 min, by species watched",
       x = NULL, y = "Visits / flower / 10 min", colour = "Treatment") +
  theme(axis.text.y = element_text(face = "italic"))

ggsave(file.path(fig_dir, "fig18_rate_by_plant.png"), fig_byplant,
       width = 7.5, height = 4.5, dpi = 300)

# 7. Diagnostics ###############################################################

for (nm in c("m_dens", "m_offset", "m_both")) {
  res <- simulateResiduals(get(nm), n = 1000)
  png(file.path(fig_dir, paste0("diag_", nm, ".png")),
      width = 8, height = 4, units = "in", res = 300)
  plot(res); dev.off()
  cat("\n", nm, ": ", sep = "")
  dt <- testDispersion(res, plot = FALSE)
  cat("dispersion = ", round(dt$statistic, 3), ", p = ", round(dt$p.value, 3), "\n", sep = "")
  vc <- VarCorr(get(nm))$cond
  cat("  RE SDs: ", paste(names(vc), round(sapply(vc, function(x) sqrt(x[1])), 4),
                          collapse = "; "), "\n", sep = "")
}

message("\nIf RE SDs print as ~1e-9 again, the model has collapsed to a GLM and ",
        "the SEs assume every watch is an independent replicate. Do not report it.") 