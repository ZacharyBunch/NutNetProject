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