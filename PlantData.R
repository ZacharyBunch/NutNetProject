# ============================================================
# Konza NutNet 2026 - paired analysis + figure workflow.
# Each analysis produces one matching figure annotated with its
# own result. Two datasets analyzed independently.
#   A1 PERMANOVA        -> NMDS ordination
#   A2 prevalence       -> consistent-species bar
#   A3 diversity ANOVA  -> richness by treatment
#   B1 visitation GLMM  -> model-predicted per-flower rate
# ============================================================

pkgs <- c("readxl","janitor","dplyr","tidyr","tibble","stringr","forcats",
          "vegan","ggplot2","glmmTMB","emmeans","car")
need <- unique(c(pkgs, "MASS"))                 # MASS used namespace-qualified only
to_install <- need[!need %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))   # MASS deliberately NOT attached

# ---- config ----
bloom_path <- "KonzaBloomingCompJune2026.xlsx"
poll_path  <- "KonzaPollObs2026Summer.xlsx"
trt_lv     <- c("C","N","P","NP","NPK")            # C = reference
min_plots_consistent <- 3; min_trt_consistent <- 2
ok_trt <- c(C="#999999", N="#E69F00", P="#56B4E9", NP="#009E73", NPK="#D55E00")
theme_set(theme_minimal(base_size = 12))
set.seed(1)

resolve_col <- function(df, pat){
  h <- names(df)[str_detect(names(df), regex(pat, ignore_case = TRUE))]
  if(!length(h)) stop("no column matched: ", pat,
                      " | have: ", paste(names(df), collapse=", "))
  h[1]
}
save_fig <- function(p, f, w=7, h=5){
  if(dir.exists("figures")) ggsave(file.path("figures", f), p, width=w, height=h, dpi=300)
  print(p)
}

# ============================================================
# PART A - BLOOMING CENSUS
# ============================================================
br <- read_excel(bloom_path) %>% clean_names()
bloom <- tibble(
  block   = factor(br[[resolve_col(br,"block")]]),
  trt     = factor(br[[resolve_col(br,"treat|^trt")]], trt_lv),
  plot    = factor(br[[resolve_col(br,"^plot")]]),
  species = str_squish(br[[resolve_col(br,"spp|species")]]),
  blooms  = suppressWarnings(as.numeric(br[[resolve_col(br,"bloom")]])),
  indiv   = suppressWarnings(as.numeric(br[[resolve_col(br,"individual|indiv")]]))
) %>% filter(!is.na(species), species!="", !is.na(blooms))

# plot x species community matrix (individuals = plant community)
w <- bloom %>% group_by(plot, trt, block, species) %>%
  summarise(val = sum(indiv, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=species, values_from=val, values_fill=0)
meta <- w %>% select(plot, trt, block) %>% as.data.frame()
mat  <- as.matrix(w %>% select(-plot,-trt,-block)); rownames(mat) <- as.character(w$plot)
keep <- rowSums(mat) > 0; mat <- mat[keep,,drop=FALSE]; meta <- meta[keep,]

# ------------------------------------------------------------
# ANALYSIS A1: community composition ~ treatment (PERMANOVA)
# FIGURE   A1: NMDS ordination annotated with R2 and p
# ------------------------------------------------------------
perm <- how(blocks = meta$block, nperm = 999)
pmv  <- adonis2(mat ~ trt, data = meta, method = "bray", permutations = perm)
a1_r2 <- pmv["trt","R2"]; a1_p <- pmv["trt","Pr(>F)"]
disp  <- betadisper(vegdist(mat,"bray"), meta$trt)
disp_p <- permutest(disp, permutations = 999)$tab[1,"Pr(>F)"]

nmds <- metaMDS(mat, distance="bray", k=2, trymax=100, trace=0)
scr  <- bind_cols(as.data.frame(scores(nmds, display="sites")), meta)
figA1 <- ggplot(scr, aes(NMDS1, NMDS2, color = trt)) +
  geom_point(size = 3) +
  stat_ellipse(type = "t", linewidth = .4, na.rm = TRUE) +
  scale_color_manual(values = ok_trt, name = "treatment") +
  labs(title = "A1. Community composition between treatments (PERMANOVA)",
       subtitle = sprintf("Bray-Curtis NMDS  |  stress %.3f  |  treatment R2 = %.2f, p = %.3f  |  dispersion p = %.3f",
                          nmds$stress, a1_r2, a1_p, disp_p))
save_fig(figA1, "A1_nmds_permanova.png", 7, 5.2)

# ------------------------------------------------------------
# ANALYSIS A2: species prevalence (consistent-species ID)
# FIGURE   A2: prevalence bar, threshold line, consistent flagged
# ------------------------------------------------------------
prev <- bloom %>% group_by(species) %>%
  summarise(n_plots=n_distinct(plot), n_trt=n_distinct(trt),
            n_block=n_distinct(block), tot=sum(blooms), .groups="drop") %>%
  mutate(consistent = n_plots >= min_plots_consistent & n_trt >= min_trt_consistent) %>%
  arrange(n_plots, tot)
consistent_species <- prev %>% filter(consistent) %>% pull(species) %>% as.character()

figA2 <- ggplot(prev, aes(reorder(species, n_plots), n_plots, fill = consistent)) +
  geom_col(color = "black", linewidth = .3) +
  geom_text(aes(label = sprintf("%d plt / %d trt / %d blk", n_plots, n_trt, n_block)),
            hjust = -.05, size = 3) +
  coord_flip(clip = "off") + expand_limits(y = max(prev$n_plots) + 3) +
  scale_fill_manual(values = c(`TRUE`="#009E73", `FALSE`="#BBBBBB"),
                    labels = c("below threshold","consistent"), name = NULL) +
  labs(x = NULL, y = "plots occupied",
       title = "A2. Consistent species across plots")
save_fig(figA2, "A2_prevalence.png", 8, 4.6)

# ------------------------------------------------------------
# ANALYSIS A3: richness ~ treatment + block (ANOVA)
# FIGURE   A3: richness by treatment annotated with F and p
# ------------------------------------------------------------
rich <- bloom %>% group_by(plot, trt, block) %>%
  summarise(richness = n_distinct(species), .groups="drop")
a3 <- summary(aov(richness ~ trt + block, data = rich))[[1]]
rn <- trimws(rownames(a3)); i <- which(rn == "trt")
a3_F <- a3[i,"F value"]; a3_p <- a3[i,"Pr(>F)"]

figA3 <- ggplot(rich, aes(trt, richness, color = trt)) +
  geom_jitter(width = .12, height = 0, size = 3) +
  stat_summary(fun = mean, geom = "crossbar", width = .4, color = "black", linewidth = .4) +
  scale_color_manual(values = ok_trt, guide = "none") +
  labs(x = "nutrient treatment", y = "blooming species / plot",
       title = "A3. Blooming richness by treatment (bar = mean)",
       subtitle = sprintf("ANOVA (treatment | block): F = %.2f, p = %.3f", a3_F, a3_p))
save_fig(figA3, "A3_richness.png", 6.5, 4.6)

# ------------------------------------------------------------
# ANALYSIS A4: total blooming individuals per plot ~ treatment
#   (all species pooled; count -> Poisson GLM, NB fallback if
#   overdispersed; block as covariate)
# FIGURE   A4: per-plot totals by treatment, mean, annotated p
# NOTE: only plots with >= 1 blooming record are present here;
#   plots with zero blooming individuals are absent. For structural
#   zeros, left_join a full 15-plot roster and set NA -> 0 before
#   modeling.
# ------------------------------------------------------------
indiv_tot <- bloom %>% group_by(plot, trt, block) %>%
  summarise(individuals = sum(indiv, na.rm = TRUE), .groups = "drop")

m_ind <- glm(individuals ~ trt + block, family = poisson, data = indiv_tot)
disp  <- sum(residuals(m_ind, "pearson")^2) / df.residual(m_ind)
if (is.finite(disp) && disp > 1.5)
  m_ind <- MASS::glm.nb(individuals ~ trt + block, data = indiv_tot)

m_ind0 <- update(m_ind, . ~ . - trt)            # LRT for treatment (family-agnostic)
a4_p <- pchisq(2 * (as.numeric(logLik(m_ind)) - as.numeric(logLik(m_ind0))),
               df = length(coef(m_ind)) - length(coef(m_ind0)), lower.tail = FALSE)
a4_fam <- if (inherits(m_ind, "negbin")) "negative-binomial" else "Poisson"

figA4 <- ggplot(indiv_tot, aes(trt, individuals, color = trt)) +
  geom_jitter(width = .12, height = 0, size = 3) +
  stat_summary(fun = mean, geom = "crossbar", width = .4, color = "black", linewidth = .4) +
  scale_color_manual(values = ok_trt, guide = "none") +
  labs(x = "nutrient treatment", y = "blooming individuals / plot (all species)",
       title = "A4. Blooming individuals per plot by treatment (bar = mean)",
       subtitle = sprintf("%s GLM (treatment | block): treatment p = %.3f", a4_fam, a4_p))
save_fig(figA4, "A4_individuals.png", 6.5, 4.6)

# ============================================================
# PART B - POLLINATOR WATCHES
# ============================================================
pr <- read_excel(poll_path) %>% clean_names()
poll <- tibble(
  plot    = factor(pr[[resolve_col(pr,"^plot")]]),
  block   = factor(pr[[resolve_col(pr,"block")]]),
  trt     = factor(pr[[resolve_col(pr,"^trt|treat")]], trt_lv),
  date    = as.Date(pr[[resolve_col(pr,"date")]]),
  round   = pr[[resolve_col(pr,"round")]],
  day     = as.character(pr[[resolve_col(pr,"morning|evening")]]),
  plant   = str_squish(as.character(pr[[resolve_col(pr,"plant_?spp|plant_species")]])),
  vis     = str_squish(as.character(pr[[resolve_col(pr,"vistor|visitor")]])),
  flowers = suppressWarnings(as.numeric(pr[[resolve_col(pr,"open_?flower_?num|flower_num")]]))
) %>% mutate(plant = na_if(plant,""), vis = na_if(vis,""),
             is_visit = !is.na(vis) & !str_detect(tolower(vis), "^none$"))

# plant-watch (adjust key if a unique watch/observation ID exists)
watch <- poll %>% filter(!is.na(plant), tolower(plant) != "none") %>%
  group_by(plot, block, trt, date, round, day, plant) %>%
  summarise(flowers = suppressWarnings(max(flowers, na.rm=TRUE)),
            visits  = sum(is_visit), .groups="drop") %>%
  filter(is.finite(flowers), flowers > 0)

# ------------------------------------------------------------
# ANALYSIS A2-obs: plant prevalence from the OBSERVATION data.
#   PlantSpp presence per plot. Different sampling frame than the
#   census: reflects plants selected for timed watches, not a
#   systematic floral count, so read as "consistently watched".
# FIGURE   A2-obs: consistent-species bar (obs-based)
# ------------------------------------------------------------
presence_obs <- poll %>%
  filter(!is.na(plant), tolower(plant) != "none") %>%
  distinct(plot, block, trt, plant)

prev_obs <- presence_obs %>% group_by(plant) %>%
  summarise(n_plots = n_distinct(plot), n_trt = n_distinct(trt),
            n_block = n_distinct(block), .groups = "drop") %>%
  mutate(consistent = n_plots >= min_plots_consistent & n_trt >= min_trt_consistent) %>%
  arrange(n_plots)
consistent_species_obs <- prev_obs %>% filter(consistent) %>% pull(plant) %>% as.character()

figA2_obs <- ggplot(prev_obs, aes(reorder(plant, n_plots), n_plots, fill = consistent)) +
  geom_col(color = "black", linewidth = .3) +
  geom_text(aes(label = sprintf("%d plt / %d trt / %d blk", n_plots, n_trt, n_block)),
            hjust = -.05, size = 3) +
  coord_flip(clip = "off") + expand_limits(y = max(prev_obs$n_plots) + 3) +
  scale_fill_manual(values = c(`TRUE`="#009E73", `FALSE`="#BBBBBB"),
                    labels = c("below threshold","consistent"), name = NULL) +
  labs(x = NULL, y = "plots watched",
       title = "A2-obs. Consistent watched plants (observation data)")
save_fig(figA2_obs, "A2obs_prevalence.png", 8, 4.6)

# ------------------------------------------------------------
# ANALYSIS A3-obs: watched-plant richness ~ treatment + block
# FIGURE   A3-obs: richness by treatment annotated with F and p
# ------------------------------------------------------------
rich_obs <- presence_obs %>% group_by(plot, trt, block) %>%
  summarise(richness = n_distinct(plant), .groups = "drop")
a3o <- summary(aov(richness ~ trt + block, data = rich_obs))[[1]]
rn_o <- trimws(rownames(a3o)); io <- which(rn_o == "trt")
a3o_F <- a3o[io,"F value"]; a3o_p <- a3o[io,"Pr(>F)"]

figA3_obs <- ggplot(rich_obs, aes(trt, richness, color = trt)) +
  geom_jitter(width = .12, height = 0, size = 3) +
  stat_summary(fun = mean, geom = "crossbar", width = .4, color = "black", linewidth = .4) +
  scale_color_manual(values = ok_trt, guide = "none") +
  labs(x = "nutrient treatment", y = "watched plant species / plot",
       title = "A3-obs. Watched-plant richness by treatment (bar = mean)",
       subtitle = sprintf("ANOVA (treatment | block): F = %.2f, p = %.3f", a3o_F, a3o_p))
save_fig(figA3_obs, "A3obs_richness.png", 6.5, 4.6)

# focal = consistent plants from A2, aggregated to plot x species
plot_sp <- watch %>% filter(plant %in% consistent_species) %>%
  group_by(trt, block, plot, plant) %>%
  summarise(visits = sum(visits), flowers = sum(flowers), .groups="drop") %>%
  filter(flowers > 0) %>% mutate(vpf = visits / flowers)

# ------------------------------------------------------------
# ANALYSIS B1: visits ~ treatment + plant, offset(log flowers)
#              (per-flower rate; nbinom2 GLMM, block RE)
# FIGURE   B1: model emmeans per-flower rate by treatment (CIs)
#              over observed points, annotated with treatment test
# ------------------------------------------------------------
m <- tryCatch(
  glmmTMB(visits ~ trt + plant + offset(log(flowers)) + (1|block),
          family = nbinom2, data = plot_sp),
  error = function(e){ message("GLMM error: ", conditionMessage(e)); NULL })
if (is.null(m) || (inherits(m,"glmmTMB") && isFALSE(m$sdr$pdHess))) {
  message("GLMM unstable; Poisson GLM fallback (no random effect).")
  m <- glm(visits ~ trt + plant + offset(log(flowers)),
           family = poisson, data = plot_sp)
}
b1_p <- if (inherits(m,"glmmTMB")) car::Anova(m, type=2)["trt","Pr(>Chisq)"] else
  anova(update(m, .~. - trt), m, test="Chisq")$`Pr(>Chi)`[2]

emm <- emmeans(m, ~ trt, offset = 0, type = "response")   # offset 0 => per single flower
ed  <- as.data.frame(emm)
rcol <- if ("response" %in% names(ed)) "response" else "rate"
lcl  <- grep("LCL", names(ed), value=TRUE)[1]; ucl <- grep("UCL", names(ed), value=TRUE)[1]

figB1 <- ggplot() +
  geom_jitter(data = plot_sp, aes(trt, vpf, color = trt),
              width = .12, height = 0, size = 2, alpha = .5) +
  geom_point(data = ed, aes(trt, .data[[rcol]]), size = 3, color = "black") +
  geom_errorbar(data = ed, aes(trt, ymin = .data[[lcl]], ymax = .data[[ucl]]),
                width = .2, color = "black") +
  scale_color_manual(values = ok_trt, guide = "none") +
  labs(x = "nutrient treatment", y = "visits per open flower",
       title = "B1. Per-flower visitation to consistent plants by treatment",
       subtitle = sprintf("points = observed plot x species; black = model estimate +/- 95%% CI; treatment p = %.3f",
                          b1_p))
save_fig(figB1, "B1_visitation_emmeans.png", 7, 5)

# ---- console results ----
cat("\n== A1 PERMANOVA ==\n");            print(pmv)
cat("\n== A2 consistent species ==\n");   print(rev(consistent_species))
cat("\n== A3 richness ANOVA ==\n");        print(a3)
cat("\n== A4 blooming individuals GLM ==\n"); cat(a4_fam, "| treatment p =", signif(a4_p,3), "\n"); print(summary(m_ind))
cat("\n== A2-obs consistent watched plants ==\n"); print(rev(consistent_species_obs))
cat("\n== A3-obs richness ANOVA ==\n");     print(a3o)
cat("\n== B1 treatment test (p) ==\n");    print(b1_p); print(summary(emm))