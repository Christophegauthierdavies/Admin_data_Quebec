library(data.table)
library(haven)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(openxlsx)

# =====================================================================
# 1) LECTURE DES DONNÉES
# =====================================================================

# ramq_fipa  <- read_sas("CHEMIN/ramq_fipa.sas7bdat")
# meq_frq    <- fread("meq_pps_freq_40255.csv")
# setDT(meq_frq)
# meq_r_appre <- read_sas("CHEMIN/meq_pps_reslt_appre_40255.sas7bdat")

# =====================================================================
# 2) PRÉPARER meq_frq
# =====================================================================

meq_frq_df <- as.data.frame(meq_frq) %>%
  mutate(
    NOINDIV         = as.character(NOINDIV),
    REGRP_EHDAA_chr = as.character(CD_REGRP_EHDAA)
  ) %>%
  group_by(NOINDIV, AN_SCOLR) %>%
  summarise(
    cr = as.integer(any(REGRP_EHDAA_chr == "06")),
    .groups = "drop"
  )

# =====================================================================
# 3) ASSIGNER LES COHORTES VIA ramq_fipa
# =====================================================================

ramq_cohortes <- ramq_fipa %>%
  mutate(
    NOINDIV            = as.character(NOINDIV),
    NAIS_AAAAMMJJ_FIPA = str_squish(NAIS_AAAAMMJJ_FIPA),
    date_naissance     = ymd(NAIS_AAAAMMJJ_FIPA),
    cohorte_scolaire = case_when(
      date_naissance >= ymd("2006-01-01") & date_naissance <= ymd("2006-09-30") ~ "2005-2006",
      date_naissance >= ymd("2006-10-01") & date_naissance <= ymd("2007-09-30") ~ "2006-2007",
      date_naissance >= ymd("2007-10-01") & date_naissance <= ymd("2008-09-30") ~ "2007-2008",
      TRUE ~ NA_character_
    ),
    sec4_theorique = case_when(
      cohorte_scolaire == "2005-2006" ~ "2021-2022",
      cohorte_scolaire == "2006-2007" ~ "2022-2023",
      cohorte_scolaire == "2007-2008" ~ "2023-2024"
    ),
    sec4_compact = case_when(
      cohorte_scolaire == "2005-2006" ~ "20212022",
      cohorte_scolaire == "2006-2007" ~ "20222023",
      cohorte_scolaire == "2007-2008" ~ "20232024"
    ),
    sec4_prev_year = case_when(
      cohorte_scolaire == "2005-2006" ~ "20202021",
      cohorte_scolaire == "2006-2007" ~ "20212022",
      cohorte_scolaire == "2007-2008" ~ "20222023"
    ),
    sec5_theorique = case_when(
      cohorte_scolaire == "2005-2006" ~ "2022-2023",
      cohorte_scolaire == "2006-2007" ~ "2023-2024",
      cohorte_scolaire == "2007-2008" ~ "2024-2025"
    ),
    sec5_compact = case_when(
      cohorte_scolaire == "2005-2006" ~ "20222023",
      cohorte_scolaire == "2006-2007" ~ "20232024",
      cohorte_scolaire == "2007-2008" ~ "20242025"
    ),
    sec5_prev_year = case_when(
      cohorte_scolaire == "2005-2006" ~ "20212022",
      cohorte_scolaire == "2006-2007" ~ "20222023",
      cohorte_scolaire == "2007-2008" ~ "20232024"
    )
  ) %>%
  filter(!is.na(cohorte_scolaire)) %>%
  distinct(NOINDIV, SEXE, cohorte_scolaire, sec4_theorique, sec4_compact, sec4_prev_year,
           sec5_theorique, sec5_compact, sec5_prev_year)

# =====================================================================
# 4) CONSTRUIRE LES DÉNOMINATEURS (SEC 4 ET SEC 5)
# =====================================================================

bd_sec4 <- meq_frq_df %>%
  inner_join(ramq_cohortes, by = "NOINDIV") %>%
  mutate(matches_prev_year = (AN_SCOLR == sec4_prev_year)) %>%
  filter(matches_prev_year == TRUE) %>%
  distinct(NOINDIV, .keep_all = TRUE) %>%
  select(NOINDIV, SEXE, cohorte_scolaire, sec4_theorique, sec4_compact, cr) %>%
  mutate(groupe_cr_sec4 = if_else(cr == 1L, "Centre de réadaptation", "Population"))

bd_sec5 <- meq_frq_df %>%
  inner_join(ramq_cohortes, by = "NOINDIV") %>%
  mutate(matches_prev_year = (AN_SCOLR == sec5_prev_year)) %>%
  filter(matches_prev_year == TRUE) %>%
  distinct(NOINDIV, .keep_all = TRUE) %>%
  select(NOINDIV, SEXE, cohorte_scolaire, sec5_theorique, sec5_compact, cr) %>%
  mutate(groupe_cr_sec5 = if_else(cr == 1L, "Centre de réadaptation", "Population"))

# =====================================================================
# 5) FONCTION D'ARRONDISSEMENT CONTRÔLÉ (CRR - Base 5)
# =====================================================================

CR_round <- function(x, base = 5) {
  lower <- floor(x / base) * base
  prob_up <- (x - lower) / base
  rounded <- ifelse(runif(length(x), min = 0, max = 1) < prob_up,
                    lower + base, lower)
  rounded <- ifelse(rounded == 0, base, rounded)
  return(rounded)
}

# =====================================================================
# 5b) FONCTION POUR CALCULER IC 95% (Wilson Score)
# =====================================================================

calculer_ic_wilson <- function(successes, trials) {
  # Wilson score interval pour proportions binomiales
  if (trials == 0) return(list(p = 0, ci_low = 0, ci_high = 0))
  
  p <- successes / trials
  z <- 1.96  # 95% confidence
  
  denominator <- 1 + (z^2 / trials)
  centre <- (p + (z^2 / (2 * trials))) / denominator
  margin <- z * sqrt((p * (1 - p) / trials) + (z^2 / (4 * trials^2))) / denominator
  
  ci_low <- max(0, centre - margin)
  ci_high <- min(1, centre + margin)
  
  return(list(p = p * 100, ci_low = ci_low * 100, ci_high = ci_high * 100))
}

# =====================================================================
# 6) DATES LIMITES POUR FILTRAGE
# =====================================================================

date_limites_sec4 <- tibble(
  sec4_theorique = c("2021-2022", "2022-2023", "2023-2024"),
  date_limite = ymd(c("2022-12-31", "2023-12-31", "2024-12-31"))
)

date_limites_sec5 <- tibble(
  sec5_theorique = c("2022-2023", "2023-2024", "2024-2025"),
  date_limite = ymd(c("2023-12-31", "2024-12-31", "2025-12-31"))
)

# =====================================================================
# FONCTIONS DE GRAPHIQUES AVEC IC
# =====================================================================

# Figure combinée (3 panneaux) - non stratifiée - AVEC IC
faire_plot_combinee_3_ic <- function(tab, titre, fichier) {
  tab %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                  position = position_dodge(width = 0.65),
                  width = 0.3, linewidth = 0.5, color = "black") +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_wrap(~epreuve, nrow = 1) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 19, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 14, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 14, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 11, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 11),
      legend.text        = element_text(size = 11),
      legend.position    = "bottom",
      legend.margin      = margin(t = 10),
      strip.text         = element_text(size = 12, face = "bold", margin = margin(b = 5)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 15, height = 6.5, dpi = 300, bg = "white")
}

# Figure combinée (2 panneaux) - non stratifiée - AVEC IC
faire_plot_combinee_2_ic <- function(tab, titre, fichier) {
  tab %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                  position = position_dodge(width = 0.65),
                  width = 0.3, linewidth = 0.5, color = "black") +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_wrap(~epreuve, nrow = 1) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 19, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 14, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 14, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 11, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 11),
      legend.text        = element_text(size = 11),
      legend.position    = "bottom",
      legend.margin      = margin(t = 10),
      strip.text         = element_text(size = 12, face = "bold", margin = margin(b = 5)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 11, height = 6.5, dpi = 300, bg = "white")
}

# Figure combinée (3 panneaux) - stratifiée par SEXE - AVEC IC
faire_plot_combinee_3_sexe_ic <- function(tab, titre, fichier) {
  tab %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                  position = position_dodge(width = 0.65),
                  width = 0.25, linewidth = 0.4, color = "black") +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_grid(SEXE ~ epreuve, labeller = labeller(SEXE = c("1" = "Garçons", "2" = "Filles"))) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 13, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 13, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 10, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 10),
      legend.text        = element_text(size = 10),
      legend.position    = "bottom",
      legend.margin      = margin(t = 8),
      strip.text         = element_text(size = 11, face = "bold", margin = margin(b = 4, t = 4)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 16, height = 7.5, dpi = 300, bg = "white")
}

# Figure combinée (2 panneaux) - stratifiée par SEXE - AVEC IC
faire_plot_combinee_2_sexe_ic <- function(tab, titre, fichier) {
  tab %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low, ymax = ic_high),
                  position = position_dodge(width = 0.65),
                  width = 0.25, linewidth = 0.4, color = "black") +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_grid(SEXE ~ epreuve, labeller = labeller(SEXE = c("1" = "Garçons", "2" = "Filles"))) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 13, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 13, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 10, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 10),
      legend.text        = element_text(size = 10),
      legend.position    = "bottom",
      legend.margin      = margin(t = 8),
      strip.text         = element_text(size = 11, face = "bold", margin = margin(b = 4, t = 4)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 12, height = 7.5, dpi = 300, bg = "white")
}

# =====================================================================
# VERSIONS AVEC IC SEULEMENT POUR CENTRE DE RÉADAPTATION
# =====================================================================

# Figure combinée (3 panneaux) - IC seulement CR
faire_plot_combinee_3_ic_cr <- function(tab, titre, fichier) {
  # Préparer données avec marqueur pour IC
  tab_ic <- tab %>%
    mutate(affiche_ic = groupe_cr == "Centre de réadaptation",
           ic_low_display = ifelse(affiche_ic, ic_low, NA),
           ic_high_display = ifelse(affiche_ic, ic_high, NA))
  
  tab_ic %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low_display, ymax = ic_high_display),
                  position = position_dodge(width = 0.65),
                  width = 0.3, linewidth = 0.5, color = "black", na.rm = TRUE) +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_wrap(~epreuve, nrow = 1) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 19, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 14, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 14, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 11, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 11),
      legend.text        = element_text(size = 11),
      legend.position    = "bottom",
      legend.margin      = margin(t = 10),
      strip.text         = element_text(size = 12, face = "bold", margin = margin(b = 5)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 15, height = 6.5, dpi = 300, bg = "white")
}

# Figure combinée (2 panneaux) - IC seulement CR
faire_plot_combinee_2_ic_cr <- function(tab, titre, fichier) {
  tab_ic <- tab %>%
    mutate(affiche_ic = groupe_cr == "Centre de réadaptation",
           ic_low_display = ifelse(affiche_ic, ic_low, NA),
           ic_high_display = ifelse(affiche_ic, ic_high, NA))
  
  tab_ic %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low_display, ymax = ic_high_display),
                  position = position_dodge(width = 0.65),
                  width = 0.3, linewidth = 0.5, color = "black", na.rm = TRUE) +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_wrap(~epreuve, nrow = 1) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 19, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 14, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 14, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 11, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 11),
      legend.text        = element_text(size = 11),
      legend.position    = "bottom",
      legend.margin      = margin(t = 10),
      strip.text         = element_text(size = 12, face = "bold", margin = margin(b = 5)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 11, height = 6.5, dpi = 300, bg = "white")
}

# Figure combinée (3 panneaux) - stratifiée par SEXE - IC seulement CR
faire_plot_combinee_3_sexe_ic_cr <- function(tab, titre, fichier) {
  tab_ic <- tab %>%
    mutate(affiche_ic = groupe_cr == "Centre de réadaptation",
           ic_low_display = ifelse(affiche_ic, ic_low, NA),
           ic_high_display = ifelse(affiche_ic, ic_high, NA))
  
  tab_ic %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low_display, ymax = ic_high_display),
                  position = position_dodge(width = 0.65),
                  width = 0.25, linewidth = 0.4, color = "black", na.rm = TRUE) +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_grid(SEXE ~ epreuve, labeller = labeller(SEXE = c("M" = "Garçons", "F" = "Filles"))) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 13, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 13, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 10, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 10),
      legend.text        = element_text(size = 10),
      legend.position    = "bottom",
      legend.margin      = margin(t = 8),
      strip.text         = element_text(size = 11, face = "bold", margin = margin(b = 4, t = 4)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 16, height = 7.5, dpi = 300, bg = "white")
}

# Figure combinée (2 panneaux) - stratifiée par SEXE - IC seulement CR
faire_plot_combinee_2_sexe_ic_cr <- function(tab, titre, fichier) {
  tab_ic <- tab %>%
    mutate(affiche_ic = groupe_cr == "Centre de réadaptation",
           ic_low_display = ifelse(affiche_ic, ic_low, NA),
           ic_high_display = ifelse(affiche_ic, ic_high, NA))
  
  tab_ic %>%
    ggplot(aes(x = sec_theorique, y = pct_participe, fill = groupe_cr)) +
    geom_col(position = position_dodge(width = 0.65), width = 0.65,
             color = "black", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ic_low_display, ymax = ic_high_display),
                  position = position_dodge(width = 0.65),
                  width = 0.25, linewidth = 0.4, color = "black", na.rm = TRUE) +
    scale_fill_manual(values = c("Population" = "#95a5a6", "Centre de réadaptation" = "#e74c3c"),
                      name = "") +
    facet_grid(SEXE ~ epreuve, labeller = labeller(SEXE = c("1" = "Garçons", "2" = "Filles"))) +
    scale_y_continuous(labels = function(x) paste0(x, " %"),
                       expand = expansion(mult = c(0, 0.12)),
                       limits = c(0, 100)) +
    labs(title = titre, x = "Année scolaire", y = "Participation (%)") +
    theme_minimal() +
    theme(
      plot.title         = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
      axis.title.x       = element_text(size = 13, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 13, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 10, margin = margin(t = 5)),
      axis.text.y        = element_text(size = 10),
      legend.text        = element_text(size = 10),
      legend.position    = "bottom",
      legend.margin      = margin(t = 8),
      strip.text         = element_text(size = 11, face = "bold", margin = margin(b = 4, t = 4)),
      panel.grid.major.y = element_line(color = "gray88", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(10, 15, 10, 10)
    ) -> p
  
  print(p)
  ggsave(fichier, p, width = 12, height = 7.5, dpi = 300, bg = "white")
}

dir.create("outputs_epreuves", showWarnings = FALSE)

# =====================================================================
# 7a) MATHÉMATIQUES SEC 4
# =====================================================================

cat("1. Mathématiques 4ᵉ secondaire...\n")

codes_math_sec4 <- c("063420", "064420", "065420", "563420", "564420", "565420")

math_sec4_indiv <- meq_r_appre %>%
  filter(CD_COURS %in% codes_math_sec4) %>%
  mutate(
    NOINDIV = as.character(NOINDIV),
    NOTE_MINST_BRUT = str_squish(as.character(NOTE_MINST_BRUT)),
    a_participe = case_when(
      is.na(NOTE_MINST_BRUT) | NOTE_MINST_BRUT == "" ~ 0L,
      NOTE_MINST_BRUT == "ABS" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  left_join(ramq_cohortes %>% select(NOINDIV, sec4_theorique), by = "NOINDIV") %>%
  left_join(date_limites_sec4, by = "sec4_theorique") %>%
  filter(!is.na(DT_OBTEN_RESLT) & DT_OBTEN_RESLT <= date_limite) %>%
  group_by(NOINDIV) %>%
  summarise(a_participe = max(a_participe), .groups = "drop")

bd_sec4_math <- bd_sec4 %>%
  left_join(math_sec4_indiv, by = "NOINDIV") %>%
  mutate(a_participe = ifelse(is.na(a_participe), 0L, a_participe))

# Non stratifié
tab_math_sec4 <- bd_sec4_math %>%
  group_by(cohorte_scolaire, sec4_theorique, groupe_cr_sec4) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Mathématiques\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# Stratifié par SEXE
tab_math_sec4_sexe <- bd_sec4_math %>%
  group_by(sec4_theorique, groupe_cr_sec4, SEXE) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Mathématiques\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, SEXE, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# =====================================================================
# 7b) SCIENCES SEC 4
# =====================================================================

cat("2. Sciences 4ᵉ secondaire...\n")

codes_sciences_sec4 <- c("555410", "557410", "055410", "057410")

sciences_sec4_indiv <- meq_r_appre %>%
  filter(CD_COURS %in% codes_sciences_sec4) %>%
  mutate(
    NOINDIV = as.character(NOINDIV),
    NOTE_MINST_BRUT = str_squish(as.character(NOTE_MINST_BRUT)),
    a_participe = case_when(
      is.na(NOTE_MINST_BRUT) | NOTE_MINST_BRUT == "" ~ 0L,
      NOTE_MINST_BRUT == "ABS" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  left_join(ramq_cohortes %>% select(NOINDIV, sec4_theorique), by = "NOINDIV") %>%
  left_join(date_limites_sec4, by = "sec4_theorique") %>%
  filter(!is.na(DT_OBTEN_RESLT) & DT_OBTEN_RESLT <= date_limite) %>%
  group_by(NOINDIV) %>%
  summarise(a_participe = max(a_participe), .groups = "drop")

bd_sec4_sciences <- bd_sec4 %>%
  left_join(sciences_sec4_indiv, by = "NOINDIV") %>%
  mutate(a_participe = ifelse(is.na(a_participe), 0L, a_participe))

tab_sciences_sec4 <- bd_sec4_sciences %>%
  group_by(cohorte_scolaire, sec4_theorique, groupe_cr_sec4) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Sciences\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

tab_sciences_sec4_sexe <- bd_sec4_sciences %>%
  group_by(sec4_theorique, groupe_cr_sec4, SEXE) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Sciences\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, SEXE, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# =====================================================================
# 7c) HISTOIRE SEC 4
# =====================================================================

cat("3. Histoire 4ᵉ secondaire...\n")

codes_histoire_sec4 <- c("085404", "585404")

histoire_sec4_indiv <- meq_r_appre %>%
  filter(CD_COURS %in% codes_histoire_sec4) %>%
  mutate(
    NOINDIV = as.character(NOINDIV),
    NOTE_MINST_BRUT = str_squish(as.character(NOTE_MINST_BRUT)),
    a_participe = case_when(
      is.na(NOTE_MINST_BRUT) | NOTE_MINST_BRUT == "" ~ 0L,
      NOTE_MINST_BRUT == "ABS" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  left_join(ramq_cohortes %>% select(NOINDIV, sec4_theorique), by = "NOINDIV") %>%
  left_join(date_limites_sec4, by = "sec4_theorique") %>%
  filter(!is.na(DT_OBTEN_RESLT) & DT_OBTEN_RESLT <= date_limite) %>%
  group_by(NOINDIV) %>%
  summarise(a_participe = max(a_participe), .groups = "drop")

bd_sec4_histoire <- bd_sec4 %>%
  left_join(histoire_sec4_indiv, by = "NOINDIV") %>%
  mutate(a_participe = ifelse(is.na(a_participe), 0L, a_participe))

tab_histoire_sec4 <- bd_sec4_histoire %>%
  group_by(cohorte_scolaire, sec4_theorique, groupe_cr_sec4) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec4_theorique != "2021-2022") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Histoire\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

tab_histoire_sec4_sexe <- bd_sec4_histoire %>%
  group_by(sec4_theorique, groupe_cr_sec4, SEXE) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec4_theorique != "2021-2022") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Histoire\n4ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec4_theorique, groupe_cr = groupe_cr_sec4, SEXE, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# =====================================================================
# 7d) FRANÇAIS SEC 5
# =====================================================================

cat("4. Français 5ᵉ secondaire...\n")

codes_francais_sec5 <- c("634520", "634530", "635520", "635530", "132520")

francais_sec5_indiv <- meq_r_appre %>%
  filter(CD_COURS %in% codes_francais_sec5) %>%
  mutate(
    NOINDIV = as.character(NOINDIV),
    NOTE_MINST_BRUT = str_squish(as.character(NOTE_MINST_BRUT)),
    a_participe = case_when(
      is.na(NOTE_MINST_BRUT) | NOTE_MINST_BRUT == "" ~ 0L,
      NOTE_MINST_BRUT == "ABS" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  left_join(ramq_cohortes %>% select(NOINDIV, sec5_theorique), by = "NOINDIV") %>%
  left_join(date_limites_sec5, by = "sec5_theorique") %>%
  filter(!is.na(DT_OBTEN_RESLT) & DT_OBTEN_RESLT <= date_limite) %>%
  group_by(NOINDIV) %>%
  summarise(a_participe = max(a_participe), .groups = "drop")

bd_sec5_francais <- bd_sec5 %>%
  left_join(francais_sec5_indiv, by = "NOINDIV") %>%
  mutate(a_participe = ifelse(is.na(a_participe), 0L, a_participe))

tab_francais_sec5 <- bd_sec5_francais %>%
  group_by(cohorte_scolaire, sec5_theorique, groupe_cr_sec5) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec5_theorique != "2024-2025") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Français\n5ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec5_theorique, groupe_cr = groupe_cr_sec5, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

tab_francais_sec5_sexe <- bd_sec5_francais %>%
  group_by(sec5_theorique, groupe_cr_sec5, SEXE) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec5_theorique != "2024-2025") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Français\n5ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec5_theorique, groupe_cr = groupe_cr_sec5, SEXE, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# =====================================================================
# 7e) ANGLAIS SEC 5
# =====================================================================

cat("5. Anglais 5ᵉ secondaire...\n")

codes_anglais_sec5 <- c("134510", "134530", "136540", "136550", "612520", "612530")

anglais_sec5_indiv <- meq_r_appre %>%
  filter(CD_COURS %in% codes_anglais_sec5) %>%
  mutate(
    NOINDIV = as.character(NOINDIV),
    NOTE_MINST_BRUT = str_squish(as.character(NOTE_MINST_BRUT)),
    a_participe = case_when(
      is.na(NOTE_MINST_BRUT) | NOTE_MINST_BRUT == "" ~ 0L,
      NOTE_MINST_BRUT == "ABS" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  left_join(ramq_cohortes %>% select(NOINDIV, sec5_theorique), by = "NOINDIV") %>%
  left_join(date_limites_sec5, by = "sec5_theorique") %>%
  filter(!is.na(DT_OBTEN_RESLT) & DT_OBTEN_RESLT <= date_limite) %>%
  group_by(NOINDIV) %>%
  summarise(a_participe = max(a_participe), .groups = "drop")

bd_sec5_anglais <- bd_sec5 %>%
  left_join(anglais_sec5_indiv, by = "NOINDIV") %>%
  mutate(a_participe = ifelse(is.na(a_participe), 0L, a_participe))

tab_anglais_sec5 <- bd_sec5_anglais %>%
  group_by(cohorte_scolaire, sec5_theorique, groupe_cr_sec5) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec5_theorique != "2024-2025") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Anglais\n5ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec5_theorique, groupe_cr = groupe_cr_sec5, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

tab_anglais_sec5_sexe <- bd_sec5_anglais %>%
  group_by(sec5_theorique, groupe_cr_sec5, SEXE) %>%
  summarise(n = n(), n_participe = sum(a_participe), .groups = "drop") %>%
  filter(sec5_theorique != "2024-2025") %>%
  mutate(
    n_arrondi = CR_round(n, base = 5),
    n_participe_arrondi = CR_round(n_participe, base = 5),
    pct_participe = 100 * n_participe_arrondi / n_arrondi,
    epreuve = "Anglais\n5ᵉ secondaire"
  ) %>%
  rowwise() %>%
  mutate(
    ic_result = list(calculer_ic_wilson(n_participe_arrondi, n_arrondi)),
    ic_low = ic_result$ci_low,
    ic_high = ic_result$ci_high
  ) %>%
  select(sec_theorique = sec5_theorique, groupe_cr = groupe_cr_sec5, SEXE, epreuve,
         n_arrondi, n_participe_arrondi, pct_participe, ic_low, ic_high)

# =====================================================================
# CRÉER LES FIGURES AVEC IC
# =====================================================================

cat("\n📊 Création des figures avec IC 95%...\n")

# GROUPE 1 : Mathématiques + Sciences (2 panneaux, Sec 4)
tab_groupe_mathsci <- bind_rows(tab_math_sec4, tab_sciences_sec4)
faire_plot_combinee_2_ic(tab_groupe_mathsci,
                         "Participation aux épreuves ministérielles",
                         "outputs_epreuves/01_GROUPE_MATH_SCIENCES_AVEC_IC.png")

tab_groupe_mathsci_sexe <- bind_rows(tab_math_sec4_sexe, tab_sciences_sec4_sexe)
faire_plot_combinee_2_sexe_ic(tab_groupe_mathsci_sexe,
                              "Participation aux épreuves ministérielles - par sexe",
                              "outputs_epreuves/01_GROUPE_MATH_SCIENCES_SEXE_AVEC_IC.png")

# GROUPE 2 : Français + Anglais + Histoire (3 panneaux, Sec 5 + Sec 4)
tab_groupe_fah <- bind_rows(tab_francais_sec5, tab_anglais_sec5, tab_histoire_sec4)
faire_plot_combinee_3_ic(tab_groupe_fah,
                         "Participation aux épreuves ministérielles",
                         "outputs_epreuves/02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_AVEC_IC.png")

tab_groupe_fah_sexe <- bind_rows(tab_francais_sec5_sexe, tab_anglais_sec5_sexe, tab_histoire_sec4_sexe)
faire_plot_combinee_3_sexe_ic(tab_groupe_fah_sexe,
                              "Participation aux épreuves ministérielles - par sexe",
                              "outputs_epreuves/02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_SEXE_AVEC_IC.png")

# =====================================================================
# CRÉER LES FIGURES AVEC IC SEULEMENT POUR CENTRE DE RÉADAPTATION
# =====================================================================

cat("\n📊 Création des figures avec IC seulement pour CR...\n")

# GROUPE 1 : Mathématiques + Sciences (IC seulement CR)
tab_groupe_mathsci <- bind_rows(tab_math_sec4, tab_sciences_sec4)
faire_plot_combinee_2_ic_cr(tab_groupe_mathsci,
                            "Participation aux épreuves ministérielles",
                            "outputs_epreuves/01_GROUPE_MATH_SCIENCES_IC_CR_ONLY.png")

tab_groupe_mathsci_sexe <- bind_rows(tab_math_sec4_sexe, tab_sciences_sec4_sexe)
faire_plot_combinee_2_sexe_ic_cr(tab_groupe_mathsci_sexe,
                                 "Participation aux épreuves ministérielles - par sexe",
                                 "outputs_epreuves/01_GROUPE_MATH_SCIENCES_SEXE_IC_CR_ONLY.png")

# GROUPE 2 : Français + Anglais + Histoire (IC seulement CR)
tab_groupe_fah <- bind_rows(tab_francais_sec5, tab_anglais_sec5, tab_histoire_sec4)
faire_plot_combinee_3_ic_cr(tab_groupe_fah,
                            "Participation aux épreuves ministérielles",
                            "outputs_epreuves/02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_IC_CR_ONLY.png")

tab_groupe_fah_sexe <- bind_rows(tab_francais_sec5_sexe, tab_anglais_sec5_sexe, tab_histoire_sec4_sexe)
faire_plot_combinee_3_sexe_ic_cr(tab_groupe_fah_sexe,
                                 "Participation aux épreuves ministérielles - par sexe",
                                 "outputs_epreuves/02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_SEXE_IC_CR_ONLY.png")

# =====================================================================
# CRÉER LE FICHIER EXCEL AVEC TOUS LES TABLEAUX
# =====================================================================

cat("\n📊 Création du fichier Excel...\n")

# Préparer tous les tableaux pour Excel
tab_math_excel <- tab_math_sec4 %>%
  mutate(SEXE = "Total") %>%
  bind_rows(tab_math_sec4_sexe) %>%
  select(Épreuve = epreuve, `Année scolaire` = sec_theorique, `Groupe` = groupe_cr, SEXE,
         `N` = n_arrondi, `Participants` = n_participe_arrondi, 
         `% (IC 95%)` = pct_participe, `IC Inf` = ic_low, `IC Sup` = ic_high) %>%
  mutate(`% (IC 95%)` = round(`% (IC 95%)`, 1),
         `IC Inf` = round(`IC Inf`, 1),
         `IC Sup` = round(`IC Sup`, 1))

tab_sciences_excel <- tab_sciences_sec4 %>%
  mutate(SEXE = "Total") %>%
  bind_rows(tab_sciences_sec4_sexe) %>%
  select(Épreuve = epreuve, `Année scolaire` = sec_theorique, `Groupe` = groupe_cr, SEXE,
         `N` = n_arrondi, `Participants` = n_participe_arrondi, 
         `% (IC 95%)` = pct_participe, `IC Inf` = ic_low, `IC Sup` = ic_high) %>%
  mutate(`% (IC 95%)` = round(`% (IC 95%)`, 1),
         `IC Inf` = round(`IC Inf`, 1),
         `IC Sup` = round(`IC Sup`, 1))

tab_histoire_excel <- tab_histoire_sec4 %>%
  mutate(SEXE = "Total") %>%
  bind_rows(tab_histoire_sec4_sexe) %>%
  select(Épreuve = epreuve, `Année scolaire` = sec_theorique, `Groupe` = groupe_cr, SEXE,
         `N` = n_arrondi, `Participants` = n_participe_arrondi, 
         `% (IC 95%)` = pct_participe, `IC Inf` = ic_low, `IC Sup` = ic_high) %>%
  mutate(`% (IC 95%)` = round(`% (IC 95%)`, 1),
         `IC Inf` = round(`IC Inf`, 1),
         `IC Sup` = round(`IC Sup`, 1))

tab_francais_excel <- tab_francais_sec5 %>%
  mutate(SEXE = "Total") %>%
  bind_rows(tab_francais_sec5_sexe) %>%
  select(Épreuve = epreuve, `Année scolaire` = sec_theorique, `Groupe` = groupe_cr, SEXE,
         `N` = n_arrondi, `Participants` = n_participe_arrondi, 
         `% (IC 95%)` = pct_participe, `IC Inf` = ic_low, `IC Sup` = ic_high) %>%
  mutate(`% (IC 95%)` = round(`% (IC 95%)`, 1),
         `IC Inf` = round(`IC Inf`, 1),
         `IC Sup` = round(`IC Sup`, 1))

tab_anglais_excel <- tab_anglais_sec5 %>%
  mutate(SEXE = "Total") %>%
  bind_rows(tab_anglais_sec5_sexe) %>%
  select(Épreuve = epreuve, `Année scolaire` = sec_theorique, `Groupe` = groupe_cr, SEXE,
         `N` = n_arrondi, `Participants` = n_participe_arrondi, 
         `% (IC 95%)` = pct_participe, `IC Inf` = ic_low, `IC Sup` = ic_high) %>%
  mutate(`% (IC 95%)` = round(`% (IC 95%)`, 1),
         `IC Inf` = round(`IC Inf`, 1),
         `IC Sup` = round(`IC Sup`, 1))

# Créer le workbook
wb <- createWorkbook()

# Ajouter les feuilles
addWorksheet(wb, "Mathématiques")
writeData(wb, "Mathématiques", tab_math_excel)

addWorksheet(wb, "Sciences")
writeData(wb, "Sciences", tab_sciences_excel)

addWorksheet(wb, "Histoire")
writeData(wb, "Histoire", tab_histoire_excel)

addWorksheet(wb, "Français")
writeData(wb, "Français", tab_francais_excel)

addWorksheet(wb, "Anglais")
writeData(wb, "Anglais", tab_anglais_excel)

# Sauvegarder
saveWorkbook(wb, "donnees_participation_epreuves.xlsx", overwrite = TRUE)

cat("   ✅ donnees_participation_epreuves.xlsx\n\n")

# =====================================================================
# RÉSUMÉ FINAL
# =====================================================================

cat("════════════════════════════════════════════════════════════════\n")
cat("✅ ANALYSES COMPLÉTÉES AVEC SUCCÈS\n")
cat("════════════════════════════════════════════════════════════════\n\n")
cat("📁 Figures sauvegardées dans : outputs_epreuves/\n")
cat("   📊 VERSION 1 : IC pour les 2 groupes (CR + Population)\n")
cat("      - 01_GROUPE_MATH_SCIENCES_AVEC_IC.png\n")
cat("      - 01_GROUPE_MATH_SCIENCES_SEXE_AVEC_IC.png\n")
cat("      - 02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_AVEC_IC.png\n")
cat("      - 02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_SEXE_AVEC_IC.png\n\n")
cat("   📊 VERSION 2 : IC seulement pour Centre de réadaptation\n")
cat("      - 01_GROUPE_MATH_SCIENCES_IC_CR_ONLY.png\n")
cat("      - 01_GROUPE_MATH_SCIENCES_SEXE_IC_CR_ONLY.png\n")
cat("      - 02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_IC_CR_ONLY.png\n")
cat("      - 02_GROUPE_FRANCAIS_ANGLAIS_HISTOIRE_SEXE_IC_CR_ONLY.png\n\n")
cat("📊 Fichier Excel:\n")
cat("   - donnees_participation_epreuves.xlsx\n")
cat("   - 5 onglets (une épreuve par onglet)\n")
cat("   - Colonnes: Épreuve, Année, Groupe, Sexe, N, Participants, %, IC Inf, IC Sup\n\n")
