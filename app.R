# ============================================================
# ClimaMissing v2
# Visualisation des Données Manquantes Climatiques
# Nouveautés : multi-stations, multi-paramètres, graphiques
#              temporels, indicateurs qualité, export CSV/Excel/PNG
# ============================================================

library(shiny)
library(dplyr)
library(tidyr)
library(gt)
library(bslib)
library(htmltools)
library(ggplot2)
library(writexl)

# Limite de téléchargement : 500 MB (défaut Shiny = 5 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# ============================================================
# DONNÉES EXEMPLE
# ============================================================
generate_sample_data <- function() {
  set.seed(42)
  station_types <- c("Synoptique", "Agrométéo", "Hydrologique", "Climatologique")
  stations <- data.frame(
    Station_type = rep(station_types, each = 3),
    Station_name = c(
      "Ouagadougou-Aéro", "Ouagadougou-Ville", "Ouaga-Centre",
      "Bobo-Dioulasso", "Dédougou", "Fada N'Gourma",
      "Kompienga", "Bagré", "Nazinon",
      "Dori", "Ouahigouya", "Kaya"
    )
  )
  parameters <- c("Température Max", "Température Min", "Précipitations",
                  "Humidité Relative", "Vent (vitesse)", "Rayonnement")
  expand.grid(
    Station_name = stations$Station_name,
    Year  = 2010:2023,
    Month = 1:12,
    Day   = 1:28,
    Parameter = parameters,
    stringsAsFactors = FALSE
  ) %>%
    left_join(stations, by = "Station_name") %>%
    mutate(
      miss_prob = 0.08 +
        0.10 * (Parameter == "Précipitations") +
        0.06 * (Month %in% c(6, 7, 8, 9)) +
        0.04 * (Station_type == "Hydrologique"),
      Value = ifelse(runif(n()) < miss_prob, NA_real_,
                     rnorm(n(), mean = 25, sd = 5))
    ) %>%
    select(Station_type, Station_name, Year, Month, Day, Parameter, Value)
}

sample_data <- generate_sample_data()

# ============================================================
# UTILITAIRES
# ============================================================
month_labels <- c("Jan","Fév","Mar","Avr","Mai","Jun",
                  "Jul","Aoû","Sep","Oct","Nov","Déc")

PALETTE <- c("#d1fae5","#a7f3d0","#fef08a",
             "#fdba74","#f97316","#ef4444","#991b1b")

quality_label <- function(pct) {
  dplyr::case_when(
    pct == 0  ~ "Complet",
    pct <  5  ~ "Excellent",
    pct < 10  ~ "Bon",
    pct < 20  ~ "Acceptable",
    pct < 30  ~ "Dégradé",
    pct < 50  ~ "Mauvais",
    TRUE      ~ "Critique"
  )
}

# Convertit Year/Month/Day en Date
to_date <- function(year, month, day) {
  as.Date(paste(year, month, day, sep = "-"))
}

# Calcule la plus longue séquence consécutive de NA (en jours)
longest_gap <- function(dates, values) {
  if (length(values) == 0) return(list(days = 0L, start = NA, end = NA))
  # Trier par date
  ord    <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]
  is_na  <- is.na(values)
  if (!any(is_na)) return(list(days = 0L, start = NA, end = NA))
  
  # Run-length encoding des NA
  rle_res <- rle(is_na)
  lengths <- rle_res$lengths
  values_ <- rle_res$values
  na_lens <- lengths[values_]
  if (length(na_lens) == 0) return(list(days = 0L, start = NA, end = NA))
  
  max_len <- max(na_lens)
  # Trouver la position de début de la plus longue séquence
  cum_pos <- cumsum(lengths)
  idx_run <- which(values_ & lengths == max_len)[1]
  end_pos <- cum_pos[which(values_)[idx_run]]
  start_pos <- end_pos - max_len + 1
  list(days  = max_len,
       start = format(dates[start_pos], "%d/%m/%Y"),
       end   = format(dates[end_pos],   "%d/%m/%Y"))
}

# Fiche complète par station × paramètre
compute_station_fiche <- function(data, station, parameter) {
  df <- data %>%
    filter(Station_name == station, Parameter == parameter) %>%
    mutate(Date = to_date(Year, Month, Day)) %>%
    arrange(Date)
  
  if (nrow(df) == 0) return(NULL)
  
  # Dates de première et dernière mesure non-NA
  obs <- df %>% filter(!is.na(Value))
  date_first <- if (nrow(obs) > 0) format(min(obs$Date), "%d/%m/%Y") else "—"
  date_last  <- if (nrow(obs) > 0) format(max(obs$Date), "%d/%m/%Y") else "—"
  
  # Durée totale de la série (première date à dernière date dans df complet)
  date_debut_serie <- format(min(df$Date), "%d/%m/%Y")
  date_fin_serie   <- format(max(df$Date), "%d/%m/%Y")
  n_jours_serie    <- as.integer(max(df$Date) - min(df$Date)) + 1L
  
  # % global manquant
  n_total   <- nrow(df)
  n_missing <- sum(is.na(df$Value))
  pct_global <- round(100 * n_missing / n_total, 1)
  
  # Valeurs extrêmes
  val_min  <- if (nrow(obs) > 0) round(min(obs$Value,  na.rm = TRUE), 2) else NA
  val_max  <- if (nrow(obs) > 0) round(max(obs$Value,  na.rm = TRUE), 2) else NA
  val_mean <- if (nrow(obs) > 0) round(mean(obs$Value, na.rm = TRUE), 2) else NA
  val_sd   <- if (nrow(obs) > 0) round(sd(obs$Value,   na.rm = TRUE), 2) else NA
  
  date_min <- if (nrow(obs) > 0)
    format(obs$Date[which.min(obs$Value)], "%d/%m/%Y") else "—"
  date_max <- if (nrow(obs) > 0)
    format(obs$Date[which.max(obs$Value)], "%d/%m/%Y") else "—"
  
  # Plus longue séquence de NA
  gap <- longest_gap(df$Date, df$Value)
  
  # Répartition annuelle des manquants
  annual <- df %>%
    group_by(Year) %>%
    summarise(
      n_tot  = n(),
      n_miss = sum(is.na(Value)),
      pct    = round(100 * n_miss / n_tot, 1),
      .groups = "drop"
    )
  
  list(
    station         = station,
    parameter       = parameter,
    date_debut_serie= date_debut_serie,
    date_fin_serie  = date_fin_serie,
    n_jours_serie   = n_jours_serie,
    date_first_obs  = date_first,
    date_last_obs   = date_last,
    n_total         = n_total,
    n_missing       = n_missing,
    pct_global      = pct_global,
    val_min         = val_min,
    val_max         = val_max,
    val_mean        = val_mean,
    val_sd          = val_sd,
    date_min        = date_min,
    date_max        = date_max,
    gap_days        = gap$days,
    gap_start       = gap$start,
    gap_end         = gap$end,
    annual          = annual
  )
}

compute_missing <- function(data, stations, parameters, yr1, yr2) {
  data %>%
    filter(Station_name %in% stations,
           Parameter    %in% parameters,
           Year >= yr1, Year <= yr2) %>%
    group_by(Station_name, Parameter, Year, Month) %>%
    summarise(
      n_total   = n(),
      n_missing = sum(is.na(Value)),
      pct_miss  = round(100 * n_missing / n_total, 1),
      .groups   = "drop"
    )
}

# Tableau GT heatmap (années × mois) pour une station + paramètre
make_gt_heatmap <- function(df, station, param, yr1, yr2) {
  wide <- df %>%
    filter(Station_name == station, Parameter == param) %>%
    mutate(Month_lbl = month_labels[Month]) %>%
    select(Year, Month_lbl, pct_miss) %>%
    pivot_wider(names_from = Month_lbl, values_from = pct_miss,
                names_expand = FALSE)
  
  pm <- month_labels[month_labels %in% names(wide)]
  wide <- wide %>%
    select(Year, all_of(pm)) %>%
    mutate(Moy = round(rowMeans(select(., all_of(pm)), na.rm = TRUE), 1))
  
  wide %>%
    gt(rowname_col = "Year") %>%
    tab_header(
      title    = md(paste0("**", station, "** — ", param)),
      subtitle = md(paste0("% données manquantes · ", yr1, " – ", yr2))
    ) %>%
    tab_spanner(label = "Mois", columns = all_of(pm)) %>%
    cols_label(Moy = "Moy.") %>%
    tab_stubhead(label = "Année") %>%
    data_color(
      columns  = c(all_of(pm), "Moy"),
      method   = "numeric",
      palette  = PALETTE,
      domain   = c(0, 100),
      na_color = "#e5e7eb"
    ) %>%
    fmt_number(columns = c(all_of(pm), "Moy"),
               decimals = 1, pattern = "{x}%") %>%
    sub_missing(missing_text = "—") %>%
    cols_width(Year ~ px(68), Moy ~ px(70), everything() ~ px(60)) %>%
    tab_style(style = list(cell_fill("#1e293b"),
                           cell_text(color = "#f1f5f9", weight = "bold")),
              locations = cells_column_labels()) %>%
    tab_style(style = list(cell_fill("#1e293b"),
                           cell_text(color = "#f1f5f9", weight = "bold")),
              locations = cells_column_spanners()) %>%
    tab_style(style = list(cell_fill("#0f172a"),
                           cell_text(color = "#94a3b8", weight = "bold")),
              locations = cells_stub()) %>%
    tab_style(style = list(cell_fill("#0f172a"),
                           cell_text(color = "#38bdf8", size = px(15))),
              locations = cells_title("title")) %>%
    tab_style(style = list(cell_fill("#0f172a"),
                           cell_text(color = "#64748b", size = px(12))),
              locations = cells_title("subtitle")) %>%
    tab_style(style = cell_borders(sides = "all", color = "#334155",
                                   style = "solid", weight = px(1)),
              locations = cells_body()) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = "Moy")) %>%
    tab_options(
      table.background.color            = "#1e293b",
      table.border.top.color            = "#334155",
      table.border.bottom.color         = "#334155",
      column_labels.border.bottom.color = "#475569",
      stub.border.color                 = "#334155",
      row.striping.background_color     = "#162032",
      row.striping.include_stub         = TRUE,
      table.font.names                  = "Inter",
      table.font.size                   = px(13),
      heading.padding                   = px(12),
      data_row.padding                  = px(6)
    )
}

# Thème ggplot
theme_clima <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#1e293b", color = NA),
      panel.background  = element_rect(fill = "#0f172a", color = NA),
      panel.grid.major  = element_line(color = "#334155", linewidth = .3),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = "#94a3b8", size = 9),
      axis.title        = element_text(color = "#cbd5e1", size = 10),
      plot.title        = element_text(color = "#f1f5f9", size = 13, face = "bold"),
      plot.subtitle     = element_text(color = "#64748b", size = 10),
      legend.background = element_rect(fill = "#1e293b", color = NA),
      legend.text       = element_text(color = "#94a3b8", size = 8),
      legend.title      = element_text(color = "#cbd5e1", size = 9),
      strip.background  = element_rect(fill = "#162032", color = NA),
      strip.text        = element_text(color = "#38bdf8", size = 9, face = "bold")
    )
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tags$span(
    style = "font-weight:700; letter-spacing:.03em;",
    tags$span(style = "color:#38bdf8", "Clima"),
    tags$span(style = "color:#f1f5f9", "Missing"),
    tags$span(style = "color:#475569; font-size:.8rem; margin-left:8px;",
              "v2 · Données Manquantes Climatiques")
  ),
  theme = bs_theme(
    version      = 5,
    bg           = "#0f172a",
    fg           = "#f1f5f9",
    primary      = "#38bdf8",
    secondary    = "#64748b",
    base_font    = font_google("Inter"),
    heading_font = font_google("Space Grotesk")
  ),
  bg      = "#0f172a",
  inverse = TRUE,
  
  tags$head(tags$style(HTML("
    .sidebar-panel { background:#1e293b !important; border-radius:12px; padding:16px; }
    .main-panel    { background:#1e293b !important; border-radius:12px; padding:20px; }
    .form-select, .form-control {
      background:#0f172a !important; color:#f1f5f9 !important;
      border-color:#334155 !important; }
    .form-label { color:#94a3b8 !important; font-size:.78rem;
                  text-transform:uppercase; letter-spacing:.05em; }
    .stat-card  { background:#0f172a; border:1px solid #334155;
                  border-radius:10px; padding:12px 16px; text-align:center; }
    .stat-val   { font-size:1.6rem; font-weight:700; color:#38bdf8; line-height:1.2; }
    .stat-lbl   { font-size:.68rem; color:#64748b;
                  text-transform:uppercase; letter-spacing:.06em; margin-top:2px; }
    .sec-title  { color:#38bdf8; font-size:.8rem; font-weight:600;
                  text-transform:uppercase; letter-spacing:.07em;
                  border-bottom:1px solid #334155; padding-bottom:4px;
                  margin:16px 0 8px 0; }
    .legend-row  { display:flex; gap:5px; flex-wrap:wrap; margin-top:6px; }
    .legend-item { display:flex; align-items:center; gap:4px;
                   font-size:.7rem; color:#94a3b8; }
    .legend-sw   { width:16px; height:16px; border-radius:3px; }
    .export-row  { display:flex; gap:8px; flex-wrap:wrap; margin-bottom:12px; }
    .selectize-input { background:#0f172a !important; color:#f1f5f9 !important;
                       border-color:#334155 !important; }
    .selectize-dropdown { background:#1e293b !important; color:#f1f5f9 !important; }
    .selectize-dropdown .option:hover { background:#334155 !important; }
    .nav-tabs .nav-link        { color:#94a3b8 !important; }
    .nav-tabs .nav-link.active { color:#38bdf8 !important;
                                 border-bottom:2px solid #38bdf8 !important; }
    .irs--shiny .irs-bar        { background:#38bdf8 !important; }
    .irs--shiny .irs-handle     { background:#38bdf8 !important; border-color:#38bdf8 !important; }
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single     { background:#334155 !important; color:#f1f5f9 !important; }
    hr.sep { border-color:#334155; margin:14px 0; }
    .fiche-grid { display:grid; grid-template-columns:1fr 1fr; gap:14px; margin-bottom:18px; }
    .fiche-card { background:#0f172a; border:1px solid #334155; border-radius:10px; padding:14px 16px; }
    .fiche-card-title { font-size:.72rem; color:#64748b; text-transform:uppercase;
                        letter-spacing:.06em; margin-bottom:10px; font-weight:600; }
    .fiche-row  { display:flex; justify-content:space-between; align-items:center;
                  padding:4px 0; border-bottom:1px solid #1e293b; font-size:.83rem; }
    .fiche-row:last-child { border-bottom:none; }
    .fiche-label { color:#94a3b8; }
    .fiche-value { color:#f1f5f9; font-weight:600; text-align:right; }
    .fiche-value.accent  { color:#38bdf8; }
    .fiche-value.danger  { color:#ef4444; }
    .fiche-value.success { color:#22c55e; }
    .fiche-value.warn    { color:#f59e0b; }
    .fiche-section-title { color:#38bdf8; font-size:.8rem; font-weight:600;
                           text-transform:uppercase; letter-spacing:.07em;
                           border-bottom:1px solid #334155; padding-bottom:4px;
                           margin:20px 0 10px 0; }
    .gap-badge { display:inline-block; background:#7f1d1d; color:#fca5a5;
                 border-radius:6px; padding:3px 10px; font-size:.8rem; font-weight:700; }
    .gap-badge.ok { background:#14532d; color:#86efac; }
  "))),
  
  # ================================================================
  # ONGLET 1 — ANALYSE PAR STATION
  # ================================================================
  nav_panel(
    title = tags$span(icon("table-cells"), " Analyse Station"),
    layout_sidebar(
      sidebar = sidebar(
        width = 290, class = "sidebar-panel",
        
        # Chargement données
        tags$div(class = "sec-title", icon("database"), " Données"),
        actionButton("use_sample", "Données exemple",
                     class = "btn btn-outline-info btn-sm w-100",
                     icon  = icon("flask")),
        tags$div(style = "margin-top:8px"),
        fileInput("upload_file", NULL, accept = ".csv",
                  buttonLabel = "Charger CSV",
                  placeholder = "Aucun fichier sélectionné"),
        
        tags$hr(class = "sep"),
        
        # Filtres
        tags$div(class = "sec-title", icon("filter"), " Filtres"),
        
        tags$div(class = "form-label", "Type de station"),
        selectInput("station_type", NULL,
                    choices  = c("Tous", sort(unique(sample_data$Station_type))),
                    selected = "Tous"),
        
        tags$div(class = "form-label", "Stations (multi-sélection)"),
        selectInput("station_names", NULL,
                    choices  = NULL,
                    multiple = TRUE,
                    selectize = TRUE),
        
        tags$div(class = "form-label", "Paramètres (multi-sélection)"),
        selectInput("parameters", NULL,
                    choices  = sort(unique(sample_data$Parameter)),
                    selected = "Précipitations",
                    multiple = TRUE,
                    selectize = TRUE),
        
        tags$div(class = "form-label", "Période"),
        sliderInput("year_range", NULL,
                    min   = min(sample_data$Year),
                    max   = max(sample_data$Year),
                    value = c(min(sample_data$Year), max(sample_data$Year)),
                    step  = 1, sep = ""),
        
        tags$hr(class = "sep"),
        
        # Légende
        tags$div(class = "sec-title", icon("palette"), " Légende"),
        tags$div(class = "legend-row",
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#d1fae5"), "0%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#a7f3d0"), "<5%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#fef08a"), "5–10%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#fdba74"), "10–20%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#f97316"), "20–30%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#ef4444"), "30–50%"),
                 tags$div(class="legend-item", tags$div(class="legend-sw",style="background:#991b1b"), ">50%")
        )
      ),
      
      # Panneau principal
      tags$div(class = "main-panel",
               
               # KPIs
               fluidRow(
                 column(3, tags$div(class="stat-card",
                                    tags$div(class="stat-val", textOutput("kpi_global")),
                                    tags$div(class="stat-lbl", "% Manquant global"))),
                 column(3, tags$div(class="stat-card",
                                    tags$div(class="stat-val", textOutput("kpi_worst_month")),
                                    tags$div(class="stat-lbl", "Mois le + affecté"))),
                 column(3, tags$div(class="stat-card",
                                    tags$div(class="stat-val", textOutput("kpi_worst_year")),
                                    tags$div(class="stat-lbl", "Année la + affectée"))),
                 column(3, tags$div(class="stat-card",
                                    tags$div(class="stat-val", textOutput("kpi_complete")),
                                    tags$div(class="stat-lbl", "Mois complets (0%)")))
               ),
               
               tags$div(style = "height:14px"),
               
               # Export buttons
               tags$div(class = "export-row",
                        downloadButton("dl_csv",   "CSV",   class="btn btn-sm btn-outline-secondary",
                                       icon=icon("file-csv")),
                        downloadButton("dl_excel", "Excel", class="btn btn-sm btn-outline-success",
                                       icon=icon("file-excel")),
                        downloadButton("dl_png",   "PNG",   class="btn btn-sm btn-outline-warning",
                                       icon=icon("image"))
               ),
               
               # Tabs internes
               tabsetPanel(
                 id = "main_tabs",
                 
                 # --- Heatmap GT ---
                 tabPanel("Heatmap",
                          tags$div(style="margin-top:12px"),
                          uiOutput("heatmap_ui")
                 ),
                 
                 # --- Graphiques temporels ---
                 tabPanel("Évolution temporelle",
                          tags$div(style="margin-top:12px"),
                          plotOutput("plot_trend",  height = "320px"),
                          tags$div(style="height:12px"),
                          plotOutput("plot_monthly", height = "280px")
                 ),
                 
                 # --- Indicateurs qualité ---
                 tabPanel("Indicateurs Qualité",
                          tags$div(style="margin-top:12px"),
                          gt_output("quality_table")
                 ),
                 
                 # --- Fiche Station ---
                 tabPanel("Fiche Station",
                          tags$div(style="margin-top:12px"),
                          uiOutput("fiche_ui")
                 )
               )
      )
    )
  ),
  
  # ================================================================
  # ONGLET 2 — COMPARAISON MULTI-STATIONS
  # ================================================================
  nav_panel(
    title = tags$span(icon("chart-bar"), " Comparaison"),
    layout_sidebar(
      sidebar = sidebar(
        width = 260, class = "sidebar-panel",
        
        tags$div(class = "sec-title", icon("filter"), " Filtres"),
        tags$div(class = "form-label", "Paramètre"),
        selectInput("cmp_param", NULL,
                    choices  = sort(unique(sample_data$Parameter)),
                    selected = "Précipitations"),
        tags$div(class = "form-label", "Année"),
        selectInput("cmp_year", NULL,
                    choices  = sort(unique(sample_data$Year)),
                    selected = max(sample_data$Year)),
        
        tags$hr(class = "sep"),
        tags$div(class = "export-row",
                 downloadButton("dl_cmp_csv",   "CSV",   class="btn btn-sm btn-outline-secondary"),
                 downloadButton("dl_cmp_excel", "Excel", class="btn btn-sm btn-outline-success")
        )
      ),
      tags$div(class = "main-panel",
               tabsetPanel(
                 tabPanel("Tableau",
                          tags$div(style="margin-top:12px"),
                          gt_output("compare_table")),
                 tabPanel("Graphique",
                          tags$div(style="margin-top:12px"),
                          plotOutput("compare_plot", height = "500px"))
               )
      )
    )
  ),
  
  # ================================================================
  # ONGLET 3 — À PROPOS
  # ================================================================
  nav_panel(
    title = tags$span(icon("circle-info"), " À propos"),
    tags$div(
      style = "max-width:720px; margin:40px auto; color:#94a3b8; line-height:1.8; padding:0 16px;",
      tags$h3(style="color:#38bdf8; font-weight:700;", "ClimaMissing v2"),
      tags$p("Application de diagnostic des lacunes dans les séries climatiques.
              Développée pour les services météorologiques nationaux."),
      
      tags$h5(style="color:#f1f5f9; margin-top:24px", "Nouveautés v2"),
      tags$ul(style="color:#94a3b8",
              tags$li("Sélection multi-stations et multi-paramètres"),
              tags$li("Graphiques temporels : tendance annuelle et profil mensuel"),
              tags$li("Indicateurs de qualité détaillés par station/paramètre"),
              tags$li("Export CSV, Excel (multi-feuilles) et PNG")
      ),
      
      tags$h5(style="color:#f1f5f9; margin-top:24px", "Format des données"),
      tags$pre(style="background:#0f172a; padding:12px; border-radius:8px; color:#7dd3fc; font-size:.85rem;",
               "Station_type, Station_name, Year, Month, Day, Parameter, Value"),
      tags$p("Les valeurs manquantes doivent être des cellules vides (NA)."),
      
      tags$h5(style="color:#f1f5f9; margin-top:24px", "Échelle de qualité"),
      tags$table(
        style="border-collapse:collapse; font-size:.85rem; width:100%;",
        tags$tr(
          tags$th(style="padding:6px 12px; color:#64748b; text-align:left","Niveau"),
          tags$th(style="padding:6px 12px; color:#64748b; text-align:left","% Manquant"),
          tags$th(style="padding:6px 12px; color:#64748b; text-align:left","Couleur")
        ),
        tags$tr(tags$td(style="padding:4px 12px","Complet"),   tags$td("0%"),   tags$td(tags$span(style="background:#d1fae5;padding:2px 10px;border-radius:4px;color:#064e3b","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Excellent"), tags$td("< 5%"), tags$td(tags$span(style="background:#a7f3d0;padding:2px 10px;border-radius:4px;color:#064e3b","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Bon"),       tags$td("5–10%"),tags$td(tags$span(style="background:#fef08a;padding:2px 10px;border-radius:4px;color:#713f12","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Acceptable"),tags$td("10–20%"),tags$td(tags$span(style="background:#fdba74;padding:2px 10px;border-radius:4px;color:#431407","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Dégradé"),   tags$td("20–30%"),tags$td(tags$span(style="background:#f97316;padding:2px 10px;border-radius:4px;color:#fff","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Mauvais"),   tags$td("30–50%"),tags$td(tags$span(style="background:#ef4444;padding:2px 10px;border-radius:4px;color:#fff","■"))),
        tags$tr(tags$td(style="padding:4px 12px","Critique"),  tags$td("> 50%"), tags$td(tags$span(style="background:#991b1b;padding:2px 10px;border-radius:4px;color:#fff","■")))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── Données actives ────────────────────────────────────────
  active_data <- reactiveVal(sample_data)
  
  observeEvent(input$use_sample, {
    active_data(sample_data)
    showNotification("Données exemple chargées.", type = "message", duration = 3)
  })
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    withProgress(message = "Chargement en cours...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Lecture du fichier...")
        # Lecture rapide : data.table si dispo, sinon read.csv
        df <- if (requireNamespace("data.table", quietly = TRUE)) {
          as.data.frame(data.table::fread(input$upload_file$datapath,
                                          na.strings = c("", "NA", "N/A", "na")))
        } else {
          read.csv(input$upload_file$datapath, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", "N/A", "na"))
        }
        incProgress(0.4, detail = "Vérification des colonnes...")
        need <- c("Station_type","Station_name","Year","Month","Day","Parameter","Value")
        if (!all(need %in% names(df)))
          stop("Colonnes manquantes : ", paste(setdiff(need, names(df)), collapse=", "))
        incProgress(0.3, detail = "Conversion des types...")
        df$Value <- suppressWarnings(as.numeric(df$Value))
        df$Year  <- as.integer(df$Year)
        df$Month <- as.integer(df$Month)
        df$Day   <- as.integer(df$Day)
        incProgress(0.1, detail = "Finalisation...")
        active_data(df)
        n_rows <- format(nrow(df), big.mark = " ")
        showNotification(
          paste0("✓ Fichier chargé : ", n_rows, " lignes · ",
                 length(unique(df$Station_name)), " stations · ",
                 length(unique(df$Parameter)), " paramètres"),
          type = "message", duration = 5
        )
      }, error = function(e) {
        showNotification(paste("Erreur :", e$message), type = "error", duration = 8)
      })
    })
  })
  
  # ── Mise à jour COMPLÈTE des filtres quand les données changent ──
  # Déclenché par : chargement fichier OU bouton données exemple
  observeEvent(active_data(), {
    df <- active_data()
    
    # --- Types de station ---
    types <- sort(unique(df$Station_type))
    updateSelectInput(session, "station_type",
                      choices  = c("Tous", types),
                      selected = "Tous")
    
    # --- Paramètres (onglet Analyse) ---
    params <- sort(unique(df$Parameter))
    updateSelectInput(session, "parameters",
                      choices  = params,
                      selected = params[1])
    
    # --- Slider années ---
    yr_min <- min(df$Year, na.rm = TRUE)
    yr_max <- max(df$Year, na.rm = TRUE)
    updateSliderInput(session, "year_range",
                      min   = yr_min,
                      max   = yr_max,
                      value = c(yr_min, yr_max))
    
    # --- Paramètre onglet Comparaison ---
    updateSelectInput(session, "cmp_param",
                      choices  = params,
                      selected = params[1])
    
    # --- Année onglet Comparaison ---
    years <- sort(unique(df$Year))
    updateSelectInput(session, "cmp_year",
                      choices  = years,
                      selected = max(years))
  }, ignoreInit = TRUE)
  
  # --- Stations : réagit au type sélectionné ET au changement de données ---
  observe({
    df  <- active_data()
    typ <- input$station_type
    if (!is.null(typ) && typ != "Tous") df <- df %>% filter(Station_type == typ)
    stns <- sort(unique(df$Station_name))
    updateSelectInput(session, "station_names",
                      choices  = stns,
                      selected = stns[1:min(2, length(stns))])
  })
  
  # ── Données manquantes réactives ──────────────────────────
  miss_df <- reactive({
    req(input$station_names, input$parameters, input$year_range)
    compute_missing(active_data(),
                    input$station_names, input$parameters,
                    input$year_range[1], input$year_range[2])
  })
  
  # ── KPIs ──────────────────────────────────────────────────
  output$kpi_global <- renderText({
    df <- miss_df(); req(nrow(df) > 0)
    paste0(round(mean(df$pct_miss, na.rm = TRUE), 1), "%")
  })
  output$kpi_worst_month <- renderText({
    df <- miss_df(); req(nrow(df) > 0)
    m <- df %>% group_by(Month) %>% summarise(m = mean(pct_miss)) %>% slice_max(m, n=1)
    month_labels[m$Month[1]]
  })
  output$kpi_worst_year <- renderText({
    df <- miss_df(); req(nrow(df) > 0)
    y <- df %>% group_by(Year) %>% summarise(m = mean(pct_miss)) %>% slice_max(m, n=1)
    as.character(y$Year[1])
  })
  output$kpi_complete <- renderText({
    df <- miss_df(); req(nrow(df) > 0)
    sum(df$pct_miss == 0, na.rm = TRUE)
  })
  
  # ── Heatmaps GT (une par station × paramètre) ────────────
  output$heatmap_ui <- renderUI({
    df  <- miss_df(); req(nrow(df) > 0)
    combos <- df %>% distinct(Station_name, Parameter)
    tagList(lapply(seq_len(nrow(combos)), function(i) {
      id <- paste0("gt_", i)
      tags$div(
        style = "margin-bottom:24px;",
        gt_output(id)
      )
    }))
  })
  
  observe({
    df  <- miss_df(); req(nrow(df) > 0)
    combos <- df %>% distinct(Station_name, Parameter)
    for (i in seq_len(nrow(combos))) {
      local({
        ii     <- i
        stn    <- combos$Station_name[ii]
        par    <- combos$Parameter[ii]
        out_id <- paste0("gt_", ii)
        output[[out_id]] <- render_gt({
          make_gt_heatmap(df, stn, par,
                          input$year_range[1], input$year_range[2])
        })
      })
    }
  })
  
  # ── Graphique : évolution annuelle ────────────────────────
  output$plot_trend <- renderPlot({
    df <- miss_df(); req(nrow(df) > 0)
    trend <- df %>%
      group_by(Station_name, Parameter, Year) %>%
      summarise(pct = mean(pct_miss, na.rm = TRUE), .groups = "drop") %>%
      mutate(combo = paste0(Station_name, " · ", Parameter))
    
    ggplot(trend, aes(Year, pct, color = combo, group = combo)) +
      geom_line(linewidth = .9, alpha = .85) +
      geom_point(size = 2.2) +
      geom_hline(yintercept = c(5, 10, 20), linetype = "dashed",
                 color = "#475569", linewidth = .4) +
      annotate("text", x = min(trend$Year) + .2, y = c(5.8, 10.8, 20.8),
               label = c("Seuil 5%","Seuil 10%","Seuil 20%"),
               color = "#64748b", size = 2.8, hjust = 0) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"),
                         limits = c(0, NA)) +
      scale_color_brewer(palette = "Set2") +
      labs(title    = "Évolution annuelle du % de données manquantes",
           subtitle = "Moyenne sur les 12 mois de l'année",
           x = NULL, y = "% Manquant", color = "Station · Paramètre") +
      theme_clima() +
      theme(legend.position = "bottom",
            legend.key.size  = unit(.7, "lines"))
  }, bg = "#1e293b")
  
  # ── Graphique : profil mensuel moyen ─────────────────────
  output$plot_monthly <- renderPlot({
    df <- miss_df(); req(nrow(df) > 0)
    monthly <- df %>%
      group_by(Station_name, Parameter, Month) %>%
      summarise(pct = mean(pct_miss, na.rm = TRUE), .groups = "drop") %>%
      mutate(Month_lbl = factor(month_labels[Month], levels = month_labels),
             combo = paste0(Station_name, " · ", Parameter))
    
    ggplot(monthly, aes(Month_lbl, pct, fill = pct)) +
      geom_col(color = "#0f172a", linewidth = .3) +
      scale_fill_gradientn(colors = PALETTE, limits = c(0, 100),
                           labels = function(x) paste0(x, "%"),
                           name   = "% Manquant") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      facet_wrap(~combo, ncol = 2) +
      labs(title    = "Profil mensuel moyen du % de données manquantes",
           subtitle = paste0("Période ", input$year_range[1], " – ", input$year_range[2]),
           x = NULL, y = "% Manquant") +
      theme_clima() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  }, bg = "#1e293b")
  
  # ── Tableau indicateurs qualité ───────────────────────────
  output$quality_table <- render_gt({
    df <- miss_df(); req(nrow(df) > 0)
    
    qual <- df %>%
      group_by(Station = Station_name, Parametre = Parameter) %>%
      summarise(
        Moy_pct    = round(mean(pct_miss, na.rm = TRUE), 1),
        Max_pct    = round(max(pct_miss,  na.rm = TRUE), 1),
        n_total    = n(),
        n_complet  = sum(pct_miss == 0, na.rm = TRUE),
        n_bon      = sum(pct_miss < 10, na.rm = TRUE),
        n_critique = sum(pct_miss >= 50, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        Qualite       = quality_label(Moy_pct),
        `% Complets`  = round(100 * n_complet  / n_total),
        `% Critiques` = round(100 * n_critique / n_total)
      ) %>%
      arrange(desc(Moy_pct)) %>%
      select(Station, Parametre,
             `Moy. %` = Moy_pct,
             `Max %`  = Max_pct,
             Qualite,
             `% Complets`, `% Critiques`)
    
    qual %>%
      gt() %>%
      data_color(columns = `Moy. %`, method = "numeric",
                 palette = PALETTE, domain = c(0, 100)) %>%
      data_color(columns = `Max %`,  method = "numeric",
                 palette = PALETTE, domain = c(0, 100)) %>%
      data_color(columns = `% Critiques`, method = "numeric",
                 palette = c("#22c55e","#fef08a","#ef4444"), domain = c(0, 100)) %>%
      data_color(columns = `% Complets`,  method = "numeric",
                 palette = c("#ef4444","#fef08a","#22c55e"), domain = c(0, 100)) %>%
      fmt_number(columns = c(`Moy. %`, `Max %`),
                 decimals = 1, pattern = "{x}%") %>%
      fmt_number(columns = c(`% Complets`, `% Critiques`),
                 decimals = 0, pattern = "{x}%") %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_body(columns = Qualite)) %>%
      tab_header(
        title    = md("**Indicateurs de Qualité** par Station et Paramètre"),
        subtitle = md(paste0("Période ", input$year_range[1],
                             " – ", input$year_range[2]))
      ) %>%
      tab_style(style = list(cell_fill("#1e293b"),
                             cell_text(color="#f1f5f9", weight="bold")),
                locations = cells_column_labels()) %>%
      tab_style(style = list(cell_fill("#0f172a"),
                             cell_text(color="#38bdf8", size=px(15))),
                locations = cells_title("title")) %>%
      tab_style(style = list(cell_fill("#0f172a"),
                             cell_text(color="#64748b", size=px(12))),
                locations = cells_title("subtitle")) %>%
      tab_style(style = cell_borders(sides = "all", color = "#334155",
                                     style = "solid", weight = px(1)),
                locations = cells_body()) %>%
      tab_options(
        table.background.color            = "#1e293b",
        table.border.top.color            = "#334155",
        table.border.bottom.color         = "#334155",
        column_labels.border.bottom.color = "#475569",
        table.font.names  = "Inter",
        table.font.size   = px(13),
        heading.padding   = px(12),
        data_row.padding  = px(7)
      )
  })
  
  # ── Fiche Station ─────────────────────────────────────────
  output$fiche_ui <- renderUI({
    req(input$station_names, input$parameters)
    data <- active_data()
    
    # Une fiche par combinaison station × paramètre
    combos <- expand.grid(
      station   = input$station_names,
      parameter = input$parameters,
      stringsAsFactors = FALSE
    )
    
    tagList(lapply(seq_len(nrow(combos)), function(i) {
      fi <- compute_station_fiche(data, combos$station[i], combos$parameter[i])
      if (is.null(fi)) return(NULL)
      
      # Couleur gap
      gap_class <- if (fi$gap_days == 0) "ok" else if (fi$gap_days > 30) "" else "ok"
      gap_class <- ifelse(fi$gap_days > 60, "", ifelse(fi$gap_days > 0, "ok", "ok"))
      gap_class <- dplyr::case_when(
        fi$gap_days == 0  ~ "ok",
        fi$gap_days <= 7  ~ "ok",
        fi$gap_days <= 30 ~ "",
        TRUE              ~ ""
      )
      
      # Qualité globale
      qual_color <- dplyr::case_when(
        fi$pct_global == 0  ~ "success",
        fi$pct_global < 10  ~ "success",
        fi$pct_global < 20  ~ "warn",
        TRUE                ~ "danger"
      )
      
      tags$div(
        style = "margin-bottom:32px;",
        
        # En-tête fiche
        tags$div(
          style = paste0(
            "background:linear-gradient(135deg,#162032,#0f172a);",
            "border:1px solid #334155; border-radius:12px;",
            "padding:16px 20px; margin-bottom:14px;"
          ),
          tags$div(style="display:flex; justify-content:space-between; align-items:flex-start;",
                   tags$div(
                     tags$div(style="font-size:1.1rem; font-weight:700; color:#f1f5f9;",
                              fi$station),
                     tags$div(style="font-size:.82rem; color:#38bdf8; margin-top:2px;",
                              icon("chart-line"), " ", fi$parameter)
                   ),
                   tags$div(
                     tags$span(class=paste("fiche-value", qual_color),
                               style="font-size:1.5rem;",
                               paste0(fi$pct_global, "%")),
                     tags$div(style="font-size:.65rem; color:#64748b; text-align:right;",
                              "MANQUANT GLOBAL")
                   )
          )
        ),
        
        # Grille 2 colonnes
        tags$div(class = "fiche-grid",
                 
                 # Carte 1 : Chronologie
                 tags$div(class = "fiche-card",
                          tags$div(class="fiche-card-title", icon("calendar"), " Chronologie"),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Début série"),
                                   tags$span(class="fiche-value accent", fi$date_debut_serie)),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Fin série"),
                                   tags$span(class="fiche-value accent", fi$date_fin_serie)),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Durée totale"),
                                   tags$span(class="fiche-value", paste0(fi$n_jours_serie, " jours"))),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "1ère mesure valide"),
                                   tags$span(class="fiche-value success", fi$date_first_obs)),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Dernière mesure valide"),
                                   tags$span(class="fiche-value success", fi$date_last_obs))
                 ),
                 
                 # Carte 2 : Valeurs extrêmes
                 tags$div(class = "fiche-card",
                          tags$div(class="fiche-card-title", icon("chart-simple"), " Valeurs Extrêmes"),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Minimum"),
                                   tags$span(class="fiche-value",
                                             if (!is.na(fi$val_min)) fi$val_min else "—")),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Date du minimum"),
                                   tags$span(class="fiche-value", fi$date_min)),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Maximum"),
                                   tags$span(class="fiche-value warn",
                                             if (!is.na(fi$val_max)) fi$val_max else "—")),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Date du maximum"),
                                   tags$span(class="fiche-value", fi$date_max)),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Moyenne"),
                                   tags$span(class="fiche-value",
                                             if (!is.na(fi$val_mean)) fi$val_mean else "—")),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Écart-type"),
                                   tags$span(class="fiche-value",
                                             if (!is.na(fi$val_sd)) fi$val_sd else "—"))
                 ),
                 
                 # Carte 3 : Lacunes
                 tags$div(class = "fiche-card",
                          tags$div(class="fiche-card-title", icon("triangle-exclamation"),
                                   " Plus Longue Lacune"),
                          tags$div(style="text-align:center; padding:10px 0 6px;",
                                   tags$span(class=paste("gap-badge", gap_class),
                                             paste0(fi$gap_days, " jour", if(fi$gap_days > 1) "s" else ""))
                          ),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Début lacune"),
                                   tags$span(class="fiche-value danger",
                                             if (!is.na(fi$gap_start)) fi$gap_start else "—")),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Fin lacune"),
                                   tags$span(class="fiche-value danger",
                                             if (!is.na(fi$gap_end)) fi$gap_end else "—"))
                 ),
                 
                 # Carte 4 : Statistiques données
                 tags$div(class = "fiche-card",
                          tags$div(class="fiche-card-title", icon("database"), " Disponibilité"),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Total enregistrements"),
                                   tags$span(class="fiche-value", format(fi$n_total, big.mark=" "))),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Valeurs présentes"),
                                   tags$span(class="fiche-value success",
                                             format(fi$n_total - fi$n_missing, big.mark=" "))),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "Valeurs manquantes"),
                                   tags$span(class=paste("fiche-value", qual_color),
                                             format(fi$n_missing, big.mark=" "))),
                          tags$div(class="fiche-row",
                                   tags$span(class="fiche-label", "% Manquant global"),
                                   tags$span(class=paste("fiche-value", qual_color),
                                             paste0(fi$pct_global, "%")))
                 )
        ),
        
        # Tableau annuel compact
        tags$div(class="fiche-section-title",
                 icon("table"), " Répartition annuelle des données manquantes"),
        tags$div(
          style="overflow-x:auto;",
          tags$table(
            style=paste0(
              "width:100%; border-collapse:collapse; font-size:.8rem;",
              "background:#0f172a; border-radius:8px; overflow:hidden;"
            ),
            tags$thead(
              tags$tr(
                lapply(c("Année","Total","Manquants","% Manquant","Qualité"), function(h) {
                  tags$th(style=paste0(
                    "padding:8px 12px; text-align:left; color:#64748b;",
                    "background:#162032; font-weight:600; font-size:.72rem;",
                    "text-transform:uppercase; letter-spacing:.05em;"), h)
                })
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(fi$annual)), function(j) {
                row   <- fi$annual[j, ]
                qual  <- quality_label(row$pct)
                bg_color <- dplyr::case_when(
                  row$pct == 0  ~ "#d1fae5",
                  row$pct < 5   ~ "#a7f3d0",
                  row$pct < 10  ~ "#fef08a",
                  row$pct < 20  ~ "#fdba74",
                  row$pct < 30  ~ "#f97316",
                  row$pct < 50  ~ "#ef4444",
                  TRUE          ~ "#991b1b"
                )
                txt_color <- if (row$pct >= 20) "#ffffff" else "#1f2937"
                tags$tr(
                  style = if (j %% 2 == 0) "background:#162032;" else "background:#0f172a;",
                  tags$td(style="padding:6px 12px; color:#94a3b8; font-weight:600;",
                          row$Year),
                  tags$td(style="padding:6px 12px; color:#64748b;",
                          format(row$n_tot, big.mark=" ")),
                  tags$td(style="padding:6px 12px; color:#94a3b8;",
                          format(row$n_miss, big.mark=" ")),
                  tags$td(style="padding:6px 12px;",
                          tags$span(
                            style=paste0(
                              "background:", bg_color, "; color:", txt_color, ";",
                              "padding:2px 8px; border-radius:4px; font-weight:700;"
                            ),
                            paste0(row$pct, "%")
                          )
                  ),
                  tags$td(style=paste0("padding:6px 12px; color:", txt_color,
                                       "; font-weight:600;"), qual)
                )
              })
            )
          )
        ),
        tags$hr(style="border-color:#334155; margin:24px 0;")
      )
    }))
  })
  
  # ── Comparaison multi-stations ────────────────────────────
  cmp_df <- reactive({
    req(input$cmp_param, input$cmp_year)
    active_data() %>%
      filter(Parameter == input$cmp_param, Year == as.integer(input$cmp_year)) %>%
      group_by(Station_type, Station_name, Month) %>%
      summarise(pct_miss = round(100 * sum(is.na(Value)) / n(), 1), .groups = "drop") %>%
      mutate(Month_lbl = month_labels[Month]) %>%
      pivot_wider(names_from = Month_lbl, values_from = pct_miss, names_expand = FALSE)
  })
  
  output$compare_table <- render_gt({
    df <- cmp_df(); req(nrow(df) > 0)
    pm <- month_labels[month_labels %in% names(df)]
    df2 <- df %>%
      select(Station_type, Station_name, all_of(pm)) %>%
      mutate(Moy = round(rowMeans(select(., all_of(pm)), na.rm = TRUE), 1)) %>%
      arrange(Station_type, Station_name)
    
    df2 %>%
      gt(groupname_col = "Station_type", rowname_col = "Station_name") %>%
      data_color(columns = c(all_of(pm), "Moy"), method = "numeric",
                 palette = PALETTE, domain = c(0, 100), na_color = "#e5e7eb") %>%
      fmt_number(columns = c(all_of(pm), "Moy"),
                 decimals = 1, pattern = "{x}%") %>%
      sub_missing(missing_text = "—") %>%
      cols_width(Station_name ~ px(155), Moy ~ px(68), everything() ~ px(58)) %>%
      tab_style(style = list(cell_fill("#162032"),
                             cell_text(color="#38bdf8", weight="bold")),
                locations = cells_row_groups()) %>%
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_body(columns = "Moy")) %>%
      tab_header(
        title    = md(paste0("Comparaison — **", input$cmp_param, "**")),
        subtitle = md(paste0("Année ", input$cmp_year,
                             " · % données manquantes par mois"))
      ) %>%
      tab_style(style = list(cell_fill("#1e293b"),
                             cell_text(color="#f1f5f9", weight="bold")),
                locations = cells_column_labels()) %>%
      tab_style(style = list(cell_fill("#0f172a"),
                             cell_text(color="#38bdf8", size=px(15))),
                locations = cells_title("title")) %>%
      tab_style(style = list(cell_fill("#0f172a"),
                             cell_text(color="#64748b", size=px(12))),
                locations = cells_title("subtitle")) %>%
      tab_style(style = list(cell_fill("#0f172a"),
                             cell_text(color="#94a3b8")),
                locations = cells_stub()) %>%
      tab_style(style = cell_borders(sides = "all", color = "#334155",
                                     style = "solid", weight = px(1)),
                locations = cells_body()) %>%
      tab_options(
        table.background.color            = "#1e293b",
        table.border.top.color            = "#334155",
        table.border.bottom.color         = "#334155",
        column_labels.border.bottom.color = "#475569",
        stub.border.color  = "#334155",
        table.font.names   = "Inter",
        table.font.size    = px(13),
        heading.padding    = px(12),
        data_row.padding   = px(6),
        row_group.padding  = px(8)
      )
  })
  
  output$compare_plot <- renderPlot({
    df <- cmp_df(); req(nrow(df) > 0)
    pm <- month_labels[month_labels %in% names(df)]
    long <- df %>%
      select(Station_type, Station_name, all_of(pm)) %>%
      pivot_longer(all_of(pm), names_to = "Mois", values_to = "pct") %>%
      mutate(Mois = factor(Mois, levels = month_labels))
    
    ggplot(long, aes(Mois, pct, fill = pct)) +
      geom_col(color = "#0f172a", linewidth = .3) +
      scale_fill_gradientn(colors = PALETTE, limits = c(0, 100),
                           labels = function(x) paste0(x, "%"),
                           name   = "% Manquant") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      facet_wrap(~Station_name, ncol = 4) +
      labs(title    = paste0("Profil mensuel — ", input$cmp_param,
                             " (", input$cmp_year, ")"),
           subtitle = "% de données manquantes par mois",
           x = NULL, y = "% Manquant") +
      theme_clima() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            legend.position = "right")
  }, bg = "#1e293b")
  
  # ── Exports ───────────────────────────────────────────────
  export_wide <- reactive({
    df <- miss_df(); req(nrow(df) > 0)
    df %>%
      mutate(Month_lbl = month_labels[Month]) %>%
      select(-Month) %>%
      pivot_wider(names_from = Month_lbl,
                  values_from = c(pct_miss, n_total, n_missing),
                  names_sep   = "_") %>%
      arrange(Station_name, Parameter, Year)
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("manquants_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(export_wide(), f, row.names = FALSE)
  )
  
  output$dl_excel <- downloadHandler(
    filename = function() paste0("manquants_", Sys.Date(), ".xlsx"),
    content  = function(f) {
      writexl::write_xlsx(
        list(
          `Résumé (large)`  = export_wide(),
          `Détail (long)`   = miss_df(),
          `Qualité`         = miss_df() %>%
            group_by(Station = Station_name, Parametre = Parameter) %>%
            summarise(Moy_pct = round(mean(pct_miss),1),
                      Max_pct = round(max(pct_miss),1),
                      Qualite = quality_label(round(mean(pct_miss),1)),
                      .groups="drop")
        ),
        path = f
      )
    }
  )
  
  output$dl_png <- downloadHandler(
    filename = function() paste0("heatmap_", Sys.Date(), ".png"),
    content  = function(f) {
      df  <- miss_df(); req(nrow(df) > 0)
      stn <- input$station_names[1]
      par <- input$parameters[1]
      sub <- df %>% filter(Station_name == stn, Parameter == par)
      
      p <- ggplot(
        sub %>% mutate(Month_lbl = factor(month_labels[Month], levels = month_labels)),
        aes(Month_lbl, factor(Year), fill = pct_miss)
      ) +
        geom_tile(color = "#0f172a", linewidth = .5) +
        scale_fill_gradientn(colors = PALETTE, limits = c(0, 100),
                             labels = function(x) paste0(x, "%"),
                             name   = "% Manquant",
                             na.value = "#334155") +
        geom_text(aes(label = ifelse(is.na(pct_miss), "—",
                                     paste0(pct_miss, "%"))),
                  color = "white", size = 2.6, fontface = "bold") +
        labs(title    = paste0(stn, " — ", par),
             subtitle = paste0("% données manquantes · ",
                               input$year_range[1], "–", input$year_range[2]),
             x = NULL, y = "Année") +
        theme_clima() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(f, p, width = 14, height = 7, dpi = 150, bg = "#1e293b")
    }
  )
  
  output$dl_cmp_csv <- downloadHandler(
    filename = function() paste0("comparaison_", input$cmp_year, "_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(cmp_df(), f, row.names = FALSE)
  )
  
  output$dl_cmp_excel <- downloadHandler(
    filename = function() paste0("comparaison_", input$cmp_year, "_", Sys.Date(), ".xlsx"),
    content  = function(f) writexl::write_xlsx(cmp_df(), path = f)
  )
}

# ============================================================
shinyApp(ui = ui, server = server)