# ========================================================================
# COMPARISON ANALYSIS - MULTIPLE STEEL TYPES
# ========================================================================
#
# DESCRIPTION:
#   Comprehensive analysis and comparison tool for inclusion data from 
#   multiple steel types. Creates extensive visualizations including violin 
#   plots, box plots, bar plots, histograms, stacked bar charts, heatmaps, 
#   and scatter plots. All plots are generated in both log scale and normal 
#   scale versions for comprehensive analysis.
#
# FEATURES:
#   - Multi-steel type comparison and analysis
#   - Statistical analysis (ANOVA, Tukey HSD, Kruskal-Wallis tests)
#   - Comprehensive visualization suite (7 different plot types)
#   - Dual scale plotting (log and normal scales)
#   - Automatic file discovery and processing
#   - High-resolution PNG export with timestamps
#   - Excel output with multiple analysis sheets
#   - Configurable plot settings and color palettes
#
# VISUALIZATIONS:
#   1. Violin Plots - Distribution of size, area, circularity, aspect ratio
#   2. Box Plots - Comparison by chemical and morphological classes
#   3. Bar Plots - Inclusion counts and average sizes
#   4. Histograms - Size and circularity distributions
#   5. Stacked Bar Charts - Chemical classes, morphology, size categories
#   6. Heatmaps - Chemical classes by steel type
#   7. Scatter Plots - Parameter correlations
#
# STATISTICAL ANALYSES:
#   - Summary statistics by steel type
#   - ANOVA tests for differences between groups
#   - Tukey HSD post-hoc analysis
#   - Kruskal-Wallis non-parametric tests
#
# USAGE:
#   # Quick start (recommended)
#   source("comparison_analysis_multipleplots.r")
#   
#   # The script will automatically:
#   # - Search for files with pattern "klasificirani_vkljucki_*.xlsx"
#   # - Create steel type names from file names
#   # - Perform analysis and create plots
#
# INPUT REQUIREMENTS:
#   - Excel files with classification results for each steel type
#   - Files should be in the same directory
#   - Each file must contain a "Data" sheet with classified data
#   - Required columns: steel_type, kem_klasa, morph_shape, ECD_um, etc.
#
# OUTPUT:
#   - Excel file with combined data and summaries
#   - PNG files for all visualizations (timestamped)
#   - Automatic output directory creation
#   - High-resolution plots (600 DPI)
#
# DEPENDENCIES:
#   - tidyverse: Data manipulation and visualization
#   - readxl/writexl: Excel file handling
#   - ggplot2: Advanced plotting
#   - gridExtra: Plot arrangement
#   - RColorBrewer: Color palettes
#   - scales: Scale functions
#   - pheatmap: Heatmap creation
#
# AUTHOR: Vid Kuder Marušič
# EMAIL: vidkm30@gmail.com
# REPOSITORY: PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
# DATE: 2025
# VERSION: 1.0.0
# COMPATIBILITY: R 3.6.0 or later
#
# EXAMPLE:
#   # Place your Excel files in the working directory
#   # Files should be named: klasificirani_vkljucki_SteelType1.xlsx, etc.
#   source("comparison_analysis_multipleplots.r")
#
# TROUBLESHOOTING:
#   - Ensure Excel files contain "Data" sheet
#   - Check that required columns are present
#   - Verify file naming pattern matches expectations
#   - Check write permissions for output directory
#
# ========================================================================
#
# Loading required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(gridExtra)
  library(RColorBrewer)
  library(scales)
  library(ggExtra)
  library(cowplot)
})

options(dplyr.summarise.inform = FALSE)


# ---- Pomožne funkcije za elemente ----
# Funkcija za identifikacijo elementov v imenih stolpcev
identify_elements <- function(column_names) {
  # Poišči vse elemente, ki so prisotni v imenih stolpcev
  # Uporabi regex za iskanje kemijskih elementov (velika črka + opcijsko mala črka)
  element_pattern <- "\\b[A-Z][a-z]?\\b"
  found_elements <- unique(str_extract_all(column_names, element_pattern, simplify = TRUE))
  found_elements <- found_elements[found_elements != ""]
  return(found_elements)
}

# Funkcija za pridobitev stolpcev z elementi
get_element_columns <- function(data, prefix = "") {
  if (prefix != "") {
    pattern <- paste0("^", prefix, ".*")
    element_cols <- names(data)[grepl(pattern, names(data))]
  } else {
    # Poišči stolpce, ki vsebujejo kemijske elemente
    element_pattern <- "\\b[A-Z][a-z]?\\b"
    element_cols <- names(data)[grepl(element_pattern, names(data))]
  }
  return(element_cols)
}

# ---- Pomožne funkcije za lokalizacijo ----
# Funkcija za prevod morfologije v slovenščino
translate_morphology <- function(morph_vector) {
  morph_translations <- c(
    "spherical" = "Sferični",
    "globular" = "Globularni", 
    "elongated" = "Podolgovati",
    "stringer" = "Špinel",
    "plate-like" = "Ploščati",
    "polyhedral" = "Polieder",
    "irregular" = "Nepravilni",
    "oversize_DS" = "Preveliki DS"
  )
  
  # Prevedi vrednosti
  translated <- morph_translations[morph_vector]
  # Če ni prevoda, obdrži originalno vrednost
  translated[is.na(translated)] <- morph_vector[is.na(translated)]
  return(translated)
}

# Funkcija za zamenjavo (Wt%) z (mas. %)
translate_wt_percent <- function(text) {
  gsub("\\(Wt%\\)", "(mas. %)", text)
}

# ---- Nastavitve ----
# Pot do datotek (prilagodite glede na vašo strukturo)
base_path <- "C:/ ..."

# Pot za izhodne datoteke (prilagodite glede na vaše potrebe)
output_dir <- "C: ..."

# Ustvari izhodno mapo, če ne obstaja
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("✅ Ustvarjena izhodna mapa:", output_dir, "\n")
}

# Samodejno poišči datoteke z vzorcem "enostavna_klasifikacija_"
cat("=== SAMODEJNO ISKANJE DATOTEK ===\n")
datoteke <- list.files(path = base_path, 
                       pattern = "enostavna_klasifikacija_.*\\.xlsx", 
                       full.names = FALSE)

if (length(datoteke) == 0) {
  stop("Ni najdenih datotek z vzorcem 'enostavna_klasifikacija_*.xlsx' v mapi: ", base_path)
}

cat("Najdene datoteke:\n")
for (i in seq_along(datoteke)) {
  cat(i, ":", datoteke[i], "\n")
}

# Samodejno ustvari imena tipov jekla iz imen datotek
steel_types <- gsub("enostavna_klasifikacija_(.*)_2025\\..*", "\\1", datoteke)

# Če vzorec ne deluje, uporabi številke datotek
if (any(steel_types == datoteke)) {
  steel_types <- gsub("enostavna_klasifikacija_(.*)_2025\\..*", "Steel_\\1", datoteke)
  # Če še vedno ne deluje, uporabi zaporedne številke
  if (any(steel_types == datoteke)) {
    steel_types <- paste0("Steel_Type_", seq_along(datoteke))
  }
}

cat("\nUstvarjena imena tipov jekla:\n")
for (i in seq_along(steel_types)) {
  cat(i, ":", steel_types[i], "\n")
}

# ---- Funkcije za branje podatkov ----
read_classification_data <- function(file_path, steel_type) {
  # Preveri, ali datoteka obstaja
  if (!file.exists(file_path)) {
    cat("Datoteka ne obstaja:", file_path, "\n")
    return(NULL)
  }
  
  # Preberi podatke iz Data lista
  tryCatch({
    df <- read_xlsx(file_path, sheet = "Data")
    df$steel_type <- steel_type
    df$file_id <- basename(file_path)
    cat("Uspešno prebrano:", steel_type, "-", nrow(df), "vključkov\n")
    return(df)
  }, error = function(e) {
    cat("Napaka pri branju:", file_path, "-", e$message, "\n")
    return(NULL)
  })
}

# ---- Branje vseh podatkov ----
cat("=== BRANJE PODATKOV ===\n")
all_data <- list()

for (i in seq_along(datoteke)) {
  file_path <- file.path(base_path, datoteke[i])
  steel_type <- steel_types[i]
  
  data <- read_classification_data(file_path, steel_type)
  if (!is.null(data)) {
    all_data[[steel_type]] <- data
  }
}

# Združi vse podatke
combined_data <- bind_rows(all_data, .id = "steel_type")

if (nrow(combined_data) == 0) {
  stop("Ni podatkov za analizo!")
}

cat("Skupaj vključkov:", nrow(combined_data), "\n")
cat("Tipi jekla:", length(unique(combined_data$steel_type)), "\n")

# ---- Priprava podatkov za analizo ----
combined_data <- combined_data %>%
  mutate(
    # Čiščenje podatkov
    steel_type = factor(steel_type),
    kem_klasa = factor(kem_klasa),
    morph_shape = factor(morph_shape),
    morph_ISO = factor(morph_ISO),
    
    # Dodaj slovenske prevode morfologije
    morph_shape_slo = factor(translate_morphology(morph_shape)),
    
    # Velikostne kategorije
    size_category = case_when(
      ECD_um <= 2 ~ "≤2 µm",
      ECD_um <= 5 ~ "2-5 µm", 
      ECD_um <= 10 ~ "5-10 µm",
      ECD_um <= 20 ~ "10-20 µm",
      ECD_um > 20 ~ ">20 µm",
      TRUE ~ "Unknown"
    ),
    size_category = factor(size_category, levels = c("≤2 µm", "2-5 µm", "5-10 µm", "10-20 µm", ">20 µm", "Unknown")),
    
    # Logaritmične transformacije za boljšo vizualizacijo
    log_area = log10(pmax(`Area (sq. µm)`, 0.1)),
    log_ecd = log10(pmax(ECD_um, 0.1)),
    log_length = log10(pmax(`Length (µm)`, 0.1))
  )

# ---- 1. VIOLIN PLOTS ----
cat("\n=== USTVARJANJE VIOLIN PLOTS ===\n")

# Ustvari timestamp za imena datotek
timestamp <- format(Sys.time(), "%Y.%m.%d.%H.%M.%S")
cat("Timestamp za datoteke:", timestamp, "\n")

# Preveri prisotnost stolpcev
cat("\n=== PREVERJANJE STOLPCEV ===\n")
required_columns <- c("steel_type", "ECD_um", "Circularity", "AR", "Area (sq. µm)", "kem_klasa", "morph_shape")
missing_columns <- required_columns[!required_columns %in% names(combined_data)]
if (length(missing_columns) > 0) {
  cat("❌ Manjkajo stolpci:", paste(missing_columns, collapse = ", "), "\n")
} else {
  cat("✅ Vsi potrebni stolpci so prisotni\n")
}

# Preveri podatke
cat("Skupaj vrstic:", nrow(combined_data), "\n")
cat("Tipi jekla:", length(unique(combined_data$steel_type)), "\n")
cat("Kemijski razredi:", length(unique(combined_data$kem_klasa)), "\n")
cat("Morfološki razredi:", length(unique(combined_data$morph_shape)), "\n")

# Diagnostika krožnosti
if ("Circularity" %in% names(combined_data)) {
  circularity_stats <- combined_data %>%
    summarise(
      min_circularity = min(Circularity, na.rm = TRUE),
      max_circularity = max(Circularity, na.rm = TRUE),
      mean_circularity = mean(Circularity, na.rm = TRUE),
      problematic_count = sum(Circularity > 1, na.rm = TRUE),
      problematic_percentage = round(sum(Circularity > 1, na.rm = TRUE) / n() * 100, 2)
    )
  
  cat("\n=== DIAGNOSTIKA KROŽNOSTI ===\n")
  cat("Min krožnost:", round(circularity_stats$min_circularity, 3), "\n")
  cat("Max krožnost:", round(circularity_stats$max_circularity, 3), "\n")
  cat("Povprečna krožnost:", round(circularity_stats$mean_circularity, 3), "\n")
  cat("Problematični vključki (>1):", circularity_stats$problematic_count, "\n")
  cat("Odstotek problematičnih:", circularity_stats$problematic_percentage, "%\n")
  
  if (circularity_stats$problematic_count > 0) {
    cat("⚠️  OPOZORILO: Najdeni vključki s krožnostjo > 1!\n")
    cat("   To kaže na probleme s kakovostjo podatkov (Perimeter/Area).\n")
  }
}

# ---- 1.1. VIOLIN PLOTS - LOG SKALA ----
cat("\n=== USTVARJANJE VIOLIN PLOTS - LOG SKALA ===\n")

# Violin plot za velikost (ECD) - LOG SKALA
p1_log <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija velikosti vključkov (ECD) - LOG SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za površino - LOG SKALA
p2_log <- ggplot(combined_data, aes(x = steel_type, y = `Area (sq. µm)`, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija površine vključkov - LOG SKALA",
       x = "Vrsta jekla", y = "Površina (µm²)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za krožnost - LOG SKALA
p3_log <- ggplot(combined_data, aes(x = steel_type, y = Circularity, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija krožnosti vključkov - LOG SKALA",
       x = "Vrsta jekla", y = "Krožnost", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za aspect ratio - LOG SKALA
p4_log <- ggplot(combined_data, aes(x = steel_type, y = AR, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija razmerja stranic (AR) - LOG SKALA",
       x = "Vrsta jekla", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Shrani violin plots - LOG SKALA
cat("Shranjujem violin plots - LOG SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("violin_plots_combined_LOG_", timestamp, ".png")), 
         grid.arrange(p1_log, p2_log, p3_log, p4_log, ncol = 2), 
         width = 18, height = 14, dpi = 300)
  cat("✅ Violin plots - LOG SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju violin plots - LOG SKALA:", e$message, "\n")
})

# ---- 1.2. VIOLIN PLOTS - NORMALNA SKALA ----
cat("\n=== USTVARJANJE VIOLIN PLOTS - NORMALNA SKALA ===\n")

# Violin plot za velikost (ECD) - NORMALNA SKALA
p1_normal <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(title = "Distribucija velikosti vključkov (ECD) - NORMALNA SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za površino - NORMALNA SKALA
p2_normal <- ggplot(combined_data, aes(x = steel_type, y = `Area (sq. µm)`, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(title = "Distribucija površine vključkov - NORMALNA SKALA",
       x = "Vrsta jekla", y = "Površina (µm²)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za krožnost - NORMALNA SKALA
p3_normal <- ggplot(combined_data, aes(x = steel_type, y = Circularity, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(title = "Distribucija krožnosti vključkov - NORMALNA SKALA",
       x = "Vrsta jekla", y = "Krožnost", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Violin plot za aspect ratio - NORMALNA SKALA
p4_normal <- ggplot(combined_data, aes(x = steel_type, y = AR, fill = steel_type)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(title = "Distribucija razmerja stranic (AR) - NORMALNA SKALA",
       x = "Vrsta jekla", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Shrani violin plots - NORMALNA SKALA
cat("Shranjujem violin plots - NORMALNA SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("violin_plots_combined_NORMAL_", timestamp, ".png")), 
         grid.arrange(p1_normal, p2_normal, p3_normal, p4_normal, ncol = 2), 
         width = 18, height = 14, dpi = 300)
  cat("✅ Violin plots - NORMALNA SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju violin plots - NORMALNA SKALA:", e$message, "\n")
})
# ---- 2. BOX PLOTS ----
cat("\n=== USTVARJANJE BOX PLOTS ===\n")

# ---- 2.1. BOX PLOTS - LOG SKALA ----
cat("\n=== USTVARJANJE BOX PLOTS - LOG SKALA ===\n")

# Box plot za kemijske klase - LOG SKALA
p5_log <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = kem_klasa)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Velikost vključkov po kemijskih razredih - LOG SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Kemijski razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot za morfološke klase - LOG SKALA
p6_log <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = morph_ISO)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Velikost vključkov po morfologiji - LOG SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Morfologija") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Shranjujem box plots - LOG SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("box_plots_combined_LOG_", timestamp, ".png")), 
         grid.arrange(p5_log, p6_log, ncol = 1), 
         width = 12, height = 10, dpi = 300)
  cat("✅ Box plots - LOG SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju box plots - LOG SKALA:", e$message, "\n")
})

# ---- 2.2. BOX PLOTS - NORMALNA SKALA ----
cat("\n=== USTVARJANJE BOX PLOTS - NORMALNA SKALA ===\n")

# Box plot za kemijske klase - NORMALNA SKALA
p5_normal <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = kem_klasa)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Velikost vključkov po kemijskih razredih - NORMALNA SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Kemijski razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot za morfološke klase - NORMALNA SKALA
p6_normal <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = morph_ISO)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Velikost vključkov po morfologiji - NORMALNA SKALA",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Morfologija") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Shranjujem box plots - NORMALNA SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("box_plots_combined_NORMAL_", timestamp, ".png")), 
         grid.arrange(p5_normal, p6_normal, ncol = 1), 
         width = 12, height = 10, dpi = 300)
  cat("✅ Box plots - NORMALNA SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju box plots - NORMALNA SKALA:", e$message, "\n")
})

# ---- 3. BAR PLOTS ----
cat("\n=== USTVARJANJE BAR PLOTS ===\n")

# Število vključkov po tipih jekla
count_by_steel <- combined_data %>%
  count(steel_type) %>%
  arrange(desc(n))

p7 <- ggplot(count_by_steel, aes(x = reorder(steel_type, n), y = n, fill = steel_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = n), hjust = -0.1) +
  coord_flip() +
  labs(title = "Število vključkov po tipih jekla",
       x = "Vrsta jekla", y = "Število vključkov") +
  theme_minimal() +
  theme(legend.position = "none")

# Povprečna velikost po tipih jekla
mean_size_by_steel <- combined_data %>%
  group_by(steel_type) %>%
  summarise(mean_ecd = mean(ECD_um, na.rm = TRUE),
            median_ecd = median(ECD_um, na.rm = TRUE),
            .groups = "drop")

# Povprečna površina po tipih jekla
mean_area_by_steel <- combined_data %>%
  group_by(steel_type) %>%
  summarise(mean_area = mean(`Area (sq. µm)`, na.rm = TRUE),
            median_area = median(`Area (sq. µm)`, na.rm = TRUE),
            .groups = "drop")

# ---- 3.1. BAR PLOTS - LOG SKALA ----
cat("\n=== USTVARJANJE BAR PLOTS - LOG SKALA ===\n")

p8_log <- ggplot(mean_size_by_steel, aes(x = reorder(steel_type, mean_ecd), y = mean_ecd, fill = steel_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(mean_ecd, 1)), hjust = -0.1) +
  coord_flip() +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Povprečna velikost vključkov po tipih jekla - LOG SKALA",
       x = "Vrsta jekla", y = "Povprečni ECD (µm)") +
  theme_minimal() +
  theme(legend.position = "none")

p8_area_log <- ggplot(mean_area_by_steel, aes(x = reorder(steel_type, mean_area), y = mean_area, fill = steel_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(mean_area, 1)), hjust = -0.1) +
  coord_flip() +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Povprečna površina vključkov po tipih jekla - LOG SKALA",
       x = "Vrsta jekla", y = "Povprečna površina (µm²)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, paste0("bar_plots_combined_LOG_", timestamp, ".png")), 
       grid.arrange(p7, p8_log, p8_area_log, ncol = 1), 
       width = 10, height = 12, dpi = 300)

# ---- 3.2. BAR PLOTS - NORMALNA SKALA ----
cat("\n=== USTVARJANJE BAR PLOTS - NORMALNA SKALA ===\n")

p8_normal <- ggplot(mean_size_by_steel, aes(x = reorder(steel_type, mean_ecd), y = mean_ecd, fill = steel_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(mean_ecd, 1)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Povprečna velikost vključkov po tipih jekla - NORMALNA SKALA",
       x = "Vrsta jekla", y = "Povprečni ECD (µm)") +
  theme_minimal() +
  theme(legend.position = "none")

p8_area_normal <- ggplot(mean_area_by_steel, aes(x = reorder(steel_type, mean_area), y = mean_area, fill = steel_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(mean_area, 1)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Povprečna površina vključkov po tipih jekla - NORMALNA SKALA",
       x = "Vrsta jekla", y = "Povprečna površina (µm²)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, paste0("bar_plots_combined_NORMAL_", timestamp, ".png")), 
       grid.arrange(p7, p8_normal, p8_area_normal, ncol = 1), 
       width = 10, height = 12, dpi = 300)

# ---- 4. HISTOGRAMS ----
cat("\n=== USTVARJANJE HISTOGRAMS ===\n")

# ---- 4.1. HISTOGRAMS - LOG SKALA ----
cat("\n=== USTVARJANJE HISTOGRAMS - LOG SKALA ===\n")

# Histogram za velikost (vse skupaj) - LOG SKALA
p9_log <- ggplot(combined_data, aes(x = ECD_um, fill = steel_type)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Histogram velikosti vključkov - LOG SKALA",
       x = "ECD (µm)", y = "Število", fill = "Vrsta jekla") +
  theme_minimal()

# Histogram za krožnost - LOG SKALA
p10_log <- ggplot(combined_data, aes(x = Circularity, fill = steel_type)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Histogram krožnosti vključkov - LOG SKALA",
       x = "Krožnost", y = "Število", fill = "Vrsta jekla") +
  theme_minimal()

cat("Shranjujem histogrami - LOG SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("histograms_combined_LOG_", timestamp, ".png")), 
         grid.arrange(p9_log, p10_log, ncol = 1), 
         width = 12, height = 8, dpi = 300)
  cat("✅ Histogrami - LOG SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju histogramov - LOG SKALA:", e$message, "\n")
})

# ---- 4.2. HISTOGRAMS - NORMALNA SKALA ----
cat("\n=== USTVARJANJE HISTOGRAMS - NORMALNA SKALA ===\n")

# Histogram za velikost (vse skupaj) - NORMALNA SKALA
p9_normal <- ggplot(combined_data, aes(x = ECD_um, fill = steel_type)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  labs(title = "Histogram velikosti vključkov - NORMALNA SKALA",
       x = "ECD (µm)", y = "Število", fill = "Vrsta jekla") +
  theme_minimal()

# Histogram za krožnost - NORMALNA SKALA
p10_normal <- ggplot(combined_data, aes(x = Circularity, fill = steel_type)) +
  geom_histogram(alpha = 0.7, bins = 20) +
  labs(title = "Histogram krožnosti vključkov - NORMALNA SKALA",
       x = "Krožnost", y = "Število", fill = "Vrsta jekla") +
  theme_minimal()

cat("Shranjujem histogrami - NORMALNA SKALA...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("histograms_combined_NORMAL_", timestamp, ".png")), 
         grid.arrange(p9_normal, p10_normal, ncol = 1), 
         width = 12, height = 8, dpi = 300)
  cat("✅ Histogrami - NORMALNA SKALA uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju histogramov - NORMALNA SKALA:", e$message, "\n")
})

# ---- 5. STACKED BAR CHARTS ----
cat("\n=== USTVARJANJE STACKED BAR CHARTS ===\n")

# Kemijske klase po tipih jekla
chem_by_steel <- combined_data %>%
  count(steel_type, kem_klasa) %>%
  group_by(steel_type) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

p11 <- ggplot(chem_by_steel, aes(x = steel_type, y = percentage, fill = kem_klasa)) +
  geom_col(position = "stack") +
  labs(title = "Kemijski razredi po tipih jekla (odstotki)",
       x = "Vrsta jekla", y = "Odstotek (%)", fill = "Kemijski razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Morfološke klase po tipih jekla
morph_by_steel <- combined_data %>%
  count(steel_type, morph_ISO) %>%
  group_by(steel_type) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

p12 <- ggplot(morph_by_steel, aes(x = steel_type, y = percentage, fill = morph_ISO)) +
  geom_col(position = "stack") +
  labs(title = "Morfološki razredi po tipih jekla (odstotki)",
       x = "Vrsta jekla", y = "Odstotek (%)", fill = "Morfologija") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Velikostne kategorije po tipih jekla
size_by_steel <- combined_data %>%
  count(steel_type, size_category) %>%
  group_by(steel_type) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

p13 <- ggplot(size_by_steel, aes(x = steel_type, y = percentage, fill = size_category)) +
  geom_col(position = "stack") +
  labs(title = "Velikostne kategorije po tipih jekla (odstotki)",
       x = "Vrsta jekla", y = "Odstotek (%)", fill = "Velikostna kategorija") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, paste0("stacked_bar_charts_", timestamp, ".png")), 
       grid.arrange(p11, p12, p13, ncol = 1), 
       width = 12, height = 15, dpi = 300)

# ---- 6. HEATMAP ----
cat("\n=== USTVARJANJE HEATMAP ===\n")

# Heatmap za kemijske klase
tryCatch({
  heatmap_data <- combined_data %>%
    count(steel_type, kem_klasa) %>%
    pivot_wider(names_from = kem_klasa, values_from = n, values_fill = 0) %>%
    column_to_rownames("steel_type")
  
  # Preveri, ali imamo dovolj podatkov za heatmap
  if (nrow(heatmap_data) >= 2 && ncol(heatmap_data) >= 2) {
    # Normaliziraj za odstotke
    heatmap_data_pct <- prop.table(as.matrix(heatmap_data), margin = 1) * 100
    
    # Ustvari heatmap
    library(pheatmap)
    png(file.path(output_dir, paste0("heatmap_chemistry_", timestamp, ".png")), 
        width = 12, height = 8, units = "in", res = 300)
    pheatmap(heatmap_data_pct, 
             cluster_rows = FALSE, 
             cluster_cols = TRUE,
             display_numbers = TRUE,
             number_format = "%.1f",
             main = "Kemijske klase po tipih jekla (%)")
    dev.off()
    cat("✅ Heatmap uspešno shranjen\n")
  } else {
    cat("❌ Premalo podatkov za heatmap (potrebno vsaj 2x2 matriko)\n")
    cat("   Število tipov jekla:", nrow(heatmap_data), "\n")
    cat("   Število kemijskih klas:", ncol(heatmap_data), "\n")
  }
}, error = function(e) {
  cat("❌ Napaka pri ustvarjanju heatmap:", e$message, "\n")
})

# ---- 7. RAZTRESENI DIAGRAMI ----
cat("\n=== USTVARJANJE RAZTRESENIH DIAGRAMOV ===\n")

# ---- 7.1. RAZTRESENI DIAGRAMI - LOG SKALA ----
cat("\n=== USTVARJANJE RAZTRESENIH DIAGRAMOV - LOG SKALA ===\n")

# Raztreseni diagram: velikost vs krožnost - LOG SKALA
p14_log <- ggplot(combined_data, aes(x = ECD_um, y = Circularity, color = steel_type)) +
  geom_point(alpha = 0.6, size = 1) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Velikost vs Krožnost - LOG SKALA",
       x = "ECD (µm)", y = "Krožnost", color = "Vrsta jekla") +
  theme_minimal()

# Raztreseni diagram: aspect ratio vs krožnost - LOG SKALA
p15_log <- ggplot(combined_data, aes(x = AR, y = Circularity, color = steel_type)) +
  geom_point(alpha = 0.6, size = 1) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Aspect Ratio vs Krožnost - LOG SKALA",
       x = "Razmerje stranic (ang. Aspect Ratio)", y = "Krožnost", color = "Vrsta jekla") +
  theme_minimal()

ggsave(file.path(output_dir, paste0("scatter_plots_LOG_", timestamp, ".png")), 
       grid.arrange(p14_log, p15_log, ncol = 1), 
       width = 12, height = 10, dpi = 300)

# ---- 7.2. RAZTRESENI DIAGRAMI - NORMALNA SKALA ----
cat("\n=== USTVARJANJE RAZTRESENIH DIAGRAMOV - NORMALNA SKALA ===\n")

# Raztreseni diagram: velikost vs krožnost - NORMALNA SKALA
p14_normal <- ggplot(combined_data, aes(x = ECD_um, y = Circularity, color = steel_type)) +
  geom_point(alpha = 0.6, size = 1) +
  labs(title = "Velikost vs Krožnost - NORMALNA SKALA",
       x = "ECD (µm)", y = "Krožnost", color = "Vrsta jekla") +
  theme_minimal()

# Raztreseni diagram: aspect ratio vs krožnost - NORMALNA SKALA
p15_normal <- ggplot(combined_data, aes(x = AR, y = Circularity, color = steel_type)) +
  geom_point(alpha = 0.6, size = 1) +
  labs(title = "Aspect Ratio vs Krožnost - NORMALNA SKALA",
       x = "Razmerje stranic (ang. Aspect Ratio)", y = "Krožnost", color = "Vrsta jekla") +
  theme_minimal()

ggsave(file.path(output_dir, paste0("scatter_plots_NORMAL_", timestamp, ".png")), 
       grid.arrange(p14_normal, p15_normal, ncol = 1), 
       width = 12, height = 10, dpi = 300)

# ---- 8. STATISTIČNI POVZETKI ----
cat("\n=== STATISTIČNI POVZETKI ===\n")

# Povzetki po tipih jekla
summary_stats <- combined_data %>%
  group_by(steel_type) %>%
  summarise(
    count = n(),
    mean_ecd = mean(ECD_um, na.rm = TRUE),
    median_ecd = median(ECD_um, na.rm = TRUE),
    sd_ecd = sd(ECD_um, na.rm = TRUE),
    mean_circularity = mean(Circularity, na.rm = TRUE),
    median_circularity = median(Circularity, na.rm = TRUE),
    mean_ar = mean(AR, na.rm = TRUE),
    median_ar = median(AR, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# ---- 9. ZAPISANJE REZULTATOV ----
cat("\n=== ZAPISANJE REZULTATOV ===\n")

# Morfologija po kemiji (kombinirana analiza)
morphology_by_chemistry <- combined_data %>%
  count(morph_shape, kem_klasa) %>%
  arrange(desc(n))

# Ustvari Excel datoteko z rezultati
out_file <- file.path(output_dir, paste0("comparison_analysis_", format(Sys.time(), "%Y.%m.%d.%H.%M.%S"), ".xlsx"))

write_xlsx(list(
  Combined_Data = combined_data,
  Summary_Stats = summary_stats,
  Count_by_Steel = count_by_steel,
  Mean_Size_by_Steel = mean_size_by_steel,
  Chemistry_by_Steel = chem_by_steel,
  Morphology_by_Steel = morph_by_steel,
  Size_by_Steel = size_by_steel,
  Morfologija_Po_Kemiji = morphology_by_chemistry
), path = out_file)

cat("✅ Rezultati zapisani v:", out_file, "\n")
cat("✅ Grafi shranjeni v:", output_dir, "\n")

# ---- 10. DODATNE ANALIZE ----
cat("\n=== DODATNE ANALIZE ===\n")

# ---- 10.1. BOX PLOTS: ASPECT RATIO PO MORFOLOŠKIH KLASAH ----
cat("\n=== BOX PLOTS: ASPECT RATIO PO MORFOLOŠKIH KLASAH ===\n")

# Box plot za aspect ratio po morfoloških klasah
p16 <- ggplot(combined_data, aes(x = morph_shape_slo, y = AR, fill = morph_shape_slo)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Aspect Ratio po morfoloških razredih",
       x = "Morfološki razred", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Morfološki razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot za aspect ratio po morfoloških klasah in tipih jekla
p17 <- ggplot(combined_data, aes(x = morph_shape_slo, y = AR, fill = steel_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Aspect Ratio po morfoloških razredih in tipih jekla",
       x = "Morfološki razred", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Shranjujem aspect ratio morphology plots...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("aspect_ratio_morphology_", timestamp, ".png")), 
         grid.arrange(p16, p17, ncol = 1), 
         width = 14, height = 12, dpi = 300)
  cat("✅ Aspect ratio morphology plots uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju aspect ratio morphology plots:", e$message, "\n")
})

# ---- 10.1.1. BOX PLOTS: ASPECT RATIO PO MORFOLOŠKIH KLASAH (LINEARNA Y SKALA) ----
cat("\n=== BOX PLOTS: ASPECT RATIO PO MORFOLOŠKIH KLASAH (LINEARNA Y SKALA) ===\n")

# Box plot za aspect ratio po morfoloških klasah z linearno Y skalo
p16_linear <- ggplot(combined_data, aes(x = morph_shape_slo, y = AR, fill = morph_shape_slo)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  labs(title = "Aspect Ratio po morfoloških razredih (linearna skala)",
       x = "Morfološki razred", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Morfološki razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot za aspect ratio po morfoloških klasah in tipih jekla z linearno Y skalo
p17_linear <- ggplot(combined_data, aes(x = morph_shape_slo, y = AR, fill = steel_type)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  labs(title = "Aspect Ratio po morfoloških razredih in tipih jekla (linearna skala)",
       x = "Morfološki razred", y = "Razmerje stranic (ang. Aspect Ratio)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Shranjujem aspect ratio morphology plots (linearna skala)...\n")
tryCatch({
  ggsave(file.path(output_dir, paste0("aspect_ratio_morphology_linear_", timestamp, ".png")), 
         grid.arrange(p16_linear, p17_linear, ncol = 1), 
         width = 14, height = 12, dpi = 300)
  cat("✅ Aspect ratio morphology plots (linearna skala) uspešno shranjeni\n")
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju aspect ratio morphology plots (linearna skala):", e$message, "\n")
})

# ---- 10.2. BAR PLOT: POVPREČNE VELIKOSTI PO KEMIJSKIH KLASAH ----
cat("\n=== BAR PLOT: POVPREČNE VELIKOSTI PO KEMIJSKIH KLASAH ===\n")

# Povprečne velikosti po kemijskih klasah
mean_size_by_chem <- combined_data %>%
  group_by(kem_klasa) %>%
  summarise(
    mean_ecd = mean(ECD_um, na.rm = TRUE),
    median_ecd = median(ECD_um, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_ecd))

# Bar plot za povprečne velikosti
p18 <- ggplot(mean_size_by_chem, aes(x = reorder(kem_klasa, mean_ecd), y = mean_ecd, fill = kem_klasa)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(mean_ecd, 1)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Povprečne velikosti vključkov po kemijskih razredih",
       x = "Kemijski razred", y = "Povprečni ECD (µm)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar plot za mediane velikosti
p19 <- ggplot(mean_size_by_chem, aes(x = reorder(kem_klasa, median_ecd), y = median_ecd, fill = kem_klasa)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(median_ecd, 1)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Medianne velikosti vključkov po kemijskih razredih",
       x = "Kemijski razred", y = "Mediana ECD (µm)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(output_dir, paste0("size_by_chemistry_", timestamp, ".png")), 
       grid.arrange(p18, p19, ncol = 1), 
       width = 12, height = 10, dpi = 300)

# ---- 10.3. VIOLIN PLOTS: ELEMENTARNA ANALIZA ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== VIOLIN PLOTS: ELEMENTARNA ANALIZA ZA VSAKO JEKLO PSEBAJ ===\n")

# Pripravi podatke za elementarno analizo
element_columns <- names(combined_data)[grepl(" \\(Wt%\\)$", names(combined_data))]
cat("Najdeni elementi:", length(element_columns), "\n")
cat("Elementi:", paste(translate_wt_percent(element_columns), collapse = ", "), "\n")

if (length(element_columns) > 0) {
  # Ustvari violin plots za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  element_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Pripravi podatke za violin plot
    element_data <- steel_data %>%
      select(all_of(element_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Element", values_to = "Wt_Percent") %>%
      mutate(Element = gsub(" \\(Wt%\\)", "", Element))
    
    # Ustvari violin plot
    p <- ggplot(element_data, aes(x = Element, y = Wt_Percent, fill = Element)) +
      geom_violin(alpha = 0.7, scale = "width") +
      geom_boxplot(width = 0.1, alpha = 0.5) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      labs(title = paste("Elementarna analiza -", steel_type),
           x = "Element", y = "mas. %", fill = "Element") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    element_plots[[i]] <- p
  }
  
  # Shrani violin plots za elementarno analizo
  cat("Shranjujem violin plots za elementarno analizo...\n")
  tryCatch({
    if (length(element_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("element_analysis_violin_", timestamp, ".png")), 
             do.call(grid.arrange, c(element_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Violin plots za elementarno analizo uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(element_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(element_plots))
        page_plots <- element_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("element_analysis_violin_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Violin plots za elementarno analizo uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju violin plots za elementarno analizo:", e$message, "\n")
  })
} else {
  cat("❌ Ni najdenih stolpcev z elementarno analizo (Wt%)\n")
}

# ---- 10.4. BOX PLOTS: ELEMENTARNA ANALIZA ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== BOX PLOTS: ELEMENTARNA ANALIZA ZA VSAKO JEKLO PSEBAJ ===\n")

if (length(element_columns) > 0) {
  # Ustvari box plots za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  element_box_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Pripravi podatke za box plot
    element_data <- steel_data %>%
      select(all_of(element_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Element", values_to = "Wt_Percent") %>%
      mutate(Element = gsub(" \\(Wt%\\)", "", Element))
    
    # Ustvari box plot
    p <- ggplot(element_data, aes(x = Element, y = Wt_Percent, fill = Element)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      labs(title = paste("Elementarna analiza (Box Plot) -", steel_type),
           x = "Element", y = "mas. %", fill = "Element") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    element_box_plots[[i]] <- p
  }
  
  # Shrani box plots za elementarno analizo
  cat("Shranjujem box plots za elementarno analizo...\n")
  tryCatch({
    if (length(element_box_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("element_analysis_boxplot_", timestamp, ".png")), 
             do.call(grid.arrange, c(element_box_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Box plots za elementarno analizo uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(element_box_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(element_box_plots))
        page_plots <- element_box_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("element_analysis_boxplot_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Box plots za elementarno analizo uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju box plots za elementarno analizo:", e$message, "\n")
  })
} else {
  cat("❌ Ni najdenih stolpcev z elementarno analizo (Wt%)\n")
}

# ---- 10.5. BOX PLOTS: NORMALIZIRANI ELEMENTI (WTN) ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== BOX PLOTS: NORMALIZIRANI ELEMENTI (WTN) ZA VSAKO JEKLO PSEBAJ ===\n")

# Pripravi podatke za normalizirane elemente
wtn_columns <- names(combined_data)[grepl("^wtn_", names(combined_data))]
# Odstrani wtn_Fe, ker je prevladujoč
wtn_columns <- wtn_columns[!grepl("^wtn_Fe$", wtn_columns)]
cat("Najdeni normalizirani elementi:", length(wtn_columns), "\n")
cat("Elementi (brez Fe):", paste(wtn_columns, collapse = ", "), "\n")

if (length(wtn_columns) > 0) {
  # Ustvari box plots za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  wtn_box_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Pripravi podatke za box plot
    wtn_data <- steel_data %>%
      select(all_of(wtn_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Element", values_to = "Wtn_Percent") %>%
      mutate(Element = gsub("^wtn_", "", Element))
    
    # Ustvari box plot
    p <- ggplot(wtn_data, aes(x = Element, y = Wtn_Percent, fill = Element)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      labs(title = paste("Normalizirani elementi (Box Plot) -", steel_type),
           x = "Element", y = "mas. % (norm.)", fill = "Element") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    wtn_box_plots[[i]] <- p
  }
  
  # Shrani box plots za normalizirane elemente
  cat("Shranjujem box plots za normalizirane elemente...\n")
  tryCatch({
    if (length(wtn_box_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("wtn_elements_boxplot_", timestamp, ".png")), 
             do.call(grid.arrange, c(wtn_box_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Box plots za normalizirane elemente uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(wtn_box_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(wtn_box_plots))
        page_plots <- wtn_box_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("wtn_elements_boxplot_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Box plots za normalizirane elemente uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju box plots za normalizirane elemente:", e$message, "\n")
  })
} else {
  cat("❌ Ni najdenih stolpcev z normaliziranimi elementi (wtn_)\n")
}

# ---- 10.6. BOX PLOTS: ORIGINALNI ELEMENTI (VKLJUČNO Z Fe IN Cu) ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== BOX PLOTS: ORIGINALNI ELEMENTI (VKLJUČNO Z Fe IN Cu) ZA VSAKO JEKLO PSEBAJ ===\n")

# Pripravi podatke za originalne elemente (vključno z Fe in Cu)
# Try different column name formats
original_wt_cols1 <- names(combined_data)[grepl("\\(Wt%\\)", names(combined_data))]
original_wt_cols2 <- names(combined_data)[grepl("\\(mas\\. %\\)", names(combined_data))]
original_wt_cols3 <- names(combined_data)[grepl("\\(wt%\\)", names(combined_data))]

# Combine all found columns
original_wt_cols <- unique(c(original_wt_cols1, original_wt_cols2, original_wt_cols3))
cat("Najdeni originalni elementi:", length(original_wt_cols), "\n")
cat("Elementi (vključno z Fe in Cu):", paste(original_wt_cols, collapse = ", "), "\n")

if (length(original_wt_cols) > 0) {
  # Ustvari box plots za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  original_box_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data[combined_data$steel_type == steel_type, ]
    
    # Pripravi podatke za box plot
    original_data <- steel_data %>%
      select(all_of(original_wt_cols)) %>%
      pivot_longer(cols = everything(), names_to = "Element", values_to = "Wt_Percent") %>%
      mutate(Element = gsub(" \\(Wt%\\)| \\(mas\\. %\\)| \\(wt%\\)", "", Element))
    
    # Ustvari box plot
    p <- ggplot(original_data, aes(x = Element, y = Wt_Percent, fill = Element)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      labs(title = paste("Originalni elementi (Box Plot) -", steel_type),
           x = "Element", y = "mas. %", fill = "Element") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    original_box_plots[[i]] <- p
  }
  
  # Shrani box plots
  cat("Shranjujem box plots za originalne elemente...\n")
  tryCatch({
    if (length(original_box_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("original_elements_boxplot_", timestamp, ".png")), 
             do.call(grid.arrange, c(original_box_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Box plots za originalne elemente uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(original_box_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(original_box_plots))
        page_plots <- original_box_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("original_elements_boxplot_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Box plots za originalne elemente uspešno shranjeni (", n_pages, "strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju box plots za originalne elemente:", e$message, "\n")
  })
} else {
  cat("❌ Ni najdenih stolpcev z originalnimi elementi (mas. %)\n")
}

# ---- 10.7. ORIGINALNI ELEMENTI: VIOLIN PLOTS ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== ORIGINALNI ELEMENTI: VIOLIN PLOTS ZA VSAKO JEKLO PSEBAJ ===\n")

if (length(original_wt_cols) > 0) {
  # Ustvari violin plots za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  original_violin_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data[combined_data$steel_type == steel_type, ]
    
    # Pripravi podatke za violin plot
    original_data <- steel_data %>%
      select(all_of(original_wt_cols)) %>%
      pivot_longer(cols = everything(), names_to = "Element", values_to = "Wt_Percent") %>%
      mutate(Element = gsub(" \\(Wt%\\)| \\(mas\\. %\\)| \\(wt%\\)", "", Element))
    
    # Ustvari violin plot
    p <- ggplot(original_data, aes(x = Element, y = Wt_Percent, fill = Element)) +
      geom_violin(alpha = 0.7, scale = "width") +
      geom_boxplot(width = 0.1, alpha = 0.5) +
      scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
      labs(title = paste("Originalni elementi (Violin Plot) -", steel_type),
           x = "Element", y = "mas. %", fill = "Element") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    original_violin_plots[[i]] <- p
  }
  
  # Shrani violin plots
  cat("Shranjujem violin plots za originalne elemente...\n")
  tryCatch({
    if (length(original_violin_plots) <= 4) {
      ggsave(file.path(output_dir, paste0("original_element_analysis_violin_", timestamp, ".png")), 
             do.call(grid.arrange, c(original_violin_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Violin plots za originalne elemente uspešno shranjeni (1 stran)\n")
    } else {
      n_per_page <- 4
      n_pages <- ceiling(length(original_violin_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(original_violin_plots))
        page_plots <- original_violin_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("original_element_analysis_violin_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Violin plots za originalne elemente uspešno shranjeni (", n_pages, "strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju violin plots za originalne elemente:", e$message, "\n")
  })
} else {
  cat("❌ Ni najdenih stolpcev z originalnimi elementi (mas. %)\n")
}

# ---- 10.8. ORIGINALNI ELEMENTI: CHEMICAL COMPOSITION BY MORPHOLOGY ----
cat("\n=== ORIGINALNI ELEMENTI: CHEMICAL COMPOSITION BY MORPHOLOGY ===\n")

if (length(original_wt_cols) > 0) {
  # Pripravi podatke za kemijsko sestavo po morfologiji
  morph_chemistry_original <- combined_data %>%
    group_by(morph_shape_slo) %>%
    summarise(across(all_of(original_wt_cols), mean, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = -morph_shape_slo, names_to = "Element", values_to = "Mean_Concentration") %>%
    mutate(Element = gsub(" \\(Wt%\\)| \\(mas\\. %\\)| \\(wt%\\)", "", Element))
  
  p_adv4_original <- ggplot(morph_chemistry_original, aes(x = morph_shape_slo, y = Mean_Concentration, fill = Element)) +
    geom_col(position = "stack") +
    labs(title = "Povprečna kemijska sestava po morfologiji (Originalni podatki)",
         x = "Morfološki razred", y = "mas. %", fill = "Element") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("original_chemical_composition_by_morphology_", timestamp, ".png")), 
         p_adv4_original, width = 14, height = 8, dpi = 300)
  
  cat("✅ Originalni kemijski sestava po morfologiji uspešno shranjena\n")
} else {
  cat("❌ Ni najdenih stolpcev z originalnimi elementi (mas. %)\n")
}

# ---- 10.9. HISTOGRAMI: POVRŠINA ZA VSAKO JEKLO PSEBAJ ----
cat("\n=== HISTOGRAMI: POVRŠINA ZA VSAKO JEKLO PSEBAJ ===\n")

# Ustvari histogram za vsako jeklo posebaj
steel_types_list <- unique(combined_data$steel_type)
histogram_plots <- list()

for (i in seq_along(steel_types_list)) {
  steel_type <- steel_types_list[i]
  steel_data <- combined_data %>% filter(steel_type == !!steel_type)
  
  # Prilagodljivi parametri histograma
  n_bins <- 25  # Število binov (možno prilagoditi: 15-50)
  alpha_level <- 0.8  # Prosojnost (0.1-1.0)
  bar_color <- "steelblue"  # Barva stolpcev
  border_color <- "white"  # Barva obrobe
  border_size <- 0.2  # Debelina obrobe
  
  # Ustvari histogram z logaritmično skalo - uporabi binwidth namesto bins
  # za boljše pokritje podatkov
  p <- ggplot(steel_data, aes(x = `Area (sq. µm)`)) +
    geom_histogram(alpha = alpha_level, binwidth = 0.1, fill = bar_color, 
                   position = "identity", color = border_color, size = border_size,
                   boundary = 0) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    labs(title = paste("Histogram površine -", steel_type),
         x = "Površina (µm²)", y = "Število") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", size = 0.5))
  
  histogram_plots[[i]] <- p
}

# ---- 10.7. HISTOGRAMI: POVRŠINA ZA VSAKO JEKLO PoSEBAJ (LINEARNA SKALA) ----
cat("\n=== HISTOGRAMI: POVRŠINA ZA VSAKO JEKLO PoSEBAJ (LINEARNA SKALA) ===\n")

# Ustvari histogram za vsako jeklo posebaj z linearno skalo
histogram_linear_plots <- list()

for (i in seq_along(steel_types_list)) {
  steel_type <- steel_types_list[i]
  steel_data <- combined_data %>% filter(steel_type == !!steel_type)
  
  # Prilagodljivi parametri histograma za linearno skalo
  n_bins_linear <- 30  # Število binov za linearno skalo
  alpha_level <- 0.8  # Prosojnost (0.1-1.0)
  bar_color <- "darkgreen"  # Barva stolpcev (drugačna od log skale)
  border_color <- "white"  # Barva obrobe
  border_size <- 0.2  # Debelina obrobe
  
  # Ustvari histogram z linearno skalo in log y skalo
  p_linear <- ggplot(steel_data, aes(x = `Area (sq. µm)`)) +
    geom_histogram(alpha = alpha_level, bins = n_bins_linear, fill = bar_color, 
                   position = "identity", color = border_color, size = border_size,
                   boundary = 0) +
    scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
    labs(title = paste("Histogram površine (linearna x, log y) -", steel_type),
         x = "Površina (µm²)", y = "Število (log skala)") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", size = 0.5))
  
  histogram_linear_plots[[i]] <- p_linear
}

# Shrani histograme v skupni graf
cat("Shranjujem histogrami po jeklih...\n")
tryCatch({
  if (length(histogram_plots) <= 4) {
    # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
    ggsave(file.path(output_dir, paste0("area_histograms_by_steel_", timestamp, ".png")), 
           do.call(grid.arrange, c(histogram_plots, ncol = 2)), 
           width = 16, height = 12, dpi = 300)
    cat("✅ Histogrami po jeklih uspešno shranjeni (1 stran)\n")
  } else {
    # Če je več tipov jekla, jih shrani v več grafov
    n_per_page <- 4
    n_pages <- ceiling(length(histogram_plots) / n_per_page)
    
    for (page in 1:n_pages) {
      start_idx <- (page - 1) * n_per_page + 1
      end_idx <- min(page * n_per_page, length(histogram_plots))
      page_plots <- histogram_plots[start_idx:end_idx]
      
      ggsave(file.path(output_dir, paste0("area_histograms_by_steel_page", page, "_", timestamp, ".png")), 
             do.call(grid.arrange, c(page_plots, ncol = 2)), 
             width = 16, height = 12, dpi = 300)
    }
    cat("✅ Histogrami po jeklih uspešno shranjeni (", n_pages, " strani)\n")
  }
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju histogramov po jeklih:", e$message, "\n")
})

# Shrani linearne histograme v skupni graf
cat("Shranjujem linearne histogrami po jeklih...\n")
tryCatch({
  if (length(histogram_linear_plots) <= 4) {
    # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
    ggsave(file.path(output_dir, paste0("area_histograms_linear_by_steel_", timestamp, ".png")), 
           do.call(grid.arrange, c(histogram_linear_plots, ncol = 2)), 
           width = 16, height = 12, dpi = 300)
    cat("✅ Linearni histogrami po jeklih uspešno shranjeni (1 stran)\n")
  } else {
    # Če je več tipov jekla, jih shrani v več grafov
    n_per_page <- 4
    n_pages <- ceiling(length(histogram_linear_plots) / n_per_page)
    
    for (page in 1:n_pages) {
      start_idx <- (page - 1) * n_per_page + 1
      end_idx <- min(page * n_per_page, length(histogram_linear_plots))
      page_plots <- histogram_linear_plots[start_idx:end_idx]
      
      ggsave(file.path(output_dir, paste0("area_histograms_linear_by_steel_page", page, "_", timestamp, ".png")), 
             do.call(grid.arrange, c(page_plots, ncol = 2)), 
             width = 16, height = 12, dpi = 300)
    }
    cat("✅ Linearni histogrami po jeklih uspešno shranjeni (", n_pages, " strani)\n")
  }
}, error = function(e) {
  cat("❌ Napaka pri shranjevanju linearnih histogramov po jeklih:", e$message, "\n")
})

# ---- 11. POVZETEK ----
cat("\n=== POVZETEK ANALIZE ===\n")
cat("Analiziranih tipov jekla:", length(unique(combined_data$steel_type)), "\n")
cat("Skupaj vključkov:", nrow(combined_data), "\n")
cat("Kemijski razredi:", length(unique(combined_data$kem_klasa)), "\n")
cat("Morfološki razredi:", length(unique(combined_data$morph_shape)), "\n")
cat("\nUstvarjeni grafi (VSE V DVEH VERZIJAH - LOG IN NORMALNA SKALA):\n")
cat("- violin_plots_combined_LOG.png / violin_plots_combined_NORMAL.png\n")
cat("- box_plots_combined_LOG.png / box_plots_combined_NORMAL.png\n")
cat("- bar_plots_combined_LOG.png / bar_plots_combined_NORMAL.png (z povprečno površino)\n")
cat("- histograms_combined_LOG.png / histograms_combined_NORMAL.png\n")
cat("- stacked_bar_charts.png\n")
cat("- heatmap_chemistry.png\n")
cat("- scatter_plots_LOG.png / scatter_plots_NORMAL.png\n")
cat("- aspect_ratio_morphology.png (log skala)\n")
cat("- aspect_ratio_morphology_linear.png (linearna skala)\n")
cat("- size_by_chemistry.png\n")
cat("- element_analysis_violin.png\n")
cat("- element_analysis_boxplot.png\n")
cat("- wtn_elements_boxplot.png\n")
cat("- area_histograms_by_steel.png\n")
cat("- area_histograms_linear_by_steel.png\n")
cat("\nNapredni grafi:\n")
cat("- circularity_vs_aspect_ratio.png\n")
cat("- size_distribution_by_morphology.png\n")
cat("- density_plots.png\n")
cat("- chemical_composition_by_morphology.png\n")
cat("- control_charts.png\n")
cat("- outlier_detection.png\n")
cat("- morphology_transition_matrix.png\n")
cat("- 3d_scatter_plot.png\n")
cat("- direction_histograms_by_steel.png\n")
cat("- direction_comparison.png\n")
cat("- direction_rose_diagram.png\n")

# ---- 12. DODATNI NAPREDNI GRAFI ----
cat("\n=== DODATNI NAPREDNI GRAFI ===\n")

# ---- 12.1. KROŽNOST VS ASPECT RATIO RAZTRESENI DIAGRAMI ----
cat("\n=== KROŽNOST VS ASPECT RATIO RAZTRESENI DIAGRAMI ===\n")

# Raztreseni diagram za krožnost vs aspect ratio po tipih jekla
p_adv1 <- ggplot(combined_data, aes(x = Circularity, y = AR, color = morph_shape_slo)) +
  geom_point(alpha = 0.6, size = 1) +
  facet_wrap(~steel_type, scales = "free") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Krožnost vs Aspect Ratio po tipih jekla",
       x = "Krožnost", y = "Aspect Ratio", color = "Morfološki razred") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("krožnost_vs_aspect_ratio_", timestamp, ".png")), 
       p_adv1, width = 16, height = 12, dpi = 300)

# ---- 12.2. SIZE DISTRIBUTION BY MORPHOLOGY ----
cat("\n=== SIZE DISTRIBUTION BY MORPHOLOGY ===\n")

p_adv2 <- ggplot(combined_data, aes(x = morph_shape_slo, y = ECD_um, fill = morph_shape_slo)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija velikosti po morfologiji",
       x = "Morfološki razred", y = "ECD (µm)", fill = "Morfološki razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("size_distribution_by_morphology_", timestamp, ".png")), 
       p_adv2, width = 12, height = 8, dpi = 300)

# ---- 12.3. DENSITY PLOTS ----
cat("\n=== DENSITY PLOTS ===\n")

p_adv3 <- ggplot(combined_data, aes(x = ECD_um, fill = steel_type)) +
  geom_density(alpha = 0.6) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija velikosti - gostota verjetnosti",
       x = "ECD (µm)", y = "Gostota", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("density_plots_", timestamp, ".png")), 
       p_adv3, width = 12, height = 8, dpi = 300)

# Density plot za površino (area)
p_adv3_area <- ggplot(combined_data, aes(x = `Area (sq. µm)`, fill = steel_type)) +
  geom_density(alpha = 0.6) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Distribucija površine - gostota verjetnosti",
       x = "Površina (µm²)", y = "Gostota", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("density_plots_area_", timestamp, ".png")), 
       p_adv3_area, width = 12, height = 8, dpi = 300)

# ---- 12.4. CHEMICAL COMPOSITION BY MORPHOLOGY ----
cat("\n=== CHEMICAL COMPOSITION BY MORPHOLOGY ===\n")

# Pripravi podatke za kemijsko sestavo po morfologiji
if (length(wtn_columns) > 0) {
  morph_chemistry <- combined_data %>%
    group_by(morph_shape_slo) %>%
    summarise(across(all_of(wtn_columns), mean, na.rm = TRUE), .groups = "drop") %>%
    pivot_longer(cols = -morph_shape_slo, names_to = "Element", values_to = "Mean_Concentration") %>%
    mutate(Element = gsub("^wtn_", "", Element))
  
  p_adv4 <- ggplot(morph_chemistry, aes(x = morph_shape_slo, y = Mean_Concentration, fill = Element)) +
    geom_col(position = "stack") +
    labs(title = "Povprečna kemijska sestava po morfologiji",
         x = "Morfološki razred", y = "mas. % (norm.)", fill = "Element") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("chemical_composition_by_morphology_", timestamp, ".png")), 
         p_adv4, width = 14, height = 8, dpi = 300)
}

# ---- 12.5. CONTROL CHARTS ----
cat("\n=== CONTROL CHARTS ===\n")

# Kontrolni graf za ECD
p_adv5 <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = steel_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = mean(combined_data$ECD_um, na.rm = TRUE), 
             color = "red", linetype = "dashed", size = 1) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Kontrolni graf - ECD",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("control_charts_", timestamp, ".png")), 
       p_adv5, width = 12, height = 8, dpi = 300)

# ---- 12.6. OUTLIER DETECTION ----
cat("\n=== OUTLIER DETECTION ===\n")

p_adv6 <- ggplot(combined_data, aes(x = steel_type, y = ECD_um, fill = steel_type)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3, alpha = 0.7) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  labs(title = "Odkrivanje izstopajočih vrednosti - ECD",
       x = "Vrsta jekla", y = "ECD (µm)", fill = "Vrsta jekla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right")

ggsave(file.path(output_dir, paste0("outlier_detection_", timestamp, ".png")), 
       p_adv6, width = 12, height = 8, dpi = 300)

# ---- 12.7. MORPHOLOGY TRANSITION MATRIX ----
cat("\n=== MORPHOLOGY TRANSITION MATRIX ===\n")

# Morfologija po velikostnih kategorijah
morph_size_data <- combined_data %>%
  mutate(size_bin = cut(ECD_um, breaks = c(0, 2, 5, 10, 20, Inf), 
                        labels = c("≤2 µm", "2-5 µm", "5-10 µm", "10-20 µm", ">20 µm"))) %>%
  count(size_bin, morph_shape_slo) %>%
  group_by(size_bin) %>%
  mutate(percentage = n / sum(n) * 100)

p_adv7 <- ggplot(morph_size_data, aes(x = size_bin, y = percentage, fill = morph_shape_slo)) +
  geom_col(position = "stack") +
  labs(title = "Morfologija po velikostnih kategorijah",
       x = "Velikostna kategorija", y = "Odstotek (%)", fill = "Morfološki razred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, paste0("morphology_transition_matrix_", timestamp, ".png")), 
       p_adv7, width = 12, height = 8, dpi = 300)

# ---- 12.8. 3D RAZTRESENI DIAGRAM ----
cat("\n=== 3D RAZTRESENI DIAGRAM ===\n")

# Preveri, ali je Direction stolpec prisoten
if ("Direction (degrees)" %in% names(combined_data)) {
  # 3D raztreseni diagram z Direction
  p_adv8 <- ggplot(combined_data, aes(x = ECD_um, y = Circularity, color = morph_shape_slo)) +
    geom_point(alpha = 0.6, size = 1) +
    facet_wrap(~steel_type) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    labs(title = "3D Raztreseni diagram: ECD vs Krožnost po tipih jekla",
         x = "ECD (µm)", y = "Krožnost", color = "Morfološki razred") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("3d_scatter_plot_", timestamp, ".png")), 
         p_adv8, width = 16, height = 12, dpi = 300)
} else {
  # Standardni 3D raztreseni diagram
  p_adv8 <- ggplot(combined_data, aes(x = ECD_um, y = Circularity, color = morph_shape_slo)) +
    geom_point(alpha = 0.6, size = 1) +
    facet_wrap(~steel_type) +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    labs(title = "3D Raztreseni diagram: ECD vs Krožnost po tipih jekla",
         x = "ECD (µm)", y = "Krožnost", color = "Morfološki razred") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("3d_scatter_plot_", timestamp, ".png")), 
         p_adv8, width = 16, height = 12, dpi = 300)
}

# ---- 12.9. DIRECTION ANALYSIS ----
cat("\n=== DIRECTION ANALYSIS ===\n")

# Preveri, ali je Direction stolpec prisoten
if ("Direction (degrees)" %in% names(combined_data)) {
  # Histogrami smeri za vsako jeklo posebaj (36 binov)
  steel_types_list <- unique(combined_data$steel_type)
  direction_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Ustvari histogram smeri z 18 binovi (10 stopinj na bin) za 0-180°
    p_dir <- ggplot(steel_data, aes(x = `Direction (degrees)`)) +
      geom_histogram(bins = 18, alpha = 0.7, fill = "steelblue", 
                     color = "white", boundary = 0) +
      scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
      labs(title = paste("Distribucija smeri -", steel_type),
           x = "Smer (stopinje)", y = "Število") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
    
    direction_plots[[i]] <- p_dir
  }
  
  # Shrani histogrami smeri
  cat("Shranjujem histogrami smeri...\n")
  tryCatch({
    if (length(direction_plots) <= 4) {
      ggsave(file.path(output_dir, paste0("direction_histograms_by_steel_", timestamp, ".png")), 
             do.call(grid.arrange, c(direction_plots, ncol = 2)), 
             width = 16, height = 12, dpi = 300)
      cat("✅ Histogrami smeri uspešno shranjeni (1 stran)\n")
    } else {
      n_per_page <- 4
      n_pages <- ceiling(length(direction_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(direction_plots))
        page_plots <- direction_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("direction_histograms_by_steel_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 16, height = 12, dpi = 300)
      }
      cat("✅ Histogrami smeri uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju histogramov smeri:", e$message, "\n")
  })
  
  # Primerjava smeri med jekli
  p_dir_comparison <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = steel_type)) +
    geom_histogram(bins = 18, alpha = 0.7, position = "identity") +
    scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
    labs(title = "Primerjava distribucije smeri med tipi jekla",
         x = "Smer (stopinje)", y = "Število", fill = "Vrsta jekla") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("direction_comparison_", timestamp, ".png")), 
         p_dir_comparison, width = 14, height = 8, dpi = 300)
  
  # Rose diagram za smeri (poln 360° krog) - za vsako jeklo posebej
  rose_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Ustvari rose diagram za posamezno jeklo
    p_rose <- ggplot(steel_data, aes(x = `Direction (degrees)`)) +
      geom_histogram(bins = 36, alpha = 0.7, fill = "steelblue", 
                     color = "white", boundary = 0) +
      coord_polar(start = -pi/2) +
      scale_x_continuous(breaks = seq(0, 360, 30), limits = c(0, 360)) +
      labs(title = paste("Rose diagram smeri -", steel_type),
           x = "Smer (stopinje)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA))
    
    rose_plots[[i]] <- p_rose
    
    # Shrani posamezni rose diagram
    ggsave(file.path(output_dir, paste0("direction_rose_", steel_type, "_", timestamp, ".png")), 
           p_rose, width = 10, height = 10, dpi = 300)
  }
  
  # Shrani rose plots v skupnih grafih (max 4 na stran)
  cat("Shranjujem rose plots v skupnih grafih...\n")
  tryCatch({
    if (length(rose_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("direction_rose_combined_page_", timestamp, ".png")), 
             do.call(grid.arrange, c(rose_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Rose plots uspešno shranjeni v skupnem grafu (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(rose_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(rose_plots))
        page_plots <- rose_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("direction_rose_combined_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Rose plots uspešno shranjeni v skupnih grafih (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju rose plots v skupnih grafih:", e$message, "\n")
  })
  
  # Kombinirani rose diagram za vse jekla
  p_rose_combined <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = steel_type)) +
    geom_histogram(bins = 36, alpha = 0.7, position = "identity") +
    coord_polar(start = -pi/2) +
    scale_x_continuous(breaks = seq(0, 360, 30), limits = c(0, 360)) +
    labs(title = "Rose diagram smeri po tipih jekla (360°)",
         x = "Smer (stopinje)", fill = "Vrsta jekla") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  ggsave(file.path(output_dir, paste0("direction_rose_combined_", timestamp, ".png")), 
         p_rose_combined, width = 12, height = 10, dpi = 300)
  
  # ---- 12.10. DIRECTION HISTOGRAMS BY MORPHOLOGY FOR EACH STEEL TYPE ----
  cat("\n=== DIRECTION HISTOGRAMS BY MORPHOLOGY FOR EACH STEEL TYPE ===\n")
  
  # Ustvari direction histograme po morfologiji za vsako jeklo posebaj
  steel_types_list <- unique(combined_data$steel_type)
  direction_morph_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Ustvari histogram smeri po morfologiji za posamezno jeklo
    p_dir_morph <- ggplot(steel_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
      geom_histogram(bins = 18, alpha = 0.7, position = "identity") +
      scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
      labs(title = paste("Distribucija smeri po morfologiji -", steel_type),
           x = "Smer (stopinje)", y = "Število", fill = "Morfološki razred") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            legend.position = "bottom")
    
    direction_morph_plots[[i]] <- p_dir_morph
  }
  
  # Shrani direction histograme po morfologiji
  cat("Shranjujem direction histograme po morfologiji...\n")
  tryCatch({
    if (length(direction_morph_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("direction_by_morphology_", timestamp, ".png")), 
             do.call(grid.arrange, c(direction_morph_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Direction histogrami po morfologiji uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(direction_morph_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(direction_morph_plots))
        page_plots <- direction_morph_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("direction_by_morphology_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Direction histogrami po morfologiji uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju direction histogramov po morfologiji:", e$message, "\n")
  })
  
  # ---- 12.11. DIRECTION ROSE PLOTS BY MORPHOLOGY FOR EACH STEEL TYPE ----
  cat("\n=== DIRECTION ROSE PLOTS BY MORPHOLOGY FOR EACH STEEL TYPE ===\n")
  
  # Ustvari rose plots po morfologiji za vsako jeklo posebaj
  direction_rose_morph_plots <- list()
  
  for (i in seq_along(steel_types_list)) {
    steel_type <- steel_types_list[i]
    steel_data <- combined_data %>% filter(steel_type == !!steel_type)
    
    # Ustvari rose plot po morfologiji za posamezno jeklo
    p_rose_morph <- ggplot(steel_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
      geom_histogram(bins = 36, alpha = 0.7, position = "identity") +
      coord_polar(start = -pi/2) +
      scale_x_continuous(breaks = seq(0, 360, 30), limits = c(0, 360)) +
      labs(title = paste("Rose diagram smeri po morfologiji -", steel_type),
           x = "Smer (stopinje)", fill = "Morfološki razred") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            legend.position = "bottom")
    
    direction_rose_morph_plots[[i]] <- p_rose_morph
  }
  
  # Shrani rose plots po morfologiji
  cat("Shranjujem rose plots po morfologiji...\n")
  tryCatch({
    if (length(direction_rose_morph_plots) <= 4) {
      # Če je 4 ali manj tipov jekla, jih prikaži v enem grafu
      ggsave(file.path(output_dir, paste0("direction_rose_by_morphology_", timestamp, ".png")), 
             do.call(grid.arrange, c(direction_rose_morph_plots, ncol = 2)), 
             width = 20, height = 16, dpi = 300)
      cat("✅ Rose plots po morfologiji uspešno shranjeni (1 stran)\n")
    } else {
      # Če je več tipov jekla, jih shrani v več grafov
      n_per_page <- 4
      n_pages <- ceiling(length(direction_rose_morph_plots) / n_per_page)
      
      for (page in 1:n_pages) {
        start_idx <- (page - 1) * n_per_page + 1
        end_idx <- min(page * n_per_page, length(direction_rose_morph_plots))
        page_plots <- direction_rose_morph_plots[start_idx:end_idx]
        
        ggsave(file.path(output_dir, paste0("direction_rose_by_morphology_page", page, "_", timestamp, ".png")), 
               do.call(grid.arrange, c(page_plots, ncol = 2)), 
               width = 20, height = 16, dpi = 300)
      }
      cat("✅ Rose plots po morfologiji uspešno shranjeni (", n_pages, " strani)\n")
    }
  }, error = function(e) {
    cat("❌ Napaka pri shranjevanju rose plots po morfologiji:", e$message, "\n")
  })
  
  # ---- 12.12. NORMAL DIRECTION HISTOGRAMS BY MORPHOLOGY (ALL STEEL TYPES COMBINED) ----
  cat("\n=== NORMAL DIRECTION HISTOGRAMS BY MORPHOLOGY (ALL STEEL TYPES COMBINED) ===\n")
  
  # Ustvari normalne direction histograme po morfologiji za vse jekla skupaj
  p_dir_morph_all <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
    geom_histogram(bins = 18, alpha = 0.7, position = "identity") +
    scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
    labs(title = "Distribucija smeri po morfologiji (vsa jekla skupaj)",
         x = "Smer (stopinje)", y = "Število", fill = "Morfološki razred") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0("direction_by_morphology_all_steel_", timestamp, ".png")), 
         p_dir_morph_all, width = 14, height = 8, dpi = 300)
  
  # Ustvari normalne direction histograme po morfologiji z facet_wrap za vsako jeklo
  p_dir_morph_faceted <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
    geom_histogram(bins = 18, alpha = 0.7, position = "identity") +
    facet_wrap(~steel_type, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
    labs(title = "Distribucija smeri po morfologiji in tipih jekla",
         x = "Smer (stopinje)", y = "Število", fill = "Morfološki razred") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0("direction_by_morphology_faceted_", timestamp, ".png")), 
         p_dir_morph_faceted, width = 16, height = 12, dpi = 300)
  
  # Ustvari normalne direction histograme po morfologiji z stacked bars
  p_dir_morph_stacked <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
    geom_histogram(bins = 18, alpha = 0.7, position = "stack") +
    scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
    labs(title = "Distribucija smeri po morfologiji (stacked bars)",
         x = "Smer (stopinje)", y = "Število", fill = "Morfološki razred") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0("direction_by_morphology_stacked_", timestamp, ".png")), 
         p_dir_morph_stacked, width = 14, height = 8, dpi = 300)
  
  # Ustvari normalne direction histograme po morfologiji z dodano steel_type informacijo
  p_dir_morph_steel <- ggplot(combined_data, aes(x = `Direction (degrees)`, fill = morph_shape_slo)) +
    geom_histogram(bins = 18, alpha = 0.7, position = "identity") +
    facet_grid(steel_type ~ morph_shape_slo, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 180, 20), limits = c(0, 180)) +
    labs(title = "Distribucija smeri po morfologiji in tipih jekla (detailed grid)",
         x = "Smer (stopinje)", y = "Število", fill = "Morfološki razred") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.position = "bottom")
  
  ggsave(file.path(output_dir, paste0("direction_by_morphology_detailed_grid_", timestamp, ".png")), 
         p_dir_morph_steel, width = 20, height = 16, dpi = 300)
  
  cat("✅ Normalni direction histogrami po morfologiji uspešno shranjeni\n")
  
} else {
  cat("❌ Stolpec 'Direction (degrees)' ni prisoten v podatkih\n")
}

cat("\n=== ANALIZA ZAKLJUČENA ===\n")