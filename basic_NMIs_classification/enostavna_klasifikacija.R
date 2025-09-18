# AUTHOR: Vid Kuder Marušič
# EMAIL: vidkm30@gmail.com
# DATE: 2025
# VERSION: 1.0.0
# LICENSE: MIT License
# REPOSITORY: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
#
# =======================
# ENOSTAVNA KLASIFIKACIJA VKLJUČKOV
# =======================
#
# DESCRIPTION:
#   Simplified classification script for non-metallic inclusions in steel materials.
#   Provides chemical and morphological classification with geometric analysis.
#
# =======================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(PeriodicTable)  # Za dostop do atomskih mas elementov
})

options(dplyr.summarise.inform = FALSE)


# ---- Nastavitve ----
# Pot do Excel datoteke
datoteka_pot <- "C: ..."

# Pot za izhodne datoteke
output_dir <- "C:/ ..."

# Ustvari izhodno mapo, če ne obstaja
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Ustvarjena izhodna mapa:", output_dir, "\n")
}

# ---- Elementi in atomske mase ----
# Uporaba PeriodicTable paketa za dostop do atomskih mas elementov
# PeriodicTable paket zagotavlja natančne atomske mase iz IUPAC standardov

# Ustvarimo slovar atomskih mas iz PeriodicTable paketa
# mass() funkcija vrne atomsko maso elementa v g/mol
ATOMIC_MASSES <- list(
  H = mass("H"), He = mass("He"), Li = mass("Li"), Be = mass("Be"), B = mass("B"),
  C = mass("C"), N = mass("N"), O = mass("O"), F = mass("F"), Ne = mass("Ne"),
  Na = mass("Na"), Mg = mass("Mg"), Al = mass("Al"), Si = mass("Si"), P = mass("P"),
  S = mass("S"), Cl = mass("Cl"), Ar = mass("Ar"), K = mass("K"), Ca = mass("Ca"),
  Sc = mass("Sc"), Ti = mass("Ti"), V = mass("V"), Cr = mass("Cr"), Mn = mass("Mn"),
  Fe = mass("Fe"), Co = mass("Co"), Ni = mass("Ni"), Cu = mass("Cu"), Zn = mass("Zn"),
  Zr = mass("Zr"), Ba = mass("Ba")
)

# Seznam elementov za analizo (uporablja se za normalizacijo)
ELEMENTS_TO_ANALYZE <- c("H","He","Li","Be","B","C","N","O","F","Ne",
                        "Na","Mg","Al","Si","P","S","Cl","Ar","K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn")


# ---- Pomožne funkcije za elemente ----
# Funkcija za identifikacijo elementov v imenih stolpcev
identify_elements <- function(column_names) {
  # Poišči vse elemente, ki so prisotni v imenih stolpcev
  found_elements <- c()
  for (element in ELEMENTS_TO_ANALYZE) {
    if (any(grepl(paste0("\\b", element, "\\b"), column_names, ignore.case = TRUE))) {
      found_elements <- c(found_elements, element)
    }
  }
  return(found_elements)
}

# Funkcija za pridobitev stolpcev z elementi
get_element_columns <- function(data, prefix = "") {
  if (prefix != "") {
    pattern <- paste0("^", prefix, ".*")
    element_cols <- names(data)[grepl(pattern, names(data))]
  } else {
    element_cols <- names(data)[names(data) %in% ELEMENTS_TO_ANALYZE]
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
    "plate-like" = "Ploščati",
    "polyhedral" = "Polieder",
    "irregular" = "Nepravilni",
    "oversize_DS" = "Veliki DS"
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


# Tolerance za prepoznavanje napak in madežev
MICRODEFECT_FE_MIN <- 95    # Če je v merjenju več kot 95 % železa (Fe), je to verjetno napaka/del matrice
MICRODEFECT_O_MAX  <- 5     # In hkrati manj kot 5 % kisika (O)
FE_STAIN_FE_MIN    <- 30    # Madež FeO: vsaj 30 % železa...
FE_STAIN_O_MIN     <- 2.5   # ... in vsaj 2.5 % kisika

# ---- Branje podatkov ----
stopifnot(file.exists(datoteka_pot))
df <- read_xlsx(datoteka_pot)
ime_datoteke <- sub("\\.xlsx$", "", basename(datoteka_pot))
cat("Imena stolpcev v datoteki:\n"); print(names(df))

# ---- Pomožne funkcije ----
# Funkcija za varno deljenje
safe_ratio <- function(num, den) {
  num <- suppressWarnings(as.numeric(num))
  den <- suppressWarnings(as.numeric(den))
  ifelse(is.finite(den) & den > 0, num/den, NA_real_)
}

# Bližnjici za dostop do podatkov
w  <- function(d, el) {
  col_name <- paste0("wtn_",el)
  if(col_name %in% names(d)) d[[col_name]] else rep(0, nrow(d))
}
wr <- function(d, el) {
  # Try different column name formats
  col_name1 <- paste0(el," (Wt%)")
  col_name2 <- paste0(el," (mas. %)")
  col_name3 <- paste0(el," (wt%)")
  
  if(col_name1 %in% names(d)) {
    return(d[[col_name1]])
  } else if(col_name2 %in% names(d)) {
    return(d[[col_name2]])
  } else if(col_name3 %in% names(d)) {
    return(d[[col_name3]])
  } else {
    return(rep(0, nrow(d)))
  }
}

# Funkcija za dostop do atomske mase elementa
get_atomic_mass <- function(element) {
  if(element %in% names(ATOMIC_MASSES)) {
    return(ATOMIC_MASSES[[element]])
  } else {
    warning(paste("Element", element, "ni v seznamu atomskih mas"))
    return(NA_real_)
  }
}

# Primer uporabe atomskih mas:
# get_atomic_mass("Fe")  # Vrne atomsko maso železa (55.845 g/mol)
# get_atomic_mass("O")   # Vrne atomsko maso kisika (15.999 g/mol)
# ATOMIC_MASSES$Fe       # Direkten dostop do atomske mase železa

# Funkcija za izračun normaliziranih utežnih deležev
add_wtn <- function(d, elementi = ELEMENTS_TO_ANALYZE, matrix_elements = c("Fe","Cu")) {
  # Poišči vse elemente v podatkih - try different column name formats
  wt_cols1 <- names(d)[str_detect(names(d), "\\(Wt%\\)")]
  wt_cols2 <- names(d)[str_detect(names(d), "\\(mas\\. %\\)")]
  wt_cols3 <- names(d)[str_detect(names(d), "\\(wt%\\)")]
  
  # Combine all found columns
  wt_cols <- unique(c(wt_cols1, wt_cols2, wt_cols3))
  elementi_v_podatkih <- str_extract(wt_cols, "[A-Z][a-z]?")
  elementi_v_podatkih <- elementi_v_podatkih[!is.na(elementi_v_podatkih)]
  
  if(length(elementi_v_podatkih) == 0) {
    # Če ni elementov, dodaj prazne stolpce
    for (el in elementi) {
      d[[paste0("wtn_",el)]] <- rep(0, nrow(d))
    }
    return(d)
  }
  
  # Izračunaj normalizirane deleže (brez matričnih elementov)
  norm_els <- setdiff(elementi_v_podatkih, matrix_elements)
  if(length(norm_els) > 0) {
    # Try different column name formats for normalization
    norm_cols1 <- paste0(norm_els," (Wt%)")
    norm_cols2 <- paste0(norm_els," (mas. %)")
    norm_cols3 <- paste0(norm_els," (wt%)")
    
    existing_norm_cols <- c(
      norm_cols1[norm_cols1 %in% names(d)],
      norm_cols2[norm_cols2 %in% names(d)],
      norm_cols3[norm_cols3 %in% names(d)]
    )
    
    if(length(existing_norm_cols) > 0) {
      vsota <- rowSums(d[, existing_norm_cols, drop=FALSE], na.rm=TRUE)
      faktor <- ifelse(vsota > 0, 100/vsota, 1)
    } else {
      faktor <- rep(1, nrow(d))
    }
  } else {
    faktor <- rep(1, nrow(d))
  }
  
  # Dodaj normalizirane stolpce
  for (el in elementi_v_podatkih) {
    # Try different source column formats
    src1 <- paste0(el," (Wt%)")
    src2 <- paste0(el," (mas. %)")
    src3 <- paste0(el," (wt%)")
    
    if (src1 %in% names(d)) {
      d[[paste0("wtn_",el)]] <- d[[src1]] * faktor
    } else if (src2 %in% names(d)) {
      d[[paste0("wtn_",el)]] <- d[[src2]] * faktor
    } else if (src3 %in% names(d)) {
      d[[paste0("wtn_",el)]] <- d[[src3]] * faktor
    } else {
      d[[paste0("wtn_",el)]] <- rep(0, nrow(d))
    }
  }
  
  # Dodaj prazne stolpce za elemente, ki niso v podatkih
  for (el in setdiff(elementi, elementi_v_podatkih)) {
    d[[paste0("wtn_",el)]] <- rep(0, nrow(d))
  }
  
  d
}

# ---- Kemijska klasifikacija (enostavna) ----
enostavna_kemijska_klasifikacija <- function(d) {
  # Preveri, da imamo podatke
  if(nrow(d) == 0) {
    d$kem_klasa <- character(0)
    return(d)
  }
  
  # Ustvari vektor za klasifikacijo
  kem_klasa <- rep("Neuvrscen", nrow(d))
  
  # Mikrodefekti in madeži
  idx1 <- wr(d,"Fe") >= MICRODEFECT_FE_MIN & wr(d,"O") <= MICRODEFECT_O_MAX
  kem_klasa[idx1] <- "Microdefect / Fe-matrix"
  
  idx2 <- wr(d,"Fe") >= FE_STAIN_FE_MIN & wr(d,"O") >= FE_STAIN_O_MIN
  kem_klasa[idx2] <- "FeO stain/spots"
  
  # Sulfidi
  idx3 <- w(d,"Mn") > 30 & w(d,"S") > 20
  kem_klasa[idx3] <- "MnS"
  
  idx4 <- w(d,"Ca") > 30 & w(d,"S") > 20
  kem_klasa[idx4] <- "CaS"
  
  idx5 <- w(d,"Ca") > 10 & w(d,"Mn") > 10 & w(d,"S") > 20
  kem_klasa[idx5] <- "Ca-Mn-S"
  
  idx6 <- w(d,"S") > 20
  kem_klasa[idx6] <- "Other sulfides"
  
  # Oksidi in aluminati
  idx7 <- w(d,"Al") > 20 & w(d,"Mn") < 20 & w(d,"Ca") < 10 & w(d,"Si") < 20 & w(d,"S") < 20 & w(d,"Mg") < 5
  kem_klasa[idx7] <- "Al2O3"
  
  idx8 <- w(d,"Mg") > 5 & w(d,"Al") < 10 & w(d,"Mn") < 20 & w(d,"Ca") < 10 & w(d,"Si") < 20 & w(d,"S") < 20
  kem_klasa[idx8] <- "MgO"
  
  idx9 <- w(d,"Mg") > 5 & w(d,"Al") > 10 & w(d,"Mn") < 20 & w(d,"Ca") < 10 & w(d,"Si") < 20 & w(d,"S") < 20
  kem_klasa[idx9] <- "Al-Mg-O"
  
  idx10 <- w(d,"Ca") > 20 & w(d,"S") < 20 & w(d,"Al") < 10 & w(d,"Si") < 20 & w(d,"Mn") < 20 & w(d,"Mg") < 5
  kem_klasa[idx10] <- "CaO"
  
  idx11 <- w(d,"Ca") > 10 & w(d,"Al") > 10 & w(d,"Si") < 20 & w(d,"Mn") < 20 & w(d,"Mg") < 5 & w(d,"S") < 20
  kem_klasa[idx11] <- "Ca-Al-O"
  
  idx12 <- w(d,"Ca") > 10 & w(d,"Mg") > 5 & w(d,"Al") > 10 & w(d,"Si") < 20 & w(d,"S") < 20 & w(d,"Mn") < 20
  kem_klasa[idx12] <- "Ca-Mg-Al-O"
  
  # Silikati in kompleksni oksidi
  idx13 <- w(d,"Si") > 20 & w(d,"Mn") < 20 & w(d,"Al") < 10 & w(d,"Ca") < 20
  kem_klasa[idx13] <- "SiO2"
  
  idx14 <- w(d,"Mn") > 20 & w(d,"Al") < 10 & w(d,"Ca") < 20 & w(d,"Si") < 20
  kem_klasa[idx14] <- "MnO"
  
  idx15 <- w(d,"Mn") > 20 & w(d,"Si") > 10 & w(d,"Al") < 10 & w(d,"Ca") < 20
  kem_klasa[idx15] <- "Mn-Si-O"
  
  idx16 <- w(d,"Mn") > 20 & w(d,"Al") > 10 & w(d,"S") < 20 & w(d,"Si") < 20
  kem_klasa[idx16] <- "Mn-Al-O"
  
  # Redki oksidi
  idx17 <- w(d,"Zr") > 10 & w(d,"Al") < 10 & w(d,"Ca") < 20 & w(d,"Si") < 20 & w(d,"S") < 20 & w(d,"Mn") < 20
  kem_klasa[idx17] <- "ZrO2"
  
  idx18 <- w(d,"Ti") > 20 & w(d,"Al") < 10 & w(d,"Ca") < 20 & w(d,"Si") < 20 & w(d,"Mn") < 20 & w(d,"S") < 20
  kem_klasa[idx18] <- "TiO2"
  
  # Alkalni oksidi
  idx19 <- (w(d,"Na") + w(d,"K")) > 3
  kem_klasa[idx19] <- "Na2O + K2O"
  
  # Dodaj klasifikacijo v podatke
  d$kem_klasa <- kem_klasa
  d
}

# ---- Geometrijska analiza ----
geom_metrics <- function(d) {
  # Preveri, ali obstajajo potrebni stolpci
  needed <- c("Aspect Ratio","Area (sq. µm)","Length (µm)","Perimeter (µm)","Perimeter (um)","ECD (µm)","ECD (um)","Convex Area (sq. µm)","Convex Area (sq. um)")
  for(nm in needed) if(!nm %in% names(d)) d[[nm]] <- NA_real_
  
  d %>%
    mutate(
      # Osnovne geometrijske meritve
      AR           = suppressWarnings(as.numeric(`Aspect Ratio`)),
      Area_um2     = suppressWarnings(as.numeric(`Area (sq. µm)`)),
      Length_um    = suppressWarnings(as.numeric(`Length (µm)`)),
      Perimeter_um = suppressWarnings(as.numeric(coalesce(`Perimeter (µm)`, `Perimeter (um)`))),
      ECD_um       = suppressWarnings(as.numeric(coalesce(`ECD (µm)`, `ECD (um)`))),
      
      # ECD (Equivalent Circular Diameter)
      # Formula: 2√(A/π) where A=area
      # Diameter of circle with same area as the particle
      ECD_um = ifelse(is.finite(ECD_um), ECD_um, 
                     ifelse(is.finite(Area_um2) & Area_um2 > 0, 2*sqrt(Area_um2/pi), NA_real_)),
      
      # Krožnost (Circularity)
      # Formula: 4πA/P² where A=area, P=perimeter
      # Range: 0-1, 1=perfect circle
      Circularity = ifelse(is.finite(Perimeter_um) & Perimeter_um > 0 & is.finite(Area_um2) & Area_um2 > 0,
                          4*pi*Area_um2 / (Perimeter_um^2), NA_real_),
      
      # Alternativna krožnost (Circularity C)
      # Formula: C = (4*Area)/(π*Fmax²) where Fmax = maximum Feret diameter (Length)
      # Range: 0-1, 1=perfect circle
      Circularity_C = ifelse(is.finite(Length_um) & Length_um > 0 & is.finite(Area_um2) & Area_um2 > 0,
                            (4*Area_um2) / (pi*Length_um^2), NA_real_),
      
      # Razmerje oboda (Perimeter Ratio) - Custom metric
      # Formula: P/(π×ECD) where P=perimeter, ECD=equivalent circular diameter
      # Measures how much the perimeter deviates from a circle
      Perimeter_Ratio = ifelse(is.finite(Perimeter_um) & Perimeter_um > 0 & is.finite(ECD_um) & ECD_um > 0,
                              Perimeter_um / (pi*ECD_um), NA_real_),
      
      # Ocenjena širina (Estimated Width) - Basic geometry
      # Formula: Length/AR where AR=aspect ratio
      # Derived from AR = Length/Width, therefore Width = Length/AR
      Width_est = ifelse(is.finite(AR) & AR > 0 & is.finite(Length_um) & Length_um > 0, 
                        Length_um / AR, NA_real_),
      
      # Pravokotnost (Rectangularity) 
      # Formula: A/(L×W) where A=area, L=length, W=width
      # Range: 0-1, 1=perfect rectangle
      Rectangularity = ifelse(is.finite(Area_um2) & Area_um2 > 0 & is.finite(Length_um) & Length_um > 0 & 
                             is.finite(Width_est) & Width_est > 0,
                             Area_um2 / (Length_um * Width_est), NA_real_),
      
    )
}

# ---- Morfološka klasifikacija ----
morphology <- function(d) {
  d %>%
    mutate(
      # Izračunaj dodatne geometrijske meritve
      EL = AR,  # EL = maxFeret/minFeret (uporabljamo AR kot približek)
      minFeret_est = ifelse(is.finite(Length_um) & is.finite(AR) & AR > 0, Length_um / AR, NA_real_),
      maxFeret_est = Length_um,
      bboxArea_est = ifelse(is.finite(Length_um) & is.finite(minFeret_est), Length_um * minFeret_est, NA_real_),
      Extent = ifelse(is.finite(Area_um2) & is.finite(bboxArea_est) & bboxArea_est > 0, Area_um2 / bboxArea_est, NA_real_),
     
      morph_shape = case_when(
       
        # Threshold: diameter > 13 µm for sulfides (deformable sulfides)
        is.finite(ECD_um) & ECD_um > 13 & grepl("S", kem_klasa) ~ "oversize_DS",
        
        # 2. Stringer (aligned) 
        #is.finite(EL) & EL >= 3 & is.finite(Length_um) & Length_um >= 10 ~ "stringer",
        
        # 3. Elongated 
        # Threshold: EL ≥ 3 AND length ≥ 10 µm (elongated particles)
        is.finite(EL) & EL >= 3 & is.finite(Length_um) & Length_um >= 10 ~ "elongated",
        
        # 4. Plate-like 
        # Threshold: EL ≥ 3 AND Extent ≥ 0.7 AND minFeret ≤ 0.25×maxFeret
        is.finite(EL) & EL >= 3 & is.finite(Extent) & Extent >= 0.7 & 
        is.finite(minFeret_est) & is.finite(maxFeret_est) & minFeret_est <= 0.25 * maxFeret_est ~ "plate-like",
        
        # 5. Spherical 
        # Threshold: Circularity ≥ 0.90 AND EL ≤ 1.2 (near-perfect circles)
        is.finite(Circularity) & Circularity >= 0.90 & is.finite(EL) & EL <= 1.2 ~ "spherical",
        
        # 6. Globular (equant) 
        # Threshold: 0.80 ≤ Circularity < 0.90 AND EL ≤ 1.5 (equant particles)
        is.finite(Circularity) & Circularity >= 0.80 & Circularity < 0.90 & is.finite(EL) & EL <= 1.5 ~ "globular",
        
        # 7. Angular / polyhedral 
        # Threshold: 0.65 ≤ Circularity ≤ 0.85 (sharp corners)
        is.finite(Circularity) & Circularity >= 0.65 & Circularity <= 0.85 ~ "polyhedral",
        
        # 8. Irregular 
        # Threshold: Circularity < 0.7 (catch-all for irregular shapes)
        is.finite(Circularity) & Circularity < 0.7 ~ "irregular",
        
        # 9. Fallback - če nimamo podatkov, klasificiraj kot irregular
        TRUE ~ "irregular"
      ),
      
      # morfologija
      morph_ISO = case_when(
        morph_shape %in% c("spherical", "globular") ~ "Globular",
        morph_shape %in% c("elongated", "plate-like") ~ "Elongated",
        morph_shape %in% c("polyhedral", "polyhedral") ~ "polyhedral",
        morph_shape %in% c("irregular", "oversize_DS") ~ "Irregular",
        TRUE ~ "Irregular"  # Fallback to Irregular instead of Intermediate
      ),
      
      # DS flag za deformabilne sulfide
      DS_flag = case_when(
        grepl("S", kem_klasa) & is.finite(ECD_um) & ECD_um > 13 ~ TRUE,
        grepl("S", kem_klasa) & morph_shape %in% c("elongated") ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

# ---- Eksogeni flag ----
exogenous_flag <- function(d){
  d %>% mutate(
    exogenous_flag = (w(d,"Na")+w(d,"K")+w(d,"Ba")+w(d,"F")+w(d,"Cl") >= 2) |
                     (sum_wtn(d,c("Ca","Al","Si","O")) >= 70 & w(d,"Mg") > 0.5)
  )
}

# Pomožna funkcija za seštevanje normaliziranih deležev
sum_wtn <- function(d, elvec) {
  wt_cols <- paste0("wtn_", elvec)
  existing_cols <- wt_cols[wt_cols %in% names(d)]
  if(length(existing_cols) > 0) {
    rowSums(d[, existing_cols, drop=FALSE], na.rm=TRUE)
  } else {
    rep(0, nrow(d))
  }
}

# ---- Glavna analiza ----
cat("\n=== ENOSTAVNA KLASIFIKACIJA VKLJUČKOV ===\n")

# Pripravi podatke
df <- df |> 
  add_wtn() |>  # Uporabi privzete elemente iz ELEMENTS_TO_ANALYZE
  enostavna_kemijska_klasifikacija() |>
  geom_metrics() |>
  morphology() |>
  exogenous_flag()

# Ustvari končne klasifikacije
df <- df |> mutate(
  koncna_klasa = paste(morph_shape, kem_klasa),
  koncna_klasa_iso = paste0(morph_ISO, " ", kem_klasa)
)

# ---- Povzetki ----
cat("\n--- Kemijska klasifikacija ---\n")
kemijski_povzetek <- df %>% count(kem_klasa, sort=TRUE)
print(kemijski_povzetek)

cat("\n--- Morfološka klasifikacija ---\n")
morfoloski_povzetek <- df %>% count(morph_shape, sort=TRUE)
print(morfoloski_povzetek)

cat("\n--- Končna klasifikacija ---\n")
koncni_povzetek <- df %>% count(koncna_klasa, sort=TRUE)
print(koncni_povzetek)

# ---- Podrobnejši povzetki ----
velikost_po_kategorijah <- df %>%
  group_by(kem_klasa) %>%
  summarise(
    stevilo = n(),
    povprecna_povrsina = mean(`Area (sq. µm)`, na.rm = TRUE),
    povprecna_dolzina  = mean(`Length (µm)`, na.rm = TRUE),
    median_ECD         = median(ECD_um, na.rm = TRUE),
    median_krožnost = median(Circularity, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(desc(stevilo))

# Statistični povzetki
statistical_summary <- df %>%
  group_by(kem_klasa) %>%
  summarise(
    count = n(),
    mean_area = mean(`Area (sq. µm)`, na.rm = TRUE),
    median_area = median(`Area (sq. µm)`, na.rm = TRUE),
    mean_krožnost = mean(Circularity, na.rm = TRUE),
    mean_aspect_ratio = mean(AR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

# Distribucija velikosti
size_distribution <- df %>%
  mutate(
    size_category = case_when(
      ECD_um <= 2 ~ "≤2 µm",
      ECD_um <= 5 ~ "2-5 µm",
      ECD_um <= 10 ~ "5-10 µm",
      ECD_um > 10 ~ ">10 µm",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(kem_klasa, size_category) %>%
  pivot_wider(names_from = size_category, values_from = n, values_fill = 0)

# Kvalitetni kazalci
quality_metrics <- df %>%
  summarise(
    total_inclusions = n(),
    classified_percentage = round((n() - sum(kem_klasa == "Neuvrscen")) / n() * 100, 1),
    exogenous_percentage = round(sum(exogenous_flag, na.rm = TRUE) / n() * 100, 1),
    spherical_percentage = round(sum(morph_shape == "spherical", na.rm = TRUE) / n() * 100, 1),
    elongated_percentage = round(sum(morph_shape == "elongated", na.rm = TRUE) / n() * 100, 1)
  )

# Top klasifikacije
top_classifications <- df %>%
  count(kem_klasa, sort = TRUE) %>%
  head(10) %>%
  mutate(
    percentage = round(n / sum(df$kem_klasa != "Neuvrscen") * 100, 1)
  )

# Morfologija po kemiji
morphology_by_chemistry <- df %>%
  count(morph_shape, kem_klasa) %>%
  arrange(desc(n))

# ---- Zapisanje rezultatov ----
out_file <- file.path(output_dir, paste0("enostavna_klasifikacija_", ime_datoteke, "_", format(Sys.time(), "%Y.%m.%d.%H.%M.%S"), ".xlsx"))

# Odstrani prazne stolpce
remove_empty_columns <- function(data) {
  empty_cols <- sapply(data, function(col) {
    if (is.numeric(col)) {
      all(is.na(col)) || (all(!is.na(col)) && all(col == 0))
    } else if (is.character(col)) {
      all(is.na(col)) || (all(!is.na(col)) && all(col == ""))
    } else {
      all(is.na(col))
    }
  })
  data[, !empty_cols, drop = FALSE]
}

df_clean <- remove_empty_columns(df)

write_xlsx(list(
  Data = df_clean,
  Kemijski_Povzetek = kemijski_povzetek,
  Morfoloski_Povzetek = morfoloski_povzetek,
  Koncni_Povzetek = koncni_povzetek,
  Velikosti = velikost_po_kategorijah,
  Statisticni_Povzetki = statistical_summary,
  Distribucija_Velikosti = size_distribution,
  Kvalitetni_Kazalci = quality_metrics,
  Top_Klasifikacije = top_classifications,
  Morfologija_Po_Kemiji = morphology_by_chemistry
), path = out_file)

cat("\nDatoteka zapisana: ", out_file, "\n")
cat("\n=== ANALIZA ZAKLJUČENA ===\n")
