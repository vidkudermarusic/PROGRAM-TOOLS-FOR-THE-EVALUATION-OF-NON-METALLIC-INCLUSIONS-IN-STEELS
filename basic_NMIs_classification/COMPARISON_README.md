# Comparison Analysis - Multiple Steel Types

## Opis
Skripta za primerjavo in analizo podatkov iz različnih tipov jekla. Ustvarja obsežne vizualizacije in statistične analize.

## Funkcionalnosti

### 📊 Vizualizacije
1. **Violin Plots** - Distribucija velikosti, površine, krožnosti, aspect ratio
2. **Box Plots** - Primerjava po kemijskih in morfoloških klasah
3. **Bar Plots** - Število vključkov in povprečne velikosti
4. **Histograms** - Distribucija velikosti in krožnosti
5. **Stacked Bar Charts** - Kemijske klase, morfologija, velikostne kategorije
6. **Heatmap** - Kemijske klase po tipih jekla
7. **Raztreseni diagrami** - Korelacije med parametri

### 📈 Statistične analize
- Povzetki po tipih jekla
- ANOVA testi za razlike
- Tukey HSD post-hoc analiza
- Kruskal-Wallis testi

## Uporaba

### 🚀 HITRA UPORABA (Priporočeno)
```r
# Samodejno iskanje in analiza
source("auto_comparison.R")
```
**To je najenostavnejši način!** Skripta bo:
- Samodejno poiskala vse datoteke z vzorcem `klasificirani_vkljucki_*.xlsx`
- Ustvarila imena tipov jekla iz imen datotek
- Naredila osnovno analizo in ustvarila grafe

### 📋 PODROBNA UPORABA

#### 1. Priprava datotek
Zagotovite, da imate Excel datoteke z rezultati klasifikacije za vsak tip jekla v isti mapi.

#### 2. Konfiguracija
Uredite `comparison_config.R`:
```r
# Nastavite poti do datotek
base_path <- "C:/path/to/your/files/"

# Pot za izhodne datoteke (prilagodite glede na vaše potrebe)
output_dir <- "C:/path/to/output/folder/"

# OPCIJA 1: Samodejno iskanje (priporočeno)
# Skripta bo samodejno poiskala vse datoteke z vzorcem "enostavna_klasifikacija_*.xlsx"

# OPCIJA 2: Ročno določanje datotek
datoteke <- c("file1.xlsx", "file2.xlsx", ...)
steel_types <- c("Type_A", "Type_B", ...)
```

#### 3. Zagon analize
```r
# Opcija 1: Hitra analiza
source("auto_comparison.R")

# Opcija 2: Podrobna analiza z konfiguracijo
source("comparison_config.R")

# Opcija 3: Direktno
source("comparison_analysis.R")
```

## Izhod

### 📁 Datoteke
- **Excel datoteka** z vsemi podatki in povzetki (shranjena v `output_dir`)
- **PNG grafi** za vse vizualizacije (shranjeni v `output_dir`)
- **Avtomatsko ustvarjanje** izhodne mape, če ne obstaja
- **Timestamp v imenih** - vsi grafi imajo datum in čas v imenu (format: `YYYY.MM.DD.HH.MM.SS`)
- **Heatmap** za kemijske klase

### 📊 Grafi
1. `violin_plots_combined_YYYY.MM.DD.HH.MM.SS.png` - Violin plots za glavne parametre
2. `box_plots_combined_YYYY.MM.DD.HH.MM.SS.png` - Box plots po klasah
3. `bar_plots_combined_YYYY.MM.DD.HH.MM.SS.png` - Bar plots za števila in velikosti
4. `histograms_combined_YYYY.MM.DD.HH.MM.SS.png` - Histogrami distribucij
5. `stacked_bar_charts_YYYY.MM.DD.HH.MM.SS.png` - Stacked bar charts
6. `heatmap_chemistry_YYYY.MM.DD.HH.MM.SS.png` - Heatmap kemijskih klas
7. `scatter_plots_YYYY.MM.DD.HH.MM.SS.png` - Raztreseni diagrami korelacij

### 📋 Excel listi
- **Combined_Data** - Vsi združeni podatki
- **Summary_Stats** - Statistični povzetki
- **Count_by_Steel** - Števila po tipih jekla
- **Mean_Size_by_Steel** - Povprečne velikosti
- **Chemistry_by_Steel** - Kemijske klase
- **Morphology_by_Steel** - Morfološke klase
- **Size_by_Steel** - Velikostne kategorije

## Prilagoditve

### Barve
```r
plot_settings$color_palette <- "Set1"  # Možnosti: Set1, Set2, Set3, Paired, Dark2, Accent
```

### Velikosti grafov
```r
plot_settings$violin_width <- 20
plot_settings$violin_height <- 15
```

### Filtriranje podatkov
```r
filter_settings$min_ecd <- 1.0      # Minimalna velikost
filter_settings$max_ecd <- 50       # Maksimalna velikost
filter_settings$exclude_microdefects <- TRUE
```

## Napredne možnosti

### Dodatni statistični testi
```r
statistical_tests$perform_anova <- TRUE
statistical_tests$perform_tukey <- TRUE
statistical_tests$perform_kruskal <- TRUE
```

### Prilagojene velikostne kategorije
```r
plot_settings$size_breaks <- c(0, 1, 3, 8, 15, Inf)
plot_settings$size_labels <- c("≤1 µm", "1-3 µm", "3-8 µm", "8-15 µm", ">15 µm")
```

## Troubleshooting

### Napaka: "Datoteka ne obstaja"
- Preverite poti v `comparison_config.R`
- Preverite imena datotek

### Napaka: "Ni podatkov za analizo"
- Preverite, da datoteke vsebujejo "Data" list
- Preverite strukturo podatkov

### Grafi se ne shranijo
- Preverite dovoljenja za pisanje
- Preverite, da je `pheatmap` paket nameščen

## Zahteve

### R paketi
```r
install.packages(c("tidyverse", "readxl", "writexl", "ggplot2", 
                   "gridExtra", "RColorBrewer", "scales", "pheatmap"))
```

### Struktura podatkov
Excel datoteke morajo vsebovati "Data" list z naslednjimi stolpci:
- `steel_type` (ali podobno)
- `kem_klasa`
- `morph_shape` / `morph_ISO`
- `ECD_um`
- `Area (sq. µm)`
- `Krožnost`
- `AR` (Aspect Ratio)

## Primeri uporabe

### Hitra analiza
```r
# Uporabite privzete nastavitve
source("comparison_config.R")
```

### Prilagojena analiza
```r
# Uredite nastavitve
plot_settings$color_palette <- "Dark2"
filter_settings$min_ecd <- 2.0

# Zaženite analizo
source("comparison_analysis.R")
```

### Samo določeni grafi
```r
# Uredite comparison_analysis.R in zakomentirajte nepotrebne dele
# Na primer, če želite samo violin plots:
# Zakomentirajte vse od "# ---- 2. BOX PLOTS ----" naprej
```
