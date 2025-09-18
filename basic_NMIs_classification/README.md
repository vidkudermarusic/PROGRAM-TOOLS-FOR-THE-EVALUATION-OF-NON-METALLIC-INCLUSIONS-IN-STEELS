# Enostavna Klasifikacija Vključkov

## Opis
Poenostavljena verzija klasifikacijske skripte za analizo vključkov v materialih. Ta skripta uporablja osnovna pravila za kemijsko klasifikacijo in geometrijsko analizo.

## Glavne značilnosti

### Kemijska klasifikacija
- **Sulfidi**: MnS, CaS, Ca-Mn-S, Other sulfides
- **Oksidi**: Al2O3, MgO, CaO, SiO2, MnO, ZrO2, TiO2
- **Kompleksni oksidi**: Al-Mg-O, Ca-Al-O, Ca-Mg-Al-O, Mn-Si-O, Mn-Al-O
- **Alkalni oksidi**: Na2O + K2O
- **Napake**: Microdefect / Fe-matrix, FeO stain/spots

### Morfološka klasifikacija (po ISO standardih)
- **Spherical**: Kroglasti vključki (Krožnost ≥ 0.90, EL ≤ 1.2)
- **Globular**: Kroglast-equant vključki (0.80 ≤ Krožnost < 0.90, EL ≤ 1.5)
- **Elongated**: Podolgovati vključki (EL ≥ 3, Length ≥ 10 µm)
- **Plate-like**: Ploščati vključki (EL ≥ 3, Extent ≥ 0.7, minFeret ≤ 0.25·maxFeret)
- **Angular**: Kotasti vključki (0.65 ≤ Krožnost ≤ 0.85, Solidity ≥ 0.95)
- **Irregular**: Nepravilni vključki (Krožnost < 0.7) - catch-all kategorija
- **Dendritic**: Dendritični vključki (Solidity ≤ 0.85, Krožnost < 0.7)

### Posebne značilnosti
- **DS Flag**: Deformabilni sulfidi (diameter > 13 µm ali elongated/stringer sulfidi)
- **Priority Order**: Klasifikacija se izvaja v določenem vrstnem redu za zmanjšanje napak
- **ISO Mapping**: Avtomatsko preslikavanje v ISO kategorije (Globular/Elongated/Angular/Irregular)
- **Scientific Approach**: Vsi vključki se klasificirajo v specifične kategorije, brez "vmesnih" oznak

### Geometrijske meritve
- **Aspect Ratio (AR)** - Razmerje stranic
- **Krožnost** - Krožnost (4π·Area/Perimeter²)
- **Perimeter Ratio** - Razmerje oboda
- **Rectangularity** - Pravokotnost
- **Equivalent Circular Diameter (ECD)** - Ekvivalentni krožni premer
- **Solidity** - Trdnost (Area/ConvexArea)
- **Extent** - Razširjenost (Area/bboxArea)
- **EL (Elongation)** - Podolgovost (maxFeret/minFeret)

## Uporaba

1. **Nastavite pot do datoteke**:
   ```r
   datoteka_pot <- "C:/path/to/your/file.xlsx"
   ```

2. **Zaženite skripto**:
   ```r
   source("enostavna_klasifikacija.R")
   ```

## Izhod

Skripta ustvari Excel datoteko z naslednjimi listi:
- **Data**: Celotni podatki z klasifikacijami
- **Kemijski_Povzetek**: Število vključkov po kemijskih klasah
- **Morfoloski_Povzetek**: Število vključkov po morfologiji
- **Koncni_Povzetek**: Kombinirana klasifikacija
- **Velikosti**: Povprečne velikosti po klasah
- **Statisticni_Povzetki**: Statistični povzetki
- **Distribucija_Velikosti**: Razporeditev po velikostnih razredih
- **Kvalitetni_Kazalci**: Odstotki uspešnosti klasifikacije
- **Top_Klasifikacije**: Najpogostejše klasifikacije
- **Morfologija_Po_Kemiji**: Kombinacija morfologije in kemije

## Priporočila za uporabo

- Uporabite za **hitre analize** in **prve teste**
- Idealen za **učenje** klasifikacijskih pravil
- Dobra **osnova** za razvoj naprednejših sistemov
- Za **produkcijsko uporabo** razmislite o kompleksni verziji
