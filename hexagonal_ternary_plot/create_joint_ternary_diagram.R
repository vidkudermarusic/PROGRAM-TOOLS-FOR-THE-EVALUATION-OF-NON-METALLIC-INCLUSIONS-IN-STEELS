# AUTHOR: Vid Kuder Marušič
# EMAIL: vidkm30@gmail.com
# DATE: 2025
# VERSION: 1.0.0
# LICENSE: MIT License
# COMPATIBILITY: R 3.6.0 or later
# REPOSITORY: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
#
# ========================================================================
# CREATE_JOINT_TERNARY_DIAGRAM - Hexagonal Ternary Plot Creator
# ========================================================================
#
# DESCRIPTION:
#   Creates joint hexagonal ternary diagrams from Excel data with support for
#   complex element combinations. The hexagonal layout provides 6 different
#   ternary views of the same data, offering comprehensive visualization of
#   three-component systems with multiple element combinations.
#
# FEATURES:
#   - Hexagonal layout with 6 triangular ternary plots
#   - Support for element combinations (e.g., "SiO2 + Al2O3")
#   - Automatic package installation and loading
#   - High-quality PNG output with timestamps
#   - Customizable output directory structure
#   - Publication-ready visualizations
#
# HEXAGONAL CONFIGURATION:
#   The function creates 6 plots arranged in a hexagonal pattern:
#   - Plot 1: A vs B vs C
#   - Plot 2: C vs D vs A  
#   - Plot 3: D vs C vs E
#   - Plot 4: F vs E vs C
#   - Plot 5: C vs G vs F
#   - Plot 6: G vs C vs B
#
# SYNTAX:
#   create_joint_ternary_diagram(
#       xlsx_file,
#       output_dir = NULL,
#       working_dir = NULL,
#       ...
#   )
#
# PARAMETERS:
#   xlsx_file    - Path to Excel file containing the data
#   output_dir   - Output directory (optional, defaults to "plots_hexagonal")
#   working_dir  - Working directory (optional)
#   ...          - Exactly 7 element strings defining the hexagonal configuration
#
# ELEMENT SPECIFICATION:
#   - Single elements: "Element.(Wt%)" (e.g., "Al.(Wt%)")
#   - Combined elements: "Element1.(Wt%)+Element2.(Wt%)" (e.g., "SiO2.(Wt%)+Al2O3.(Wt%)")
#   - Must provide exactly 7 element specifications
#
# INPUT REQUIREMENTS:
#   - Excel file (.xlsx format)
#   - Data in first sheet (Sheet1)
#   - Column names matching element specifications
#   - Numerical data (typically weight percentages)
#
# OUTPUT:
#   - 6 PNG files with hexagonal ternary plots
#   - Timestamped output directory
#   - High-resolution images (publication-ready)
#   - Automatic directory creation
#
# DEPENDENCIES:
#   The function automatically installs and loads required packages:
#   - magick: Image processing
#   - openxlsx: Excel file handling
#   - Ternary: Ternary plotting
#   - PlotTools: Plot utilities
#   - grid: Grid graphics
#   - png: PNG file handling
#
# USAGE EXAMPLES:
#   # Basic usage
#   create_joint_ternary_diagram(
#       "data.xlsx",
#       "Cr.(Wt%)",
#       "C.(Wt%)+N.(Wt%)+O.(Wt%)",
#       "Al.(Wt%)+Si.(Wt%)",
#       "Mg.(Wt%)+Ca.(Wt%)",
#       "Mn.(Wt%)+Ti.(Wt%)",
#       "S.(Wt%)+P.(Wt%)",
#       "Fe.(Wt%)"
#   )
#
#   # With custom output directory
#   create_joint_ternary_diagram(
#       "data.xlsx",
#       output_dir = "custom_output",
#       "Element1.(Wt%)",
#       "Element2.(Wt%)+Element3.(Wt%)",
#       "Element4.(Wt%)",
#       "Element5.(Wt%)+Element6.(Wt%)",
#       "Element7.(Wt%)",
#       "Element8.(Wt%)",
#       "Element9.(Wt%)"
#   )
#
####TEST
#
#create_joint_ternary_diagram(
#  xlsx_file = "23458.xlsx",
#  output_dir = NULL,
# working_dir = "C: ...",
#  "S.(Wt%)",
#  "Mn.(Wt%)",
#  "O.(Wt%)",
#  "Mn.(Wt%)",
#  "S.(Wt%)",
#  "Ca.(Wt%)+Si.(Wt%)",
#  "N.(Wt%)"
#)
#
#"Si.(Wt%)+O.(Wt%)",                              
#
# TROUBLESHOOTING:
#   - Ensure exactly 7 element strings are provided
#   - Check Excel file format and column names
#   - Verify package installation permissions
#   - Check write permissions for output directory
#
# ========================================================================

create_joint_ternary_diagram <- function(
    xlsx_file,
    output_dir = NULL,
    working_dir = NULL,
    ...
) {
  # --- LIBRARIES ---
  pkgs <- c("magick", "openxlsx", "Ternary", "PlotTools", "grid", "png")
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  
  # --- Set working directory if provided ---
  if (!is.null(working_dir)) setwd(working_dir)
  
  # Helper to clean element labels
  clean_label <- function(x) gsub("\\.\\(Wt%\\)", "", x)
  
  # Parse ... as character, split by "+" and trim
  el_raw <- list(...)
  if (length(el_raw) != 7)
    stop("You must provide exactly 7 element strings, e.g. 'Cr.(Wt%)', 'C.(Wt%)+N.(Wt%)+O.(Wt%)', ...")
  element_sets <- lapply(el_raw, function(s) trimws(unlist(strsplit(s, "\\+"))))
  
  # Diagram configs
  cfg <- element_sets
  element_configs <- list(
    list(A=cfg[[1]], B=cfg[[2]], C=cfg[[3]]),
    list(A=cfg[[3]], B=cfg[[4]], C=cfg[[1]]),
    list(A=cfg[[4]], B=cfg[[3]], C=cfg[[5]]),
    list(A=cfg[[6]], B=cfg[[5]], C=cfg[[3]]),
    list(A=cfg[[3]], B=cfg[[7]], C=cfg[[6]]),
    list(A=cfg[[7]], B=cfg[[3]], C=cfg[[2]])
  )
  
  # Cleaned element labels for display
  element_labels <- vapply(cfg, function(x) paste(clean_label(x), collapse = "+"), character(1))
  all_elements <- unique(unlist(cfg))
  all_symbols <- gsub("\\..*", "", all_elements)
  elements_labels <- paste(all_symbols, collapse = ",")
  elements_labels_safe <- gsub("[^A-Za-z0-9]", "_", elements_labels)
  
  # Prepare output dir
  M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
  if (is.null(output_dir)) {
    plots_dir <- file.path(getwd(), "plots_hexagonal")
    if (!dir.exists(plots_dir)) dir.create(plots_dir)
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
    custom_folder <- file.path(plots_dir, paste0(elements_labels_safe, "_", "charge", file_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    if (dir.exists(custom_folder)) custom_folder <- file.path(plots_dir, paste0(elements_labels_safe, "_", "charge", file_base, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(custom_folder, recursive = TRUE)
  } else {
    custom_folder <- output_dir
    if (!dir.exists(custom_folder)) dir.create(custom_folder, recursive = TRUE)
  }
  
  # Crop triangle
  cut_triangle <- function(infile, outfile = infile) {
    img <- magick::image_read(infile)
    w <- magick::image_info(img)$width
    h <- magick::image_info(img)$height
    h_tri <- h * sqrt(3) / 2
    y_base <- (h - h_tri) / 2
    y_apex <- (h + h_tri) / 2
    maskfile <- tempfile(fileext = ".png")
    png(maskfile, width = w, height = h, bg = "black")
    par(mar = rep(0, 4))
    plot.new(); plot.window(xlim=c(0,w), ylim=c(0,h), asp=1)
    polygon(x=c(w/2,0,w), y=c(y_apex,y_base,y_base), col="white", border=NA)
    dev.off()
    mask <- magick::image_read(maskfile)
    unlink(maskfile)
    img_tri <- magick::image_composite(img, mask, operator="copyopacity")
    img_trim <- magick::image_trim(img_tri)
    magick::image_write(img_trim, outfile)
    invisible(outfile)
  }
  
  # Ternary plot
  plot_ternary <- function(M, elements_A, elements_B, elements_C, custom_folder, plot_num) {
    all_selected_elements <- c(elements_A, elements_B, elements_C)
    if (!all(all_selected_elements %in% colnames(M))) {
      missing <- setdiff(all_selected_elements, colnames(M))
      stop(paste("Column(s) missing in Excel file:", paste(missing, collapse=", ")))
    }
    matrika <- M[, all_selected_elements, drop = FALSE]
    row_sums <- rowSums(matrika, na.rm = TRUE)
    matrika <- matrika[row_sums > 0,, drop = FALSE]
    matrika <- na.omit(matrika)
    matrika <- as.matrix(matrika)
    ternary_data <- data.frame(
      A = if(length(elements_A)==1) matrika[,elements_A] else rowSums(matrika[,elements_A,drop=FALSE], na.rm=TRUE),
      B = if(length(elements_B)==1) matrika[,elements_B] else rowSums(matrika[,elements_B,drop=FALSE], na.rm=TRUE),
      C = if(length(elements_C)==1) matrika[,elements_C] else rowSums(matrika[,elements_C,drop=FALSE], na.rm=TRUE)
    )
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
    clean_labels_A <- paste(clean_label(elements_A), collapse = "+")
    clean_labels_B <- paste(clean_label(elements_B), collapse = "+")
    clean_labels_C <- paste(clean_label(elements_C), collapse = "+")
    plot_title <- paste0(plot_num, " Ternary Plot of ", clean_labels_A, ", ", clean_labels_B, ", ", clean_labels_C, " (charge ", file_base, ")")
    file_name <- paste0(gsub("[^A-Za-z0-9]", "_", plot_title), ".png")
    file_path <- normalizePath(file.path(custom_folder, file_name), winslash = "/", mustWork = FALSE)
    png(file_path, width = 800, height = 800, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    Ternary::TernaryPlot(
      atip = NULL, btip = NULL, ctip = NULL,
      alab = NULL, blab = NULL, clab = NULL,
      col = "lightgrey",
      grid.lines = 5,
      grid.lty = "dotted",
      grid.minor.lines = 10,
      grid.minor.lty = "dotted",
      padding = 0,
      axis.labels = FALSE
    )
    Ternary::TernaryPoints(ternary_data, type = "p", cex = 1, pch = 20, col = "black")
    dev.off()
    cut_triangle(file_path)
  }
  
  # Make 6 cropped ternary plots
  for (i in seq_along(element_configs)) {
    config <- element_configs[[i]]
    plot_ternary(
      M,
      elements_A = config$A,
      elements_B = config$B,
      elements_C = config$C,
      custom_folder = custom_folder,
      plot_num = i
    )
  }
  
  # Compose joint hexagonal diagram
  png_files <- list.files(custom_folder, pattern="\\.png$", full.names=TRUE)
  if (length(png_files) >= 6) {
    file_ctimes <- file.info(png_files)$ctime
    png_files_sorted <- png_files[order(file_ctimes, decreasing = FALSE)]
    composite_path <- file.path(custom_folder, paste0("Hexagonal_Ternary_of_", elements_labels_safe, ".png"))
    png(composite_path, width=1400, height=1400, bg="white")
    plot(NA, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), asp=1, axes=FALSE, xlab="", ylab="")
    
    title(main=paste("Združeni ternarni diagrami:", paste(element_labels, collapse = ", ")), 
          cex.main=2.5, font.main=2, line=2)
    
    # Subtitle line underneath
    title(main=paste("Na osnovi:", paste(element_labels[3], collapse = ","), paste(", šarža ", file_base) ), 
          cex.main=2.0, font.main=1, line=0.1)

    n <- 6
    tri_width <- 1.5
    radius <- tri_width / sqrt(3)
    angles <- seq(pi/6, pi/6 + 2*pi, length.out = n+1)[1:n]
    cx <- radius * cos(angles)
    cy <- radius * sin(angles)
    for (i in 1:n) {
      img <- png::readPNG(png_files_sorted[i])
      if (i %% 2 == 0) img <- as.raster(image_rotate(image_read(png_files_sorted[i]), 180))
      s <- tri_width
      h <- s * sqrt(3)/2
      v_x <- c(0,-s/2,s/2); v_y <- c(2*h/3,-h/3,-h/3)
      if (i %% 2 == 0) v_y <- -v_y
      x_min <- min(cx[i]+v_x); x_max <- max(cx[i]+v_x)
      y_min <- min(cy[i]+v_y); y_max <- max(cy[i]+v_y)
      rasterImage(img, x_min, y_min, x_max, y_max)
    }
    
    # Add element labels around the hexagon
    label_positions <- list(
      list(text = paste0(element_labels[2], " mas. % →"), pos = c(1.2, 0.7), rot = -60, size = 1.5),   # Top-right 
      list(text = paste0(element_labels[1], " mas. % →"), pos = c(0, 1.4), rot = 0, size = 1.5),       # Top
      list(text = paste0(element_labels[4], " mas. % →"), pos = c(-1.2, 0.7), rot = 60, size = 1.5),      # Top-left
      list(text = paste0("←", element_labels[5], " mas. %"), pos = c(-1.2, -0.7), rot = -60, size = 1.5),   # Bottom-Left
      list(text = paste0("←", element_labels[6], " mas. %"), pos = c(0, -1.4), rot = 0, size = 1.5),   # Bottom
      list(text = paste0("←", element_labels[7], " mas. %"), pos = c(1.2, -0.7), rot = 60, size = 1.5)      # Bottom-Right
    )
    
    # Apply all labels
    for(label in label_positions) {
      text(label$pos[1], label$pos[2], label$text, cex = label$size, 
           srt = label$rot, adj = 0.5, col = "black")
    }
    
    #text(0, -0.05, paste(element_labels[3]), cex=2.0)
    text(1.6, 0, paste(element_labels[2]), cex=2.0)
    text(0.8, 1.4, paste(element_labels[1]), cex=2.0)
    text(-0.8, 1.4, paste(element_labels[4]), cex=2.0)
    text(-1.6, 0, paste(element_labels[5]), cex=2.0)
    text(-0.8, -1.4, paste(element_labels[6]), cex=2.0)
    text(0.8, -1.4, paste(element_labels[7]), cex=2.0)
    
    dev.off()
    cat("Hexagonal ternary plot saved to:", composite_path, "\n")
  } else {
    warning("Not enough PNG files found to create a hexagonal composite plot.")
    return(NULL)
  }
}
