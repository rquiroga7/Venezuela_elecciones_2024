library(ggplot2)

create_scatterplot <- function(data_merged,group_by, merge_by, alphad=0.15) {
  # Process data
  #data2013_processed <- process_data(data2013, group_by, merge_by)
  #data2024_processed <- process_data(data2024, group_by, merge_by)
  
  # Merge data
  #data_merged <- merge(data2013_processed, data2024_processed, by = merge_by[1], suffixes = c("_2013", "_2024"))
  
  #from merge_by variable, if it exists and if it contains more than one element, delete the "ESTADO" element, which can be first or last. For remaining elements, use lower()
  if (length(merge_by) > 1) {
    merge_bys <- merge_by[-which(merge_by == "ESTADO")]
  } else {merge_bys <- merge_by}
  merge_bys <- tolower(merge_bys)
  
  # Automatically generate labels and title
  
  x_label <- paste0("Maduro_pctdiff_2013 (Maduro_pct_",merge_bys,"_2013 - avg_Maduro_pct_", group_by, "_2013)")
  y_label <- paste0("Maduro_pctdiff_2024 (Maduro_pct_",merge_bys,"_2024 - avg_Maduro_pct_", group_by, "_2024)")
  title <- paste0("Diferencia porcentual para Maduro por ",merge_bys," con respecto al promedio por ", group_by)
  output_file <- paste0("test_Maduro_pctdiff_2013_vs_Maduro_pctdiff_2024_",merge_bys,"_vs_", group_by, ".png")
  
  # Create scatterplot
  plot <- ggplot(data_merged, aes(x = Maduro_pctdiff_2013, y = Maduro_pctdiff_2024, color = ESTADO_2013, size = votos_totales_2024)) +
    geom_point(alpha = alphad, show.legend = FALSE) +
    geom_abline(slope = 1, alpha = 0.25, intercept = 0, color = "black", linetype = "dashed") +
    labs(x = x_label,
         y = y_label,
         title = title,
         size = "Votos totales",
         color = "ESTADO") +
    theme_light(base_size = 9) +
    facet_wrap(~ ESTADO_2013, scales = "fixed", ncol = 6) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.text = element_text(color = "black", face = "bold"),
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",
          legend.box.just = "center") +
    scale_size_continuous(range = c(0.2, 2.5), guide = "none") +
    xlim(-100, 100) +
    ylim(-100, 100)
  
  # Save plot
  ggsave(output_file, plot, width = 18, height = 18, units = "cm", dpi = 500)
}

create_densityplot <- function(data_merged,group_by, merge_by,alphad=0.3) {
  # Process data
  #data2013_processed <- process_data(data2013, group_by, merge_by)
  #data2024_processed <- process_data(data2024, group_by, merge_by)
  
  # Merge data
  #data_merged <- merge(data2013_processed, data2024_processed, by = merge_by[1], suffixes = c("_2013", "_2024"))
  
  #from merge_by variable, if it exists and if it contains more than one element, delete the "ESTADO" element, which can be first or last. For remaining elements, use lower()
  if (length(merge_by) > 1) {
    merge_bys <- merge_by[-which(merge_by == "ESTADO")]
  } else {merge_bys <- merge_by}
  merge_bys <- tolower(merge_bys)
  
  # Automatically generate labels and title
  
  x_label <- paste0("Maduro_pctdiff_2013 (Maduro_pct_",merge_bys,"_2013 - avg_Maduro_pct_", group_by, "_2013)")
  y_label <- paste0("Maduro_pctdiff_2024 (Maduro_pct_",merge_bys,"_2024 - avg_Maduro_pct_", group_by, "_2024)")
  title <- paste0("Diferencia porcentual para Maduro por ",merge_bys," con respecto al promedio por ", group_by)
  output_file <- paste0("test_density_Maduro_pctdiff_2013_vs_Maduro_pctdiff_2024_",merge_bys,"_vs_", group_by, ".png")
  
  # Create densityplot
  plot <- ggplot(data_merged, aes(x = Maduro_pctdiff_2013, y = Maduro_pctdiff_2024, color = ESTADO_2013, size = votos_totales_2024)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = alphad) +
    geom_abline(slope = 1, alpha = 0.25, intercept = 0, color = "black", linetype = "dashed") +
    labs(x = x_label,
         y = y_label,
         title = title,
         size = "Votos totales",
         color = "ESTADO") +
    theme_light(base_size = 9) +
    facet_wrap(~ ESTADO_2013, scales = "fixed", ncol = 6) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.text = element_text(color = "black", face = "bold"),
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",
          legend.box.just = "center") +
    scale_size_continuous(range = c(0.2, 2.5), guide = "none") +
    xlim(-100, 100) +
    ylim(-100, 100)+
    scale_fill_viridis_c(option = "inferno")
  
  # Save plot
  ggsave(output_file, plot, width = 18, height = 18, units = "cm", dpi = 500)
}

process_data <- function(data, group, eval) {
  data %>%
    group_by(across(all_of(c(eval,group)))) %>%
    summarise(Maduro = sum(Maduro), votos_totales = sum(votos_totales)) %>%
    mutate(Maduro_pct = Maduro / votos_totales) %>%
    group_by(across(all_of(group))) %>%
    mutate(avg_Maduro_pct = mean(Maduro_pct)) %>%
    ungroup() %>%
    group_by(across(all_of(c(eval,group)))) %>%
    mutate(Maduro_pctdiff = (Maduro_pct - avg_Maduro_pct) / avg_Maduro_pct * 100) %>%
    ungroup()
}