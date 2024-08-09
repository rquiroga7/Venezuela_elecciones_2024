library(tidyverse)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggrepel)

#import functions from functions.R
source("functions.R")

 data <- read.csv("RESULTADOS_2024_CSV_V1.csv", header = TRUE)

  # Create the "OTROS" column and remove the component columns
  data <- data %>%
  mutate(Codcv=paste0(CENTRO,MESA)) %>%
  mutate(Maduro=NM,Gonzalez=EG, ESTADO=EDO,municipio=MUN,parroquia=PAR,votos_totales = VOTOS_VALIDOS) %>%
  mutate(OTROS = rowSums(select(., starts_with("LM"):starts_with("BERA"))) + VOTOS_NULOS) %>%
  group_by(CENTRO, ESTADO, municipio, parroquia) %>%
   summarise(
      Maduro = sum(Maduro),
      Gonzalez = sum(Gonzalez),
      #Sum all other candidates
      OTROS = sum(OTROS),
      #Get highest value between all candidates that are not Maduro
      votos_totales = sum(votos_totales),
      #for votantes keep first value
      registrados = sum(RE),
      .groups = "drop"
    )


# Generate table of results, sum votes for Maduro and Gonzalez across ESTADOS
table <- data %>%
  group_by(ESTADO) %>%
  summarise(Maduro = sum(Maduro), Gonzalez = sum(Gonzalez), OTROS = sum(OTROS), votos_totales = sum(votos_totales),registrados=sum(registrados)) %>%
  arrange(ESTADO)

# Add Totales row
table <- table %>%
  add_row(ESTADO = "Totales",
          Maduro = sum(table$Maduro),
          Gonzalez = sum(table$Gonzalez),
          votos_totales = sum(table$votos_totales))

# Make a pretty table, for Maduro and Gonzalez show votes and in parenthesis the percentage of votos_totales
table <- table %>%
  mutate(Maduro = sprintf("%s (%.2f%%)", Maduro, Maduro/votos_totales*100),
         Gonzalez = sprintf("%s (%.2f%%)", Gonzalez, Gonzalez/votos_totales*100)) %>%
  select(ESTADO, Maduro, Gonzalez,  votos_totales) %>%
  kable("html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2:3, width = "6cm") %>%
  column_spec(4, width = "3cm") %>%
  row_spec(0, bold = TRUE, color = "black") %>%
  row_spec(nrow(table), bold = TRUE, color = "black")

# Write html table
writeLines(as.character(table), "results.html")


data2 <- read.csv("ven2013_estado.csv", header = TRUE)

# Generate table of results, sum votes for Maduro and Capriles across ESTADOS
table2 <- data2 %>%
  arrange(ESTADO) %>%
  select(ESTADO, Electores, Maduro, Capriles, votos_totales)

# Add Totales row
table2 <- table2 %>%
 add_row(ESTADO = "Totales",
          Electores= sum(table2$electores),
          Maduro = sum(table2$Maduro),
          Capriles = sum(table2$Capriles),
          votos_totales = sum(table2$votos_totales))

# Make a pretty table, for Maduro and Capriles show votes and in parenthesis the percentage of votos_totales
table2 <- table2 %>%
  mutate(Maduro = sprintf("%s (%.2f%%)", Maduro, Maduro/votos_totales*100),
         Capriles = sprintf("%s (%.2f%%)", Capriles, Capriles/votos_totales*100)) %>%
  select(ESTADO, Maduro, Capriles, votos_totales) %>%
  kable("html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(2:3, width = "6cm") %>%
  column_spec(4, width = "3cm") %>%
  row_spec(0, bold = TRUE, color = "black") %>%
  row_spec(nrow(table2), bold = TRUE, color = "black")

# Write html table
writeLines(as.character(table2), "results_2013.html")




#PLOTS
# Assuming your data frame is named `data` and has columns: Gonzalez, votos_totales, registrados, Estado

# Calculate Gonzalez% and participación
data3 <- data %>%
  mutate(Gonzalez_pct = Gonzalez / votos_totales,
         participacion = votos_totales / registrados,
         Maduro_pct = Maduro / votos_totales)

#remove "DTTO. " and "EDO. " from all ESTADO values
data3$ESTADO <- gsub("EDO. ", "", data3$ESTADO)
data3$ESTADO <- gsub("EDO.NVA", "NVA", data3$ESTADO)
data3$ESTADO <- gsub("DTTO. ", "", data3$ESTADO)

# Create the faceted scatterplot for Gonzalez_pct
 # Create the faceted scatterplot for Gonzalez_pct
  ggplot(data3, aes(y = participacion, x = Gonzalez_pct, color = ESTADO, size = registrados)) +
    geom_point(alpha = 0.10) +  # Transparent dots
    facet_wrap(~ ESTADO, scales = "fixed", ncol = 8) +  # Facet by Estado with fixed scales
    labs(y = "Participación (votos_totales / registrados)",
         x = "Gonzalez_pct (Gonzalez / votos_totales)",
         title = "Gonzalez_pct vs Participación por Estado") +
    theme_light(base_size = 9) +  # Use a minimal theme
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray")) +  # Set facet header background to light gray
    scale_color_viridis_d(end = 0.8) +  # Use colorblind-friendly palette
    scale_size_continuous(range = c(0.1, 1.5))  # Adjust the range of dot sizes
  ggsave("Gonzalez_vs_Participacion.png", width = 18, height = 12, units = "cm", dpi = 500)

  # Create the faceted scatterplot for Maduro_pct
  ggplot(data3, aes(y = participacion, x = Maduro_pct, color = ESTADO, size = registrados)) +
    geom_point(alpha = 0.10) +  # Transparent dots
    facet_wrap(~ ESTADO, scales = "fixed", ncol = 8) +  # Facet by Estado with fixed scales
    labs(y = "Participación (votos_totales / registrados)",
         x = "Maduro_pct (Maduro / votos_totales)",
         title = "Maduro_pct vs Participación por Estado") +
    theme_light(base_size = 9) +  # Use a minimal theme
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray")) +  # Set facet header background to light gray
    scale_color_viridis_d(end = 0.8) +  # Use colorblind-friendly palette
    scale_size_continuous(range = c(0.1, 1.5))  # Adjust the range of dot sizes
  ggsave("Maduro_vs_Participacion.png", width = 18, height = 12, units = "cm", dpi = 500)
  



#POR ESTADO



# Loop through each unique ESTADO
for (estado in unique(data$ESTADO)) {
  # Filter data for the current ESTADO
  estado_data <- data3 %>% filter(ESTADO == estado)
  
  # Create the scatterplot for Gonzalez_pct
  p1 <- ggplot(estado_data, aes(y = participacion, x = Gonzalez_pct, color = municipio, size = registrados)) +
    geom_point(alpha = 0.30) +  # Transparent dots
    labs(y = "Participación (votos_totales / registrados)",
         x = "Gonzalez_pct (Gonzalez / votos_totales)",
         title = paste("Gonzalez_pct vs Participación en", estado),
         size = "Votantes registrados por mesa",
         color = "Municipios") +
    theme_light(base_size = 9) +  # Use a minimal theme
    theme(legend.position = "bottom",  # Move legend to the bottom
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",  # Stack legends vertically
          legend.box.just = "center") +  # Align legends to the left
    scale_size_continuous(range = c(0.2, 2.5)) +  # Adjust the range of dot sizes
    guides(size = guide_legend(order = 1, title.position = "top"),  # Place size legend on top
           color = guide_legend(order = 2, title.position = "bottom"))  # Place municipio legend on bottom
  
  # Save the plot
  ggsave(paste0("Gonzalez_vs_Participacion_", estado, ".png"), plot = p1, width = 18, height = 18, units = "cm", dpi = 500)
  
  # Create the scatterplot for Maduro_pct
  p2 <- ggplot(estado_data, aes(y = participacion, x = Maduro_pct, color = municipio, size = registrados)) +
    geom_point(alpha = 0.30) +  # Transparent dots
    labs(y = "Participación (votos_totales / registrados)",
         x = "Maduro_pct (Maduro / votos_totales)",
         title = paste("Maduro_pct vs Participación en", estado),
         size = "Votantes registrados por mesa",
         color = "Municipios") +
    theme_light(base_size = 9) +  # Use a minimal theme
    theme(legend.position = "bottom",  # Move legend to the bottom
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",  # Stack legends vertically
          legend.box.just = "center") +  # Align legends to the left
    scale_size_continuous(range = c(0.2, 2.5)) +  # Adjust the range of dot sizes
    guides(size = guide_legend(order = 1, title.position = "top"),  # Place size legend on top
           color = guide_legend(order = 2, title.position = "bottom"))  # Place municipio legend on bottom
  
  # Save the plot
  ggsave(paste0("Maduro_vs_Participacion_", estado, ".png"), plot = p2, width = 18, height = 18, units = "cm", dpi = 500)
}

#######################################################
#Read 2013 results
data2024 <- read.csv("RESULTADOS_2024_CSV_V1.csv", header = TRUE)
#Remove "DTTO. " and "EDO. " from all ESTADO values
data2024$EDO <- gsub("EDO. ", "", data2024$EDO)
data2024$EDO <- gsub("EDO.", "", data2024$EDO)
data2024 <- data2024 %>% mutate(EDO=ifelse(EDO=="LA GUAIRA","La Guaira (Vargas)",EDO))
data2024$EDO<-as.factor(data2024$EDO)
#Process 2013 data
data2013 <- read.csv("resultados_elecc_2013-04-14-v4.csv", header = TRUE)
data2013 <- data2013 %>% filter(Estado != "Embajadas" & Estado != "Zonas Inhóspitas")
data2013 <- data2013 %>% mutate(Estado=ifelse(Estado=="Vargas","La Guaira (Vargas)",Estado))
data2013$Estado <- as.factor(data2013$Estado)
levels(data2024$EDO)
levels(data2013$Estado)
#Change level names of data2024$EDO to those of data2013$Estado
levels(data2024$EDO) <- levels(data2013$Estado)

#Process 2013 data
  # Create the "OTROS" column and remove the component columns
  data2013b <- data2013 %>%
  ungroup() %>%
  #Remove rows with missing values
  filter(!is.na(maduro) & !is.na(capriles)) %>%
  mutate(CENTRO=codigo.nuevo , MESA=mesa, Codcv=paste0(CENTRO,MESA),COD_MUN=as.integer(cod_mpo)) %>%
  mutate(Maduro=maduro,Capriles=capriles, ESTADO=Estado,municipio=Municipio,parroquia=Parroquia,votos_totales = votos.válidos) %>%
  mutate(OTROS = rowSums(select(., starts_with("sequera"):starts_with("mendez"))) + votos.nulos) %>%
  group_by(CENTRO, ESTADO, municipio, parroquia,COD_MUN) %>%
   summarise(
      Maduro = sum(Maduro),
      Capriles = sum(Capriles),
      #Sum all other candidates
      OTROS = sum(OTROS),
      #Get highest value between all candidates that are not Maduro
      votos_totales = sum(votos_totales),
      #for votantes keep first value
      registrados = sum(electores.esperados),
      .groups = "drop"
    )

  data2024b <- data2024 %>%
  mutate(Codcv=paste0(CENTRO,MESA)) %>%
  mutate(Maduro=NM,Gonzalez=EG, ESTADO=EDO,municipio=MUN,parroquia=PAR,votos_totales = VOTOS_VALIDOS) %>%
  #Take COD_EDO as a two digit number, and paste it with cod_mpo as a two digit number to create a four digit number
  mutate(COD_MUN = as.integer(paste0(formatC(as.integer(COD_EDO), width = 2, flag = "0"),formatC(as.integer(COD_MUN), width = 2, flag = "0")))) %>%
  mutate(OTROS = rowSums(select(., starts_with("LM"):starts_with("BERA"))) + VOTOS_NULOS) %>%
  group_by(CENTRO, ESTADO, municipio, parroquia,COD_MUN) %>%
   summarise(
      Maduro = sum(Maduro),
      Gonzalez = sum(Gonzalez),
      #Sum all other candidates
      OTROS = sum(OTROS),
      #Get highest value between all candidates that are not Maduro
      votos_totales = sum(votos_totales),
      #for votantes keep first value
      registrados = sum(RE),
      .groups = "drop"
    )


# For data2013b and data2024b, first calculate the Maduro_pct. Then, for each ESTADO, calculate the average Maduro_pct value. Then, for each row, calculate the difference between the Maduro_pct value and the average Maduro_pct value for that ESTADO. Save as Maduro_pctdiff.
data2013b2 <- process_data(data2013b, "ESTADO", "CENTRO")
data2024b2 <- process_data(data2024b, "ESTADO", "CENTRO")
#Now merge data2013b and data2024b by CENTRO, ESTADO, municipio, and parroquia. 
data_merged <- merge(data2013b2, data2024b2, by = "CENTRO", suffixes = c("_2013", "_2024"))
#Now do a scatterplot, where transparent dots are colored by ESTADO, size is determined by votos_totales, and x is Maduro_pctdiff_2013 and y is Maduro_pctdiff_2024.
create_scatterplot(data_merged, "ESTADO", c("CENTRO"))
create_densityplot(data_merged, "ESTADO", c("CENTRO"),alphad=0.8)

#Now do muni versus estado
data2013b3 <- process_data(data2013b, "municipio", c("CENTRO", "ESTADO"))
data2024b3 <- process_data(data2024b, "municipio", c("CENTRO", "ESTADO"))
#Now merge data2013b and data2024b by CENTRO, ESTADO, municipio, and parroquia. 
data_merged <- merge(data2013b3, data2024b3, by = "CENTRO", suffixes = c("_2013", "_2024"))
create_scatterplot(data_merged, "municipio", c("CENTRO", "ESTADO"))
create_densityplot(data_merged, "municipio", c("CENTRO", "ESTADO"),alphad=0.8)

#Now do muni versus estado
data2013c <- process_data(data2013b, "ESTADO", c("COD_MUN", "municipio")) %>% mutate(ANIO = 2013)
data2024c <- process_data(data2024b, "ESTADO", c("COD_MUN", "municipio")) %>% mutate(ANIO = 2024)
#Now merge data2013b and data2024b by CENTRO, ESTADO, municipio, and parroquia. 
data_mergedc <- merge(data2013c, data2024c, by = "COD_MUN", suffixes = c("_2013", "_2024"))
create_scatterplot(data_mergedc, "ESTADO", c("municipio"),alphad=0.8)

data_mergedcr <- rbind(data2013c, data2024c)
#Make a new column, grouping by ANIO and then calculating an inverse ranking of Maduro_pct. The lowest value is 1 and the largest is the amount of unique municipio values


#Now do a side by side plot, facet at the AÑO level, plotting Part_pct against Maduro_pct with a for loop that loops for each ESTADO, coloring by municipio, sizing by votos_totales, and label each dot with the municipio value.
for (estado in unique(data_mergedcr$ESTADO)) {
  # Filter data for the current ESTADO
  estado="Miranda"
  estado_data <- data_mergedcr %>% filter(ESTADO == estado)
  estado_data <- estado_data %>% group_by(ANIO) %>% mutate(Maduro_invrank = rank(Maduro_pct)) %>% ungroup()
  
  # Create the scatterplot for Maduro_pct
p1 <- ggplot(estado_data, aes(y = Part_pct, x = Maduro_invrank, color = municipio, size = votos_totales)) +
    geom_point(alpha = 0.80) +  # Transparent dots
    geom_text(aes(label = municipio, y = ifelse(ANIO == 2024, 0.40, ifelse(ANIO == 2013, 0.70, NA))), size = 2, angle = 90) +  # Add text labels at specific y-values
    labs(y = "Participación (votos_totales / registrados)",
         x = "Ranking de municipios por Maduro_pct (menor a mayor)",
         title = paste("Ranking municipios Maduro_pct vs Participación en", estado),
         size = "Votantes registrados por mesa",
         color = "Municipios") +
    theme_light(base_size = 14) +  # Use a minimal theme
    theme(legend.position = "bottom",  # Move legend to the bottom
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",  # Stack legends vertically
          legend.box.just = "center") +  # Align legends to the left

    scale_size_continuous(range = c(1.0, 6.0)) +  # Adjust the range of dot sizes
    guides(color = "none") +  # Remove color legend
    facet_wrap(. ~ ANIO, ncol = 2, scales = "free")
p1
  # Save the plot
  ggsave(paste0("./plots_estados/","Maduro_vs_Participacion_", estado, ".png"), plot = p1, width = 18, height = 12, units = "cm", dpi = 500)

}

#Using datamergedc, create a for loop that loops for each ESTADO, coloring by municipio, sizing by votos_totales, and label each dot with the municipio value.
for (estado in unique(data_mergedc$ESTADO_2013)) {
  # Filter data for the current ESTADO
  #estado="Miranda"
  estado_data <- data_mergedc %>% filter(ESTADO_2013 == estado)
  estado_data <- estado_data %>% mutate(Maduro_invrank_2024 = rank(Maduro_pct_2024),Maduro_invrank_2013 = rank(Maduro_pct_2013)) %>% ungroup()
  
  # Create the scatterplot for Maduro_pct
p2 <- ggplot(estado_data, aes(y = Maduro_invrank_2024, x = Maduro_invrank_2013, color = municipio_2024, size = votos_totales_2024)) +
    geom_point(alpha = 0.80) +  # Transparent dots
    geom_text_repel(aes(label = municipio_2024), size = 3) +  # Add text labels with repel
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # Add abline with slope 1
    scale_x_continuous(limits = c(1, NA)) +  # Set x-axis to start at 1
    scale_y_continuous(limits = c(1, NA)) +  # Set y-axis to start at 1
    labs(y = "2024: Ranking de municipios Maduro_pct (inv)",
         x = "2013: Ranking de municipios Maduro_pct (inv)",
         title = paste("Ranking municipios Maduro_pct en", estado),
         size = "Votos válidos totales por municipio",
         color = "Municipios") +
    theme_light(base_size = 12) +  # Use a minimal theme
    theme(legend.position = "bottom",  # Move legend to the bottom
          axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis text and change font size
          strip.text = element_text(color = "black", face = "bold"),  # Change facet header text color to black and bold
          strip.background = element_rect(fill = "lightgray"),
          legend.box = "vertical",  # Stack legends vertically
          legend.box.just = "center") +  # Align legends to the left
    scale_size_continuous(range = c(1.0, 6.0)) +  # Adjust the range of dot sizes
    guides(color = "none")   # Remove color legend

p2
  # Save the plot
  ggsave(paste0("./plots_estados/","Maduro_rank_2024_vs_2013_", estado, ".png"), plot = p2, width = 18, height = 14, units = "cm", dpi = 500)

}

