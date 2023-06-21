library(readxl)
library(tidyverse)

# Define the functions for loading and cleaning data
source("CT0185__source/df_generation.R")

# Load and clean data for both plates
plate1 <- clean_data("CT0185__data/plate1_DD_corrected.xls")
plate2 <- clean_data("CT0185__data/plate2_DD_corrected.xls")

# Merge the plates
merged_plates <- full_join(plate1, plate2, 
                           by=c("Sample", "Analyte", "concentration"), 
                           suffix=c("_plate1", "_plate2")) %>%
  mutate(time = recode(substr(Sample, 1, 1), "F" = "24H", "S" = "48H", "T" = "72H"),
         across(c("concentration", "FI_plate1", "FI - Bkgd_plate1", "FI_plate2", "FI - Bkgd_plate2"), as.numeric),
         Avg_FI_Bkgd = (`FI - Bkgd_plate1` + `FI - Bkgd_plate2`) / 2,
         `OOR Info_plate1` = ifelse(`Obs Conc_plate1` %in% c("OOR <", "OOR >") | grepl("\\*", `Obs Conc_plate1`), `Obs Conc_plate1`, NA),
         `OOR Info_plate2` = ifelse(`Obs Conc_plate2` %in% c("OOR <", "OOR >") | grepl("\\*", `Obs Conc_plate2`), `Obs Conc_plate2`, NA),
         `Obs Conc_plate1` = clean_data2(`Obs Conc_plate1`),
         `Obs Conc_plate2` = clean_data2(`Obs Conc_plate2`))
         
merged_plates <- merged_plates %>%
 mutate(`Avg Obs Conc` = rowMeans(select(., `Obs Conc_plate1`, `Obs Conc_plate2`), na.rm = TRUE))

merged_plates$`CV%` <- apply(merged_plates[c("Obs Conc_plate1", "Obs Conc_plate2")], 1, 
                            function(x) 100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))

# Select the desired columns
columns_to_keep <- c("Sample", "Analyte", "concentration", "FI_plate1", "FI - Bkgd_plate1", 
                     "Obs Conc_plate1", "FI_plate2", "FI - Bkgd_plate2", "Obs Conc_plate2",
                     "OOR Info_plate1", "OOR Info_plate2", "Avg_FI_Bkgd", "Avg Obs Conc", "CV%", "time")
merged_plates <- select(merged_plates, all_of(columns_to_keep))

# Filter out undesired analytes
remove_analytes <- c("CXCL11", "IFN gamma", "IL-13", "IL-6", "MIG", "PDGF-BB", "TNF alpha", "VEGF-R2")
filtered_df <- merged_plates %>% filter(!Analyte %in% remove_analytes)

# Function to create ggplots
create_plot <- function(data, x, y, color, ylab, file_name) {
  p <- ggplot(data, aes(x = get(x), y = get(y), color = get(color))) +
    geom_point() +
    facet_wrap(~ Analyte, scales = "free") +
    labs(title = paste(ylab, "by", x, "and", color),
         x = "Bleomycin Concentration [µg/ml]",
         y = ylab,
         color = color) +
    theme_minimal()
  
  ggsave(file = file_name, plot = p, bg = "white")
}

# Function to create a summary dataframe
create_summary_df <- function(data, measurement){
  data %>%
    group_by(concentration, time, Analyte) %>%
    summarise(mean = mean(get(measurement), na.rm = TRUE),
              sd = sd(get(measurement), na.rm = TRUE),
              .groups = "drop")
}

# Generate and save the plots
create_plot(filtered_df, 'concentration', 'Avg_FI_Bkgd', 'time', 'Background Adjusted FI', "CT0185__results/Background_Adjusted_FI.png")
create_plot(filtered_df, 'concentration', 'Avg Obs Conc', 'time', 'Average Observed Analyte Concentration', "CT0185__results/Avg_Observed_Concentration.png")

# Create a summary dataframe and generate plots
for (measurement in c('Avg_FI_Bkgd', 'Avg Obs Conc')) {
  summary_df <- create_summary_df(filtered_df, measurement)
  
  p <- ggplot(summary_df, aes(x = concentration, y = mean, color = time)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    facet_wrap(~ Analyte, scales = "free") +
    labs(title = paste("Mean and Standard Deviation of", measurement, "by Concentration and time"),
         x = "Bleomycin Concentration [µg/ml]",
         y = measurement,
         color = "time") +
    theme_minimal()
  
  file_name = paste0("CT0185__results/Mean_StdDev_", gsub(" ", "_", measurement), ".png")
  ggsave(file = file_name, plot = p, bg = "white")
}
