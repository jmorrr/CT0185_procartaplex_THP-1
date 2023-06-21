library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Define the function for loading and cleaning data
source("CT0185__source/df_generation.R")

# Load and clean data for both plates
plate1 <- clean_data("CT0185__data/plate1_DD_corrected.xls")
plate2 <- clean_data("CT0185__data/plate2_DD_corrected.xls")

# Merge the plates
merged_plates <- merge(plate1, plate2, 
                       by=c("Sample", "Analyte", "concentration"), 
                       suffixes=c("_plate1", "_plate2"))

# Select the desired columns
columns_to_keep <- c("Sample", "Analyte", "concentration", "FI_plate1", "FI - Bkgd_plate1", 
                     "Obs Conc_plate1", "FI_plate2", "FI - Bkgd_plate2", "Obs Conc_plate2")
merged_plates <- select(merged_plates, all_of(columns_to_keep))

# Add additional columns
merged_plates <- merged_plates %>%
  mutate(time = substr(Sample, 1, 1),
         across(c("concentration", "FI_plate1", "FI - Bkgd_plate1", "FI_plate2", "FI - Bkgd_plate2"), as.numeric),
         time = recode(time, "F" = "24H", "S" = "48H", "T" = "72H"),
         Avg_FI_Bkgd = (`FI - Bkgd_plate1` + `FI - Bkgd_plate2`) / 2)

# Define a function to clean the data
clean_data2 <- function(x){
  x <- as.character(x)
  x_n <- gsub("\\*", "", x) # Remove asterisks
  x_n <- ifelse(x_n %in% c("OOR <", "OOR >"), NA, as.numeric(x_n))
  return(x_n)
}

# Add cleaned columns and move OOR and asterisk information into new columns
merged_plates <- merged_plates %>%
  mutate(`OOR Info_plate1` = ifelse(`Obs Conc_plate1` %in% c("OOR <", "OOR >") | grepl("\\*", `Obs Conc_plate1`), `Obs Conc_plate1`, NA),
         `OOR Info_plate2` = ifelse(`Obs Conc_plate2` %in% c("OOR <", "OOR >") | grepl("\\*", `Obs Conc_plate2`), `Obs Conc_plate2`, NA),
         `Obs Conc_plate1` = clean_data2(`Obs Conc_plate1`),
         `Obs Conc_plate2` = clean_data2(`Obs Conc_plate2`))

merged_plates <- merged_plates %>%
  mutate(`Avg Obs Conc` = rowMeans(select(., `Obs Conc_plate1`, `Obs Conc_plate2`), na.rm = TRUE))

merged_plates$`CV%` <- apply(merged_plates[c("Obs Conc_plate1", "Obs Conc_plate2")], 1, 
                             function(x) 100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))

remove_analytes <- c("CXCL11", "IFN gamma", "IL-13", "IL-6", "MIG", "PDGF-BB", "TNF alpha", "VEGF-R2")

filtered_df <- merged_plates[!merged_plates$Analyte %in% remove_analytes,]


# Generate a plot with all analytes and times on a single graph
# First plot the average adjusted fluo intensity (FI)
p1 <- ggplot(data = filtered_df, aes(x = concentration, y = `Avg_FI_Bkgd`, color = time)) +
  geom_point() +
  facet_wrap(~ Analyte, scales = "free") +
  labs(title = "Background Adjusted FI by Concentration and time",
       x = "Bleomycin Concentration [µg/ml]",
       y = "Background Adjusted FI",
       color = "time") +
  theme_minimal()

# Save the plot
ggsave(file = "CT0185__results/Background_Adjusted_FI.png", plot = p1, bg = "white")

# Generate a plot with all analytes and times on a single graph
# Then plot the average Observed Concentration
# NaN values are removed resulting in much reduced graphs.
p2 <- ggplot(data = filtered_df, aes(x = concentration, y = `Avg Obs Conc`, color = time)) +
  geom_point() +
  facet_wrap(~ Analyte, scales = "free") +
  labs(title = "Avg observed analyte concentration by bleomycin concentration and time",
       x = "Bleomycin Concentration [µg/ml]",
       y = "Average Observed Analyte Concentration",
       color = "time") +
  theme_minimal()

# Save the plot
ggsave(file = "CT0185__results/Avg_Observed_Concentration.png", plot = p2, bg = "white")

# Create a summary dataframe
summary_df <- filtered_df %>%
  group_by(concentration, time, Analyte) %>%
  summarise(mean = mean(`Avg_FI_Bkgd`, na.rm = TRUE),
            sd = sd(`Avg_FI_Bkgd`, na.rm = TRUE),
            .groups = "drop") # This avoids grouping in the resulting dataframe

# Plot
p3 <- ggplot(data = summary_df, aes(x = concentration, y = mean, color = time)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  facet_wrap(~ Analyte, scales = "free") +
  labs(title = "Mean and Standard Deviation of Background Adjusted FI by Concentration and time",
       x = "Bleomycin Concentration [µg/ml]",
       y = "Background Adjusted FI",
       color = "time") +
  theme_minimal()

# Save the plot
ggsave(file = "CT0185__results/Mean_StdDev_Background_Adjusted_FI.png", plot = p3, bg = "white")

# Create a summary dataframe
summary_df2 <- filtered_df %>%
  group_by(concentration, time, Analyte) %>%
  summarise(mean = mean(`Avg Obs Conc`, na.rm = TRUE),
            sd = sd(`Avg Obs Conc`, na.rm = TRUE),
            .groups = "drop") # This avoids grouping in the resulting dataframe

# Plot
p4 <- ggplot(data = summary_df2, aes(x = concentration, y = mean, color = time)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  facet_wrap(~ Analyte, scales = "free") +
  labs(title = "Mean and Standard Deviation of Average Observed Concentration by Concentration and time",
       x = "Bleomycin Concentration [µg/ml]",
       y = "Average Observed Analyte Concentration",
       color = "time") +
  theme_minimal()

# Save the plot
ggsave(file = "CT0185__results/Mean_StdDev_Average_Observed_Concentration.png", plot = p4, bg = "white")

