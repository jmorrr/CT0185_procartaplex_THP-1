clean_data <- function(filename) {
  
  # Load the data from Excel file
  df <- read_excel(filename, skip = 7)
  
  # Remove rows where every value is NA (missing) or NA values in all but the first column
  df <- df[!apply(is.na(df[, -1]), 1, all), ]
  
  # Define the new column names
  new_colnames <- c("Analyte", "Type", "Well", "Description", "FI", "FI - Bkgd", "Std Dev", "%CV", "Obs Conc", 
                    "Exp Conc", "(Obs/Exp) * 100", "Group", "Ratio", "Dilution")
  
  # Assign the new column names to the dataframe
  names(df) <- new_colnames
  
  # Remove rows which are exactly equal to column names or with wells that contain a comma
  df <- df[!apply(df, 1, function(x) all(x == new_colnames)) & !grepl(",", df$Well), ]
  
  # Remove numbers in parentheses in the "Analyte" column
  df$Analyte <- sub("\\s*\\(\\d+\\)", "", df$Analyte)
  
  # Read the plate layout csv
  plate_df <- read.csv("CT0185__data/plate_layout-2023-06-05-Procarta_plex_THP-1_bleomycin.csv", stringsAsFactors = FALSE)
  
  # Merge dataframes by the 'Well' column
  merged_df <- merge(df, plate_df, by = "Well")
  
  # Convert your concentration information into a dataframe
  conc_df <- data.frame(
    sample = rep(c(paste0("F_", 1:24), paste0("S_", 1:24), paste0("T_", 1:24))),
    concentration = rep(c(0, 20, 40, 60, 80, 100), each = 4, times = 3))
  
  # Merge the concentrations into the merged_df
  merged_df <- merge(merged_df, conc_df, by.x = "Sample", by.y = "sample")
  
  return(merged_df)
}

clean_data2 <- function(x){
  x <- as.character(x)
  x_n <- as.numeric(gsub("\\*", "", x)) # Remove asterisks and convert to numeric
  x_n <- ifelse(x_n %in% c("OOR <", "OOR >"), NA, as.numeric(x_n)) # assign NA to "OOR <" and "OOR >" values
  return(x_n)
}

