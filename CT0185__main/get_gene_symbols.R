# Assuming your original dataframe is called 'df' and the column name is 'Analyte'

# Get unique values from the 'Analyte' column
unique_values <- unique(df$Analyte)

# Create a dataframe with a single column for unique values
analyte_df <- data.frame(symbol = unique_values, row.names = NULL)

# Save the new dataframe as a CSV file without column names in the 'CT0185_data' directory
file_name <- ("CT0185__data/unique_analytes.csv")
write.csv(analyte_df, file = file_name, row.names = FALSE, col.names = FALSE)

