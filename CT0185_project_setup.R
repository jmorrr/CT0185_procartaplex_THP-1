# wd should be set to the main project folder or then a subdirectory for this
# specific analysis. E.g. main = 'CT0XXX', analysis directory = 'CT0XXX_RNAseq_analysis'.
# Check wd
getwd()

# expt_ID should be unique to the analysis e.g. 'CT0XXX_RNAseq'.
# Preferably the name of the parent/working dir of the project.
expt_ID <- "CT0185"

### Directory creation

# A main directory to store all scripts
main_dir <- paste(expt_ID, "_main", sep = "")

# A source directory to store functions and called scripts
src_dir <- paste(expt_ID, "_source", sep = "")

# A data directory to store original unmanipulated data that the analysis
# is performed on.
data_dir <- paste(expt_ID, "_data", sep = "")

# A results directory which holds data after manipulation and all result
# outputs from the analysis such as plots etc.
results_dir <- paste(expt_ID, "_results", sep = "")


# Creates a list of the directory objects defined above
dirs <- list(main_dir, src_dir, data_dir, results_dir)

# Checks for directories and creates if not present.
for (dir in dirs) {
  
if(!file.exists(dir)){
  dir.create(dir)
} else {
  message(paste(dir, "Directory already exists."))
}
}

### File creation

# A readme file for description of the project
readme <- "README.md"

# A file to take analysis notes
notes <- "notes.md"

# Creates a list of the file objects defined above
files <- list(readme, dependencies, notes)

# Checks for files and creates if not present.
for (fil in files) {

if(!file.exists(fil)){
  file.create(fil)
  # Write the README and dependencies files with expt_ID as the title
  cat('# ', expt_ID, file = fil, append = TRUE)
} else {
  message(paste(fil, "Directory already exists."))
}
}

# Define the lines you want to write
lines <- c(".Rhistory", ".RData", ".Rproj.user/")

# Write the lines to .gitignore, appending to the file
writeLines(lines, ".gitignore", append = TRUE)

renv::init()

renv::snapshot()


