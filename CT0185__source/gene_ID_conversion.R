# Load the required package
library(biomaRt)

gene_symbols <- c("FGF2", "CXCL11", "IFNG", "IL1A", "IL1B", "IL10", "IL13", "IL17A", 
                  "IL18", "IL6", "CXCL8", "CXCL10", "CCL2", "CXCL9", "CCL3", "MMP1",
                  "MMP7", "MMP9", "PDGFB", "CXCL12", "TNF", "KDR")

# Function to convert gene symbols to ENSEMBL IDs
convertGeneSymbolsToEnsembl <- function(gene_symbols) {

  # Select the Homo sapiens dataset from ENSEMBL
  ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl", host = "https://useast.ensembl.org")
  
  converted_ids <- getBM(attributes = c('ensembl_gene_id', 'external_gene_name'),
                         filters = 'external_gene_name',
                         values = gene_symbols, 
                         mart = ensembl)
  
  # Return the dataframe of gene symbols and corresponding ENSEMBL IDs
  return(converted_ids)
}

# Function to convert ENSEMBL IDs to gene symbols
convertEnsemblToGeneSymbols <- function(ensembl_ids) {
  
  # Select the Homo sapiens dataset from ENSEMBL
  ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl", host = "https://useast.ensembl.org")
  
  converted_symbols <- getBM(attributes = c('ensembl_gene_id', 'external_gene_name'),
                             filters = 'ensembl_gene_id',
                             values = ensembl_ids, 
                             mart = ensembl)
  
  # Return the dataframe of ENSEMBL IDs and corresponding gene symbols
  return(converted_symbols)
}

procartaplex_ensemblIDs <- convertGeneSymbolsToEnsembl(gene_symbols)
save(result, file = "CT0185__data/procartaplex_ensemblIDs.Rdata")

