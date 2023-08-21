## Combining .db files with labels for CV4E COA dataset

library(DBI)
library(RSQLite)
library(dplyr)
library(data.table)

setwd('/Users/caraappel/Documents/CV4E/')

# Function to find all .db files recursively in a directory
find_db_files <- function(directory) {
  db_files <- list.files(path = directory, pattern = "\\.db$", full.names = TRUE, recursive = TRUE)
  return(db_files)
}

# List of .db files to work with
db_files <- find_db_files("data") 

# Create a new dataframe
combined_labels <- data.frame()

# Loop through each source database
for (db_file in db_files) {
  conn_source <- dbConnect(RSQLite::SQLite(), db_file)
  
  # Read the Labels table from each source database
  labels <- dbReadTable(conn_source, "Labels")
  
  # Add a new column with the db path to the Labels table
  labels <- mutate(labels, file_path = db_file)
  labels$file_path <- paste(sapply(strsplit(as.character(labels$file_path), '\\/'), '[', 1),
                            sapply(strsplit(as.character(labels$file_path), '\\/'), '[', 2),
                            sapply(strsplit(as.character(labels$file_path), '\\/'), '[', 3), sep = '/')
  
  # Write the modified Labels table to the target dataframe
  combined_labels <- bind_rows(combined_labels, labels)
  
}

  head(combined_labels)
  unique(combined_labels$file_path)

#save
  write.csv(combined_labels, 'oregon_critters/metadata_labels/labels.csv')
  
  
## Combine with metadata file
  meta <- fread('oregon_critters/metadata_labels/COA_data_summary_081523.csv')
  
  head(combined_labels); nrow(combined_labels). #93,598 labels
  head(meta) ; nrow(meta) #119,568 images
  
  #find relative path in meta
  meta$full_path <- paste(sapply(strsplit(meta$full_name, '\\/'), '[[', 6),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 7),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 8),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 9),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 10), sep = '/')
  
  #create full path in labels
  combined_labels$full_path <- paste(combined_labels$file_path, 'images', combined_labels$IName, sep = '/')
  
  #combine
  meta_labels <- merge(combined_labels, meta, by = 'full_path', all = TRUE)

  nrow(meta_labels)  #121,676 rows
  
  write.csv(meta_labels, 'oregon_critters/metadata_labels/metadata_labels.csv')
  
  