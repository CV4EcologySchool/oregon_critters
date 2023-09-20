## Combining .db files with labels for CV4E COA dataset

library(DBI)
library(RSQLite)
library(dplyr)
library(data.table)

setwd('/Users/caraappel/Documents/CV4E')

# Function to find all .db files recursively in a directory
find_db_files <- function(directory) {
  db_files <- list.files(path = directory, pattern = "\\.db$", full.names = TRUE, recursive = TRUE)
  return(db_files)
}

# List of .db files to work with (specify directory location)
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
  meta <- fread('oregon_critters/metadata_labels/metadata.csv')
  
  head(combined_labels); nrow(combined_labels) #93,592 labels (was 93,598 before cleaning in Sept 2023)
  head(meta) ; nrow(meta) #113,523 images (was 119,568 before cleaning in Sept 2023)
  
  #clean up 'meta' -- keep only columns we need
  meta <- meta[,c('full_name','project','folder','species','image_name','site','year','datetime_fixed','cam_type_final','day_night_fixed','year_fixed', 'remove.x')]
  head(meta)
  
  #find relative path in meta (relative to 'data')
  meta$full_path <- paste(sapply(strsplit(meta$full_name, '\\/'), '[[', 6),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 7),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 8),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 9),
                          sapply(strsplit(meta$full_name, '\\/'), '[[', 10), sep = '/')
  
  #create relative path without folder key for matching
  meta$full_path_original <- sub("_[a-zA-Z]{2}\\.JPG$", ".JPG", meta$full_path)
  
  #create full path in labels
  combined_labels$full_path <- paste(combined_labels$file_path, 'images', combined_labels$IName, sep = '/')
  
  #combine
  meta_labels <- merge(combined_labels, meta, by.x = 'full_path', by.y = 'full_path_original', 
                       all = FALSE)
  nrow(meta_labels)  #93,558 rows when all=FALSE; 114,663 rows when all=TRUE (was 121,676 rows before cleaning)
    
    #using all=FALSE should mean that 'meta_labels' doesn't include empty images (entry in 'meta' but not 'labels')
    #and doesn't include ones I've removed from folders (entry in 'labels' but not in 'meta')
  
  #now also remove ones I set to remove
  table(meta_labels$remove.x, useNA = 'a')
  meta_labels$remove.x <- ifelse(is.na(meta_labels$remove.x), 'keep', meta_labels$remove.x)
  meta_labels <- meta_labels[meta_labels$remove.x == 'keep',]
  
  write.csv(meta_labels, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels.csv')
  
  