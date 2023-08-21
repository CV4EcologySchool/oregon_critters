## Subset for CV4E dataset

library(data.table)
library(dplyr)
library(ggplot2)

#Read in full labels/metadata
  ml <- fread('metadata_labels/metadata_labels.csv'); ml <- ml[,-1]
  head(ml)  
  nrow(ml)  #121,676 rows
  length(unique(ml$full_path)) #119,569 images

    head(ml[,c('folder','species','image_name','CName','X','Y','W','H')])  
    
    
#How many per species?
  sort(table(ml$CName))

  # (species_project <- ml %>% group_by(CName, project) %>% summarise('nPhotos' = n()))
  (species_project <- ml %>% group_by(CName, project) %>% summarise('nPhotos' = length(unique(full_path))))
  
  ggplot(species_project[!is.na(species_project$CName),], 
         aes(x = reorder(CName, -nPhotos), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_bw() 
  
  (species_project_df <- table(ml$CName, ml$folder))
  
  #how many images?
  length(unique(ml$full_path)) #119,569
  
  #how many non-empty?
  length(unique(ml[!is.na(ml$CName),]$full_path)) #91,491
  

## CREATE DATASET
  
  #1. remove empty images (bc I don't know that they're truly empty)
  ds <- ml[!is.na(ml$CName),]
    nrow(ds)
    length(unique(ds$full_path)) #91,491
  
  #2. remove incomplete projects
  # ds <- ds[!ds$folder %in% c('hailey.hester-DUNES_Opossum','ESF_BAIT_Opossum','HJA_MARIE_Weasel'),]
    # length(unique(ds$full_name))
    
    ## ** UNNECESSARY BC I ALREADY REMOVED IMAGES WITH NO LABELS **
  
  #3. remove 'example tags" images
  ds$example <- ifelse(grepl('example',ds$full_path), 'y', 'n')
    table(ds$example, useNA = 'a')
    ds[ds$example %in% 'y',] #there were only 3, that's good
  ds <- ds[ds$example %in% 'n',]
    length(unique(ds$full_name))
  
  #4. Subsample images in those classes with > threshold images
  set.seed(300)
  thresh <- 300
  
  sp_table <- aggregate(ds, full_path ~ CName, function(x) length(unique(x)))
  (sp_thresh <- sp_table[sp_table$full_path >= thresh,]$CName) #these species are over the threshold
  
  length(unique(ds[!ds$CName %in% sp_thresh,]$full_path)) + #how many images will we end up with?
    thresh * length(sp_thresh)
      #thresh 5000: 67,049
      #thresh 1000: 25,523
      #thresh 300: 10,513
  
  ds_sp_thresh <- NULL
  for (ss in sp_thresh){
    ds_sp <- ds[ds$CName %in% ss,]
    all_files <- unique(ds_sp$full_path)
    sampled_files <- sample(all_files, size = thresh)
    ds_sp_sampled <- ds_sp[ds_sp$full_path %in% sampled_files,]
    ds_sp_thresh <- bind_rows(ds_sp_thresh, ds_sp_sampled)
  }
    aggregate(ds_sp_thresh, full_path ~ CName, function(x) length(unique(x)))
    
  #5. Add subsampled species images back to the rest of the images
  ds_rest <- ds[!ds$CName %in% sp_thresh,]
  ds_thresh <- bind_rows(ds_rest, ds_sp_thresh)
    nrow(ds_thresh)    
    length(unique(ds_thresh$full_name))
      
  #How did we do?
    sort(table(ds_thresh$CName))
    
    (species_project_thresh <- ds_thresh %>% group_by(CName, project) %>% summarise('nPhotos' = n()))
    
    ggplot(species_project_thresh, aes(x = reorder(CName, -nPhotos), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      theme_bw() 
    
    length(unique(ds_thresh$full_path))
    
    write.csv(ds_thresh, 'metadata_labels/sampled_ds_300.csv')
    # write.csv(ds_1000, 'oregon_critters/sampled_ds_1000.csv')
    
    
## Copy those images to a new location -- python script (copy_files.ipynb)
  