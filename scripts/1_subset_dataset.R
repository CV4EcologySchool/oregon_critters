## Subset for CV4E dataset

library(data.table)
library(dplyr)
library(ggplot2)

#Read in full labels/metadata
  ml <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels.csv'); ml <- ml[,-1]
  head(ml)  
  nrow(ml)  #121,676 rows
  length(unique(ml$full_path)) #119,569 images

    head(ml[,c('folder','species','image_name','CName','X','Y','W','H')])  
    

#Match with new unique filenames (8/25/23)
  nnames <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/filename_all_key.csv', header = TRUE)  
  head(nnames)    
  
  #get full and relative paths to match on
  nnames$relpath_old <- nnames$full_path
  nnames$relpath_old <- gsub('/Users/caraappel/Documents/CV4E/','',nnames$relpath_old)
  
  nnames$rel_path_new <- paste('data',nnames$project_species,'images',nnames$new_filename,sep = '/')
  
  length(unique(nnames$rel_path_new)) #119,572 (the 3 extra are example.JPG; will be removed at next step)
  
  #match
  ml$full_path_new <- nnames$rel_path_new[match(ml$full_path, nnames$relpath_old)]
  
  #save project/ID key
  key <- nnames %>%
    distinct(project_species, code, .keep_all = FALSE)
  
  #write.csv(key, '/Users/caraappel/Documents/CV4E/oregon_critters/project_rename_all_key.csv')

  
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
  

## CREATE DATASET ##############################################################
  
  #1. remove images with no labels (bc I don't know that they're truly empty)
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
    
  #4. remove COA_2020 photos for now (bc not labeled between G and T but there are both)
  ds <- ds[!ds$project  %in% 'COA_2020',]
    table(ds$project, useNA = 'a')
    length(unique(ds$full_name))
  
  ## SAVE
  #write.csv(ds, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels_newnames.csv')
    
    
################################################################################  

## From here, split into 3 datasets:
    
    ## Ground
    ## Trail
    ## Both
    
    table(ds$cam_type, useNA = 'a')
    table(ds$cam_type, ds$project, useNA = 'a') #yes, all NAs here can be considered T
    
    ds_both <- ds
    ds_ground <- ds[ds$cam_type %in% 'G',]
    ds_trail <- ds[!ds$cam_type %in% 'G',] #include both T and NA 
    
    length(unique(ds_ground$full_path)) #13,467 images in G
    length(unique(ds_trail$full_path))  #25,587 images in T
    
    #write.csv(ds_ground, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels_newnames_ground.csv')
    #write.csv(ds_trail, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels_newnames_trail.csv')

        
################################################################################    
  
  ## Select which to proceed with    
    
  ds <- ds_trail   #ground, trail, both
    
    sort(table(ds$CName))
    
    # (species_project <- ml %>% group_by(CName, project) %>% summarise('nPhotos' = n()))
    (species_project <- ds %>% group_by(CName, project) %>% summarise('nPhotos' = length(unique(full_path))))
    p1 <- ggplot(species_project[!is.na(species_project$CName),], 
           aes(x = reorder(CName, -nPhotos), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      theme_bw() 
    p1
    ggsave('figures/COA_species_project_both_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    (species_project_df <- table(ml$CName, ml$folder))
    
    #how many images? (gut check)
    length(unique(ds$full_path_new))
    
    
################################################################################    
    
  ## Now the goal is to get down to ~10k images for both; ~6000 images each for G and T datasets
    
################################################################################    
    
  #4. Subsample images in common classes with > threshold images
    
  #which threshold should we use?  
  set.seed(300)  #keep at 300!
  thresh <- 180 #adjust and see how many images we will end up with (b)
  
  sp_table <- aggregate(ds, full_path_new ~ CName, function(x) length(unique(x)))
  (sp_thresh <- sp_table[sp_table$full_path_new >= thresh,]$CName) #these species are over the threshold
  
  length(unique(ds[!ds$CName %in% sp_thresh,]$full_path_new)) + #how many images will we end up with?
    thresh * length(sp_thresh)

      #ground: thresh 620 = 6,000 images
      #trail:  thresh 180 = 6,119 images
      #both:   thresh 350 = 10,807 images
  
  #now randomly remove images above threshold for the common species
  ds_sp_thresh <- NULL
  for (ss in sp_thresh){
    ds_sp <- ds[ds$CName %in% ss,]
    all_files <- unique(ds_sp$full_path_new)
    sampled_files <- sample(all_files, size = thresh)
    ds_sp_sampled <- ds_sp[ds_sp$full_path_new %in% sampled_files,]
    ds_sp_thresh <- bind_rows(ds_sp_thresh, ds_sp_sampled)
  }
    aggregate(ds_sp_thresh, full_path_new ~ CName, function(x) length(unique(x)))  #we lost 2 here, why?
    #all common sp should have thresh number of images
  
      
  #5. Add subsampled species images back to the rest of the images
  ds_rest <- ds[!(ds$CName %in% sp_thresh),]
  ds_thresh <- bind_rows(ds_rest, ds_sp_thresh)
    nrow(ds_thresh)    
    length(unique(ds_thresh$full_path_new)) #why did we lose 43?
    
    #debug
    a <- (unique(ds_sp_thresh$full_path_new)) ; length(a) #common species (180 each)
    b <- (unique(ds_rest$full_path_new)) ; length(b) #not common species
    c <- (unique(ds_thresh$full_path_new)) ; length(c) #combined (why isn't it a + b)
    
      sum(a %in% b) ; sum(b %in% a) #shold be 0 but is 43
      sum (a %in% c) ; sum (c %in% a) #good
      sum (b %in% c) #good
      
      #these are the weird ones (43)
      a[a %in% b]
      View(ds[ds$full_path_new %in% a[a %in% b],])
      
      #ooooh, they are images with multiple species (common + not common)
      #how to handle them?
    #  
    
  #How did we do?
    sort(table(ds_thresh$CName))
    
    (species_project_thresh <- ds_thresh %>% group_by(CName, project) %>% summarise('nPhotos' = n()))
    p2 <- ggplot(species_project_thresh, aes(x = reorder(CName, -nPhotos), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      theme_bw() 
    p2
    ggsave('figures/COA_species_project_both_all_thresh.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    #write.csv(ds_thresh, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620.csv')
    #write.csv(ds_thresh, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_trail_180.csv')
    #write.csv(ds_thresh, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_both_350.csv')
    
    
## Now repeat with all / ground / trail  
      
## Then copy those images to a new location -- python script (copy_files.ipynb)
    
## And proceed with splitting (2_split_dataset.R)
  
    