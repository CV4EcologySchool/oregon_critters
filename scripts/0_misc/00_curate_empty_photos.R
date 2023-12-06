## Curate set of empty photos for YOLO training

library(data.table)
library(tidyverse)

## Extract datetimes
  #use notebook 00_get_birthtime.ipynb to create csvs
  

## If empties are not separated yet:

  ## ORSNAP
  #read in tags
    orsnap19 <- fread('/Users/caraappel/Documents/_RESEARCH/Cameras/Snapshot_USA/eMammal_2019/DataS1/SNAPSHOT_USA_2019_observations.csv')
    orsnap19 <- orsnap19[orsnap19$Camera_Trap_Array == 'OR_Forest_Oregon_State_University',]
    orsnap19_empty <- orsnap19[orsnap19$Species_Name == 'No Animal',]
      #for ORSNAP, need to use renamed files... and digikam db? this doesn't have them by file here, just by sequence, but I matched them up somehow before
      #find directory location of renamed photos, use list.files
      #read in filenames of tagged images used for CV4E
      #compare them and pull out the ones in directory not used in CV4E
      #sample as below
      #scroll through and make sure they're actually empty
    
  ## COA 2020
    #read in digiKam tags
    coa20 <- fread('/Users/caraappel/Documents/_RESEARCH/Cameras/Coast_Range/outputs/01_digikam_tags_2020_new091423.csv')
    coa20tags <- coa20[coa20$cleartext_parent %in% 'Species',]   #keep only species tags (not number, age, etc.)
    
    #this has only images that were tagged, not empty ones. so now read in all filenames (takes a long time...)
    coa20all <- list.files('/Volumes/COA_2020_camera_trap_photos/COA_photos_2020/renamed_photos/', pattern = '.JPG',
                          full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
    length(coa20all); head(coa20all)
  
    #compare with tagged photos
    coa20$fullPath <- paste(coa20$albumPath, coa20$name, sep = '/')
    coa20all_empty <- coa20all[!coa20all %in% coa20tags$fullPath,] #these ones aren't tagged
  
    #now sample among sites, below
  
  
  
  
  

## If empties are already subsetted: (ESF, DUNES)
## SELECT SUBSAMPLE ------------------------------------------------------------
  
  #read in datetimes CSV
  files <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/datetimes/HJA_MARIE_date_created_exif_BOX.csv')
    head(files)
    nrow(files)
    #if this is less than 1000, just get them all. run this and then skip to the copy and split steps
      # files_sample <- files
    
  #extract site name
#  files <- separate(files, Filename, into = c('site','image'), sep = '_', remove = FALSE)
#    table(files$site, useNA = 'a')
#    files <- files[!files$site == 'IMG',]
    #we can't do anything with the 'IMG' ones so delete them. just make sure there aren't too many

    #For HJA_Marie:
    files$site <- sapply(strsplit(as.character(files$`Full Path`), '\\/'), '[', 8)
    files$site <- sapply(strsplit(as.character(files$site), '\\_'), '[', 1)
    files$site <- sapply(strsplit(as.character(files$site), '\\-'), '[', 2)
      table(files$site, useNA = 'a')
      length(unique(files$site)) #this is only 16 out of 94 sites for HJA_Marie
            
  #get a random sample of ~1000 images, balanced among sites
  set.seed(6)
  sample_size <- 1000  
  (n_groups <- length(unique(files$site)))
  (samples_per_site <- floor(sample_size / n_groups))
  # samples_per_site <- 146 #146 for ESF_TRAIL
  samples_per_site <- 16 #for HJA_MARIE
    
    #loop thru
    files_sample <- data.frame()
    for(site in unique(files$site)){
      site_df <- files[files$site == site,]
      n_samples <- min(nrow(site_df), samples_per_site) #ensures it will copy all if there are fewer than desired
      files_sample <- rbind(files_sample, site_df[sample(1:nrow(site_df), n_samples),])
    }
    table(files_sample$site, useNA = 'a') #good
      
    #did I reach my target?
    nrow(files_sample)

      #if not, either scroll back up and manually change samples_per_site! or...
    
      #or, go back and get some more from larger folders...
      table(files$site, useNA = 'a') #which sites had more than the target?
      sample_size - nrow(files_sample) #how many more do we need?
      
        #if there is a site to add all of:
        for (rr in 1:nrow(files[files$site %in% '18712D',])){
          file_site <- files[files$site %in% '18712D',][rr,]
          if(!file_site$Filename %in% files_sample$Filename){
            files_sample <- rbind(files_sample, file_site)
            cat('\n added file:', file_site$Filename)
          } else{
            cat('\nfile already in list:', file_site$Filename)
          }
        }
    
        #and/or, add enough of a certain site to reach the target (repeat as necessary)
        (remaining <- sample_size - nrow(files_sample)) #how many more do we need?
        site_remaining <- files[files$site == '18909H' & !files$Filename %in% files_sample$Filename,] #these ones aren't included yet
        rows_to_add <- site_remaining[sample(1:nrow(site_remaining), remaining),] #sample rows from this
        files_sample <- rbind(files_sample, rows_to_add) #add them
        
          nrow(files_sample) #good
          length(unique(files_sample$Filename))
          table(files_sample$site, useNA = 'a')
        
          
  #add destination location and filename
  destination_dir <- '/Users/caraappel/Documents/CV4E/data/empty_photos/subsets_1000/HJA_MARIE/'
  files_sample$destination_path <- paste(destination_dir, files_sample$Filename, sep = '/')
  
    #HJA_MARIE: prepend site to filename
    files_sample$destination_path <- paste(destination_dir, '/', files_sample$site, '_', files_sample$Filename, sep = '')
    
  #remove duplicates
  nrow(files_sample); length(unique(files_sample$destination_path))
  files_sample <- files_sample[!duplicated(files_sample$destination_path),]
    
  #now copy them
  y=0; n=0
  for (file in 1:nrow(files_sample)){
    if(file.copy(files_sample[file,]$`Full Path`, files_sample[file,]$destination_path)) {
      cat("copied:", files_sample[file,]$Filename, 'to', destination_dir)
      cat("")
      y = y+1
    } else {
      cat('failed to copy:', files_sample[file,], 'to', destination_dir)
      n = n+1
    }
  }
  cat('successfully copied', y, 'files to', destination_dir)
  cat('failed to copy', n, 'files to', destination_dir)
  
  
## HJA_MARIE: use datetimes I extracted from both sets (BOX and HD) after copying them
  hjam <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/datetimes/HJA_MARIE_empties_date_created_exif.csv')
  hjam$site <- sapply(strsplit(as.character(hjam$Filename), '\\_'), '[', 1)
    sort(unique(hjam$site)) #47. but should be 65. that means some were dups between BOX and HD
    table(hjam$site, useNA = 'a')
      #they should all have ~16, but some have more, so e.g., 134101 must have been in both locations
    nrow(hjam)
    length(unique(hjam$Filename)) #but they're all unique, so it's ok. some sites just have more than others.
  files_sample <- hjam
  
## Now split into train/val/test -----------------------------------------------
  ds <- files_sample
  head(ds)
  
    #some formatting
    ds$datetime <- ds$`Creation Date` #rename columns
    ds$stn <- ds$site #rename columns
    ds$datetime <- as.POSIXct(strptime(ds$datetime, '%Y:%m:%d %H:%M:%S')) #format datetime column
    nrow(ds[is.na(ds$datetime),])
      ds <- ds[!is.na(ds$datetime),] #remove any without datetimes
    
    #now generate unique sequence IDs based on a threshold of #seconds
    
      # #create 'stn' column -- add 'G' or 'T' to the ones that don't have it yet
      # ds$stn <- ifelse(grepl('-G|-T', ds$site), ds$site, paste(ds$site, ds$cam_type, sep = '-'))
      # length(unique(ds$site)); length(unique(ds$stn))
  
    #set sequence threshold (seconds)
    thresh = 60
  
    #generate unique sequence IDs
    ds <- ds %>%
      arrange(stn, datetime) %>% 
      mutate(
        time_diff = difftime(datetime, lag(datetime), units = "secs"),
        site_change = ifelse(stn != lag(stn), 1, 0),
        sequence_id = cumsum(site_change | is.na(site_change) | time_diff > thresh) 
      ) %>%
      select(-time_diff, -site_change)   
    
    head(ds)
      length(unique(ds$sequence_id))   #number of unique sequences
      nrow(ds[is.na(ds$sequence_id),]) #make sure they all got a sequence ID (should be 0 here) -- if not, prob an NA in date somewhere
      
    #now split into train/val/test
    set.seed(6) 
    
      #how many do we want in each partition?
      seq_ids <- data.frame('sequence_id' = unique(ds$sequence_id))
      (tr=round(length(seq_ids$sequence_id) * 0.7))  #in train
      (vt=round(length(seq_ids$sequence_id) * 0.15)) #each in validation and test
      
      #split training
      rand1a <- sample(seq_ids$sequence_id, size = tr)
      seq_ids$group <- ifelse(seq_ids$sequence_id %in% rand1a, 'train', NA)
      
      #from remaining, split val/test
      rand1b <- sample(seq_ids[!seq_ids$group %in% 'train',]$sequence_id, size = vt)  
      seq_ids$group <- ifelse(seq_ids$sequence_id %in% rand1b, 'val', seq_ids$group)
      seq_ids$group <- ifelse(is.na(seq_ids$group), 'test', seq_ids$group)
      
      #did we do it right? view distribution of sequences
      table(seq_ids$group, useNA = 'a')
      
      #how many *images* per group?
      ds$group <- seq_ids$group[match(ds$sequence_id, seq_ids$sequence_id)]
        (groups_images <- ds %>% group_by(group) %>% summarise(photos = n_distinct(Filename)))
        (groups_pct <- ds %>% group_by(group) %>% summarise(percentage = n_distinct(Filename) / n_distinct(ds$Filename) * 100))
        #good! ~70/15/15
      
      #plot
        #ds$group <- factor(groups_images$group, levels = c('train','val','test')) #sort levels for plotting
        p1a <- ggplot(groups_images, aes(group, y = photos)) + geom_bar(stat = 'identity') + theme_bw() + theme()
        p1a
        
  ## Save
    write.csv(ds, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_hjamarie.csv')
 
    ## save txt files with just filenames ... skip, do this below
    # write.table(ds[ds$group %in% 'train',]$full_path, col.names = FALSE, row.names = FALSE,
    #             file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_train_esfground.txt')
    # write.table(ds[ds$group %in% 'val',]$full_path, col.names = FALSE, row.names = FALSE,
    #             file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_val_esfground.txt')
    # write.table(ds[ds$group %in% 'test',]$full_path, col.names = FALSE, row.names = FALSE,
    #             file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_test_esfground.txt')

            
## Now run '4_extract_image_dimensions.ipynb' with each csv as input    
    
## Combine empties from all projects -------------------------------------------    
    
    empty_dunes     <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_dunes_dim.csv')
    empty_esfbait   <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_esfbait_dim.csv')
    empty_esfground <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_esfground_dim.csv')
    empty_esftrail  <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_esftrail_dim.csv')
    empty_coa2019
    empty_coa2020 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_COA2020_dim.csv')
    empty_coa2021
    empty_hjagrid
    empty_hjamarie <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/empty/empty_split_hjamarie_dim.csv')
    empty_orsnap
    
    #make sure all filenames are unique!
      nrow(empty_dunes); length(unique(empty_dunes$Filename))
      nrow(empty_esfbait); length(unique(empty_esfbait$Filename))
      nrow(empty_esfground); length(unique(empty_esfground$Filename))
      nrow(empty_esftrail); length(unique(empty_esftrail$Filename))
      nrow(empty_coa2019); length(unique(empty_coa2019$Filename))
      nrow(empty_coa2020); length(unique(empty_coa2020$Filename)) #1 duplicate here; remove it
        empty_coa2020 <- empty_coa2020[!duplicated(empty_coa2020$Filename),]
      nrow(empty_coa2021); length(unique(empty_coa2021$Filename))
      nrow(empty_hjagrid); length(unique(empty_hjagrid$Filename))
      nrow(empty_hjamarie); length(unique(empty_hjamarie$Filename))
      nrow(empty_orsnap); length(unique(empty_orsnap$Filename))
      
   ##create .txts again, correctly
      
    #add paths as they will be on CQLS to the filenames now (or /data/ as base, right?)
    empty_dunes$project <- 'DUNES'; empty_dunes$model <- 'trail'
    empty_dunes$path <- paste('data/', empty_dunes$project, '/', empty_dunes$project, '_empty/images/', empty_dunes$Filename, sep = '')
    
    empty_esfbait$project <- 'ESF_BAIT'; empty_esfbait$model <- 'trail'
    empty_esfbait$path <- paste('data/', empty_esfbait$project, '/', empty_esfbait$project, '_empty/images/', empty_esfbait$Filename, sep = '')
    
    empty_esfground$project <- 'ESF_GROUND'; empty_esfground$model <- 'ground'
    empty_esfground$path <- paste('data/', empty_esfground$project, '/', empty_esfground$project, '_empty/images/', empty_esfground$Filename, sep = '')
    
    empty_esftrail$project <- 'ESF_TRAIL'; empty_esftrail$model <- 'trail'
    empty_esftrail$path <- paste('data/', empty_esftrail$project, '/', empty_esftrail$project, '_empty/images/', empty_esftrail$Filename, sep = '')

    empty_coa2019$project <- 'COA_2019'; empty_coa2019$model <- 'trail'
    empty_coa2019$path <- paste('data/', empty_coa2019$project, '/', empty_coa2019$project, '_empty/images/', empty_coa2019$Filename, sep = '')
    
    empty_hjagrid$project <- 'HJA_GRID'; empty_hjagrid$model <- 'trail'
    empty_hjagrid$path <- paste('data/', empty_hjagrid$project, '/', empty_hjagrid$project, '_empty/images/', empty_hjagrid$Filename, sep = '')
    
    empty_hjamarie$project <- 'HJA_MARIE'; empty_hjamarie$model <- 'trail'
    empty_hjamarie$path <- paste('data/', empty_hjamarie$project, '/', empty_hjamarie$project, '_empty/images/', empty_hjamarie$Filename, sep = '')
    
    empty_orsnap$project <- 'ORSNAP'; empty_orsnap$model <- 'trail'
    empty_orsnap$path <- paste('data/', empty_orsnap$project, '/', empty_orsnap$project, '_empty/images/', empty_orsnap$Filename, sep = '')
    
    #for ones to split between ground/trail:
    empty_coa2020$project <- 'COA_2020'
    empty_coa2020$model <- ifelse(grepl('G', empty_coa2020$stn), 'ground', 'trail')
      table(empty_coa2020$model, useNA = 'a')      
    empty_coa2020$path <- paste('data/', empty_coa2020$project, '/', empty_coa2020$project, '_empty/images/', empty_coa2020$Filename, sep = '')
    
    empty_coa2021$project <- 'COA_2021'; 
    empty_coa2021$model <- ifelse(grel('G', empty_coa2021$stn), 'ground', 'trail')
      table(empty_coa2021$model, useNA = 'a')
    empty_coa2021$path <- paste('data/', empty_coa2021$project, '/', empty_coa2021$project, '_empty/images/', empty_coa2021$Filename, sep = '')
    
    
    #combine
    empty_combined <- rbind(empty_dunes, empty_esfbait, empty_esfground, empty_esftrail, empty_coa2020, fill = TRUE)
      table(empty_combined$project, empty_combined$model, useNA = 'a')
      table(empty_combined$project, empty_combined$group, useNA = 'a')
      table(empty_combined$model, empty_combined$group, useNA = 'a')
      

      
    #read in train/val/test text files and add them for each (ground, trail, both) (train, test,)
    demo_train <- read.table('/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_train.txt', col.names = 'path')
    demo_val <- read.table('/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_val.txt', col.names = 'path')
    demo_test <- read.table('/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_test.txt', col.names = 'path')
    
    #demo is done. do the rest when I have all empties curated:
    both_train <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_train.txt', col.names = 'path')
    both_val <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_val.txt', col.names = 'path')
    both_test <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_test.txt', col.names = 'path')
    
    ground_train <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_train.txt', col.names = 'path')
    ground_val <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_val.txt', col.names = 'path')
    ground_test <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_test.txt', col.names = 'path')
    
    trail_train <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_train.txt', col.names = 'path')
    trail_val <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_val.txt', col.names = 'path')
    trail_test <- read.table('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_test.txt', col.names = 'path')

    #add empties to existing text files
    set.seed(6)
    
      #demo2
      nrow(demo_train)
      nrow(demo_val)
      nrow(demo_test)
        table(empty_combined$group)
        
        #for demo2 set (1000 total), add 70 empties to train and 15 each to val and test
        train_sample <- empty_combined %>% filter(group == 'train') %>% slice_sample(n = 70, replace = FALSE)
        val_sample   <- empty_combined %>% filter(group == 'val') %>% slice_sample(n = 15, replace = FALSE)
        test_sample  <- empty_combined %>% filter(group == 'test') %>% slice_sample(n = 15, replace = FALSE)
        
        #add to existing .txt
        demo_train <- rbind(demo_train, train_sample[, 'path'])
        demo_val   <- rbind(demo_val, val_sample[, 'path'])
        demo_test <- rbind(demo_test, test_sample[, 'path'])
        
        #save .txt files
        write.table(demo_train, col.names = FALSE, row.names = FALSE, quote = FALSE,
                    file = '/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_train.txt')
        write.table(demo_val, col.names = FALSE, row.names = FALSE, quote = FALSE,
                    file = '/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_val.txt')
        write.table(demo_test, col.names = FALSE, row.names = FALSE, quote = FALSE,
                    file = '/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_test.txt')
        
        #need to copy those empty files to CQLS too... gah. Use script prep/3_copy_files.ipynb
        #save csv with the empties we used here
        demo2_empties <- rbind(train_sample, val_sample, test_sample)
        demo2_empties$full_path <- demo2_empties$path
        demo2_empties$full_path <- gsub('data/', '', demo2_empties$full_path)
        write.csv(demo2_empties,'/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/demo2_empties.csv')
        
        
        ### NEW, haven't run yet: append these to demo2.csv in metadata_labels?
        
        
      
      #demo is done. do the rest when all empties are curated:
        
      #both
      nrow(both_train)
      nrow(both_val)
      nrow(both_test)
        table(empty_combined$group)
  
      both_train <- rbind(both_train, empty_combined[empty_combined$group == 'train', 'path'])
      both_val   <- rbind(both_val, empty_combined[empty_combined$group == 'val', 'path'])    
      both_test  <- rbind(both_test, empty_combined[empty_combined$group == 'test', 'path'])    
      
    
      #ground
      nrow(ground_train)
      nrow(ground_val)
      nrow(ground_test)
        table(empty_combined$group)
      
      ground_train <- rbind(ground_train, empty_combined[empty_combined$group == 'train', 'path']) #use subset of ground
      ground_val   <- rbind(ground_val, empty_combined[empty_combined$group == 'val', 'path'])    #use subset of ground
      ground_test  <- rbind(ground_test, empty_combined[empty_combined$group == 'test', 'path'])    #use subset of ground
      
    
      #trail
      
      
      
    #append to .csv files
      
#####
      
  ## COA_2020 and others: need to split ground and trail 
    head(empty_coa2020)
    empty_coa2020$cam_type <- ifelse(grepl('G', empty_coa2020$stn), 'ground', 'trail')
      table(empty_coa2020$cam_type, useNA = 'a')      
      
      #copy them into respective folders
      empty_coa2020$current_path <- paste('/Users/caraappel/Documents/CV4E/data/empty_photos/subsets_1000/COA_2020/',
                                          empty_coa2020$Filename, sep = '')
      empty_coa2020$destination_path_camtype <- paste('/Users/caraappel/Documents/CV4E/data/empty_photos/subsets_1000/COA_2020/',
                                                      empty_coa2020$cam_type, '/', empty_coa2020$Filename, sep = '')
      file.copy(empty_coa2020$current_path, empty_coa2020$destination_path_camtype)
      
      