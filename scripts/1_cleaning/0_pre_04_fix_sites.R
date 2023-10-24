## Add missing sites and datetimes on training images

library(data.table)
library(tidyverse)


## Read in meta (from script 'pre_03_2020_filenames.R')
  meta <- fread('/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_withdates_COA2020fixed_091523.csv')

## Make sure all sites are extracted now that I've fixed COA 2020
  nrow(meta)
  nrow(meta[is.na(meta$site)])  
  unique(meta[is.na(meta$site),]$project)  
  
  # Clean up the COA 2020 ones I just matched up
  meta$site <- ifelse(is.na(meta$site) & meta$project == 'COA_2020',
                      meta$new_stn, meta$site)
  meta$cam_type <- ifelse(is.na(meta$cam_type) & meta$project == 'COA_2020',
                          sapply(strsplit(as.character(meta$new_cam), '-'), '[', 3), meta$cam_type)
  
    # Which other ones are missing site?
    nrow(meta[is.na(meta$site) & meta$project == 'COA_2020',])   #these will all be removed; OK
    nrow(meta[is.na(meta$site) & meta$project == 'COA_2021',])   #these are just example tags; OK
    nrow(meta[is.na(meta$site) & meta$project == 'DUNES',])      #need to fix these
    nrow(meta[is.na(meta$site) & meta$project == 'ESF_BAIT',])   #need to fix these
    nrow(meta[is.na(meta$site) & meta$project == 'ESF_TRAIL',])  #need to fix these
    nrow(meta[is.na(meta$site) & meta$project == 'ESF_GROUND',]) #need to fix these
    nrow(meta[is.na(meta$site) & meta$project == 'HJA_MARIE',])  #need to fix these
    
    
## Merge metadata for ESF (data from Maggie) -----------------------------------
  esf_trail_bait <- fread('/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/ESF_All_Cam_Data.csv')
  
  nrow(meta[meta$project %in% c('ESF_BAIT','ESF_TRAIL')])
  length(unique(meta[meta$project %in% c('ESF_BAIT','ESF_TRAIL')]$image_name))
  
    #extract original filename
    meta$raw_name <- ifelse(grepl('ESF',meta$project), 
                            sub("_[a-zA-Z]{2}\\.JPG$", ".JPG", meta$image_name), NA) 
    length(unique(meta[meta$project %in% c('ESF_BAIT','ESF_TRAIL','ESF_GROUND')]$raw_name)) #not all unique

    #create a filename_species column because the original filenames are not all unique    
    meta$raw_name_species <- ifelse(grepl('ESF',meta$project),
                                    paste(meta$raw_name, meta$species, sep = ' '), NA)
    length(unique(meta[meta$project %in% c('ESF_BAIT','ESF_TRAIL','ESF_GROUND')]$raw_name_species)) #still not all unique
    #View(meta[(duplicated(meta$raw_name_species) | duplicated(meta$raw_name_species, fromLast = TRUE)),])
    
      #I moved 4 folders to '_duplicated':
        #hailey.hester-ESF_TRAIL_SpottedSkunk 
        #hailey.hester-ESF_TRAIL_NorthernFlicker 
        #hailey.hester-ESF_TRAIL_MountainQuail
        #hailey.hester-ESF_BAIT_Weasel
      
      #Remove them here too. Now they will all be unique
        meta <- meta[!meta$folder %in% c('hailey.hester-ESF_TRAIL_SpottedSkunk',
                                         'hailey.hester-ESF_TRAIL_NorthernFlicker',
                                         'hailey.hester-ESF_TRAIL_MountainQuail',
                                         'hailey.hester-ESF_BAIT_Weasel'),]
    
    #create a filename_species column in ESF metadata
    esf_trail_bait$raw_filename <- sapply(strsplit(esf_trail_bait$Filepath, '/'), '[', 3)
      nrow(esf_trail_bait)
      length(unique(esf_trail_bait$raw_filename))
      
      #clean up species names
      sort(unique(meta[meta$project %in% c('ESF_GROUND','ESF_TRAIL','ESF_BAIT'),]$species)) #just fix these ones
      sort(unique(esf_trail_bait$Species))
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Domestic dog', 'Dog', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'W. gray squirrel', 'GraySquirrel', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'CA ground squirrel', 'GroundSquirrel', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Ruffed grouse', 'Grouse', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Western skink', 'Herp', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Mountain beaver', 'MountainBeaver', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Mountain quaill', 'MountainQuail', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Northern flicker', 'NorthernFlicker', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == "Virginia's opossum", 'Opossum', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Saw-whet owl', 'Owl', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'N. Raccoon', 'Raccoon', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Spotted skunk', 'SpottedSkunk', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == "Steller's jay", 'StellersJay', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Weasel species', 'Weasel', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Dusky-footed woodrat', 'Woodrat', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == 'Bushy-tailed wodrat', 'Woodrat', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == "Bewick's wren", 'Wren', esf_trail_bait$Species)
        esf_trail_bait$Species <- ifelse(esf_trail_bait$Species == "Pacific wren", 'Wren', esf_trail_bait$Species)
        
    esf_trail_bait$raw_name_species <- paste(esf_trail_bait$raw_filename, esf_trail_bait$Species, sep = ' ')
        
    sum(meta$raw_name_species %in% esf_trail_bait$raw_name_species)
    #View(meta[!meta$raw_name_species %in% esf_trail_bait$raw_name_species,]) #hmm, not all in ESF metadata
    
  # Now match them up with data from Maggie (trail/bait only)
    meta$esf_site <- esf_trail_bait$Site[match(meta$raw_name_species, esf_trail_bait$raw_name_species)]
    meta$site <- ifelse(!is.na(meta$esf_site), meta$esf_site, meta$site)
    
    #some didn't match
    nrow(meta[is.na(meta$site) & meta$project %in% c('ESF_TRAIL','ESF_BAIT'),])
    
      #extract ones with site in filename first (do GROUND too while I'm at it)
      meta$esf_site <- ifelse(is.na(meta$site) & meta$project %in% c('ESF_TRAIL','ESF_GROUND','ESF_BAIT'),
                              ifelse(!grepl('IMG',meta$raw_name), 
                                     sapply(strsplit(meta$raw_name, '_'), '[', 1), 
                                     meta$esf_site), meta$esf_site)
      meta$site <- ifelse(!is.na(meta$esf_site), meta$esf_site, meta$site)
      nrow(meta[is.na(meta$site) & meta$project %in% c('ESF_TRAIL','ESF_BAIT'),])
      
      #for ones with generic 'IMG' name, find them in BOX folders. (not in ESF meta CSV for some reason)
      sort(unique(meta[is.na(meta$site) & meta$project %in% c('ESF_TRAIL'),]$raw_name_species))
        #they are all in trail
        #Human, Cougar, Coyote, MountainQuail, NorthernFlicker, StellersJay
        #HOSA, PUCO, CALA, ORPI, COAU, CYST
      
        meta$esf_site <- ifelse(is.na(meta$esf_site),
                                ifelse(grepl('_bj|_bp|_bg|_bl|_bm|_bh',meta$image_name), '18908G', 
                                       meta$esf_site), meta$esf_site)
        meta$site <- ifelse(!is.na(meta$esf_site), meta$esf_site, meta$site)
        nrow(meta[is.na(meta$site) & meta$project %in% c('ESF_TRAIL','ESF_BAIT'),])

        
  # Also clean up and merge datetime for ESF trail/bait
    esf_trail_bait$datetime <- paste(esf_trail_bait$Date, esf_trail_bait$Time, sep = ' ')
    esf_trail_bait$datetime <- as.POSIXct(strptime(esf_trail_bait$datetime, '%m/%d/%y %H:%M'), tz = 'America/Los_Angeles')
      
    #oh nevermind, I already extracted origin date and they seem to match. good
    
    
## Which are still missing site?
  unique(meta[is.na(meta$site),]$project)  
    
    nrow(meta[is.na(meta$site) & meta$project == 'COA_2020',])   #these will all be removed; OK
    nrow(meta[is.na(meta$site) & meta$project == 'COA_2021',])   #these are just example tags; OK
    nrow(meta[is.na(meta$site) & meta$project == 'DUNES',])      #need to fix these
    nrow(meta[is.na(meta$site) & meta$project == 'HJA_MARIE',])  #need to fix these
    

## DUNES
  nrow(meta[meta$project == 'DUNES',])
  
  meta$dunes_site <- ifelse(is.na(meta$site) & meta$project == 'DUNES',
                            sapply(strsplit(meta$image_name, '_'), '[', 1), NA)
  
  #some of these don't look like sitenames. check them:
  sort(unique(meta$dunes_site))    
  
    #fix manually
    meta$dunes_site <- ifelse(meta$dunes_site == 'IMG' & grepl('IMG_00',meta$image_name), 'DUNE4-96B', meta$dunes_site)
    meta$dunes_site <- ifelse(meta$dunes_site == 'IMG', 'DUNE10A', meta$dunes_site)
    
    #add ones I matched manually
    dunes_sites <- fread('/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/dunes_meta_tmp.csv')
    meta$dunes_site_manual <- dunes_sites$site_manual[match(meta$full_name, dunes_sites$full_name)]
    meta$dunes_site_manual <- ifelse(meta$dunes_site_manual == '', NA, meta$dunes_site_manual)
    
    meta$dunes_site <- ifelse(!is.na(meta$dunes_site_manual), meta$dunes_site_manual, meta$dunes_site)
    meta$site <- ifelse(!is.na(meta$dunes_site), meta$dunes_site, meta$site)
    
      
## HJA_MARIE
  nrow(meta[meta$project == 'HJA_MARIE',])
    
  meta$hja_site <- ifelse(is.na(meta$site) & meta$project == 'HJA_MARIE',
                          sapply(strsplit(meta$image_name, '_'), '[', 1), NA)
  
  #some of these don't look like sitenames. check them:
  sort(unique(meta$hja_site))    
    
    #fix manually
    meta$hja_site <- ifelse(meta$hja_site %in% c('10010646','10010647'), '129276', meta$hja_site)
    meta$hja_site <- ifelse(meta$hja_site %in% c('10080065','10080066','10080067'), '593665', meta$hja_site)
    
  #add to 'site' column
  meta$site <- ifelse(!is.na(meta$hja_site), meta$hja_site, meta$site)
    
  #great, got them all
  unique(meta[is.na(meta$site),]$project)  
  
  
## DATETIMES
    
## All have datetime2 now from the origin date. But can I correct some from 1970, 2014, etc?
  min(meta$datetime2)
  unique(meta[meta$datetime2 < '2016-01-01',]$folder)
  #View(meta[meta$datetime2 < '2016-01-01',])
  
## Fix 2020 datetimes from the new filenames. 
## Change them *all* because even if they already had 'datetime2' value, these were from file origin dates
  ## and may have been changed using timeshift
  nrow(meta[meta$project == 'COA_2020',])
  meta$datetime_coa2020 <- ifelse(meta$project == 'COA_2020',
                                  paste(sapply(strsplit(meta$new_name, '__'), '[', 3),
                                        sapply(strsplit(meta$new_name, '__'), '[', 4)), NA)
  meta$datetime_coa2020 <- sapply(strsplit(meta$datetime_coa2020, '\\('), '[', 1)
  meta$datetime_coa2020 <- as.POSIXct(strptime(meta$datetime_coa2020, '%Y-%m-%d %H-%M-%S'), tz = 'America/Los_Angeles')
    min(meta[!is.na(meta$datetime_coa2020),]$datetime_coa2020)
    max(meta[!is.na(meta$datetime_coa2020),]$datetime_coa2020)
    
    #View(meta[!is.na(meta$datetime_coa2020) & meta$datetime_coa2020 < '2020-01-01',])
      #10360-1-G are in 1970 still (shoot, don't know how to fix them)
      #11513-1-G are in 2017
      #10886-4-G are in 2019
      #10605-4-G are in 2019
    
    #We'll remove the 1970 ones. And for now just bump the 2017 and 2019 ones up to 2020.
    #(BUT FIX THE ACTUAL FILENAMES IN TIMESHIFT/RENAMING STEP)
    table(meta$remove.x, useNA = 'a')
    meta[meta$new_cam == '10360-1-G']$remove.x <- 'remove'
    
    #adjust 2017 (leap year but close enough)
    meta[meta$new_cam == '11513-1-G',]$datetime_coa2020 <- meta[meta$new_cam == '11513-1-G',]$datetime_coa2020 + 94608000
  
    #adjust 2019
    meta[meta$new_cam %in% c('10886-4-G','10605-4-G'),]$datetime_coa2020 <- meta[meta$new_cam %in% c('10886-4-G','10605-4-G'),]$datetime_coa2020 + 31622400
    
    #View(meta[!is.na(meta$datetime_coa2020) & meta$datetime_coa2020 < '2020-01-01',])

    
## Fix DUNES datetimes
  nrow(meta[meta$datetime2 < '2016-01-01' & grepl('DUNES',meta$folder),]) #good, only a few!
  unique(meta[meta$datetime2 < '2016-01-01' & grepl('DUNES',meta$folder),]$site)
  
  #remove the human and opossum ones at DUNE4-96B
  table(meta$remove.x, useNA = 'a')
  meta[meta$site == 'DUNE4-96B' & meta$datetime2 < '2016-01-01',]$remove.x <- 'remove'
  
  #add 6 yrs, 6 mos to marten and skunk ones at DUNE15A (**just approximate**)
  meta[meta$site == 'DUNE15A' & meta$datetime2 < '2016-01-01',]$datetime2 <- meta[meta$site == 'DUNE15A' & meta$datetime2 < '2016-01-01',]$datetime2 + 204940800
  
  
  
## Add corrections back to full 'meta'
  meta$datetime_fixed <- ifelse(!is.na(meta$datetime_coa2020),
                                as.character(meta$datetime_coa2020),
                                as.character(meta$datetime2))
  
  
## Finally, fill in cam_type  
  table(meta$cam_type, meta$project, useNA = 'a')
  
  meta$cam_type_final <- ifelse(meta$project %in% 'ORSNAP', 'T', NA)
  meta$cam_type_final <- ifelse(meta$project %in% 'HJA_MARIE', 'B', meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'HJA_GRID', 'B', meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'ESF_BAIT', 'B', meta$cam_type_final)  
  meta$cam_type_final <- ifelse(meta$project %in% 'DUNES', 'B', meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'COA_2019', 'T', meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'ESF_TRAIL', 'T', meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'ESF_GROUND', 'G', meta$cam_type_final)
  
  table(meta[is.na(meta$cam_type_final),]$cam_type, useNA = 'a')
  
  #extract camtype for fixed COA 2020 ones
  meta[meta$new_cam == '10371-3_T',]$new_cam <- '10371-3-T' #fix formatting of one
  
  meta$cam_type_coa2020 <- ifelse(meta$project == 'COA_2020',
                                  sapply(strsplit(meta$new_cam, '-'), '[', 3), NA)
  table(meta$cam_type_coa2020, useNA = 'a')
  nrow(meta[meta$project == 'COA_2020',])
  unique(meta[meta$project == 'COA_2020' & is.na(meta$cam_type_coa2020),]$remove.x)
    #good, the ones with no cam_type now are going to be removed anyway
  
  #and create COA 2020 column
  meta$cam_type_coa2021 <- ifelse(meta$project == 'COA_2021', meta$cam_type, NA)
  table(meta[meta$project == 'COA_2021',]$cam_type_coa2021, useNA = 'a')
    unique(meta[meta$project == 'COA_2021' & is.na(meta$cam_type_coa2021)]$image_name) #good
  
  #finish up
  table(meta$cam_type_final, meta$project, useNA = 'a')
  meta$cam_type_final <- ifelse(meta$project %in% 'COA_2020', meta$cam_type_coa2020, meta$cam_type_final)
  meta$cam_type_final <- ifelse(meta$project %in% 'COA_2021', meta$cam_type_coa2021, meta$cam_type_final)
  
  #good! the NAs here will be removed.
  
  
## Fix day/night
  table(meta$day_night2, useNA = 'a')
  
  #no NAs but it was before I fixed some datetimes -- redo
  meta$datetime_fixed <- as.POSIXct(strptime(meta$datetime_fixed, '%Y-%m-%d %H:%M:%S'), tz = 'America/Los_Angeles')
  meta$day_night_fixed <- ifelse(hour(meta$datetime_fixed) > 8 & hour(meta$datetime_fixed) < 20, 'day', 'night')
    table(meta$day_night_fixed, useNA = 'a')
    #close, a few switched
  
## Fix year
  table(meta$year, useNA = 'a')
  table(meta$year, meta$project, useNA = 'a')
  table(year(meta$datetime_fixed), meta$project, useNA = 'a')
  
  meta$year_fixed <- year(meta$datetime_fixed)
  table(meta$year_fixed, meta$project, useNA = 'a')
  
  
## Save
  write.csv(meta, '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_withdates_sites_COA2020fixed_091823.csv')
  write.csv(meta, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata.csv')
  

  
