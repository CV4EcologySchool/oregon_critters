# Match up filenames from COA 2020 with renaming tables so I can get site and correct datetime

library(data.table)
library(tidyverse)
library(lubridate)

# Read in metadata with datetimes (from script pre_02_get_datetimes.R)
  meta <- fread('/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_withdates_091323.csv')

# Read in 2020 renaming tables
  rtables <- list.files(path = '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_2020_renaming_tables/', pattern = '.csv',
                        recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  rtables_list <- lapply(rtables, read.table, header = TRUE, sep = ',', fill = TRUE)
  
  renames <- rbindlist(rtables_list, fill = TRUE, idcol = 'origin')
  renames[, origin := factor(origin, labels = basename(rtables))]
  renames$origin <- as.character(renames$origin)
  
  nrow(renames)                    #571,831 files
  length(unique(renames$FileName)) #127,485 unique filenames
  
  head(meta)  
  head(renames)  

  
## -----------------------------------------------------------------------------

# get just COA 2020
  coa2020 <- meta[meta$project %in% 'COA_2020',]
  
  
#1. extract original filename for ones that have 'Drive_'
  coa2020$raw_name <- ifelse(grepl('Drive', coa2020$image_name),
                             gsub('Drive_0[0-9]_', '', coa2020$image_name), coa2020$image_name)
  
  #also remove unique project/species key for matching
  coa2020$raw_name <- gsub("_[[:alpha:]]{2}\\.JPG", ".JPG", coa2020$raw_name)
  
  
#2. are all raw names in the renaming tables somewhere?
  sum(coa2020$raw_name %in% renames$FileName)
  sum(!coa2020$raw_name %in% renames$FileName) #not all

  #these are the ones that aren't. they have extra digits appended (I assume to prevent duplicates)
  coa2020[!coa2020$raw_name %in% renames$FileName,]$raw_name
  
  
#3. those digits may be useful... but ignore for now
  coa2020$match <- ifelse(coa2020$raw_name %in% renames$FileName, TRUE, FALSE)
    table(coa2020$match, useNA = 'a')
  coa2020$fix_name <- ifelse(grepl('_', coa2020$raw_name) & !grepl('IMG', coa2020$raw_name), TRUE, FALSE)
    table(coa2020$fix_name, useNA = 'a')
  
  coa2020$raw_name2 <- ifelse(coa2020$fix_name %in% TRUE,
                              paste(sapply(strsplit(coa2020$raw_name, '_'), '[', 1), 'JPG', sep = '.'),
                              coa2020$raw_name)
  
  #a few didn't get changed; look at them
  nrow(coa2020[coa2020$match %in% FALSE & coa2020$raw_name == coa2020$raw_name2,])
  #View(coa2020[coa2020$match %in% FALSE & coa2020$raw_name == coa2020$raw_name2,])
  
  #fix the ones with 'IMG' in them. the other ones must just not have a match in the renaming table
  coa2020$raw_name2 <- ifelse(coa2020$match %in% FALSE & coa2020$raw_name == coa2020$raw_name2,
                              ifelse(grepl('IMG', coa2020$raw_name2),
                                     paste('IMG_', sapply(strsplit(coa2020$raw_name2, '_'), '[', 2), '.JPG', sep = ''),
                                     coa2020$raw_name2),
                              coa2020$raw_name2)
  
  
#4. now look at matches again. are all raw names in the renaming tables somewhere?
  sum(coa2020$raw_name2 %in% renames$FileName)
  sum(!coa2020$raw_name2 %in% renames$FileName) #all have matches now -- yay!

  
# 5. proceed with finding matches
  head(coa2020[,c('image_name','raw_name2')])
  head(renames[,c('Directory','FileName','Station','Camera','DateTimeOriginal','filename_new')])
  
  #how many duplicated filenames?
  length(unique(coa2020$raw_name2))  #35,028 unique in raw_name2
  length(coa2020$raw_name2)  #64,168 (yikes, lots of duplicates)
  
  length(unique(renames$FileName)) #127,485 unique in renames
  length(renames$FileName)  #571,831 total (yikes, lots of duplicates)
 
   
# 6. combine filename and datetime
  
  #in metadata
  coa2020$name_datetime <- paste(coa2020$raw_name2, coa2020$datetime2, sep = ' ')

    length(unique(coa2020$name_datetime))
    length(coa2020$name_datetime) #almost all unique now but not all
    #View(coa2020[duplicated(coa2020$name_datetime) | duplicated(coa2020$name_datetime, fromLast = TRUE),])
  
  #in renames
    #first format dates
    nrow(renames[is.na(renames$DateTimeOriginal),]) #63 NAs. also, some are like 2020-01-01 and some are like 1/1/20
    #deal with 2 different formats
    renames$DateTimeFormatted <- ifelse(grepl('-', renames$DateTimeOriginal), 
                                        as.character(as.POSIXct(strptime(renames$DateTimeOriginal, '%Y-%m-%d %H:%M:%S'))),
                                        as.character(as.POSIXct(strptime(renames$DateTimeOriginal, '%m/%d/%y %H:%M'))))
    #deal with ones w/o time and format as POSIX
    # renames$DateTimeFormatted <- ifelse(renames$DateTimeFormatted == '1970-01-01',
    #                                     as.character(as.POSIXct(renames$DateTimeFormatted) + 1),
    #                                     as.character(renames$DateTimeFormatted))
    #ok to keep bc they'll be the same in coa2020
    nrow(renames[is.na(renames$DateTimeFormatted),]) #64 now. oh well
    
    #now create new column
    renames$name_datetime <- paste(renames$FileName, renames$DateTimeFormatted, sep = ' ')
      length(unique(renames$name_datetime))
      length(renames$name_datetime) #almost all unique now
      # View(renames[duplicated(renames$name_datetime) | duplicated(renames$name_datetime, fromLast = TRUE),])

        
# 7. view duplicates in filename_datetime (metadata)
    coa2020$has_dup <- duplicated(coa2020$name_datetime) | duplicated(coa2020$name_datetime, fromLast = TRUE)
      table(coa2020$has_dup, useNA = 'a')
      #View(coa2020[coa2020$has_dup == TRUE,])
      #oh wow, a lot of these are the exact same! need to get rid of them
      #e.g., ended up in two separate projects
      
    ## THE FOLLOWING WAS DONE ONCE TO IDENTIFY EXACT DUPLICATES, WHICH I REMOVED FROM THE FOLDERS.
    ## DO NOT NEED TO REPEAT IT.
    ## SKIP AHEAD
  
  #   #are some of these folders exact copies?
  #   table(coa2020[coa2020$has_dup == TRUE,]$folder)
  #     
  #   #I think these are ones that were started locally and finished online, or vice versa...
  #   #Check each presumed duplicate (the first one) to see if all in one are present in the other folder (the second one)
  #   #Then open the .db file in the duplicated folder (the first one) and make sure there are no labels
  #   #Then move it to the folder '_duplicated'
  #   
  # ## First, the ones with obvious copies:
  #   #AMERICAN ROBIN
  #     coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_AmericanRobin_HH_1',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_AmericanRobin_HH',]$name_datetime
  #     coa2020[coa2020$folder %in% 'nmrugg-COA_2020_AmericanRobin_NR',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_AmericanRobin_HH',]$name_datetime
  #     #yes, can move both to '_duplicated'
  #     
  #   #BLACK BEAR  
  #     coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_BlackBear_HH_1',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_BlackBear_HH',]$name_datetime
  #     coa2020[coa2020$folder %in% 'nmrugg-COA_2020_BlackBear_NR',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_BlackBear_HH',]$name_datetime
  #     #yes, can move both to '_duplicated'
  # 
  #   #BLACK-TAILED DEER  
  #     coa2020[coa2020$folder %in% 'nmrugg-COA_2020_Black-tailedDeer_NR',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_BlackTailedDeer_HH',]$name_datetime
  #     #yes, can move to '_duplicated'
  # 
  #   #COUGAR
  #     coa2020[coa2020$folder %in% 'nmrugg-COA_2020_Cougar_NR',]$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Bobcat_HH',]$name_datetime
  #     #yes, can move to '_duplicated'   
      
  # ## What's left?
  #     coa2020$remove <- ifelse(coa2020$folder %in% c('hailey.hester-COA_2020_AmericanRobin_HH_1','nmrugg-COA_2020_AmericanRobin_NR',
  #                                                    'hailey.hester-COA_2020_BlackBear_HH_1','nmrugg-COA_2020_BlackBear_NR',
  #                                                    'nmrugg-COA_2020_Black-tailedDeer_NR','nmrugg-COA_2020_Cougar_NR'), TRUE, FALSE)
  #     table(coa2020$remove, useNA = 'a')
  #     table(coa2020[coa2020$has_dup == TRUE & coa2020$remove == FALSE,]$folder)
      
  # ## Where are the other duplicates?
  #     #after viewing, filter by 'has_dup' = TRUE
  #     
  #   #BAT  
  #     View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Bat_HH',]$name_datetime,])
  #     #odd, these are varied thrush but are in a bat folder also (unlabeled). move it to '_duplicated'
  #     coa2020$remove <- ifelse(coa2020$folder %in% 'hailey.hester-COA_2020_Bat_HH', TRUE, coa2020$remove)
  #     
  #   #FLYING SQUIRREL
  #     View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_FlyingSquirrel_HH',]$name_datetime,])
  #     #I moved 4 duplicate images to '_duplicated' and one flying squirrel in the thrush folder to '_duplicated' 
  #     #The others do have multiple spcies in them
  # 
  #   #GRAY SQUIRREL
  #     View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_GraySquirrel_HH_5',]$name_datetime,])
  #     #these are just example images; moved them to '_duplicated'
  #     
  #   #INSECT
  #     View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Insect_HH',]$name_datetime,])
  #     #I moved 3 duplicate images to '_duplicated'. the others do have multiple species in them
  # 
  #     #ROOSEVELT ELK
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_RooseveltElk_HH',]$name_datetime,])
  #       #moved 1 copy. the rest truly have both (cool!)
  # 
  #     #THRUSH
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Thrush_HH',]$name_datetime,])
  #       #these do have multiple species in them
  #       
  #     #RABBIT
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Rabbit_HH',]$name_datetime,])
  #       #these do have multiple species in them
  #       
  #     #HERP
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Herp_HH',]$name_datetime,])
  #       #these do have multiple species in them
  #       
  #     #DOUGLAS SQUIRREL
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_DouglasSquirrel_HH',]$name_datetime,])
  #       #these do have both douglas squirrel and bobcat! are they labeled as both in both?
  #       
  #     #DARK-EYED JUNCO
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_DarkEyedJunco_HH',]$name_datetime,])
  #       #these do have both dark-eyed junco and garter snake! are they labeled as both in both?
  #       
  #     #BOBCAT
  #       View(coa2020[coa2020$name_datetime %in% coa2020[coa2020$folder %in% 'hailey.hester-COA_2020_Bobcat_HH',]$name_datetime,])
  #       #some of these truly do have both species in them, and they are labeled in both. but should only keep one copy... hmm
      
      ## Left off here 9/7/23
      
      ## what to do when an image has 2 species in it? it doesn't need to be in both folders, 
      ## but make sure the one I keep has both tags present in the .db
      ## (probably not a huge deal if we don't get all of these but it could mess it up for validation,
      ## e.g., if we have an image with only bobcat labeled and it finds a chipmunk that we didn't label but is correct)
      ## -- or I could just remove all images with multiple species for simplicity... see how many that would remove
      
      
    ## OK, so now we have some that are truly the same image but have different full names because they are in multiple species folders
      ## e.g., Drive_02_04090056_ij.JPG in Thrush and Drive_02_04090056_iu.JPG in Varied Thrush
    ## So keep working with the name_datetime field
      
      
# 7. view duplicates in filename_datetime (metadata)
    renames$has_dup <- duplicated(renames$name_datetime) | duplicated(renames$name_datetime, fromLast = TRUE)
      table(renames$has_dup, useNA = 'a')
      #View(renames[renames$has_dup == TRUE,])
      
    #any full duplicates?
    nrow(renames); nrow(dplyr::distinct(renames)) #no, they are all distinct in some way
    
    #so what's going on with them? don't see anything suspicious so they must truly just have the exact same datetime and name
    #try to match up with name, date, *and* species tag now
      
      
# 9. read in digikam tags (from script Cameras/scripts/COA_01b_digiKam_tags.R)
    tags <- fread('/Users/caraappel/Documents/_RESEARCH/Cameras/Coast_Range/outputs/01_digikam_tags_2020_new091423.csv')
      head(tags)        
      #this has only images that were tagged, not empty ones
      
    #keep only species tags (not number, age, etc.)
    tags <- tags[tags$cleartext_parent %in% 'Species',]
      
    #match up with renames
    renames_tags <- merge(renames, tags, by.x = 'filename_new', by.y = 'name', all.x = FALSE, all.y = FALSE)
      #should be able to keep only the ones with tags. the others won't be in training data right?
      head(renames_tags[,c('filename_new','name_datetime','site','cleartext_child')])      
      
    
# 8. make sure species names match
    sort(unique(renames_tags$cleartext_child))
    sort(unique(coa2020$species))
    
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'American robin', 'AmericanRobin', renames_tags$cleartext_child)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Bat sp', 'Bat', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Black bear', 'BlackBear', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Black-capped chickadee', 'Chickadee', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Black-tailed deer', 'BlackTailedDeer', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Chestnut-backed chickadee', 'Chickadee', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Chickadee sp', 'Chickadee', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Dark-eyed junco', 'DarkEyedJunco', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Douglas squirrel', 'DouglasSquirrel', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Flying squirrel', 'FlyingSquirrel', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Gray squirrel', 'GraySquirrel', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Ground squirrel', 'GroundSquirrel', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Herp sp', 'Herp', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Insect sp', 'Invertebrate', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Mountain beaver', 'MountainBeaver', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Mountain quail', 'MountainQuail', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Northern flicker', 'NorthernFlicker', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Other bird', 'OtherBird', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Other large mammal', 'OtherLargeMammal', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Other small mammal', 'OtherSmallMammal', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Rabbit or hare', 'Rabbit', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Roosevelt elk', 'RooseveltElk', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Spotted skunk', 'SpottedSkunk', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == "Steller's jay", 'StellersJay', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Striped skunk', 'StripedSkunk', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Thrush sp', 'Thrush', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == "Townsend's chipmunk", 'TownsendsChipmunk', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Varied thrush', 'VariedThrush', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Warbler sp', 'Warbler', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Weasel sp', 'Weasel', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Warbler sp', 'Warbler', renames_tags$species)
    renames_tags$species <- ifelse(renames_tags$cleartext_child == 'Woodpecker sp', 'Woodpecker', renames_tags$species)
      sort(unique(renames_tags$species))    
      
      
# 9. create 'name_datetime_species' columns
      
    #in renaming/tags  
    renames_tags$name_datetime_species <- paste(renames_tags$name_datetime, renames_tags$species, sep = ' ')
    
    #in photos
    coa2020$name_datetime_species <- paste(coa2020$name_datetime, coa2020$species, sep = ' ')
    

# 8. try to match them up now
    head(coa2020)
    head(renames_tags)    
    
    coa2020 <- coa2020[,-'V1'] #get rid of this column bc it will give an error when merging
      
    sum(coa2020$name_datetime_species %in% renames_tags$name_datetime_species)
    sum(renames_tags$name_datetime_species %in% coa2020$name_datetime_species)
    
    #match
    coa2020_matches <- merge(coa2020, renames_tags, by = 'name_datetime_species', all.x = TRUE, all.y = FALSE)
      nrow(coa2020)    
      nrow(coa2020_matches)
      #nrow(renames_tags)  #don't need to match all renames. many were empty and didn't end up in species folders
      
      #review ones that didn't match
      nrow(coa2020_matches[!is.na(coa2020_matches$filename_new),])
      nrow(coa2020_matches[is.na(coa2020_matches$filename_new),])  #some still didn't match up
      # View(coa2020_matches[is.na(coa2020_matches$filename_new),])
      
      #what about ones that had same name but not datetime? could it be rounding or something
      # View(coa2020_matches[!is.na(coa2020_matches$filename_new),])
      # View(coa2020_matches[is.na(coa2020_matches$filename_new),])

      #ok, some don't match because we fixed the timeshifts... hmm
      
    
# 9. try some matches based on just name and species (not date) 
    
    #just compare the ones that are left  
    coa2020_noMatch <- coa2020_matches[is.na(coa2020_matches$filename_new),] #the ones left
    renames_tags_noMatch <- renames_tags[!(renames_tags$name_datetime_species %in% coa2020_matches$name_datetime_species),]
    
    #create new columns for matching
    coa2020_noMatch$name_species <- paste(sapply(strsplit(coa2020_noMatch$name_datetime_species, ' '), '[', 1),
                                          sapply(strsplit(coa2020_noMatch$name_datetime_species, ' '), '[', 4), sep = ' ')
    renames_tags_noMatch$name_species <- paste(sapply(strsplit(renames_tags_noMatch$name_datetime_species, ' '), '[', 1),
                                              sapply(strsplit(renames_tags_noMatch$name_datetime_species, ' '), '[', 4), sep = ' ')
    
    #will there be multiple matches?
    length(unique(coa2020_noMatch$name_species)); nrow(coa2020_noMatch)
    length(unique(renames_tags_noMatch$name_species)); nrow(renames_tags_noMatch) #yeah there probably will be
    
    #any full dups?
    nrow(renames_tags_noMatch); nrow(dplyr::distinct(renames_tags_noMatch)) #no

    #match
    coa2020_sp_name_matches <- merge(coa2020_noMatch, renames_tags_noMatch, by = 'name_species', all.x = TRUE, all.y = FALSE)
      nrow(coa2020_noMatch)    
      nrow(coa2020_sp_name_matches) #some had multiple matches
      
      #what to do about multiple matches? 
      nrow(coa2020_sp_name_matches); nrow(dplyr::distinct(coa2020_sp_name_matches)) #no full dups
      
      count_matches <- data.frame(table(coa2020_sp_name_matches$name_species))
      #View(coa2020_sp_name_matches[count_matches[coa2020_sp_name_matches$name_species] > 1,])
      coa2020_sp_name_matches$nDup <- count_matches$Freq[match(coa2020_sp_name_matches$name_species, count_matches$Var1)]
      #85 of these
        #Some of these had datetime adjusted so the name_datetime didn't match up with anything
        # but there are multiple matches for name_species
        # e.g., '04090083.JPG VariedThrush' could either be 11324-1__11324-1-G__2020-04-09__12-48-51(2) or 9113-4__9113-4-T__2020-04-09__10-30-55(5) so it created a row for each potential match
        # both are varied thrush but the datetime was adjusted so doesn't match either
        #Too much work to try to match up to the correct one on case-by-case basis...
      
        #Others of these are because two generic filenames both matched with the same renamed name
        # e.g., '04140019.JPG TownsendsChipmunk' and '04140019.JPG TownsendsChipmunk' obviously both matched with 8149-2__8149-2-G__2020-04-14__08-49-25(1).JPG
        # but their full raw filenames were actually different (Drive_06_04140019_12063_ik.JPG and Drive_06_04140019_12244_ik.JPG)
        # *ahh. those are the same image! *I MANUALLY DELETED THESE DUPS.* 9/14/23
        # But not all of them are the same... e.g., Drive_02_04300151_ij.JPG and Drive_06_04300151_13115_ij.JPG are both thrush but are not the same image
        # Assume it's the same image if Drive and base name are the same?
      
        # Just ignore these 85 dups that I cannot reconcile
        coa2020_sp_name_matches$remove <- ifelse(coa2020_sp_name_matches$nDup > 1,
                                                 'remove', 'keep')
        table(coa2020_sp_name_matches$remove, useNA = 'a')
        
      #review ones that didn't match
      nrow(coa2020_sp_name_matches[!is.na(coa2020_sp_name_matches$filename_new.y),]) #1810 matched
      nrow(coa2020_sp_name_matches[is.na(coa2020_sp_name_matches$filename_new.y),])  #only 411 didn't match
      #View(coa2020_sp_name_matches[is.na(coa2020_sp_name_matches$filename_new.y),])  #kinda oddballs

      #just delete them... it will be OK
      coa2020_sp_name_matches$remove <- ifelse(is.na(coa2020_sp_name_matches$filename_new.y),
                                               'remove', coa2020_sp_name_matches$remove)
      table(coa2020_sp_name_matches$remove, useNA = 'a')  
      
      
# 10. merge all back together
    head(coa2020)
      nrow(coa2020); length(unique(coa2020$full_name)) #based on this field
      
    #merge from coa2020_matches
    coa2020_yesMatch <- coa2020_matches[!is.na(coa2020_matches$filename_new)]  
    
    head(coa2020_yesMatch)  
      nrow(coa2020_yesMatch); length(unique(coa2020_yesMatch$full_name)) #good
      
    coa2020$new_name <- coa2020_yesMatch$filename_new[match(coa2020$full_name, coa2020_yesMatch$full_name)]
    coa2020$new_stn <- coa2020_yesMatch$Station[match(coa2020$full_name, coa2020_yesMatch$full_name)]
    coa2020$new_cam <- coa2020_yesMatch$Camera[match(coa2020$full_name, coa2020_yesMatch$full_name)]
    coa2020$remove   <- ifelse(coa2020$full_name %in% coa2020_yesMatch$full_name, 'keep', NA)
      table(coa2020$remove, useNA = 'a')
    
    #merge from coa2020_noMatch (coa2020_sp_name_matches)
    head(coa2020_sp_name_matches)
      nrow(coa2020_sp_name_matches); length(unique(coa2020_sp_name_matches$full_name)) #nope
      nrow(coa2020_sp_name_matches[coa2020_sp_name_matches$remove == 'keep']); length(unique(coa2020_sp_name_matches[coa2020_sp_name_matches$remove == 'keep',]$full_name)) #ok
      
      #for dups of 'full_name', which one to keep?
      table(coa2020_sp_name_matches[coa2020_sp_name_matches$nDup > 1,]$remove, useNA = 'a') #oh right, they are all to remove, that's easier
      
      #first split into two dataframes
      coa2020_sp_name_matches_dups   <- coa2020_sp_name_matches[coa2020_sp_name_matches$nDup > 1,]; nrow(coa2020_sp_name_matches_dups)
      coa2020_sp_name_matches_noDups <- coa2020_sp_name_matches[coa2020_sp_name_matches$nDup <= 1,]; nrow(coa2020_sp_name_matches_noDups)

      sum(unique(coa2020_sp_name_matches_dups$full_name) %in% unique(coa2020_sp_name_matches_noDups$full_name))
      sum(unique(coa2020_sp_name_matches_noDups$full_name) %in% unique(coa2020_sp_name_matches_dups$full_name))
        #then why are there 51 already to remove below
      
      #keep only unique in 'dups'
      length(unique(coa2020_sp_name_matches_dups$full_name)); nrow(coa2020_sp_name_matches_dups)
      coa2020_sp_name_matches_dups <- coa2020_sp_name_matches_dups %>% distinct(full_name, .keep_all = TRUE)
      length(unique(coa2020_sp_name_matches_dups$full_name)); nrow(coa2020_sp_name_matches_dups)
      
      #add values from 'dups' to main
      table(coa2020$remove, useNA = 'a')
      table(coa2020_sp_name_matches_dups$remove, useNA = 'a')
        coa2020_merge1 <- merge(coa2020, coa2020_sp_name_matches_dups[,c('full_name','remove')], by = 'full_name', all.x = TRUE)
        coa2020_merge1$remove.x <- ifelse(!is.na(coa2020_merge1$remove.y), coa2020_merge1$remove.y, coa2020_merge1$remove.x)
      table(coa2020_merge1$remove.x, useNA = 'a') #good
      
      #add values from 'noDups' to main
      table(coa2020_sp_name_matches_noDups$remove, useNA='a') #should add 411 to 'remove'
      
      coa2020_merge2 <- merge(coa2020_merge1, coa2020_sp_name_matches_noDups[,c('full_name','remove','filename_new.y','Station.y','Camera.y')],
                              by = 'full_name', all.x=TRUE)
      coa2020_merge2$remove.x <- ifelse(!is.na(coa2020_merge2$remove), coa2020_merge2$remove, coa2020_merge2$remove.x)
      table(coa2020_merge2$remove.x, useNA = 'a')
        #great, none left to match!
      
      #combine new columns
      coa2020_merge2$new_name <- ifelse(!is.na(coa2020_merge2$filename_new.y), coa2020_merge2$filename_new.y, coa2020_merge2$new_name)
      coa2020_merge2$new_stn  <- ifelse(!is.na(coa2020_merge2$Station.y), coa2020_merge2$Station.y, coa2020_merge2$new_stn)
      coa2020_merge2$new_cam <- ifelse(!is.na(coa2020_merge2$Camera.y), coa2020_merge2$Camera.y, coa2020_merge2$new_cam)
      
        #COLUMNS TO USE: new_name, new_stn, new_cam, remove.x
        
        #did we get them all?
        nrow(coa2020_merge2[is.na(coa2020_merge2$new_name),]) #not all have new name
          table(coa2020_merge2[is.na(coa2020_merge2$new_name),]$remove.x, useNA = 'a') #but the ones that don't should be removed
          
        #good!
        
          
          
  #now delete the ones to remove
        
    #or rather just copy the ones without remove to some location when I'm ready to do on aviann?
  
  #will also have to make sure they are ignored in .db (going from .db to COCO...)
        
        
          
  #and then extract all relevenat info (site, datetime, camera type)
          
  head(coa2020_merge2)
  nrow(coa2020_merge2)
    length(unique(coa2020_merge2$full_name))
          
        
  #and match coa2020 back up with all the rest
  
  head(meta)
  nrow(meta[meta$project %in% 'COA_2020',])
  
  meta_2020merge <- merge(meta[,-'V1'], coa2020_merge2[,c('full_name','new_name','new_stn','new_cam','remove.x')], by = 'full_name', all.x = TRUE)
  
  nrow(meta)        
  nrow(meta_2020merge)
    nrow(meta_2020merge[meta_2020merge$project %in% 'COA_2020',])
    head(meta_2020merge[meta_2020merge$project %in% 'COA_2020',])
  
  
  #save
    write.csv(meta_2020merge, '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_withdates_COA2020fixed_091523.csv')
    
    
    
    