# Organize metadata for images that didn't have complete filenames
# This will also be useful for matching COA 2020 images with renaming table info

library(data.table)
library(lubridate)

# Read in metadata (from script pre_01_get_metadata.R)
  meta <- fread('/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_091323.csv')
  
# Read in origin datetimes I extracted from exiftool
  exif <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/dataset_date_created_exif.csv')

  head(meta)  
  head(exif)  
  
## -----------------------------------------------------------------------------
  
# 1. match up extracted origin datetimes
  meta$origin_date <- exif$`Creation Date`[match(meta$full_name, exif$`Full Path`)]
    nrow(meta[is.na(meta$origin_date),]) #good, they all matched
    
  #format
  meta$origin_date <- as.POSIXct(strptime(meta$origin_date, '%Y:%m:%d %H:%M:%S'), tz = 'America/Los_Angeles')

  #if it already has datetime, I'll use that one. otherwise, use origin date
  meta$datetime2 <- ifelse(is.na(meta$datetime), 
                           format(meta$origin_date, format = "%Y-%m-%d %H:%M:%S %Z"),
                           format(meta$datetime, format = "%Y-%m-%d %H:%M:%S %Z"))
  meta$datetime2 <- as.POSIXct(meta$datetime2)

  #do some QC
  nrow(meta[is.na(meta$datetime2),])
  min(meta$datetime2)
  max(meta$datetime2)
  
    View(meta[meta$datetime2 < '2017-01-01']) 
    #that's too bad. can't do much about these. did I correct them in timeshifts?
    #the 2016 ones in HJA_MARIE are okay, though
    
    
# 2. add night/day
  meta$day_night2 <- ifelse(hour(meta$datetime2) > 8 & hour(meta$datetime2) < 20, 'day', 'night')
    table(meta$day_night2, useNA = 'a')
    
    
# 3. add year
  meta$year2 <- ifelse(is.na(meta$year), 
                       lubridate::year(meta$datetime2),
                       meta$year)
  table(meta$year2, meta$project, useNA = 'a') #looks good! 
  table(year(meta$datetime2), meta$project, useNA = 'a') #they are not all correct in the datetime field, though

  
# 4. add sequence -- this time as a unique identifier based on datetime. shoot, I also need site though
  ##skip for now
  
  
# 5. preview  
  head(meta[,c('project','year','species','site','cam_type','image_name','datetime2','day_night2', 'year2')])
    
  
# 6. save
  write.csv(meta, '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_withdates_091323.csv')
  
  
## -----------------------------------------------------------------------------
  
# 6. Now try to match up COA 2020
  
  ## script 'pre_03_2020_filenames.R'
  
  
  
## -----------------------------------------------------------------------------
  
# 7. Add sites for ESF, DUNES, HJA_MARIE
  
  
  
    