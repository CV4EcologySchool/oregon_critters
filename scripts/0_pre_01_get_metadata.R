## Summarizing and creating meatadata of COA data for CV4E project

library(data.table)
library(tidyverse)
library(lubridate)


# List all images in directory (this is all ~100k of them) ---------------------
dirs <- list.files(path = '/Users/caraappel/Documents/CV4E/data', pattern = '.JPG',
                   recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  head(dirs)
  length(dirs) #113,520 after removing Insect ones with no site ID and some duplicates (was 117,426 after removing duplicates in COA_2020) (was 119,572 before)

  
# Extract and format elements --------------------------------------------------
data <- data.frame('full_name' = dirs)
data$dir_elements <- sapply(lengths(strsplit(data$full_name, '\\/')), '[', 1)
  table(data$dir_elements, useNA = 'a') #good

  #extract project and folder
  data$project <- sapply(strsplit(data$full_name, '\\/'), '[[', 7)
  data$folder <- sapply(strsplit(data$full_name, '\\/'), '[[', 8)
  
  #extract species name
  data$species <- gsub('1AM|ZF|2AM|AM|3|4|5|2|cappel-COA021|COA2019|hailey.hester-|COA2019|ORSNAP|HJA_MARIE|HJA_GRID|ESF_TRAIL|ESF_GROUND|ESF_BAIT|DUNES|cappel-COA|01|1|00|0|8|6|HH|NR|cappel|nmrugg|-|COA|_', '', data$folder)
  sort(unique(data$species))
    #clean up a few
    data$species <- ifelse(data$species %in% 'BlacktailedDeer', 'BlackTailedDeer',
                           ifelse(data$species %in% 'ChickadeeSp', 'Chickadee',
                                  ifelse(data$species %in% 'Insect','Invertebrate',
                                         ifelse(data$species %in% 'MoutainQuail', 'MountainQuail',
                                                ifelse(data$species %in% 'Racoon', 'Raccoon', 
                                                       ifelse(data$species %in% 'TounsendsChipmunk', 'TownsendsChipmunk', 
                                                              ifelse(data$species %in% 'StellarsJay','StellersJay', data$species)))))))
    sort(unique(data$species))

  #extract image name
  data$image_name <- sapply(strsplit(data$full_name, '\\/'), '[[', 10)

  
# Check if filenames have full metadata ----------------------------------------
data$renamed <- ifelse(grepl('__', data$image_name), 'y', 'n')
  table(data$renamed, useNA = 'a')
  table(data[data$renamed %in% 'n',]$project) #from these projects
  
  #for the renamed ones, how are they formatted? some have hex__hex-stn__date__time and some are hex-stn__date__time
  data$name_elements <- ifelse(data$renamed %in% 'y', sapply(lengths(strsplit(data$image_name, '\\__')), '[', 1), NA)
    table(data$name_elements, useNA = 'a')

  #get sites from these
  data$site <- ifelse(data$name_elements == 3,
                      sapply(strsplit(as.character(data$image_name), '\\__'), '[', 1),
                      ifelse(data$name_elements == 4,
                             sapply(strsplit(as.character(data$image_name), '\\__'), '[', 2),
                             ifelse(data$name_elements == 2,
                                    sapply(strsplit(as.character(data$image_name), '\\__'), '[', 1), NA)))
  unique(data$site) #I'll have to manually fix some of these from DUNES, ESF, HJA_MARIE

  #ground or trail?
  data$cam_type <- ifelse(data$project %in% 'ESF_GROUND', 'G',
                          ifelse(data$project %in% 'ESF_TRAIL', 'T', 
                                 ifelse(grepl('-T', data$image_name), 'T',
                                        ifelse(grepl('-G', data$image_name), 'G', NA))))
  table(data$cam_type, useNA = 'a')
  table(data$project, data$cam_type) #makes sense. COA_2020 also had G/T but need full filenames; will try to match up later
  
  #get dates if there
  data$date <- ifelse(data$name_elements == 4,
                      sapply(strsplit(as.character(data$image_name), '\\__'), '[', 3),
                      ifelse(data$name_elements == 3,
                             sapply(strsplit(as.character(data$image_name), '\\__'), '[', 2),
                             ifelse(data$name_elements == 2,
                                    sapply(strsplit(as.character(data$image_name), '\\__'), '[', 2), NA)))
  unique(data$date)
  data$date <- gsub('_IMG_0226_cp.JPG', '', data$date) #fix a couple
  data$date <- gsub('_IMG_0226_cn.JPG', '', data$date)
  
    #dates for MARIE_HJA
    data$date <- ifelse(data$project %in% 'HJA_MARIE', 
                        sapply(strsplit(as.character(data$image_name), '\\_'), '[', 2), 
                        data$date)
      data$date <- ifelse(data$project %in% 'HJA_MARIE',
                          ifelse(grepl('.JPG', data$date), NA, data$date), data$date)
      data$date <- ifelse(data$project %in% 'HJA_MARIE',
                          ifelse(grepl('2017-12-19', data$image_name), '2017-12-19', data$date), data$date) #fix a few
      data$date <- ifelse(data$project %in% 'HJA_MARIE',
                          ifelse(grepl('_co.JPG', data$image_name), NA, data$date), data$date) #fix a few
 
  #get times
  data$time <- ifelse(data$name_elements == 4,
                      sapply(strsplit(as.character(sapply(strsplit(as.character(data$image_name), '\\__'), '[', 4)), '\\('), '[', 1),
                      ifelse(data$name_elements == 3,
                             sapply(strsplit(as.character(sapply(strsplit(as.character(data$image_name), '\\__'), '[', 3)), '\\('), '[', 1),
                             NA))
  unique(data$time)
  
  #datetimes
  data$datetime <- paste(data$date, data$time, sep = ' ')
  data$datetime <- as.POSIXct(strptime(data$datetime, '%Y-%m-%d %H-%M-%S'), tz = 'America/Los_Angeles')
  data$datetime_round <- floor_date(data$datetime, 'minute')
  
    nrow(data[is.na(data$datetime),])
    unique(data[is.na(data$datetime),]$project)
    
    #check some missing datetime
    #View(data[is.na(data$datetime) & data$renamed %in% 'y',])
    toFix1 <- which(is.na(data[,'datetime']) & data[,'renamed'] == 'y' & data[,'project'] == 'COA_2021')
    #toFix2 <- which(is.na(data[,'datetime']) & data[,'renamed'] == 'y' & data[,'project'] == 'HJA_MARIE') #these don't have times, it's OK
    
      #COA 2021 (daylight savings time missing hour) -- make them all 3am
      data[toFix1,]$datetime <- as.POSIXct(paste(data[toFix1,]$date, data[toFix1,]$time, sep = ' ')) + 3600*2
      data[toFix1,]$datetime_round <- floor_date(data[toFix1,]$datetime, 'minute')
        #View(data[toFix1,])
      
  #get sequence
  data$seq <- gsub('.*[()]([^.]+)[)].*', '\\1', data$image_name)
  data$seq <- ifelse(grepl('JPG', data$seq), NA, data$seq)
    table(data$seq, useNA = 'a')  

  #night or day? (choose a cutoff -- currently 8 am - 8 pm)
  data$day_night <- ifelse(hour(data$datetime) > 8 & hour(data$datetime) < 20, 'day', 'night')
    table(data$day_night, useNA = 'a')    
  
  #year
  data$year <- lubridate::year(data$datetime)
    table(data$year, useNA = 'a')
    table(data[is.na(data$year),]$folder)
    table(data[is.na(data$year),]$project)
    #fix some:
      data[data$project %in% 'COA_2020',]$year <- '2020'
      data[data$project %in% 'COA_2021',]$year <- '2021'
      data[data$project %in% 'DUNES',]$year <- '2021'
      data[data$project %in% c('ESF_BAIT','ESF_TRAIL','ESF_GROUND'),]$year <- '2022'
      #did I get them all?
      table(data$year, useNA = 'a')
      #View(data[is.na(data$year),]) #only HJA_MARIE are left; I'll work on those later
      
  #preview
  head(data[,c('project','year','species','site','cam_type','image_name','datetime','day_night')])
      
  #save
  #write.csv(data, 'COA_training/COA_data_summary_081523.csv')
  write.csv(data, '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_data_summary_091323.csv')
  
  
## Summarize
  
  #species totals
  sort(table(data$species))
  (species <- data %>% group_by(species) %>% summarise('nPhotos' = n()))

  #species/project
  (species_project <- data %>% group_by(species, project) %>% summarise('nPhotos' = n()))
  
  ggplot(species_project, aes(x = reorder(species, -nPhotos), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_bw() 
  
  species_project_df <- table(data$species, data$project)
  write.csv(species_project_df, '/Users/caraappel/Documents/_RESEARCH/YOLO/COA_training/COA_species_project.csv')
  
  #species/daynight
  (species_day_night <- data %>% group_by(species, day_night) %>% summarise('nPhotos' = n()))
  
  ggplot(species_day_night, aes(x = reorder(species, -nPhotos), y = nPhotos, fill = day_night)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_bw()

  #species/camtype
  (species_cam_type <- data %>% group_by(species, cam_type) %>% summarise('nPhotos' = n()))
  
  ggplot(species_cam_type, aes(x = reorder(species, -nPhotos), y = nPhotos, fill = cam_type)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_bw()
  
  

  