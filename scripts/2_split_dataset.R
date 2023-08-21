## Split CV4E dataset

library(data.table)
library(dplyr)
library(ggplot2)

## Read in metadata and labels for my dataset ----------------------------------
  dat <- fread('metadata_labels/sampled_ds_300.csv'); dat <- dat[,-1]
    length(unique(dat$full_path))
    
    
## View splits/summary of final subset -----------------------------------------
    
  #species totals 
    (species_totals <- dat %>% group_by(CName) %>% summarise('nPhotos' = length(unique(full_path))))
    p1 <- ggplot(species_totals, aes(reorder(CName, -nPhotos), y = nPhotos)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
    p1
  
  #species/camera
    (species_site <- aggregate(dat, site ~ CName, function(x) length(unique(x))))
    p2 <- ggplot(species_site, aes(x = reorder(CName, -site, sum), y = site)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
    p2
    
  #species/project
    (species_project <- dat %>% group_by(CName, project) %>% summarise('nPhotos' = length(unique(full_path))))
    p3 <- ggplot(species_project, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
    p3
    
  #species/daynight
    (species_day_night <- dat %>% group_by(CName, day_night) %>% summarise('nPhotos' = n()))
    p4 <- ggplot(species_day_night, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
    p4  
    
  #species/camtype
    (species_cam_type <- dat %>% group_by(CName, cam_type) %>% summarise('nPhotos' = n()))
    p5 <- ggplot(species_cam_type, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
    p5
    

## SPLIT -----------------------------------------------------------------------
    
## GOAL: 75% training, 15% validation, 15% test    
    
## 1. ensure rare species end up in all sets

  #set a threshold and find the rare species
    rare_thresh = 100
    (rare_sp <- species_totals[species_totals$nPhotos <= rare_thresh,]$CName)

  #find the cameras with photos of these species
    rare_cams <- data.frame()
    for (rr in rare_sp){
      sp_cams <- unique(dat[dat$CName %in% rr]$site)
      rare_cams <- bind_rows(rare_cams, data.frame('sp' = rr, 'cams' = sp_cams))
    }
    table(rare_cams$cams, useNA = 'a') 
      #GRID_05_05A shows up for two species (coyote and Canada jay) -- prioritize coyote
    
  #now choose cameras for each species (in order of priority in case there are conflicting cameras):
    camera_groups <- data.frame('camera' = unique(rare_cams$cams), 'group' = NA)
    dat$group <- rep(NA, nrow(dat))
    
    #--Red fox (because there's only one camera w/ red fox, do this one first)
    rare_cams[rare_cams$sp %in% 'RedFox',]$cams
    camera_groups[camera_groups$camera %in% rare_cams[rare_cams$sp %in% 'RedFox',]$cams,]$group <- 'train' 
    #actually, split by image, not whole camera
      dat[dat$CName %in% 'RedFox',]$full_path
    
    dat$group <- ifelse(dat$full_path %in% c(), 'train', 
                        ifelse(dat$full_name %in% c(), 'val',
                               ifelse(dat$full_name %in% c(), 'test', dat$group)))
    
    #--Marten
    rare_cams[rare_cams$sp %in% 'Marten',]$cams
    #shoot, these don't have site names!!! ugh. but there are 63 marten photos so I'll sort them as best I can and fix it later
    #ended up with 49 train, 7 val, 8 test 
      dat[dat$CName %in% 'Marten',]$full_path
      dat$group <- ifelse(dat$full_path %in% c("data/DUNES/hailey.hester-DUNES_Marten/images/PICT0010.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0011.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0012.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0127.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0128.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0151.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0152.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0153.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0157.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0158.JPG",
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0159.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0181.JPG",                           
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0182.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0199.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0200.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0201.JPG",                           
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0265.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0266.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0267.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0469.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0470.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0471.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0488.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0489.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0490.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0491.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0492.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0496.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0497.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0498.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0502.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0503.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0503.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0504.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0505.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0506.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0507.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0550.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0556.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0557.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0558.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0559.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0560.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0561.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0562.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0563.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0564.JPG",                           
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0646.JPG",                            
                                               "data/DUNES/hailey.hester-DUNES_Marten/images/PICT0647.JPG"), 'train',
                          ifelse(dat$full_path %in% c("data/DUNES/hailey.hester-DUNES_Marten/images/DUNE15A_IMG_0187.JPG",                   
                                                      "data/DUNES/hailey.hester-DUNES_Marten/images/DUNE15A_IMG_0188.JPG",                    
                                                      "data/DUNES/hailey.hester-DUNES_Marten/images/DUNE15A_IMG_0189.JPG",
                                                      "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/SM-01C_2018-12-11_IMG_0028.JPG",
                                                      "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/SM-PG_2018-01-21_IMG_0286.JPG",
                                                      "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/SM-PG_2018-01-21_IMG_0287.JPG",
                                                      "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/SM-PG_2018-01-21_IMG_0288.JPG"), 'val',
                                 ifelse(dat$full_path %in% c("data/DUNES/hailey.hester-DUNES_Marten/images/DUNE5B_IMG_0460.JPG",
                                                             "data/DUNES/hailey.hester-DUNES_Marten/images/DUNE5B_IMG_0461.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0001.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0002.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0003.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0004.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0005.JPG",
                                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Marten/images/HOBO-337_2018-02-11_IMG_0006.JPG"), 'test', dat$group)))
      
    #--Coyote
    rare_cams[rare_cams$sp %in% 'Coyote',]$cams
    #there are 77 here and they also don't all have site/camera names, but many do
    dat[dat$CName %in% 'Coyote',]$full_path
    #choose manually...
    dat$group <- ifelse(dat$full_path %in% c("data/COA_2020/hailey.hester-COA_2020_Bobcat_HH/images/Drive_02_04040037.JPG",                         
                                             "data/COA_2020/hailey.hester-COA_2020_Bobcat_HH/images/Drive_02_04040038.JPG",                         
                                             "data/COA_2020/hailey.hester-COA_2020_Bobcat_HH/images/Drive_02_04040039.JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/20577-4__20577-4-T__2021-03-21__20-17-52(2).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/20577-4__20577-4-T__2021-03-21__20-17-53(3).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/20577-4__20577-4-T__2021-04-03__18-19-29(1).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/20577-4__20577-4-T__2021-04-03__18-19-30(2).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/20577-4__20577-4-T__2021-04-03__18-19-31(3).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/21994-4__21994-4-G__2021-04-26__07-05-08(1).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/21994-4__21994-4-T__2021-04-26__07-04-11(1).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/21994-4__21994-4-T__2021-04-26__07-04-12(2).JPG",
                                             "data/COA_2021/cappel_COA-COA_2021_Coyote_1_AM/images/21994-4__21994-4-T__2021-04-26__07-04-13(3).JPG",
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/DUNE5B_IMG_0541.JPG",                                    
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/DUNE5B_IMG_0542.JPG",                                    
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/DUNE5B_IMG_0543.JPG",                                    
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/DUNE5B_IMG_0544.JPG",                                    
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0229.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0230.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0231.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0631.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0632.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0633.JPG",                                           
                                             "data/DUNES/hailey.hester-DUNES_Coyote/images/PICT0634.JPG",                   
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0049.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0050.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0051.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0052.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0053.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0054.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0055.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0056.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0057.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0058.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0059.JPG",                             
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Coyote/images/19006D_bait_0060.JPG",
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0076.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0077.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0078.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0079.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0080.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0081.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0082.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0083.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19006D_trail_0084.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0040.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0041.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0042.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0042.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0322.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0322.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0323.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0323.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0325.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0326.JPG",                          
                                             "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/19102E_trail_0327.JPG"), 'train',
                        ifelse(dat$full_path %in% c("data/COA_2019/hailey.hester-COA2019_Coyote/images/10022-4__2019-07-30__08-43-18(1).JPG",              
                                                    "data/COA_2019/hailey.hester-COA2019_Coyote/images/10022-4__2019-07-30__08-43-19(2).JPG",              
                                                    "data/COA_2019/hailey.hester-COA2019_Coyote/images/10022-4__2019-07-30__08-43-19(3).JPG",              
                                                    "data/COA_2019/hailey.hester-COA2019_Coyote/images/10886-3__2019-07-16__10-38-54(1).JPG",              
                                                    "data/COA_2019/hailey.hester-COA2019_Coyote/images/10886-3__2019-07-16__10-39-04(1).JPG",
                                                    "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/IMG_0010.JPG",                                   
                                                    "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/IMG_0011.JPG",                                   
                                                    "data/ESF_TRAIL/hailey.hester-ESF_TRAIL_Coyote/images/IMG_0012.JPG",
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/Hobo-063_2018-07-01_IMG_0247.JPG",               
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/Hobo-063_2018-07-01_IMG_0248.JPG",               
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/Hobo-063_2018-07-01_IMG_0249.JPG",               
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/Hobo-273_2017-07-13_IMG_0007.JPG",               
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/Hobo-273_2017-07-13_IMG_0008.JPG"), 'val',
                               ifelse(dat$full_path %in% c("data/HJA_GRID/hailey.hester-HJA_GRID_Coyote/images/GRID_05_05A__2020-01-29__06-30-13(1).JPG",         
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-08-12_08120301.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-08-12_08120302.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-08-12_08120303.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060040.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060041.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060042.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060044.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060045.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060046.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060047.JPG",                 
                                                           "data/HJA_MARIE/hailey.hester-HJA_MARIE_Coyote/images/159408_2018-12-06_12060048.JPG"), 'test', dat$group)))
    
    
    #--Mink
    rare_cams[rare_cams$sp %in% 'Mink',]$cams
    #there are 13 here and they also don't all have site/camera names, but many do
    dat[dat$CName %in% 'Mink',]$full_path
    #choose manually...
    dat$group <- ifelse(dat$full_path %in% c("data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2017-08-16_IMG_0013.JPG",          
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2017-08-16_IMG_0014.JPG",          
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2017-08-16_IMG_0015.JPG",          
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2018-09-27_IMG_0052.JPG",          
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2018-09-27_IMG_0053.JPG",          
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/Hobo-347_2018-09-27_IMG_0054.JPG"), 'train',
                        ifelse(dat$full_path %in% c("data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/HOBO-016_2017-12-24_IMG_0004.JPG",          
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/HOBO-211_2018-01-23_IMG_0217.JPG",          
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/HOBO-211_2018-01-23_IMG_0218.JPG",          
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Mink/images/HOBO-211_2018-01-23_IMG_0219.JPG"), 'val',
                               ifelse(dat$full_path %in% c("data/COA_2021/cappel-COA_2021_Mink1_ZF/images/21414-2__21414-2-G__2021-03-30__15-49-39(1).JPG",
                                                           "data/COA_2021/cappel-COA_2021_Mink1_ZF/images/21414-2__21414-2-G__2021-03-30__15-49-39(2).JPG",
                                                           "data/COA_2021/cappel-COA_2021_Mink1_ZF/images/21414-2__21414-2-G__2021-03-30__15-49-40(3).JPG"), 'test', dat$group)))
    #6 train/4 test/3 val
    
    #--Owl
    rare_cams[rare_cams$sp %in% 'Owl',]$cams
    #there are 17 here and they also don't all have site/camera names, but many do
    dat[dat$CName %in% 'Owl',]$full_path
    dat$group <- ifelse(dat$full_path %in% c("data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/21106-4__21106-4-T__2021-05-13__05-37-43(1).JPG",
                                             "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/21106-4__21106-4-T__2021-05-13__05-37-43(2).JPG",
                                             "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/21106-4__21106-4-T__2021-05-13__05-37-44(3).JPG",
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/18909B_bait_0028.JPG",                           
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/18909B_bait_0029.JPG",                           
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/18909B_bait_0030.JPG",                           
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/19008A_bait_0049.JPG",                           
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/19008A_bait_0050.JPG",                           
                                             "data/ESF_BAIT/hailey.hester-ESF_BAIT_Owl/images/19008A_bait_0051.JPG",                           
                                             "data/HJA_MARIE/hailey.hester-HJA_MARIE_Owl/images/128101_2018-08-23_08230109.JPG",               
                                            "data/HJA_MARIE/hailey.hester-HJA_MARIE_Owl/images/128101_2018-08-23_08230110.JPG"  ), 'train',
                        ifelse(dat$full_path %in% c("data/HJA_MARIE/hailey.hester-HJA_MARIE_Owl/images/HOBO-016_2018-01-09_IMG_0016.JPG",             
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Owl/images/HOBO-016_2018-01-09_IMG_0017.JPG",             
                                                    "data/HJA_MARIE/hailey.hester-HJA_MARIE_Owl/images/HOBO-016_2018-01-09_IMG_0018.JPG"), 'val',
                               ifelse(dat$full_path %in% c("data/ORSNAP/hailey.hester-ORSNAP_Owl/images/ORSNAP04__2021-10-09__15-51-53(1).JPG",             
                                                           "data/ORSNAP/hailey.hester-ORSNAP_Owl/images/ORSNAP04__2021-10-09__15-51-54(2).JPG",              
                                                           "data/ORSNAP/hailey.hester-ORSNAP_Owl/images/ORSNAP04__2021-10-09__15-51-55(3).JPG"), 'test', dat$group)))
    #11/3/3
    
    #--Bat
    rare_cams[rare_cams$sp %in% 'Bat',]$cams
      #how many photos per camera?
      (bat_cams <- aggregate(dat[dat$CName %in% 'Bat'], full_path ~ site, function(x) unique(length(x))))
      rare_cams$n_photos <- bat_cams$full_path[match(c(rare_cams$cams), bat_cams$site)]
      #make choices
      # camera_groups$group <- ifelse(camera_groups$camera %in% '8749-3', 'train',
      #                           ifelse(camera_groups$camera %in% '9475-5', 'val',
      #                                  ifelse(camera_groups$camera %in% c('10316-5','10831-1'), 'test', camera_groups$group)))
      dat$group <- ifelse(dat$site %in% c('8749-3'), 'train',
                          ifelse(dat$site %in% c('9475-5'), 'val',
                                 ifelse(dat$site %in% c('10316-5','10831-1'), 'test', dat$group)))
      
    #--Pileated woodpecker
      rare_cams[rare_cams$sp %in% 'PileatedWoodpecker',]$cams
      (pw_cams <- aggregate(dat[dat$CName %in% 'PileatedWoodpecker'], full_path ~ site, function(x) unique(length(x))))
      dat[dat$CName %in% 'PileatedWoodpecker',]$full_path
      dat$group <- ifelse(dat$full_path %in% c("data/COA_2019/hailey.hester-COA2019_PileatedWoodpecker/images/8393-4__2019-07-28__07-02-59(1).JPG",          
                                               "data/COA_2019/hailey.hester-COA2019_PileatedWoodpecker/images/8393-4__2019-07-28__07-03-00(1).JPG",          
                                               "data/COA_2019/hailey.hester-COA2019_PileatedWoodpecker/images/8393-4__2019-07-28__07-03-01(2).JPG"), 'train',
                          ifelse(dat$full_path %in% c("data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1).JPG",
                                                      "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2).JPG",
                                                      "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3).JPG"), 'val',
                                 ifelse(dat$full_path %in% c("data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1).JPG",                 
                                                             "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2).JPG",                 
                                                             "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3).JPG"), 'test', dat$group)))
      
    #--Canada Jay
      rare_cams[rare_cams$sp %in% 'CanadaJay',]$cams
      #don't move GRID_05_05A because I used those for coyote
      #how many photos per camera?
      (cj_cams <- aggregate(dat[dat$CName %in% 'CanadaJay'], full_path ~ site, function(x) unique(length(x))))
      rare_cams$n_photos <- cj_cams$full_path[match(c(rare_cams$cams), cj_cams$site)]
      #make choices
      # camera_groups$group <- ifelse(camera_groups %in% c('9426-2','21808-3-G','21911-2-G','7619-3'), 'train',
      #                           ifelse(camera_groups %in% c('7762-4','9476-1'), 'val',
      #                                  ifelse(camera_groups %in% c('GRID_02_02_2020','GRID_02_02_2021'), 'test', camera_groups$group)))
      dat$group <- ifelse(dat$site %in% c('9426-2','21808-3-G','21911-2-G','7619-3'), 'train',
                          ifelse(dat$site %in% c('7762-4','9476-1'), 'val',
                                 ifelse(dat$site %in% c('GRID_02_02_2020','GRID_02_02_2021'), 'test', dat$group)))
      #actually I can use this one bc didn't include this whole folder in coyote like i should have (change when all have site names)
      dat$group <- ifelse(dat$full_path %in% 'data/HJA_GRID/hailey.hester-HJA_GRID_CanadaJay/images/GRID_05_05A__2019-09-28__16-38-19(1).JPG', 'train',dat$group)

      
      ## Did I get them all?
      # View(dat[dat$CName %in% rare_sp,]) 
      nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
      
      
## ** Separate images already assigned from those that are left **
  
    dat_rare <- dat[!is.na(dat$group),]
    dat_rest <- dat[is.na(dat$group),]
    
    
## 2. From the remaining images, now work by camera. Select randomly 75% / 15% / 15% split

    #how many do we want in each partition?
    length(unique(dat_rest$full_path))         #10,245 imagse left to assign
    length(unique(dat_rest$full_path)) * 0.70  #7173 in train
    length(unique(dat_rest$full_path)) * 0.15  #1536 each in validation and test
    
    #ok, not all of these have site names. how many do?
    dat_rest_cams <- unique(dat_rest[,c('full_path','site')]) #10,245
    nrow(dat_rest_cams[!is.na(dat_rest_cams$site),]) #4,083 *do* have site
    nrow(dat_rest_cams[is.na(dat_rest_cams$site),])  #6,207 do not have site
    
      #yeah, these will be hard.
      #first, I'll split by site (75, 15, 15)
      #then, because sites have uneven numbers of images..
        #I'll randomly allocate the remaining images that don't have a site to fill out the rest.
        #separate them...
        dat_rest_site   <- dat_rest[!is.na(dat_rest$site),]
        dat_rest_nosite <- dat_rest[is.na(dat_rest$site),]
    
    ## 2-1: Start with the ones that *do* have site/camera name:
    #how many cameras do we have to assign?
      dat_rest_site_cams <- data.frame('site' = unique(dat_rest_site$site))
      length(unique(dat_rest_site_cams$site))          #614 cameras to assign
      length(unique(dat_rest_site_cams$site)) * 0.70   #~430 in training
      length(unique(dat_rest_site_cams$site)) * 0.15   #~92 each in validation and test
      
    #random draw
      set.seed(42)
      
      #split training
      rand1a <- sample(dat_rest_site_cams$site, size = 430)
        dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1a, 'train', NA)

      #from rest, split val/test
      rand1b <- sample(dat_rest_site_cams[!dat_rest_site_cams$rand1 %in% 'train',]$site, size = 92)  
        dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1b, 'val', dat_rest_site_cams$rand1)
        dat_rest_site_cams$rand1 <- ifelse(is.na(dat_rest_site_cams$rand1), 'test', dat_rest_site_cams$rand1)
      
      #did we do it right?
      table(dat_rest_site_cams$rand1, useNA = 'a') #yes
      
      #how many *images* per group?
      dat_rest_site$group <- dat_rest_site_cams$rand1[match(dat_rest_site$site, dat_rest_site_cams$site)]
        table(dat_rest_site$group, useNA = 'a')
          #actually this is really close! 69 / 16 / 15

    #2-2 Now do the ones that *don't* have camera name
      #how many images do we have to assign?
        dat_rest_nosite_imgs <- data.frame('image' = unique(dat_rest_nosite$full_path))
        length(unique(dat_rest_nosite_imgs$image))          #6207 images to assign
        length(unique(dat_rest_nosite_imgs$image)) * 0.70   #4345 in training
        length(unique(dat_rest_nosite_imgs$image)) * 0.15   #931 each in validation and test
        
      #random draw
        set.seed(42)
        
      #split training
        rand1c <- sample(dat_rest_nosite_imgs$image, size = 4345)
        dat_rest_nosite_imgs$rand1 <- ifelse(dat_rest_nosite_imgs$image %in% rand1c, 'train', NA)
        
      #from rest, split val/test
        rand1d <- sample(dat_rest_nosite_imgs[!dat_rest_nosite_imgs$rand1 %in% 'train',]$image, size = 931)
        dat_rest_nosite_imgs$rand1 <- ifelse(dat_rest_nosite_imgs$image %in% rand1d, 'val', dat_rest_nosite_imgs$rand1)
        dat_rest_nosite_imgs$rand1 <- ifelse(is.na(dat_rest_nosite_imgs$rand1), 'test', dat_rest_nosite_imgs$rand1)
        
      #did we do it right? (this *is* the number of images)
        table(dat_rest_nosite_imgs$rand1, useNA = 'a') #yes
  
      #merge with full columns
        dat_rest_nosite$group <- dat_rest_nosite_imgs$rand1[match(dat_rest_nosite$full_path, dat_rest_nosite_imgs$image)]
        table(dat_rest_nosite$group, useNA = 'a')
          #not exact bc rows are boxes, not images
        
        
    #2-3 Combine them back together (rest_site + rest_nosite)
        head(dat_rest_site[,c('full_path','site','group')]); nrow(dat_rest_site)
        head(dat_rest_nosite[,c('full_path','site','group')]); nrow(dat_rest_nosite)
        
      dat_rest_groups <- bind_rows(dat_rest_site, dat_rest_nosite)
      table(dat_rest_groups$group, useNA = 'a')
        
      
## ** Now combine rare + rest    
    
    dat_groups <- bind_rows(dat_rare, dat_rest_groups)    
      nrow(dat_groups)
      length(unique(dat_groups$full_path))  #good, we have them all back (10,510)
      
      
## 3. See how we did with our split!
      
    #By number of images  
    (unq <- dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path))))
        #good! 70 train / 15 val / 15 test
      
    #By images per species
    (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos), y = nPhotos, fill = group)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p1
    p1a #all 7 red fox images are from the same sequence so don't really want to split it up
    
    ggsave('figures/dataset_images_species_split.png', 
           units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    
    
    #By images per species per study
    (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid( ~group)
    p3
    p3a
    
    ggsave('/Users/caraappel/Desktop/Mangochi/mangochi_points.png', 
           units="in", width=7, height=5, dpi=600, bg = 'transparent')
    

    #By images per species per night/day
    (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
    p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p4  
    p4a
    
    #species/camtype
    (species_cam_type_g <- dat_groups %>% group_by(CName, cam_type, group) %>% summarise('nPhotos' = n()))
    p5a <- ggplot(species_cam_type_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p5
    p5a  
  
          
  #save!
  write.csv(dat_groups, 'metadata_labels/sampled_ds_300_split.csv')
    write.csv(dat_groups[dat_groups$group %in% 'train',], 'metadata_labels/sampled_ds_300_train.csv')
    write.csv(dat_groups[dat_groups$group %in% 'val',], 'metadata_labels/sampled_ds_300_val.csv')
    write.csv(dat_groups[dat_groups$group %in% 'test',], 'metadata_labels/sampled_ds_300_test.csv')
    
    
  #save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path, col.names = FALSE, row.names = FALSE,
              file = 'metadata_labels/sampled_ds_300_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path, col.names = FALSE, row.names = FALSE,
              file = 'metadata_labels/sampled_ds_300_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path, col.names = FALSE, row.names = FALSE,
              file = 'metadata_labels/sampled_ds_300_test.txt')
  
    
    