## Split CV4E dataset

library(data.table)
library(dplyr)
library(ggplot2)


## Read in metadata and labels for my dataset ----------------------------------

  #do separately for both, ground, trail
  dat <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_both_350.csv'); dat <- dat[,-1]
    head(dat)
    length(unique(dat$full_path))
    
    
## View summaries of datasets --------------------------------------------------
    
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
    table(dat$cam_type, useNA = 'a')
    p5
    

## SPLIT (BOTH A) --------------------------------------------------------------
    
## GOAL: 75% training, 15% validation, 15% test    
    
## 1. ensure rare species end up in all sets

  #set a threshold and find the rare species (this will be diff. for both/ground/trail)
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
    #camera_groups <- data.frame('camera' = unique(rare_cams$cams), 'group' = NA)
    dat$group <- rep(NA, nrow(dat))
    
    rare_sp
    
    #--Red fox 
    rare_cams[rare_cams$sp %in% 'RedFox',]$cams  #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'RedFox', 'train', dat$group)
      
    #--Marten
    rare_cams[rare_cams$sp %in% 'Marten',]$cams
    #shoot, these don't have site names!!! ugh. but there are 64 marten photos so I'll sort them as best I can and fix it later
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
      #rare_cams$n_photos <- bat_cams$full_path[match(c(rare_cams$cams), bat_cams$site)]
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

     
    #--Woodpecker
      rare_cams[rare_cams$sp %in% 'Woodpecker',]$cams
      dat[dat$CName %in% 'Woodpecker',]$full_path
      dat$group <- ifelse(dat$full_path %in% c("data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-12__14-30-39(1).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-12__14-30-40(2).JPG",    
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-12__14-30-41(3).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-19__09-53-50(1).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-19__09-53-51(2).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10171-4__2019-08-19__09-53-52(3).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10316-3__2019-07-21__19-27-37(2).JPG",
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10316-3__2019-07-21__19-27-38(3).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-2__2019-07-10__19-04-37(2).JPG",
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-2__2019-07-10__19-04-38(3).JPG",
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-17__09-21-30(1).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-17__09-21-31(2).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-17__09-21-32(3).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-18__07-15-36(1).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-18__07-15-37(2).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/10460-3__2019-07-18__07-15-38(3).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/11419-3__2019-09-03__12-13-49(1).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/11419-3__2019-09-03__12-13-51(2).JPG",     
                                               "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/11419-3__2019-09-03__12-13-52(3).JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-54(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-55(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-56(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-56(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-57(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-58(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-15(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-16(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-17(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-02(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-43-55(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-19(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-20(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-21(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-12(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-13(2).JPG",   
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-14(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-46(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-47(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-48(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-31(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-24(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-42(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-43(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-08__07-26-06(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-11(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-15(3).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22371-4__22371-4-T__2021-05-08__06-58-59(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22371-4__22371-4-T__2021-05-08__06-59-00(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/22371-4__22371-4-T__2021-05-08__06-59-01(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(1).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(2).JPG",             
                                               "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-58(3).JPG"), 'train',
                          ifelse(dat$full_path %in% c("data/COA_2019/hailey.hester-COA2019_Woodpecker/images/7619-4__2019-07-31__15-07-22(1).JPG",     
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/7619-4__2019-07-31__15-07-23(2).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/7619-4__2019-07-31__15-07-24(3).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8494-1__2019-07-19__16-54-32(1).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8494-1__2019-07-19__16-54-32(2).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8494-1__2019-07-19__16-54-33(3).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8644-3__2019-09-17__09-22-14(2).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8907-3__2019-08-06__08-56-40(1).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8907-3__2019-08-06__08-56-41(2).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/8907-3__2019-08-06__08-56-42(3).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/9927-2__2019-07-30__10-04-33(1).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/9927-2__2019-07-30__10-04-34(2).JPG",      
                                                      "data/COA_2019/hailey.hester-COA2019_Woodpecker/images/9927-2__2019-07-30__10-04-35(3).JPG"), 'val',
                                 ifelse(dat$full_path %in% c("data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-32-32(1).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-32-33(2).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-32-34(3).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-34-49(1).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-34-50(2).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2019-09-26__10-34-51(3).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2020-06-08__14-02-28(1).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2020-06-08__14-02-29(2).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05B__2020-06-08__14-02-30(3).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-32(1).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-33(2).JPG",
                                                             "data/HJA_GRID/hailey.hester-HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-34(3).JPG",
                                                             "data/HJA_GRID/HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-32(1).JPG",              
                                                             "data/HJA_GRID/HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-33(2).JPG",              
                                                             "data/HJA_GRID/HJA_GRID_Woodpecker/images/GRID_05_05C__2021-10-31__08-14-34(3).JPG"), 'test', dat$group)))
      
    #--Herp
      rare_cams[rare_cams$sp %in% 'Herp',]$cams
      dat[dat$CName %in% 'Herp',]$full_path
      dat$group <- ifelse(dat$full_path %in% c("data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_0016.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_0017.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19693.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19694.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19695.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19696.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19697.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19698.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19699.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19700.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19701.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19702.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19703.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19704.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19705.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19706.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19707.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19708.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19709.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19710.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19711.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19712.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19713.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19714.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19715.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19716.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19717.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19718.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19719.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19720.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19721.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19722.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19723.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19724.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19725.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19726.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19727.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_19728.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3733.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3734.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3735.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3736.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3737.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3738.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3739.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3740.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3741.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3742.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3743.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3744.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3745.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3746.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3747.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3748.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3749.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3750.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3751.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3752.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3753.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3754.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3755.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3756.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3757.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3758.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3759.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3760.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3761.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3762.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3763.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3764.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3765.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3766.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3767.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3768.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3769.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3770.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3771.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3772.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3773.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3774.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3775.JPG", "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3776.JPG", 
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Herp/images/18909H_small_3777.JPG"), 'train',
                          ifelse(dat$full_path %in% c("data/COA_2019/hailey.hester-COA2019_Herp/images/8397-1__2019-06-04__17-43-36(1).JPG",
                                                      "data/COA_2019/hailey.hester-COA2019_Herp/images/8397-1__2019-06-04__17-43-36(1).JPG",
                                                      "data/COA_2019/hailey.hester-COA2019_Herp/images/8397-1__2019-06-04__17-43-37(2).JPG",
                                                      "data/COA_2019/hailey.hester-COA2019_Herp/images/8397-1__2019-06-04__17-43-37(2).JPG"), 'val', dat$group))

      
       
      ## Did I get them all?
      # View(dat[dat$CName %in% rare_sp,]) 
      nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
      
      
## ** Separate images already assigned from those that are left **
  
    dat_rare <- dat[!is.na(dat$group),]; nrow(dat_rare)
    dat_rest <- dat[is.na(dat$group),]; nrow(dat_rest)
    
    
## 2. From the remaining images, now work by camera. Select randomly 75% / 15% / 15% split

    #how many do we want in each partition?
    length(unique(dat_rest$full_path))         #10,314 imagse left to assign
    length(unique(dat_rest$full_path)) * 0.70  #~7219 in train
    length(unique(dat_rest$full_path)) * 0.15  #~1547 each in validation and test
    
    #ok, not all of these have site names. how many do?
    dat_rest_cams <- unique(dat_rest[,c('full_path','site')]) #10,245
    nrow(dat_rest_cams[!is.na(dat_rest_cams$site),]) #7778 *do* have site
    nrow(dat_rest_cams[is.na(dat_rest_cams$site),])  #2536 do not have site
    
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
      length(unique(dat_rest_site_cams$site))          #707 cameras to assign
      length(unique(dat_rest_site_cams$site)) * 0.70   #~494 in training
      length(unique(dat_rest_site_cams$site)) * 0.15   #~106 each in validation and test
      
    #random draw 
      set.seed(42) #keep at 42
      
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
          #actually this is close ish. 64/12/24

    #2-2 Now do the ones that *don't* have camera name
      #how many images do we have to assign?
        dat_rest_nosite_imgs <- data.frame('image' = unique(dat_rest_nosite$full_path))
        length(unique(dat_rest_nosite_imgs$image))          #2536 images to assign
        (ts <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.70 ))  #1775 in training
        (tv <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.15))   #380 each in validation and test
        
      #random draw
        set.seed(42)
        
      #split training
        rand1c <- sample(dat_rest_nosite_imgs$image, size = ts) #size=training above
        dat_rest_nosite_imgs$rand1 <- ifelse(dat_rest_nosite_imgs$image %in% rand1c, 'train', NA)
        
      #from rest, split val/test
        rand1d <- sample(dat_rest_nosite_imgs[!dat_rest_nosite_imgs$rand1 %in% 'train',]$image, size = tv)
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
      length(unique(dat_groups$full_path))  #do we have them all back?


## 3. See how we did with our split!

    #By number of images  
    (unq <- dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path))))
        #66/13/21

    #sort levels
      dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
      
    #By images per species
    (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos), y = nPhotos, fill = group)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p1
    ggsave('figures/COA_species_bothA_imgs_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p1a #all 7 red fox images are from the same sequence so don't really want to split it up
    ggsave('figures/COA_species_bothA_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    #By images per species per study
    (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid( ~group)
    p3
    ggsave('figures/COA_species_bothA_study_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p3a
    ggsave('figures/COA_species_bothA_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    

    #By images per species per night/day
    (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
    p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p4
    ggsave('figures/COA_species_bothA_daynight_all.jpeg', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p4a
    ggsave('figures/COA_species_bothA_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    #species/camtype
    (species_cam_type_g <- dat_groups %>% group_by(CName, cam_type, group) %>% summarise('nPhotos' = n()))
    p5a <- ggplot(species_cam_type_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p5
    ggsave('figures/COA_species_bothA_camtype_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p5a  
    ggsave('figures/COA_species_bothA_camtype_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
          
  #save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_bothA_350_split.csv')
    write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_300_train.csv')
    write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_300_val.csv')
    write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_300_test.csv')
    
    
  #save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_bothA_350_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_bothA_350_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_bothA_350_test.txt')
  
    
  
################################################################################

## GROUND BATCH  ---------------------------------------------------------------
  
  ### Read in metadata and labels for my dataset 
  
  dat <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620.csv'); dat <- dat[,-1]
  head(dat)
  length(unique(dat$full_path))
  
  
  ## View summaries of datasets 
  
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
  table(dat$cam_type, useNA = 'a')
  p5
  
  
## SPLIT (GROUND) ------------------------------------------------------------
  
  ## GOAL: 75% training, 15% validation, 15% test    
  
  ## 1. ensure rare species end up in all sets
  
  #set a threshold and find the rare species (this will be diff. for both/ground/trail)
  rare_thresh = 50
  (rare_sp <- species_totals[species_totals$nPhotos <= rare_thresh,]$CName)
  
  #find the cameras with photos of these species
  rare_cams <- data.frame()
  for (rr in rare_sp){
    sp_cams <- unique(dat[dat$CName %in% rr]$site)
    rare_cams <- bind_rows(rare_cams, data.frame('sp' = rr, 'cams' = sp_cams))
  }
  table(rare_cams$cams, useNA = 'a') 
  #a couple sites with >1
  
  #now choose cameras for each species (in order of priority in case there are conflicting cameras):
  #camera_groups <- data.frame('camera' = unique(rare_cams$cams), 'group' = NA)
  dat$group <- rep(NA, nrow(dat))
  
  rare_sp
  
  #--Coyote
  dat[dat$sp %in% 'Coyote',]$full_path_new #only 1 photo. put in 'train'
  dat$group <- ifelse(dat$CName %in% 'Coyote', 'train', dat$group)
  
  #--Mink 
  dat[dat$sp %in% 'Mink',]$full_path_new #only 1 sequence put in 'train'
  dat$group <- ifelse(dat$CName %in% 'Mink', 'train', dat$group)
  
  #--Striped skunk
  dat[dat$sp %in% 'StripedSkunk',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-49(1)_fq.JPG",
                                               "data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-50(2)_fq.JPG",
                                               "data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-50(3)_fq.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/22205-2__22205-2-G__2021-05-14__21-07-44(1)_fq.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/21620-3__21620-3-G__2021-06-10__04-06-27(1)_fq.JPG"), 'test' ,dat$group)))
  
  #--Cougar
  dat[dat$sp %in% 'Cougar',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-54(1)_do.JPG",
                                               "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-55(2)_do.JPG",
                                               "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-55(3)_do.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/21189-4__21189-4-G__2021-04-11__08-26-11(1)_do.JPG",
                                                      "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/21189-4__21189-4-G__2021-04-11__08-26-12(2)_do.JPG"), 'val', dat$group))
  
  #--CanadaJay
  dat[dat$sp %in% 'CanadaJay',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-26(1)_fm.JPG",
                                               "data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-27(2)_fm.JPG",
                                               "data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-28(3)_fm.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/CanadaJay_1AM/images/21808-3__21808-3-G__2021-05-10__08-24-08(1)_fm.JPG",
                                                      "data/COA_2021/CanadaJay_1AM/images/21808-3__21808-3-G__2021-05-10__08-24-08(2)_fm.JPG"), 'val', dat$group))
  
  #--Warbler
  dat[dat$sp %in% 'Warbler',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-03(1)_fz.JPG",
                                               "data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-03(2)_fz.JPG",
                                               "data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-04(3)_fz.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-15(1)_eu.JPG",
                                                      "data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-17(2)_eu.JPG",
                                                      "data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-18(3)_eu.JPG"), 'val', dat$group))
  #--PileatedWoodpecker
  dat[dat$sp %in% 'PileatedWoodpecker',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1)_eb.JPG",
                                               "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2)_eb.JPG",
                                               "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3)_eb.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1)_dz.JPG",
                                                      "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2)_dz.JPG",
                                                      "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3)_dz.JPG"), 'val', dat$group))
  
  #--Human
  dat[dat$CName %in% 'Human',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0064_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0064_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0065_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0065_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0066_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0067_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0068_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0069_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0069_cv.JPG"), 'train',
                      ifelse(dat$full_path_new %in% c("data/COA_2021/FlyingSquirrel_1AM/images/21911-4__21911-4-G__2021-04-07__10-54-45(1)_gp.JPG"), 'val' ,dat$group))

  #--Opossum
  dat[dat$sp %in% 'Opossum',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21189-4__21189-4-G__2021-04-26__03-07-40(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21189-4__21189-4-G__2021-04-26__03-07-40(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-55(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-56(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-56(3)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/22579-1__22579-1-G__2021-03-13__19-19-41(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-07(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-08(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-09(3)_fc.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21398-3__21398-3-G__2021-05-14__23-22-10(1)_fc.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21398-3__21398-3-G__2021-05-14__23-22-10(2)_fc.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21896-4__21896-4-G__2021-04-18__00-39-43(1)_fc.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21896-4__21896-4-G__2021-04-18__00-39-44(2)_fc.JPG"), 'test', dat$group)))

  #--Weasel
  dat[dat$sp %in% 'Weasel',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/21696-4__21696-4-G__2021-04-09__06-25-12(1)_ek.JPG",
                                               "data/COA_2021/Weasel_ZF/images/22195-2__22195-2-G__2021-03-19__01-07-09(1)_ek.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0151_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0152_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0153_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0154_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0155_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0156_cy.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-05__01-55-33(1)_ek.JPG",
                                                      "data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-05__01-55-34(2)_ek.JPG",
                                                      "data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-08__10-34-06(1)_ek.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-58(1)_ek.JPG",
                                                             "data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-59(2)_ek.JPG",
                                                             "data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-59(3)_ek.JPG"), 'test', dat$group)))

  #--Chickadee
  dat[dat$sp %in% 'Chickadee',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-16(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-16(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-17(3)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-4__20274-4-G__2021-04-11__12-29-51(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-4__20274-4-G__2021-04-11__12-29-52(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-12(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-13(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-13(3)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/22565-2__22565-2-G__2021-05-22__14-16-33(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/22565-2__22565-2-G__2021-05-22__14-16-33(2)_gt.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-14(1)_ds.JPG",
                                                      "data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-15(2)_ds.JPG",
                                                      "data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-15(3)_ds.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-33(1)_gt.JPG",
                                                             "data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-34(2)_gt.JPG",
                                                             "data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-35(3)_gt.JPG"), 'test', dat$group)))
  
  #--MountainQuail
  dat[dat$sp %in% 'MountainQuail',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-14-17(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-14-18(2)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-28(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-29(2)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-54(4)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-55(5)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-56(6)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/23042-1__23042-1-G__2021-04-09__15-05-17(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22276-3__22276-3-G__2021-03-31__07-27-07(1)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-33(1)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-34(2)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-35(3)_fp.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-53(1)_ea.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-53(2)_ea.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-54(3)_ea.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-29(1)_fp.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-30(2)_fp.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-31(3)_fp.JPG"), 'test', dat$group)))
  
  #--MountainBeaver
  dat[dat$sp %in% 'MountainBeaver',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-04-24__02-18-59(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-04-30__23-50-30(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-05-06__22-40-48(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-27(4)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-28(5)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-29(6)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(2)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(3)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-15(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-15(2)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-16(3)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23153-4__23153-4-G__2021-04-26__03-18-48(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23153-4__23153-4-G__2021-04-26__03-19-27(1)_ff.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23336-1__23336-1-G__2021-04-30__22-22-17(1)_ff.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21620-1__21620-1-G__2021-05-24__02-18-51(1)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-11(1)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-12(2)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-12(3)_ei.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-04(1)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-05(2)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-06(3)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-27__04-48-31(1)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-27__04-48-32(2)_ei.JPG"), 'test', dat$group)))
  #--GraySquirrel
  dat[dat$sp %in% 'GraySquirrel',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-4__21620-4-G__2021-05-03__11-56-45(1)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-56(1)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-57(2)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-58(3)_ey.JPG"), 'val', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-4__21620-4-G__2021-05-03__11-56-45(1)_ey.JPG"), 'test',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-34(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-34(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-35(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-00(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-01(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-02(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-10(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-10(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-11(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-16(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-17(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-18(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-34(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-34(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-35(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-22(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-23(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-24(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-53(4)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-36(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-37(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-37(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__12-03-05(1)_ey.JPG"), 'train', dat$group)))
  
  #--Grouse
  dat[dat$sp %in% 'Grouse',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-25(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-26(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-27(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-57(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-09(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-10(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-10(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-20(4)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-21(5)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-22(6)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-17__12-45-52(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-17__17-57-39(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-14(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-15(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-15(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-05-12__20-46-40(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-05-13__20-34-56(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-52(1)_hc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-53(2)_hc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-53(3)_hc.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-27(1)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-28(2)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-29(3)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-16(1)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-16(2)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-17(3)_em.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-25(1)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-26(2)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-27(3)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(1)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(2)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-57(3)_em.JPG"), 'test', dat$group)))
  
  #--Woodpecker
  dat[dat$sp %in% 'Woodpecker',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-43-55(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-19(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-20(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-21(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-12(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-13(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-14(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-46(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-47(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-48(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-31(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-24(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-42(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-43(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-08__07-26-06(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-11(3)_fw.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-54(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-55(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-56(3)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-56(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-57(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-58(3)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-15(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-16(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-17(3)_fw.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-02(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(3)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-15(3)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-58(3)_fw.JPG"), 'test', dat$group)))
  
  
  #--RooseveltElk
  dat[dat$sp %in% 'RooseveltElk',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-39(1)_en.JPG", 
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-40(2)_en.JPG", 
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-40(3)_en.JPG", 
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-15(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-15(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-25(4)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-25(5)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-26(6)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-36(7)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-37(8)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-37(9)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-47(10)_ez.JPG",       
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-48(11)_ez.JPG",       
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-11-06(1)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-11-07(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__22-57-10(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__22-57-10(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-21-53(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-21-54(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-22-11(1)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-36-55(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-33(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-33(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-46(4)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-46(5)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-47(6)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/22186-1__22186-1-G__2021-03-20__01-05-38(2)_du.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-40(2)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-41(3)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-50(4)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-08(1)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-09(3)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-57(13)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-57(14)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-59(15)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-53-09(1)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-53-10(3)_du.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-13(1)_du.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-14(2)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-15(3)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-24(4)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-25(5)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-26(6)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-43-53(6)_gd.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-35(1)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-36(2)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-37(3)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-02(1)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-03(2)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-03(3)_gd.JPG" ), 'test', dat$group)))
  
  ## Did I get them all?
  nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
  
  
  ## ** Separate images already assigned from those that are left **
  dat_rare <- dat[!is.na(dat$group),]; length(unique(dat_rare$full_path_new))
  dat_rest <- dat[is.na(dat$group),]; length(unique(dat_rest$full_path_new))
  
  
  ## 2. From the remaining images, now work by camera. Select randomly 75% / 15% / 15% split
  
  #how many do we want in each partition?
  length(unique(dat_rest$full_path))         #images left to assign
  (tr=round(length(unique(dat_rest$full_path)) * 0.70))  # in train
  (vt=round(length(unique(dat_rest$full_path)) * 0.15))  # each in validation and test
  
  #ok, not all of these have site names. how many do?
  dat_rest_cams <- unique(dat_rest[,c('full_path','site')]) #10,245
  nrow(dat_rest_cams[!is.na(dat_rest_cams$site),]) #7778 *do* have site
  nrow(dat_rest_cams[is.na(dat_rest_cams$site),])  #2536 do not have site
  
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
  length(unique(dat_rest_site_cams$site))          #cameras to assign
  (trs=round(length(unique(dat_rest_site_cams$site)) * 0.70))   #in training
  (vts=round(length(unique(dat_rest_site_cams$site)) * 0.15))   #each in validation and test
  
  #random draw 
  set.seed(42) #keep at 42
  
  #split training
  rand1a <- sample(dat_rest_site_cams$site, size = trs)
  dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1a, 'train', NA)
  
  #from rest, split val/test
  rand1b <- sample(dat_rest_site_cams[!dat_rest_site_cams$rand1 %in% 'train',]$site, size = vts)  
  dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1b, 'val', dat_rest_site_cams$rand1)
  dat_rest_site_cams$rand1 <- ifelse(is.na(dat_rest_site_cams$rand1), 'test', dat_rest_site_cams$rand1)
  
  #did we do it right?
  table(dat_rest_site_cams$rand1, useNA = 'a') #yes
  
  #how many *images* per group?
  dat_rest_site$group <- dat_rest_site_cams$rand1[match(dat_rest_site$site, dat_rest_site_cams$site)]
  table(dat_rest_site$group, useNA = 'a')
    #actually this is close ish. 71/12/16
  
  #2-2 Now do the ones that *don't* have camera name
  #how many images do we have to assign?
  dat_rest_nosite_imgs <- data.frame('image' = unique(dat_rest_nosite$full_path))
  length(unique(dat_rest_nosite_imgs$image))          #images to assign
  (trn <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.70 ))  #in training
  (vtn <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.15))   #each in validation and test
  
  #random draw
  set.seed(42)
  
  #split training
  rand1c <- sample(dat_rest_nosite_imgs$image, size = trn) #size=training above
  dat_rest_nosite_imgs$rand1 <- ifelse(dat_rest_nosite_imgs$image %in% rand1c, 'train', NA)
  
  #from rest, split val/test
  rand1d <- sample(dat_rest_nosite_imgs[!dat_rest_nosite_imgs$rand1 %in% 'train',]$image, size = vtn)
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
    length(unique(dat_groups$full_path))  #do we have them all back?
  
  
  ## 3. See how we did with our split!
  
  #By number of images  
  (unq <- dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path))))
    #71/13/16
  
  #sort levels
  dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
  
  #By images per species
  (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos), y = nPhotos, fill = group)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
    p1
    ggsave('figures/COA_species_ground_imgs_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p1a #all 7 red fox images are from the same sequence so don't really want to split it up
    ggsave('figures/COA_species_ground_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
  #By images per species per study
  (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid( ~group)
  p3
  ggsave('figures/COA_species_ground_study_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p3a
  ggsave('figures/COA_species_ground_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    
  
  #By images per species per night/day
  (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
  p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p4
  ggsave('figures/COA_species_ground_daynight_all.jpeg', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p4a
  ggsave('figures/COA_species_ground_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #species/camtype
  (species_cam_type_g <- dat_groups %>% group_by(CName, cam_type, group) %>% summarise('nPhotos' = n()))
  p5a <- ggplot(species_cam_type_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p5
  ggsave('figures/COA_species_ground_camtype_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p5a  
  ggsave('figures/COA_species_ground_camtype_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  
  #save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_split.csv')
  write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_train.csv')
  write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_val.csv')
  write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_test.csv')
  
  
  #save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_test.txt')
  
  
### 
################################################################################
  
## TRAIL BATCH  ---------------------------------------------------------------
  
  ### Read in metadata and labels for my dataset 
  
  dat <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_trail_180.csv'); dat <- dat[,-1]
  head(dat)
  length(unique(dat$full_path_new))
  
  
  ## View summaries of datasets 
  
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
  table(dat$cam_type, useNA = 'a')
  p5
  
  
## SPLIT (TRAIL) ------------------------------------------------------------
  
  ## GOAL: 75% training, 15% validation, 15% test    
  
  ## 1. ensure rare species end up in all sets
  
  #set a threshold and find the rare species (this will be diff. for both/ground/trail)
  rare_thresh = 50
  (rare_sp <- species_totals[species_totals$nPhotos <= rare_thresh,]$CName)
  
  #find the cameras with photos of these species
  rare_cams <- data.frame()
  for (rr in rare_sp){
    sp_cams <- unique(dat[dat$CName %in% rr]$site)
    rare_cams <- bind_rows(rare_cams, data.frame('sp' = rr, 'cams' = sp_cams))
  }
  table(rare_cams$cams, useNA = 'a') 
  #a couple sites with >1
  
  #now choose cameras for each species (in order of priority in case there are conflicting cameras):
  #camera_groups <- data.frame('camera' = unique(rare_cams$cams), 'group' = NA)
  dat$group <- rep(NA, nrow(dat))
  
  rare_sp
  
  #--Coyote
  dat[dat$sp %in% 'Coyote',]$full_path_new #only 1 photo. put in 'train'
  dat$group <- ifelse(dat$CName %in% 'Coyote', 'train', dat$group)
  
  #--Mink 
  dat[dat$sp %in% 'Mink',]$full_path_new #only 1 sequence put in 'train'
  dat$group <- ifelse(dat$CName %in% 'Mink', 'train', dat$group)
  
  #--Striped skunk
  dat[dat$sp %in% 'StripedSkunk',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-49(1)_fq.JPG",
                                               "data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-50(2)_fq.JPG",
                                               "data/COA_2021/StripedSkunk_ZF/images/22394-3__22394-3-G__2021-06-16__11-02-50(3)_fq.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/22205-2__22205-2-G__2021-05-14__21-07-44(1)_fq.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/StripedSkunk_ZF/images/21620-3__21620-3-G__2021-06-10__04-06-27(1)_fq.JPG"), 'test' ,dat$group)))
  
  #--Cougar
  dat[dat$sp %in% 'Cougar',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-54(1)_do.JPG",
                                               "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-55(2)_do.JPG",
                                               "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/20891-1__20891-1-G__2021-04-23__01-59-55(3)_do.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/21189-4__21189-4-G__2021-04-11__08-26-11(1)_do.JPG",
                                                      "data/COA_2021/cappel_COA-COA_2021_Cougar_1_AM/images/21189-4__21189-4-G__2021-04-11__08-26-12(2)_do.JPG"), 'val', dat$group))
  
  #--CanadaJay
  dat[dat$sp %in% 'CanadaJay',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-26(1)_fm.JPG",
                                               "data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-27(2)_fm.JPG",
                                               "data/COA_2021/CanadaJay_1AM/images/21911-2__21911-2-G__2021-04-12__19-15-28(3)_fm.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/CanadaJay_1AM/images/21808-3__21808-3-G__2021-05-10__08-24-08(1)_fm.JPG",
                                                      "data/COA_2021/CanadaJay_1AM/images/21808-3__21808-3-G__2021-05-10__08-24-08(2)_fm.JPG"), 'val', dat$group))
  
  #--Warbler
  dat[dat$sp %in% 'Warbler',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-03(1)_fz.JPG",
                                               "data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-03(2)_fz.JPG",
                                               "data/COA_2021/Warbler_1AM/images/21808-3__21808-3-G__2021-05-05__13-39-04(3)_fz.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-15(1)_eu.JPG",
                                                      "data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-17(2)_eu.JPG",
                                                      "data/COA_2021/Warbler_ZF/images/21800-1__21800-1-G__2021-05-05__11-51-18(3)_eu.JPG"), 'val', dat$group))
  #--PileatedWoodpecker
  dat[dat$sp %in% 'PileatedWoodpecker',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1)_eb.JPG",
                                               "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2)_eb.JPG",
                                               "data/COA_2021/cappel-COA_2021_PileatedWoodpecker1_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3)_eb.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-56(1)_dz.JPG",
                                                      "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(2)_dz.JPG",
                                                      "data/COA_2021/PileatedWoodpecker_ZF/images/22103-2__22103-2-G__2021-04-08__10-41-57(3)_dz.JPG"), 'val', dat$group))
  
  #--Human
  dat[dat$CName %in% 'Human',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0064_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0064_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0065_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0065_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0066_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0067_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0068_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0069_cv.JPG",
                                               "data/ESF_GROUND/ESF_GROUND_Human/images/18909H_small_0069_cv.JPG"), 'train',
                      ifelse(dat$full_path_new %in% c("data/COA_2021/FlyingSquirrel_1AM/images/21911-4__21911-4-G__2021-04-07__10-54-45(1)_gp.JPG"), 'val' ,dat$group))
  
  #--Opossum
  dat[dat$sp %in% 'Opossum',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21189-4__21189-4-G__2021-04-26__03-07-40(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21189-4__21189-4-G__2021-04-26__03-07-40(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-55(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-56(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21294-3__21294-3-G__2021-05-04__21-57-56(3)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/22579-1__22579-1-G__2021-03-13__19-19-41(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-07(1)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-08(2)_fc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/23153-4__23153-4-G__2021-04-18__05-24-09(3)_fc.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21398-3__21398-3-G__2021-05-14__23-22-10(1)_fc.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21398-3__21398-3-G__2021-05-14__23-22-10(2)_fc.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21896-4__21896-4-G__2021-04-18__00-39-43(1)_fc.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Opossum1_AM/images/21896-4__21896-4-G__2021-04-18__00-39-44(2)_fc.JPG"), 'test', dat$group)))
  
  #--Weasel
  dat[dat$sp %in% 'Weasel',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/21696-4__21696-4-G__2021-04-09__06-25-12(1)_ek.JPG",
                                               "data/COA_2021/Weasel_ZF/images/22195-2__22195-2-G__2021-03-19__01-07-09(1)_ek.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0151_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0152_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0153_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0154_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0155_cy.JPG",
                                               "data/ESF_GROUND/hailey.hester-ESF_GROUND_Weasel/images/18907D_small_0156_cy.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-05__01-55-33(1)_ek.JPG",
                                                      "data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-05__01-55-34(2)_ek.JPG",
                                                      "data/COA_2021/Weasel_ZF/images/20779-2__20779-2-G__2021-04-08__10-34-06(1)_ek.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-58(1)_ek.JPG",
                                                             "data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-59(2)_ek.JPG",
                                                             "data/COA_2021/Weasel_ZF/images/22297-1__22297-1-G__2021-04-22__01-50-59(3)_ek.JPG"), 'test', dat$group)))
  
  #--Chickadee
  dat[dat$sp %in% 'Chickadee',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-16(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-16(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-1__20274-1-G__2021-04-21__10-26-17(3)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-4__20274-4-G__2021-04-11__12-29-51(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/20274-4__20274-4-G__2021-04-11__12-29-52(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-12(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-13(2)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/21808-1__21808-1-G__2021-04-24__11-09-13(3)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/22565-2__22565-2-G__2021-05-22__14-16-33(1)_gt.JPG",
                                               "data/COA_2021/Chickadee_1AM/images/22565-2__22565-2-G__2021-05-22__14-16-33(2)_gt.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-14(1)_ds.JPG",
                                                      "data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-15(2)_ds.JPG",
                                                      "data/COA_2021/cappel-COA_2021_ChickadeeSp1_ZF/images/22205-4__22205-4-G__2020-05-03__11-48-15(3)_ds.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-33(1)_gt.JPG",
                                                             "data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-34(2)_gt.JPG",
                                                             "data/COA_2021/Chickadee_1AM/images/22571-1__22571-1-G__2021-04-17__14-43-35(3)_gt.JPG"), 'test', dat$group)))
  
  #--MountainQuail
  dat[dat$sp %in% 'MountainQuail',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-14-17(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-14-18(2)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-28(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-29(2)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-54(4)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-55(5)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/20987-1__20987-1-G__2021-04-03__12-16-56(6)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/23042-1__23042-1-G__2021-04-09__15-05-17(1)_ea.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22276-3__22276-3-G__2021-03-31__07-27-07(1)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-33(1)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-34(2)_fp.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/22297-3__22297-3-G__2021-04-25__12-22-35(3)_fp.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-53(1)_ea.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-53(2)_ea.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainQuail1_AM/images/22289-4__22289-4-G__2021-04-12__11-40-54(3)_ea.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-29(1)_fp.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-30(2)_fp.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainQuail1_ZF/images/20266-4__20266-4-G__2021-04-01__13-39-31(3)_fp.JPG"), 'test', dat$group)))
  
  #--MountainBeaver
  dat[dat$sp %in% 'MountainBeaver',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-04-24__02-18-59(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-04-30__23-50-30(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/20574-2__20574-2-G__2021-05-06__22-40-48(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-27(4)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-28(5)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/21911-1__21911-1-G__2021-04-10__04-44-29(6)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(2)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-03-13__20-50-06(3)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-15(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-15(2)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/22486-4__22486-4-G__2021-04-08__03-57-16(3)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23153-4__23153-4-G__2021-04-26__03-18-48(1)_ff.JPG",
                                               "data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23153-4__23153-4-G__2021-04-26__03-19-27(1)_ff.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_AM/images/23336-1__23336-1-G__2021-04-30__22-22-17(1)_ff.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21620-1__21620-1-G__2021-05-24__02-18-51(1)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-11(1)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-12(2)_ei.JPG",
                                                      "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21800-1__21800-1-G__2021-05-17__22-55-12(3)_ei.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-04(1)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-05(2)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-14__02-08-06(3)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-27__04-48-31(1)_ei.JPG",
                                                             "data/COA_2021/cappel-COA_2021_MountainBeaver1_ZF/images/21997-1__21997-1-G__2021-05-27__04-48-32(2)_ei.JPG"), 'test', dat$group)))
  #--GraySquirrel
  dat[dat$sp %in% 'GraySquirrel',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-4__21620-4-G__2021-05-03__11-56-45(1)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-56(1)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-57(2)_ey.JPG",
                                               "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/22205-3__22205-3-G__2021-05-27__11-39-58(3)_ey.JPG"), 'val', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-4__21620-4-G__2021-05-03__11-56-45(1)_ey.JPG"), 'test',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-34(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-34(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-41-35(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-00(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-01(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-24__12-42-02(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-10(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-10(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-04-27__08-44-11(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-16(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-17(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-01__12-13-18(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-34(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-34(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-17-35(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-22(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-23(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-24(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-05-04__11-21-53(4)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-36(1)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-37(2)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__11-43-37(3)_ey.JPG",
                                                             "data/COA_2021/cappel-COA_2021_GraySquirrel1_ZF/images/21620-3__21620-3-G__2021-06-07__12-03-05(1)_ey.JPG"), 'train', dat$group)))
  
  #--Grouse
  dat[dat$sp %in% 'Grouse',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-25(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-26(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-27(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-57(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-09(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-10(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-10(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-20(4)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-21(5)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-4__23153-4-G__2021-04-05__16-09-22(6)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-17__12-45-52(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-17__17-57-39(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-14(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-15(2)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-04-25__14-16-15(3)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-05-12__20-46-40(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23343-1__23343-1-G__2021-05-13__20-34-56(1)_em.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-52(1)_hc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-53(2)_hc.JPG",
                                               "data/COA_2021/cappel-COA_2021_Grouse1_ZF/images/20266-3__20266-3-G__2021-03-30__18-26-53(3)_hc.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-27(1)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-28(2)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/20274-1__20274-1-G__2021-03-20__09-53-29(3)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-16(1)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-16(2)_em.JPG",
                                                      "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21297-5__21297-5-G__2021-05-14__10-22-17(3)_em.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-25(1)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-26(2)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/21604-1__21604-1-G__2021-05-15__08-17-27(3)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(1)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-56(2)_em.JPG",
                                                             "data/COA_2021/cappel-COA_2021_Grouse1_AM/images/23153-2__23153-2-G__2021-04-17__09-47-57(3)_em.JPG"), 'test', dat$group)))
  
  #--Woodpecker
  dat[dat$sp %in% 'Woodpecker',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-43-55(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-19(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-20(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-44-21(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-12(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-13(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-45-14(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-46(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-47(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-04-25__08-46-48(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-30(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-01__07-51-31(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-24(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__08-34-25(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-42(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-07__18-48-43(3)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-08__07-26-06(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(1)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-10(2)_fw.JPG",
                                               "data/COA_2021/Woodpecker_ZF/images/21620-3__21620-3-G__2021-05-29__12-46-11(3)_fw.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-54(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-55(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-03-26__11-10-56(3)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-56(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-57(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-01__08-36-58(3)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-15(1)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-16(2)_fw.JPG",
                                                      "data/COA_2021/Woodpecker_ZF/images/20266-1__20266-1-G__2021-04-11__09-01-17(3)_fw.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-02(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/21414-2__21414-2-G__2021-03-24__14-12-03(3)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-14(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/22205-2__22205-2-G__2021-05-11__14-39-15(3)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(1)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-57(2)_fw.JPG",
                                                             "data/COA_2021/Woodpecker_ZF/images/23039-1__23039-1-G__2021-05-02__13-17-58(3)_fw.JPG"), 'test', dat$group)))
  
  
  #--RooseveltElk
  dat[dat$sp %in% 'RooseveltElk',]$full_path_new 
  dat$group <- ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-39(1)_en.JPG", 
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-40(2)_en.JPG", 
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk2_ZF/images/22379-4__22379-4-G__2021-03-26__12-05-40(3)_en.JPG", 
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-15(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-15(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-25(4)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-25(5)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-26(6)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-36(7)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-37(8)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-37(9)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-47(10)_ez.JPG",       
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-03-48(11)_ez.JPG",       
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-11-06(1)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__19-11-07(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__22-57-10(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__22-57-10(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-21-53(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-21-54(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-22-11(1)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-05-18__23-36-55(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-33(2)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-33(3)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-46(4)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-46(5)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_RooseveltElk_3/images/22565-2__22565-2-G__2021-06-13__11-03-47(6)_ez.JPG",        
                                               "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/22186-1__22186-1-G__2021-03-20__01-05-38(2)_du.JPG"), 'train', 
                      ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-40(2)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-41(3)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-51-50(4)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-08(1)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-09(3)_du.JPG", 
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-57(13)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-57(14)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-52-59(15)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-53-09(1)_du.JPG",
                                                      "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/20572-4__20572-4-G__2021-05-09__07-53-10(3)_du.JPG"), 'val',
                             ifelse(dat$full_path_new %in% c("data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-13(1)_du.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-14(2)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-15(3)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-24(4)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-25(5)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_AM/images/21896-1__21896-1-G__2021-04-11__19-54-26(6)_du.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-43-53(6)_gd.JPG",
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-35(1)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-36(2)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-45-37(3)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-02(1)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-03(2)_gd.JPG", 
                                                             "data/COA_2021/cappel-COA_2021_RooseveltElk1_ZF/images/22088-4__22088-4-G__2021-03-11__17-46-03(3)_gd.JPG" ), 'test', dat$group)))
  
  ## Did I get them all?
  nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
  
  
  ## ** Separate images already assigned from those that are left **
  dat_rare <- dat[!is.na(dat$group),]; length(unique(dat_rare$full_path_new))
  dat_rest <- dat[is.na(dat$group),]; length(unique(dat_rest$full_path_new))
  
  
  ## 2. From the remaining images, now work by camera. Select randomly 75% / 15% / 15% split
  
  #how many do we want in each partition?
  length(unique(dat_rest$full_path))         #images left to assign
  (tr=round(length(unique(dat_rest$full_path)) * 0.70))  # in train
  (vt=round(length(unique(dat_rest$full_path)) * 0.15))  # each in validation and test
  
  #ok, not all of these have site names. how many do?
  dat_rest_cams <- unique(dat_rest[,c('full_path','site')]) #10,245
  nrow(dat_rest_cams[!is.na(dat_rest_cams$site),]) #7778 *do* have site
  nrow(dat_rest_cams[is.na(dat_rest_cams$site),])  #2536 do not have site
  
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
  length(unique(dat_rest_site_cams$site))          #cameras to assign
  (trs=round(length(unique(dat_rest_site_cams$site)) * 0.70))   #in training
  (vts=round(length(unique(dat_rest_site_cams$site)) * 0.15))   #each in validation and test
  
  #random draw 
  set.seed(42) #keep at 42
  
  #split training
  rand1a <- sample(dat_rest_site_cams$site, size = trs)
  dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1a, 'train', NA)
  
  #from rest, split val/test
  rand1b <- sample(dat_rest_site_cams[!dat_rest_site_cams$rand1 %in% 'train',]$site, size = vts)  
  dat_rest_site_cams$rand1 <- ifelse(dat_rest_site_cams$site %in% rand1b, 'val', dat_rest_site_cams$rand1)
  dat_rest_site_cams$rand1 <- ifelse(is.na(dat_rest_site_cams$rand1), 'test', dat_rest_site_cams$rand1)
  
  #did we do it right?
  table(dat_rest_site_cams$rand1, useNA = 'a') #yes
  
  #how many *images* per group?
  dat_rest_site$group <- dat_rest_site_cams$rand1[match(dat_rest_site$site, dat_rest_site_cams$site)]
  table(dat_rest_site$group, useNA = 'a')
  #actually this is close ish. 71/12/16
  
  #2-2 Now do the ones that *don't* have camera name
  #how many images do we have to assign?
  dat_rest_nosite_imgs <- data.frame('image' = unique(dat_rest_nosite$full_path))
  length(unique(dat_rest_nosite_imgs$image))          #images to assign
  (trn <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.70 ))  #in training
  (vtn <- round(length(unique(dat_rest_nosite_imgs$image)) * 0.15))   #each in validation and test
  
  #random draw
  set.seed(42)
  
  #split training
  rand1c <- sample(dat_rest_nosite_imgs$image, size = trn) #size=training above
  dat_rest_nosite_imgs$rand1 <- ifelse(dat_rest_nosite_imgs$image %in% rand1c, 'train', NA)
  
  #from rest, split val/test
  rand1d <- sample(dat_rest_nosite_imgs[!dat_rest_nosite_imgs$rand1 %in% 'train',]$image, size = vtn)
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
  length(unique(dat_groups$full_path))  #do we have them all back?
  
  
  ## 3. See how we did with our split!
  
  #By number of images  
  (unq <- dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path))))
  #71/13/16
  
  #sort levels
  dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
  
  #By images per species
  (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos), y = nPhotos, fill = group)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p1
  ggsave('figures/COA_species_ground_imgs_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p1a #all 7 red fox images are from the same sequence so don't really want to split it up
  ggsave('figures/COA_species_ground_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #By images per species per study
  (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid( ~group)
  p3
  ggsave('figures/COA_species_ground_study_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p3a
  ggsave('figures/COA_species_ground_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    
  
  #By images per species per night/day
  (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
  p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p4
  ggsave('figures/COA_species_ground_daynight_all.jpeg', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p4a
  ggsave('figures/COA_species_ground_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #species/camtype
  (species_cam_type_g <- dat_groups %>% group_by(CName, cam_type, group) %>% summarise('nPhotos' = n()))
  p5a <- ggplot(species_cam_type_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p5
  ggsave('figures/COA_species_ground_camtype_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p5a  
  ggsave('figures/COA_species_ground_camtype_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  
  #save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_split.csv')
  write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_train.csv')
  write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_val.csv')
  write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_test.csv')
  
  
  #save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path_new, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_test.txt')
  
  
### BOTH B -- can just combine?