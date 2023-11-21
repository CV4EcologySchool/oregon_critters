## Split CV4E dataset (now with full dataset, not prototype sample)

library(data.table)
library(dplyr)
library(ggplot2)


## GROUND BATCH  ---------------------------------------------------------------
  
## Read in metadata and labels
  
  dat <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels_ground.csv'); dat <- dat[,-1]
    head(dat)
    length(unique(dat$full_path))   #30,418 images
    length(unique(dat$sequence_id)) #12,128 sequences
  
    
## View summaries of dataset (by sequence?)
  
  #species totals 
  (species_totals <- dat %>% group_by(CName) %>% summarise('nSequences' = length(unique(sequence_id))))
  (species_totals_photos <- dat %>% group_by(CName) %>% summarise('nPhotos' = length(unique(full_name))))
  p1 <- ggplot(species_totals, aes(reorder(CName, -nSequences, sum), y = nSequences)) + ggtitle('Ground') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p1
  
  #species/camera
  (species_site <- aggregate(dat, stn ~ CName, function(x) length(unique(x))))
  p2 <- ggplot(species_site, aes(x = reorder(CName, -stn, sum), y = stn)) + ggtitle('Ground') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p2
  
  #species/project
  (species_project <- dat %>% group_by(CName, project) %>% summarise('nSequences' = length(unique(sequence_id))))
  p3 <- ggplot(species_project, aes(x = reorder(CName, -nSequences, sum), y = nSequences, fill = project)) + ggtitle('Ground') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p3
  
  #species/daynight
  (species_day_night <- dat %>% group_by(CName, day_night) %>% summarise('nSequences' = n()))
  p4 <- ggplot(species_day_night, aes(x = reorder(CName, -nSequences, sum), y = nSequences, fill = day_night)) + ggtitle('Ground') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p4  
  
  
## Split 
  
  ## GOAL: 75% training, 15% validation, 15% test    
  
  ## 1. ensure rare species end up in all sets
  
  #set a threshold and find the rare species (this will be different for both/ground/trail)
  rare_thresh = 50
  # (rare_sp <- species_totals[species_totals$nSequences <= rare_thresh,]$CName)
  (rare_sp <- species_totals[species_totals_photos$nPhotos <= rare_thresh,]$CName)
    #do I want fewer than 50 photos or fewer than 50 sequences?
    #go with <50 photos. the others will end up OK
  
  #now allocate sequences into train/val/test groups:
  dat$group <- rep(NA, nrow(dat))
  
    #--Coyote
    table(dat[dat$CName %in% 'Coyote',]$sequence_id) #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'Coyote', 'train', dat$group)
    
    #--Mink 
    table(dat[dat$CName %in% 'Mink',]$sequence_id) #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'Mink', 'train', dat$group)
    
    #--Striped skunk
    table(dat[dat$CName %in% 'StripedSkunk',]$sequence_id) #3 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('16869'), 'train',
                        ifelse(dat$sequence_id %in% c('15744'), 'val',
                               ifelse(dat$sequence_id %in% c('13877'), 'test', dat$group)))
    
    #--Cougar
    table(dat[dat$CName %in% 'Cougar',]$sequence_id) #5 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('28','8443'), 'train',
                        ifelse(dat$sequence_id %in% c('27','12733'), 'val',
                               ifelse(dat$sequence_id %in% c('12053'), 'test', dat$group)))
    
    #--CanadaJay
    table(dat[dat$CName %in% 'CanadaJay',]$sequence_id) #2 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('14549'), 'train',
                        ifelse(dat$sequence_id %in% c('14339'), 'val', dat$group))
    
    #--OtherBird
    table(dat[dat$CName %in% 'OtherBird',]$sequence_id) #2 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('2015'), 'train',
                        ifelse(dat$sequence_id %in% c('8164'), 'val', dat$group))
    
    #--PileatedWoodpecker
    table(dat[dat$CName %in% 'PileatedWoodpecker',]$sequence_id) #1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'PileatedWoodpecker', 'train', dat$group)
    
    #--MountainQuail
    table(dat[dat$CName %in% 'MountainQuail',]$sequence_id) #7 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('10443','12177','12178','15979','17754'), 'train',
                        ifelse(dat$sequence_id %in% c('16111'), 'val',
                               ifelse(dat$sequence_id %in% c('16293'), 'test', dat$group)))
    table(dat[dat$CName %in% 'MountainQuail',]$group)
    
    #--Human
    table(dat[dat$CName %in% 'Human',]$sequence_id) #4 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('9601'), 'train',
                        ifelse(dat$sequence_id %in% c('9602'), 'val',
                               ifelse(dat$sequence_id %in% c('5452','14564'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Human',]$group)
    
    #--Raccoon
    table(dat[dat$CName %in% 'Racoon',]$sequence_id) #3 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('21459'), 'train',
                        ifelse(dat$sequence_id %in% c('4874'), 'val',
                               ifelse(dat$sequence_id %in% c('5565'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Racoon',]$group)
    
    #--Weasel
    table(dat[dat$CName %in% 'Weasel',]$sequence_id) #24 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('491',  '1407',  '1830',  '2780',  '2781',  '4167',  '4415',  '4564',  '5662',  '5785',  '5797',  '5974',  '6329',  '6494',  '6674',  '8501',  '9562', '11516','11520'), 'train',
                        ifelse(dat$sequence_id %in% c('14147','23652'), 'val',
                               ifelse(dat$sequence_id %in% c('15510','16131','22175'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Weasel',]$group, useNA = 'a')
    
    #--Opossum
    table(dat[dat$sp %in% 'Opossum',]$sequence_id) #19 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('2573','2582','4481','5312','5313','5316','5317','9194','12790','12937','13293','14509','17277','24620'), 'train',
                        ifelse(dat$sequence_id %in% c('18093','21961'), 'val',
                               ifelse(dat$sequence_id %in% c('23788','24615','24616'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Opossum',]$group, useNA = 'a')
  
  ## Did I get them all?
  nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
  table(dat[dat$CName %in% rare_sp]$group, useNA = 'a')
  
  ## Now separate the images already assigned from those that are left
  dat_rare <- dat[!is.na(dat$group),]; length(unique(dat_rare$full_path))
  dat_rest <- dat[is.na(dat$group),]; length(unique(dat_rest$full_path))
  
  
  ## 2. From the remaining images, now work by sequence. Select randomly 75% / 15% / 15% split
  
    #how many do we want in each partition?
    length(unique(dat_rest$sequence_id))         #sequences left to assign
    dat_rest_seq <- data.frame('sequence_id' = unique(dat_rest$sequence_id))
      (tr=round(length(unique(dat_rest$sequence_id)) * 0.7))  # in train
      (vt=round(length(unique(dat_rest$sequence_id)) * 0.15))  # each in validation and test

    #random draw (42)
    set.seed(42)
    
    #split training
    rand1a <- sample(dat_rest_seq$sequence_id, size = tr)
    dat_rest_seq$rand1 <- ifelse(dat_rest_seq$sequence_id %in% rand1a, 'train', NA)
    
    #from remaining, split val/test
    rand1b <- sample(dat_rest_seq[!dat_rest_seq$rand1 %in% 'train',]$sequence_id, size = vt)  
    dat_rest_seq$rand1 <- ifelse(dat_rest_seq$sequence_id %in% rand1b, 'val', dat_rest_seq$rand1)
    dat_rest_seq$rand1 <- ifelse(is.na(dat_rest_seq$rand1), 'test', dat_rest_seq$rand1)

    #did we do it right?
    table(dat_rest_seq$rand1, useNA = 'a')
    
    #how many *images* per group?
    dat_rest$group <- dat_rest_seq$rand1[match(dat_rest$sequence_id, dat_rest_seq$sequence_id)]
      dat_rest %>% group_by(group) %>% summarise(photos = n_distinct(full_path))
      dat_rest %>% group_by(group) %>% summarise(percentage = n_distinct(full_path) / n_distinct(dat_rest$full_path) * 100)
      #good! 70/15/15
  
    ## Now combine rare + rest    
    dat_groups <- bind_rows(dat_rare, dat_rest)    
      nrow(dat_groups)
      length(unique(dat_groups$full_path))  #do we have them all back?
  
    ## See how we did with our split
    dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path)))
    dat_groups %>% group_by(group) %>% summarise('percentage' = n_distinct(full_path) / n_distinct(dat_groups$full_path) * 100)
      #70/15/15 after rounding!
  
    
  ## 3. Plot
    
    #sort levels for plotting
    dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
  
    #By images per species
    (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos, sum), y = nPhotos, fill = group)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p1
    #   ggsave('/oregon_critters/figures/COA_species_ground_imgs_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p1a #all 7 red fox images are from the same sequence so don't really want to split it up
      ggsave('oregon_critters/figures/COA_species_ground_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    #By images per species per study
    (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
    p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid( ~group)
    p3
      # ggsave('figures/COA_species_ground_study_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p3a
      ggsave('oregon_critters/figures/COA_species_ground_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    
    
    #By images per species per night/day
    (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
    p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
      geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
      facet_grid(~group)
    p4
    # ggsave('figures/COA_species_ground_daynight_all.jpeg', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    p4a
    ggsave('oregon_critters/figures/COA_species_ground_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  ## save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_split.csv')
  write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_train.csv')
  write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_val.csv')
  write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_test.csv')
  
  ## save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_test.txt')
  
  # **need to remove quotations from these .txt files!!!!**
  
  
## TRAIL BATCH  ---------------------------------------------------------------
  
## Read in metadata and labels
  
  dat <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/metadata_labels_trail.csv'); dat <- dat[,-1]
  head(dat)
  length(unique(dat$full_path))   #60,627 images
  length(unique(dat$sequence_id)) #160,66 sequences
  

## View summaries of dataset (by sequence?)
  
  #species totals 
  (species_totals <- dat %>% group_by(CName) %>% summarise('nSequences' = length(unique(sequence_id))))
  (species_totals_photos <- dat %>% group_by(CName) %>% summarise('nPhotos' = length(unique(full_name))))
  p1 <- ggplot(species_totals, aes(reorder(CName, -nSequences, sum), y = nSequences)) + ggtitle('Trail') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p1
  
  #species/camera
  (species_site <- aggregate(dat, stn ~ CName, function(x) length(unique(x))))
  p2 <- ggplot(species_site, aes(x = reorder(CName, -stn, sum), y = stn)) + ggtitle('Trail') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p2
  
  #species/project
  (species_project <- dat %>% group_by(CName, project) %>% summarise('nSequences' = length(unique(sequence_id))))
  p3 <- ggplot(species_project, aes(x = reorder(CName, -nSequences, sum), y = nSequences, fill = project)) + ggtitle('Trail') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p3
  
  #species/daynight
  (species_day_night <- dat %>% group_by(CName, day_night) %>% summarise('nSequences' = n()))
  p4 <- ggplot(species_day_night, aes(x = reorder(CName, -nSequences, sum), y = nSequences, fill = day_night)) + ggtitle('Trail') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
  p4  
  
  
## Split 
  
  ## GOAL: 75% training, 15% validation, 15% test    
  
  ## 1. ensure rare species end up in all sets
  
    #set a threshold and find the rare species (this will be different for both/ground/trail)
    rare_thresh = 100
    # (rare_sp <- species_totals[species_totals$nSequences <= rare_thresh,]$CName)
    (rare_sp <- species_totals[species_totals_photos$nPhotos <= rare_thresh,]$CName)
    #do I want fewer than 50 photos or fewer than 50 sequences?
    #go with <50 photos. the others will end up OK
    
    #now allocate sequences into train/val/test groups:
    dat$group <- rep(NA, nrow(dat))
    
    #--RedFox
    table(dat[dat$CName %in% 'RedFox',]$sequence_id) #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'RedFox', 'train', dat$group)
    
    #--PileatedWoodpecker 
    table(dat[dat$CName %in% 'PileatedWoodpecker',]$sequence_id) #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'PileatedWoodpecker', 'train', dat$group)
    
    #--Herp
    table(dat[dat$CName %in% 'Herp',]$sequence_id) #only 1 sequence. put in 'train'
    dat$group <- ifelse(dat$CName %in% 'Herp', 'train', dat$group)
    
    #--Mink
    table(dat[dat$CName %in% 'Mink',]$sequence_id) #4 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('27766','27765'), 'train',
                        ifelse(dat$sequence_id %in% c('27591'), 'val',
                               ifelse(dat$sequence_id %in% c('27574'), 'test', dat$group)))
    #--GroundSquirrel
    table(dat[dat$CName %in% 'GroundSquirrel',]$sequence_id) #4 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('26856','26849'), 'train',
                        ifelse(dat$sequence_id %in% c('26847'), 'val',
                               ifelse(dat$sequence_id %in% c('26833'), 'test', dat$group)))
    #--Bat
    table(dat[dat$CName %in% 'Bat',]$sequence_id) #5 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('24088','21744'), 'train',
                        ifelse(dat$sequence_id %in% c('21743'), 'val',
                               ifelse(dat$sequence_id %in% c('5462','2093'), 'test', dat$group)))
    #--Owl
    table(dat[dat$CName %in% 'Owl',]$sequence_id) #6 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('9219','9593','9758','12522'), 'train',
                        ifelse(dat$sequence_id %in% c('27576'), 'val',
                               ifelse(dat$sequence_id %in% c('27865'), 'test', dat$group)))
    #--CandaJay
    table(dat[dat$CName %in% 'CanadaJay',]$sequence_id) #10 sequences
    dat$group <- ifelse(dat$sequence_id %in% c('23800','23801','23802','24105','27310','27418','27433'), 'train',
                        ifelse(dat$sequence_id %in% c('18778','18979'), 'val',
                               ifelse(dat$sequence_id %in% c('23799'), 'test', dat$group)))
    #--Marten
    table(dat[dat$CName %in% 'Marten',]$sequence_id) 
    dat$group <- ifelse(dat$sequence_id %in% c('26570','26574','26582','26583','26584','26585','26586','26588','26589','26592','26605','26609','26613'), 'train',
                        ifelse(dat$sequence_id %in% c('26616','26618','26623','28193'), 'val',
                               ifelse(dat$sequence_id %in% c('26869','27649','27650','28191'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Marten',]$group, useNA = 'a')
    
    #--Coyote
    table(dat[dat$CName %in% 'Coyote',]$sequence_id)
    dat$group <- ifelse(dat$sequence_id %in% c('103','5893','9429','9430','9431','9589','9734','9735','9737','9761','9763','9764','11248'), 'train',
                        ifelse(dat$sequence_id %in% c('11251','14684','24092','27685','27437'), 'val',
                               ifelse(dat$sequence_id %in% c('26591','26594','26620','26873','27735'), 'test', dat$group)))
    table(dat[dat$CName %in% 'Coyote',]$group, useNA = 'a')
    
    ## Did I get them all?
    nrow(dat[dat$CName %in% rare_sp & is.na(dat$group),]) #yep, no NAs in group
    table(dat[dat$CName %in% rare_sp]$group, useNA = 'a')
    
    ## Now separate the images already assigned from those that are left
    dat_rare <- dat[!is.na(dat$group),]; length(unique(dat_rare$full_path))
    dat_rest <- dat[is.na(dat$group),]; length(unique(dat_rest$full_path))
  

  ## 2. From the remaining images, now work by sequence. Select randomly 75% / 15% / 15% split
  
    #how many do we want in each partition?
    length(unique(dat_rest$sequence_id))         #sequences left to assign
    dat_rest_seq <- data.frame('sequence_id' = unique(dat_rest$sequence_id))
      (tr=round(length(unique(dat_rest$sequence_id)) * 0.7))  # in train
      (vt=round(length(unique(dat_rest$sequence_id)) * 0.15))  # each in validation and test
    
    #random draw (42)
    set.seed(42)
  
    #split training
    rand1a <- sample(dat_rest_seq$sequence_id, size = tr)
    dat_rest_seq$rand1 <- ifelse(dat_rest_seq$sequence_id %in% rand1a, 'train', NA)
  
    #from remaining, split val/test
    rand1b <- sample(dat_rest_seq[!dat_rest_seq$rand1 %in% 'train',]$sequence_id, size = vt)  
    dat_rest_seq$rand1 <- ifelse(dat_rest_seq$sequence_id %in% rand1b, 'val', dat_rest_seq$rand1)
    dat_rest_seq$rand1 <- ifelse(is.na(dat_rest_seq$rand1), 'test', dat_rest_seq$rand1)
    
    #did we do it right?
    table(dat_rest_seq$rand1, useNA = 'a')
  
    #how many *images* per group?
    dat_rest$group <- dat_rest_seq$rand1[match(dat_rest$sequence_id, dat_rest_seq$sequence_id)]
    dat_rest %>% group_by(group) %>% summarise(photos = n_distinct(full_path))
    dat_rest %>% group_by(group) %>% summarise(percentage = n_distinct(full_path) / n_distinct(dat_rest$full_path) * 100)
      #good! 70/15/15
    
    ## Now combine rare + rest    
    dat_groups <- bind_rows(dat_rare, dat_rest)    
    nrow(dat_groups)
    length(unique(dat_groups$full_path))  #do we have them all back?
  
    ## See how we did with our split
    dat_groups %>% group_by(group) %>% summarise('nPhotos' = length(unique(full_path)))
    dat_groups %>% group_by(group) %>% summarise('percentage' = n_distinct(full_path) / n_distinct(dat_groups$full_path) * 100)
      #70/15/15 after rounding!
  
  ## 3. Plot
  
  #sort levels for plotting
  dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
  
  #By images per species
  (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos, sum), y = nPhotos, fill = group)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p1
  #   ggsave('/oregon_critters/figures/COA_species_ground_imgs_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p1a #all 7 red fox images are from the same sequence so don't really want to split it up
  ggsave('oregon_critters/figures/COA_species_trail_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #By images per species per study
  (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid( ~group)
  p3
  # ggsave('figures/COA_species_ground_study_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p3a
  ggsave('oregon_critters/figures/COA_species_trail_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    
  
  #By images per species per night/day
  (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
  p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p4
  # ggsave('figures/COA_species_ground_daynight_all.jpeg', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  p4a
  ggsave('oregon_critters/figures/COA_species_trail_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  ## save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_split.csv')
  write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_train.csv')
  write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_val.csv')
  write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_test.csv')
  
  ## save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_test.txt')
  
  # **need to remove quotations from these .txt files!!!!**
  
## BOTH COMBINED ---------------------------------------------------------------
  
  #How does it look if I just combine them with splits as-is?
  tr <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/trail_split.csv')
  gr <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/ground_split.csv')
  
    length(unique(tr$full_path)) #60,627 trail
    length(unique(gr$full_path)) #30,418 ground
  
  #Trail splits:
  length(unique(tr[tr$group %in% 'train',]$full_path)) #71%
  length(unique(tr[tr$group %in% 'val',]$full_path))   #15%
  length(unique(tr[tr$group %in% 'test',]$full_path))  #14%
  
  #Ground splits:
  length(unique(gr[gr$group %in% 'train',]$full_path)) #70%
  length(unique(gr[gr$group %in% 'val',]$full_path))   #15%
  length(unique(gr[gr$group %in% 'test',]$full_path))  #15%
  
  #Combine
  both <- bind_rows(tr,gr)
  length(unique(both$full_path)) #191,048
  
  #Combined splits:
  length(unique(both[both$group %in% 'train',]$full_path)) #70%
  length(unique(both[both$group %in% 'val',]$full_path))   #15%
  length(unique(both[both$group %in% 'test',]$full_path))  #15%
  #looks great!
  
  #How about rare species? Should be fine
  
  dat_groups <- both
  
  #sort levels
  dat_groups$group <- factor(dat_groups$group, levels = c('train','val','test'))
  
  #By images per species
  (species_totals_g <- dat_groups %>% group_by(CName, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p1a <- ggplot(species_totals_g, aes(reorder(CName, -nPhotos, sum), y = nPhotos, fill = group)) + ggtitle('Both') +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) +
    facet_grid(~group)
  p1a #all 7 red fox images are from the same sequence so don't really want to split it up
  ggsave('oregon_critters/figures/COA_species_trail_imgs_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #By images per species per study
  (species_project_g <- dat_groups %>% group_by(CName, project, group) %>% summarise('nPhotos' = length(unique(full_path))))
  p3a <- ggplot(species_project_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = project)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) + ggtitle('Both') +
    facet_grid( ~group)
  p3a
  ggsave('oregon_critters/figures/COA_species_bothBl_study_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')    
  
  #By images per species per night/day
  (species_day_night_g <- dat_groups %>% group_by(CName, day_night, group) %>% summarise('nPhotos' = n()))
  p4a <- ggplot(species_day_night_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = day_night)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) + ggtitle('Both') +
    facet_grid(~group)
  p4a
  ggsave('oregon_critters/figures/COA_species_bothB_daynight_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  #species/camtype
  dat_groups$cam_type_2 <- ifelse(dat_groups$cam_type == 'G', 'G', 'T')
  (species_cam_type_g <- dat_groups %>% group_by(CName, cam_type_2, group) %>% summarise('nPhotos' = n()))
  p5a <- ggplot(species_cam_type_g, aes(x = reorder(CName, -nPhotos, sum), y = nPhotos, fill = cam_type_2)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_bw() + theme(axis.title.y = element_blank()) + ggtitle('Both') +
    facet_grid(~group)
  p5a  
  ggsave('oregon_critters/figures/COA_species_bothB_camtype_split.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  
  #save!
  write.csv(dat_groups, '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_split.csv')
  write.csv(dat_groups[dat_groups$group %in% 'train',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_train.csv')
  write.csv(dat_groups[dat_groups$group %in% 'val',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_val.csv')
  write.csv(dat_groups[dat_groups$group %in% 'test',], '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_test.csv')
  
  
  #save txt files with just filenames
  write.table(dat_groups[dat_groups$group %in% 'train',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_train.txt')
  write.table(dat_groups[dat_groups$group %in% 'val',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_val.txt')
  write.table(dat_groups[dat_groups$group %in% 'test',]$full_path, col.names = FALSE, row.names = FALSE,
              file = '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/both_test.txt')
  
  # **need to remove quotations from these .txt files!!!!**