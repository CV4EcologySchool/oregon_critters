
## make some plots

library(data.table)
library(ggplot2)
library(tidyverse)

## PLOT mAP FROM GROUND VS TRAIL VS BOTH MODELS --------------------------------

  #read in results from yolo val function
    gg1 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_07ground_on_ground.csv'); gg1$model_nickname = 'ground1'
    gg2 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_07ground_on_ground_rep2.csv'); gg2$model_nickname = 'ground2'
    gg3 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_07ground_on_ground_rep3.csv'); gg3$model_nickname = 'ground3'
    gg <- rbindlist(list(gg1,gg2,gg3))
    gg$model <- '07_mc_ground_med'; gg$val_set <- 'ground'
      head(gg)

    tt1 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_08trail_on_trail.csv'); tt1$model_nickname = 'trail1'
    tt2 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_08trail_on_trail_rep2.csv'); tt2$model_nickname = 'trail2'
    tt3 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_08trail_on_trail_rep3.csv'); tt3$model_nickname = 'trail3'
    tt <- rbindlist(list(tt1,tt2,tt3))
    tt$model <- '08_mc_trail_med'; tt$val_set <- 'trail'
      head(tt)
      
    bg1 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_ground.csv'); bg1$model_nickname = 'both1'
    bg2 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_ground_rep2.csv'); bg2$model_nickname = 'both2'
    bg3 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_ground_rep3.csv'); bg3$model_nickname = 'both3'
    bg <- rbindlist(list(bg1,bg2,bg3))
    bg$model <- '09_mc_both_med'; bg$val_set <- 'ground'
      head(bg)  
      
    bt1 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_trail.csv'); bt1$model_nickname = 'both1'
    bt2 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_trail_rep2.csv'); bt2$model_nickname = 'both2'
    bt3 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_trail_rep3.csv'); bt3$model_nickname = 'both3'
    bt <- rbindlist(list(bt1,bt2,bt3))
    bt$model <- '09_mc_both_med'; bt$val_set <- 'trail'
      head(bt)  
      
    bb1 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_both.csv'); bb1$model_nickname = 'both1'
    bb2 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_both_rep2.csv'); bb2$model_nickname = 'both2'
    bb3 <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/validation_results/validation_09both_on_both_rep3.csv'); bb3$model_nickname = 'both3'
    bb <- rbindlist(list(bb1,bb2,bb3))
    bb$model <- '09_mc_both_med'; bb$val_set <- 'both'
      head(bb)  
      
  #combine into long-form 
    df_list <- list(gg, tt, bg, bt, bb)
    val_results <- rbindlist(df_list)  
      head(val_results)    
      
      #sort model levels
      val_results$model <- factor(val_results$model, levels = c("09_mc_both_med", "07_mc_ground_med", "08_mc_trail_med"))
      
  #plot (overall mAP50)
    p0 <- ggplot(val_results[val_results$Class %in% 'all',], aes(x = val_set, y = mAP50, color = model, shape = model)) +
      geom_point(size=6, position = position_dodge(width = 0.2)) + 
      scale_color_manual(values = c('#666666', '#D6705C','#2A90CB')) +
      ylim(0.45,0.65) +
      theme_bw() +
      xlab('Validation set') +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
            legend.position = 'none')
    p0  
    #ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/mAP_all.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
    
    
        
  #plot (mAP50 by species)
    
    #first order classes by mAP (choose which model)
    ground_results <- rbind(gg,bg)
    ground_results_sub <- ground_results[ground_results$model_nickname %in% 'both1' & ground_results$val_set %in% 'ground',]
      class_order_gr <- ground_results_sub$Class[order(ground_results_sub$mAP50, decreasing = TRUE)]
      ground_results$Class <- factor(ground_results$Class, levels = class_order_gr)
    
    #ground vs. both
    p1 <- ggplot(ground_results[!ground_results$Class %in% 'all' & ground_results$model_nickname %in% c('ground1','trail1','both1'),],
                 aes(x = Class, y = mAP50, color = model, shape = model)) +
      geom_point(size = 4, position = position_jitter(width = 0.2)) +
      # geom_point(data = ground_results[!ground_results$Class %in% 'all' & ground_results$model_nickname %in% c('ground2','trail2','both2'),],
      #            aes(x = Class, y = mAP50, color = model, shape = model), size = 3, alpha = 0.5, position = position_jitter(width = 0.2)) +
      # geom_point(data = ground_results[!ground_results$Class %in% 'all' & ground_results$model_nickname %in% c('ground3','trail3','both3'),],
      #            aes(x = Class, y = mAP50, color = model, shape = model), size = 3, alpha = 0.5, position = position_jitter(width = 0.2)) +
      scale_color_manual(values = c('#D6705C','#777777')) + #alt colors:  #1F77B4, #FF7F0E
      theme_bw() +
      geom_hline(color = '#777777', linetype = 'dashed', lwd = 0.8, yintercept = ground_results[ground_results$Class %in% 'all' & ground_results$model_nickname %in% 'both1',]$mAP50) +
      geom_hline(color = '#D6705C', linetype = 'dashed', lwd = 0.8, yintercept = ground_results[ground_results$Class %in% 'all' & ground_results$model_nickname %in% 'ground1',]$mAP50) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 18),
            legend.position = 'none',
            plot.margin = margin(0.5,0.5,0.5,2, "cm"))
    p1  
    #ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/mAP_ground_vs_both_sp.png', units="in", width=14, height=5, dpi=600, bg = 'transparent')
    
    
    #trail vs. both
    trail_results <- rbind(tt,bt)
      trail_results_sub <- trail_results[trail_results$model_nickname %in% 'both1' & trail_results$val_set %in% 'trail',]
      class_order_tr <- trail_results_sub$Class[order(trail_results_sub$mAP50, decreasing = TRUE)]
      trail_results$Class <- factor(trail_results$Class, levels = class_order_tr)
    
    p2 <- ggplot(trail_results[!trail_results$Class %in% 'all' & trail_results$model_nickname %in% c('ground1','trail1','both1'),], 
                 aes(x = Class, y = mAP50, color = model, shape = model)) +
      geom_point(size = 4, position = position_jitter(width = 0.2)) +
      scale_color_manual(values = c('#2A90CB','#777777')) + #alt colors:  #1F77B4, #FF7F0E
      theme_bw() +
      geom_hline(color = '#777777', linetype = 'dashed', lwd = 0.8, yintercept = trail_results[trail_results$Class %in% 'all' & trail_results$model_nickname %in% 'both1',]$mAP50) +
      geom_hline(color = '#2A90CB', linetype = 'dashed', lwd = 0.8, yintercept = trail_results[trail_results$Class %in% 'all' & trail_results$model_nickname %in% 'trail1',]$mAP50) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 18),
            legend.position = 'none',
            plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
    p2  
    #ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/mAP_trail_vs_both_sp.png', units="in", width=14, height=5, dpi=600, bg = 'transparent')
    
    
    
## PLOT DIFF IN mAP RELATED TO TRAINING SIZE -----------------------------------
    
  head(ground_results)
  head(trail_results)
  
  ground1 <- rbind(gg1, bg1)
  trail1 <- rbind(tt1,bt1)
  
  ground_score_diff <- ground1 %>% group_by(Class) %>% 
    select(Class, mAP50, model_nickname) %>% 
    pivot_wider(names_from = model_nickname, values_from = mAP50) %>%
    mutate(score_diff = ground1 - both1) 

  trail_score_diff <- trail1 %>% group_by(Class) %>% 
    select(Class, mAP50, model_nickname) %>% 
    pivot_wider(names_from = model_nickname, values_from = mAP50) %>%
    mutate(score_diff = trail1 - both1) 
  
  #merge
  combined_df <- bind_rows(
    ground_score_diff %>% select(Class, score_diff),
    trail_score_diff %>% select(Class, score_diff),
    .id = "type"  # Creates a 'type' column to distinguish between the sources
  ) %>%
    mutate(type = ifelse(type == "1", "ground", "trail")) %>% # Rename type values
    mutate(appearance = ifelse("ground" %in% type & "trail" %in% type, "both", type))
  
  #
  g0 <- ggplot(combined_df[!combined_df$Class %in% 'all',], aes(x = reorder(Class, -score_diff), y = score_diff, fill = type)) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 0.9, preserve = "single")) +
    theme_bw() +
#    facet_grid(~appearance) +
    ylim(-1,1) +
    scale_fill_manual(values = c('#D6705C','#2A90CB')) +
    ylab('Difference in mAP50 from combined model') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = 'none',
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  g0
  #ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/mAP_diff_by_species.png', units="in", width=14, height=5, dpi=600, bg = 'transparent')
  

  ## Read in training size
  groundcsv <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels_old/sampled_ds_ground_620_split.csv'); groundcsv <- groundcsv[,-1]
  trailcsv <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels_old/sampled_ds_trail_180_split.csv'); trailcsv <- trailcsv[,-1]
  bothcsv <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels_old/sampled_ds_bothB_split.csv'); bothcsv <- bothcsv[,-1]
  
  ground_split <- groundcsv %>% group_by(CName, group) %>% summarise(total = n()) %>%
    group_by(group) %>% mutate(percentage = (total / sum(total)) * 100)
    
  trail_split <- trailcsv %>% group_by(CName, group) %>% summarise(total = n()) %>%
    group_by(group) %>% mutate(percentage = (total / sum(total)) * 100)

  both_split <- bothcsv %>% group_by(CName, group) %>% summarise(total = n()) %>%
    group_by(group) %>% mutate(percentage = (total / sum(total)) * 100)
  
  #combine
  combined_pct <- bind_rows(
    ground_split %>% select(CName, group, percentage) %>% mutate(source = "ground"),
    trail_split %>% select(CName, group, percentage) %>% mutate(source = "trail"),
    both_split %>% select(CName, group, percentage) %>% mutate(source = "both")
  )
  
  # Calculate the differences between A and C, and B and C
  pct_diff <- combined_pct %>%
    pivot_wider(names_from = source, values_from = percentage) %>%
    mutate(diff_ground_both = ground - both, diff_trail_both = trail - both) %>% 
    filter(group == 'train') %>%
    select(CName, diff_ground_both, diff_trail_both)
  
  plot_data <- pct_diff[,c('CName','diff_ground_both','diff_trail_both')] %>%
    gather(key = "comparison", value = "difference", -CName)
  
  g0b <- ggplot(plot_data, aes(x = reorder(CName, -difference), y = difference, fill = comparison)) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 0.9, preserve = "single")) +
    theme_bw() +
    #scale_fill_manual(values = c('#D6705C','#2A90CB')) +
    #ylab('Difference in mAP50 from combined model') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          #legend.position = 'none',
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  g0b
  
  ## train/val plot by species
  splits_df <- bind_rows(
    ground_split %>% select(CName, group, total) %>% mutate(source = "ground"),
    trail_split %>% select(CName, group, total) %>% mutate(source = "trail"),
  )
  
  g0c <- ggplot(splits_df[splits_df$group %in% 'train',], aes(x = reorder(CName, -total), y = total, fill = source)) +
    #facet_grid(~ group) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 0.9, preserve = 'single')) +
    theme_bw() +
    scale_fill_manual(values = c('#D6705C','#2A90CB')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = 'none',
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  g0c
  #ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/training_by_species.png', units="in", width=14, height=5, dpi=600, bg = 'transparent')
  
  
## TO REPORT FOR ORTWS ABSTRACT ------------------------------------------------
  head(ground_results)
  head(trail_results)
  head(ground_split); head(trail_split); head(both_split)
  
  #summary of mAP50
  summary(ground_results$mAP50) #range 0-0.995, mean 0.5623
  summary(trail_results$mAP50) #range 0-0.995, mean 0.5566
  
  #match training data sizes
  ground_ground_df <- merge(ground_results[ground_results$model == '07_mc_ground_med'], 
                     ground_split[ground_split$group == 'train',], by.x = 'Class', by.y = 'CName', all = TRUE)
  ground_both_df <- merge(ground_results[ground_results$model == '09_mc_both_med'], 
                     both_split[both_split$group == 'train',], by.x = 'Class', by.y = 'CName', all = TRUE)
  ground_df <- rbind(ground_ground_df, ground_both_df)
  
  trail_trail_df <- merge(trail_results[trail_results$model == '08_mc_trail_med'], 
                    trail_split[trail_split$group == 'train',], by.x = 'Class', by.y = 'CName', all = TRUE)
  trail_both_df <- merge(trail_results[trail_results$model == '09_mc_both_med'], 
                    both_split[both_split$group == 'train',], by.x = 'Class', by.y = 'CName', all = TRUE)
  trail_df <- rbind(trail_trail_df, trail_both_df)
  
  #for each species, keep the highest mAP50 of the 3 model replicates
  ground_max <- ground_df %>% group_by(Class, model) %>% filter(mAP50 == max(mAP50))
  trail_max <- trail_df %>% group_by(Class, model) %>% filter(mAP50 == max(mAP50))
  
  #plot training size against mAP50
  plot(ground_max$mAP50 ~ ground_max$total)
  
  ggplot(ground_max, aes(x = total, y = mAP50, color = Class)) +
    geom_point(size = 2) +
    geom_text(aes(label = Class), hjust = 1.2, vjust = 0.5) +
    theme_bw() + 
    facet_grid(~model) +
    theme(legend.position = 'none')
  
  ggplot(trail_max, aes(x = total, y = mAP50, color = Class)) +
    geom_point(size = 2) +
    geom_text(aes(label = Class), hjust = 1.2, vjust = 0.5) +
    theme_bw() + 
    facet_grid(~model) +
    theme(legend.position = 'none')
  
  
## PLOT HISTOGRAMS OF CONFIDENCE SCORES ----------------------------------------
    
  #read in prediction/truth comparisons
  bg_compare <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/predictions_on_val/pred_true_compare_09_both_on_ground.csv')
  bt_compare <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/predictions_on_val/pred_true_compare_09_both_on_trail.csv')

  gg_compare <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/predictions_on_val/pred_true_compare_07_ground.csv')
  tt_compare <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/predictions_on_val/pred_true_compare_08_trail.csv')
  
    head(bg_compare)    
    head(bt_compare)    

  par(mfrow=c(2,1))
    
  #plot BOTH on GROUND
  p3a <- ggplot(bg_compare[bg_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle('"Both" model on ground images')
  p3a
  
  #plot GROUND on GROUND
  p3b <- ggplot(gg_compare[gg_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle('"Ground" model on ground images')
  p3b
  

  #plot BOTH on TRAIL
  p4a <- ggplot(bt_compare[bt_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle('"Both" model on trail images')
  p4a
  
  #plot TRAIL on TRAIL
  p4b <- ggplot(tt_compare[tt_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle('"Trail" model on trail images')
  p4b
  
#######
  
  #by species:
  species = 'FlyingSquirrel'
  p5a <- ggplot(bg_compare[bg_compare$true_class %in% species & bg_compare$match %in% TRUE,], 
               aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle(print(species))
  p5a
  ggsave('/Users/caraappel/Documents/CV4E/oregon_critters/figures/histogram_flyingsquirrel_bothground.png', units="in", width=7, height=5, dpi=600, bg = 'transparent')
  
  p5b <- ggplot(bt_compare[bt_compare$true_class %in% species & bt_compare$match %in% TRUE,], 
               aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle(print(species))
  p5b
    
  species = 'DouglasSquirrel'
  p6 <- ggplot(bg_compare[bg_compare$true_class %in% species & bg_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle(species)
  p6    
  
  species = 'BlackBear'
  p7 <- ggplot(bt_compare[bt_compare$true_class %in% species & bt_compare$match %in% TRUE,], aes(x = confidence, fill = class_match)) +
    geom_histogram(color = "#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#D6705C", "#2A90CB")) +
    labs(fill = '') +
    theme_bw() +
    ggtitle(print(paste(species, '(Trail)')))
  p7
  
  