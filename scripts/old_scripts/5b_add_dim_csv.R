## Misc CV4E oregon_critters

library(data.table)


## Add image width and height to CSV (extracted in python)

## GROUND

  #read in
  groundcsv <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_split.csv')
  groundadj <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_adjxy.csv')

  #combine
  head(groundcsv)
  head(groundadj)  

  nrow(groundcsv)  
  nrow(groundadj)  

  groundcsv$img_width <- groundadj$width[match(groundcsv$full_path_new, groundadj$file_name)]
  groundcsv$img_height <- groundadj$height[match(groundcsv$full_path_new, groundadj$file_name)]
  
    head(groundcsv)

  #save
  write.csv(groundcsv,
            '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_ground_620_split_adjxy.csv')
  
## TRAIL
  
  #read in
  trailcsv <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_trail_180_split.csv')
  trailadj <- fread('/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_trail_180_adjxy.csv')
  
  #combine
  head(groundcsv)
  head(trailadj)  
  
  nrow(trailcsv)  
  nrow(trailadj)  
  
  trailcsv$img_width <- trailadj$width[match(trailcsv$full_path_new, trailadj$file_name)]
  trailcsv$img_height <- trailadj$height[match(trailcsv$full_path_new, trailadj$file_name)]
  
  head(trailcsv)
  
  #save
  write.csv(trailcsv,
            '/Users/caraappel/Documents/CV4E/oregon_critters/metadata_labels/sampled_ds_trail_180_split_adjxy.csv')
  
  
  