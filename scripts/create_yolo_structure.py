# Create 'labels' folders where images are to store YOLO .txt files

import os
import pandas as pd 

#read in file paths
image_csv = pd.read_csv('/home/cara/oregon_critters/sampled_ds_300_split.csv')
image_paths = image_csv['full_path'].values

#extract the directories from the image paths
directories = [os.path.dirname(image_path) for image_path in image_paths]

#specify the target directory
target_base_directory = '/mnt/ssd-cluster/cara/'  #the 'data' folder is within here

#create a 'labels' subfolder in each folder
for directory in directories:
    new_folder = directory.replace("images", "labels")
    new_dir = target_base_directory + new_folder
    os.makedirs(new_dir, exist_ok=True)




