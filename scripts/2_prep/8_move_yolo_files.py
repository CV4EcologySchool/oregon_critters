# Move YOLO .txt files into the 'labels' folders next to each images folder

import os
import shutil
from tqdm import tqdm

# Specify the location of images and labels
source_base_directory = '/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/'

############

# Define function to create target path name (just replaces 'images' with 'labels')
def transform_path(source_path):
    target_path = source_path.replace("/images/", "/labels/")
    return target_path

# Loop through specified directory. Creates 'labels' folders and moves .txt files there
for root, _, files in os.walk(source_base_directory):
    for file in tqdm(files):

        if file.endswith(".txt") and '/images' in root:
            source_file_path = os.path.join(root, file)
            target_file_path = transform_path(source_file_path)

            #Does 'labels' folder exist? If not, create it
            target_directory = os.path.dirname(target_file_path)
            if not os.path.exists(target_directory):
                print(f"Creating target directory: '{target_directory}'")
                os.makedirs(target_directory)  
            
            #Move files
            shutil.move(source_file_path, target_file_path)
