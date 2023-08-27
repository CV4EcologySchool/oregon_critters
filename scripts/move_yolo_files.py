# Move YOLO .txt files into the 'labels' folders next to each images folder

import os
import shutil
from tqdm import tqdm

# Specify the source and target base directories
#source_base_directory = "/home/cara/oregon_critters/yolo_txt/labels"
#target_base_directory = "/mnt/ssd-cluster/cara"
source_base_directory = '/mnt/ssd-cluster/cara/upload_ground_thresh620'
#target_base_directory = '/mnt/ssd-cluster/cara'

def transform_path(source_path):
    #replace 'images' with 'labels'
    target_path = source_path.replace("/images/", "/labels/")
    #source_path = target_path.replace("/labels/", "/images/")
    return target_path

for root, _, files in os.walk(source_base_directory):
    for file in tqdm(files):

        if file.endswith(".txt") and '/images' in root:
            source_file_path = os.path.join(root, file)
            target_file_path = transform_path(source_file_path)

            # print(source_file_path)
            # print(target_file_path)
            # print()

            #Does target directory exist?
            # target_directory = os.path.dirname(target_file_path)
            # if not os.path.exists(target_directory):
            #     raise ValueError(f"Target directory '{target_directory}' does not exist.")

            shutil.move(source_file_path, target_file_path)



# Function to transform source file path to target file path
#def transform_path(source_path):
#    # Replace 'images' with 'labels' and '.txt' extension
#    target_path = source_path.replace(source_base_directory, target_base_directory).replace("/images/", "/labels/")
#    return target_path

#Walk through the source directory and move files
# for root, _, files in os.walk(source_base_directory):
#     for file in tqdm(files):
#         if file.endswith(".txt"): #and "images" in root:
#             source_file_path = os.path.join(root, file)
#             target_file_path = transform_path(source_file_path)
            
#             # Check if the target directory exists
#             target_directory = os.path.dirname(target_file_path)
#             if not os.path.exists(target_directory):
#                 raise ValueError(f"Target directory '{target_directory}' does not exist.")

#             shutil.move(source_file_path, target_file_path)
            
