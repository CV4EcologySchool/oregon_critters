#extract birthtime of image files

import os
import exifread
import pandas as pd

#define funcitons
def get_creation_date(file_path):
    try:
        with open(file_path, 'rb') as f:
            tags = exifread.process_file(f, details=False, stop_tag='EXIF DateTimeOriginal')
            if 'EXIF DateTimeOriginal' in tags:
                return tags['EXIF DateTimeOriginal']
            else:
                return None
    except Exception as e:
        return str(e)

def extract_metadata_from_directory(root_directory):
    file_list = []
    for root, _, files in os.walk(root_directory):
        for file_name in files:
            if file_name.lower().endswith('.jpg'):
                file_path = os.path.join(root, file_name)
                creation_date = get_creation_date(file_path)
                file_list.append({
                    'Filename': file_name,
                    'Full Path': file_path,
                    'Creation Date': creation_date
                })
    return file_list

# Set directory path
root_directory = '/Users/caraappel/Library/CloudStorage/Box-Box/ForCara_EmptyTrailcamPhotos/'

# Extract birthtimes
metadata_list = extract_metadata_from_directory(root_directory)

# Create a dataframe
df = pd.DataFrame(metadata_list)

#print(df)

# Save as CSV
df.to_csv('/Users/caraappel/Documents/CV4E/oregon_critters/datetimes/HJA_MARIE_date_created_exif_BOX.csv', index=False)

