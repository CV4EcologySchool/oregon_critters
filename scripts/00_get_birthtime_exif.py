import exifread
import sys

# Replace 'your_image_file_path' with the path to your image file
#image_file_path = 'your_image_file_path'

image_file_path = sys.argv[1]

def get_creation_date(file_path):
    try:
        with open(file_path, 'rb') as f:
            tags = exifread.process_file(f, details=False, stop_tag='EXIF DateTimeOriginal')
            if 'EXIF DateTimeOriginal' in tags:
                return tags['EXIF DateTimeOriginal']
            else:
                return "Creation date not found in metadata"
    except Exception as e:
        return str(e)

creation_date = get_creation_date(image_file_path)
print("Creation Date:", creation_date)
