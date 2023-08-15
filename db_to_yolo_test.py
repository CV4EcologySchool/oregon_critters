import sqlite3
from PIL import Image

# Define get_image_dimensions
def get_image_dimensions(image_path):
    with Image.open(image_path) as img:
        return img.width, img.height

# Connect to the SQLite database
db_path = 'COA_2021_GraySquirrel1_AM.db'
conn = sqlite3.connect(db_path)
cursor = conn.cursor()

# Fetch annotation information
cursor.execute("""
    SELECT Images.IName, Labels.X, Labels.Y, Labels.W, Labels.H, Classes.CName
    FROM Labels
    INNER JOIN Images ON Labels.IName = Images.IName
    INNER JOIN Classes ON Labels.CName = Classes.CName
""")
annotations = cursor.fetchall()

#print(annotations)

# Process annotations and write to YOLO-formatted text files
for annotation in annotations:
    IName, X, Y, W, H, CName = annotation
    
    # Get image dimensions using the defined function
    image_width, image_height = get_image_dimensions(IName)
    
    print(image_width)
    print(image_height)

# Process annotations and write to YOLO-formatted text files
for annotation in annotations:
    image_name, x_min, y_min, width, height, class_label = annotation
    
    # Get image dimensions if needed (replace this with your own method to get image dimensions)
    image_width, image_height = get_image_dimensions(image_name)
    
    # Convert annotations to YOLO format
    x_center = (x_min + width / 2) / image_width
    y_center = (y_min + height / 2) / image_height
    normalized_width = width / image_width
    normalized_height = height / image_height
    
    yolo_annotation = f"{class_label} {x_center:.6f} {y_center:.6f} {normalized_width:.6f} {normalized_height:.6f}"
    
    # Write the YOLO annotation to a text file
    yolo_filename = image_name.replace('.jpg', '.txt')
    with open(yolo_filename, 'w') as yolo_file:
        yolo_file.write(yolo_annotation)

        print(yolo_filename)
        print(yolo_file)

# Close the database connection
conn.close()
