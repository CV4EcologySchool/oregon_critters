#Convert YOLO predictions CSV file to text file needed for Njobvu-AI input

import pandas as pd
import argparse
import os
from PIL import Image


#define function to get image dimensions
def get_image_dimensions(filename):
    with Image.open(filename) as img:
        return img.size
    

#define conversion function (check "filename" path format below!)
def convert_entry(entry):

    class_id = entry["class_id"]
    class_name = entry["class_name"]

    bbox = entry["box_coord"].strip('[]').split(',')
    bbox = [float(item) for item in bbox]

    converted_entry = {
        "frame_id": 1,
        # "filename": f"/data/{entry['image_id']}.JPG",  #for finding files locally
        # "filename": f"/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/COA_training/oregon_critters/demo2/{dimensions['full_path'].values[0]}", #for files on server
        "filename": entry['path'],
        "objects": []
    }

    #extract dimensions
    dimensions = get_image_dimensions(converted_entry["filename"])
    
    #to handle potentially empty dimensions
    if dimensions is not None and all(dimensions):
        converted_entry["objects"].append({
                "class_id": int(class_id),
                "name": class_name,
                "image_width": int(dimensions[0]),
                "image_height": int(dimensions[1]),
                "relative_coordinates": {
                    "center_x": float(bbox[0] / int(dimensions[0])),  #check math here
                    "center_y": float(bbox[1] / int(dimensions[1])),  
                    "width": float(bbox[2] / int(dimensions[0])), 
                    "height": float(bbox[3] / int(dimensions[1])),  
                },
                "confidence": float(entry["confidence"])
            })
    else:
        print(f"No dimensions found for image ID: {entry['image_id']}")

    return converted_entry

#define main
def main(preds_path, out_dir):

    #load predictions from CSV file    
    yolo_data = pd.read_csv(preds_path, low_memory=False)

    #convert each row to dictionary entry
    yolo_data['dict_entry'] = yolo_data.apply(convert_entry, axis=1)

    #convert these all to a JSON
    json_output = yolo_data['dict_entry'].to_json(orient='records')

    #save as JSON
    json_output_path = os.path.join(out_dir, "labels_for_njobvu.json")
    with open(json_output_path, 'w') as json_file:
        json_file.write(json_output)
    
    #save as text file
    text_output_path = os.path.join(out_dir, "labels_for_njobvu.txt")
    with open(text_output_path, 'w') as text_file:
        text_file.write(json_output)

    print(f"Converted data saved to {json_output_path} and {text_output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert YOLO predictions to Njobvu-AI input format")
    parser.add_argument("--preds_path", type=str)
    parser.add_argument("--out_dir", type=str)
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.preds_path, args.out_dir)
