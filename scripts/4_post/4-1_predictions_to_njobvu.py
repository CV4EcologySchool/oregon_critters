#Convert YOLO predictions to format needed for Njobvu-AI input

import pandas as pd
import json
import yaml
import argparse
import os

#define conversion function (check "filename" path format below!)
def convert_entry(entry, class_ids, img_dims):
    class_id = entry["category_id"]
    class_name = class_ids.get(class_id, "unknown")
    dimensions = img_dims[img_dims['image_name'] == entry['image_id'] + '.JPG']

    converted_entry = {
        "frame_id": 1,
        # "filename": f"/data/{entry['image_id']}.JPG",  #for finding files locally
        "filename": f"/nfs6/FW_HMSC/Levi_Lab/YOLO_photos/COA_training/oregon_critters/demo2/{dimensions['full_path'].values[0]}", #for files on server
        "objects": []
    }
    
    #to handle potentially empty dimensions
    if not dimensions.empty:
        converted_entry["objects"].append({
                "class_id": int(class_id),
                "name": class_name,
                "image_width": int(dimensions['img_width'].values[0]),
                "image_height": int(dimensions['img_height'].values[0]),
                "relative_coordinates": {
                    "center_x": float(entry["bbox"][0] / dimensions['img_width'].values[0]),  #check math here
                    "center_y": float(entry["bbox"][1] / dimensions['img_height'].values[0]),  
                    "width": float(entry["bbox"][2] / dimensions['img_width'].values[0]), 
                    "height": float(entry["bbox"][3] / dimensions['img_height'].values[0]),  
                },
                "confidence": float(entry["score"])
            })
    else:
        print(f"No dimensions found for image ID: {entry['image_id']}")

    return converted_entry

#define main
def main(yolo_path, yaml_path, csv_path, out_dir):
    #load input files
    with open(yolo_path, 'r') as json_file:
        yolo_data = json.load(json_file)
    with open(yaml_path, 'r') as yaml_file:
        yaml_data = yaml.safe_load(yaml_file)
        class_ids = yaml_data.get('names', {})
    img_dims = pd.read_csv(csv_path)
    
    #convert each entry
    converted_json = [convert_entry(entry, class_ids, img_dims) for entry in yolo_data]

    #save as JSON
    json_output_path = os.path.join(out_dir, "converted_data.json")
    with open(json_output_path, "w") as json_output_file:
        json.dump(converted_json, json_output_file, indent=2)

    #also save as text file
    text_output_path = os.path.join(out_dir, "converted_data.txt")
    with open(text_output_path, "w") as text_output_file:
        text_output_file.write(json.dumps(converted_json, indent=2))

    print(f"Converted data saved to {json_output_path} and {text_output_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert YOLO predictions to Njobvu-AI input format")
    parser.add_argument("--yolo_path", type=str)
    parser.add_argument("--yaml_path", type=str)
    parser.add_argument("--csv_path", type=str)
    parser.add_argument("--output_dir", type=str)
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.yolo_path, args.yaml_path, args.csv_path, args.out_dir)
