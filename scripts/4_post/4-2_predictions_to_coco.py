## Convert YOLO predictions json to COCO json format (used to create a FiftyOne dataset or visualize with 2_prep/6_visualize_boxes.ipynb)

## turn this into a .py

import pandas as pd
import json
import yaml
import argparse
import os

# Define conversion function
def convert_to_coco(original_json, class_ids, img_metadata):
    coco_data = {
        "info": {},
        "licenses": {},
        "categories": [],
        "images": [],
        "annotations": [],
        "ground_truth": []
    }

    # Add 'categories' dictionary
    for class_id, class_name in class_ids.items():
        coco_data["categories"].append({
            "id": int(class_id),
            "name": class_name,
            "supercategory": "object"
        })

    # Create a mapping dictionary for categories
    category_name_to_id = {category["name"]: category["id"] for category in coco_data["categories"]}

    # Initialize counters
    image_id_counter = 1
    annotation_id_counter = 1

    # Keep track of unique image_ids
    unique_image_ids = set()

    # For each entry, extract 'images' and 'annotations' info
    for entry in original_json:
        # image_filename = f"/data/{entry['image_id']}.JPG" #for filename only
        # image_filename = '/' + img_metadata.loc[img_metadata['image_name'] == entry['image_id'] + ".JPG", 'full_path'].values[0] #data/ and path
        image_filename = img_metadata.loc[img_metadata['image_name'] == entry['image_id'] + ".JPG", 'full_path'].values[0].split('data/')[1] #just path

        # Convert bbox coordinates from center origin to top-left origin
        x_center, y_center, width, height = entry['bbox']
        # x1 = float(x_center - width/2)
        # y1 = float(y_center - height/2) #actually don't need to adjust them... why does YOLO have it right this time?!
        area = float(width*height)

        # Add image info (first check if it has been added already)
        if entry['image_id'] not in unique_image_ids:
            coco_data["images"].append({
                "id": image_id_counter,
                "file_name": image_filename,
                "width": int(img_metadata.loc[img_metadata['image_name'] == entry['image_id'] + ".JPG", 'img_width'].values[0]),
                "height": int(img_metadata.loc[img_metadata['image_name'] == entry['image_id'] + ".JPG", 'img_height'].values[0]),
                "date_captured": img_metadata.loc[img_metadata['image_name'] == entry['image_id'] + ".JPG", 'datetime'].values[0],
            })

            # Increment counters and mark image_id as complete
            image_id_counter += 1
            unique_image_ids.add(entry['image_id'])

        # Add annotation info
        coco_data["annotations"].append({
            "id": annotation_id_counter,
            "image_id": image_id_counter - 1,
            "category_id": entry["category_id"],
            # "bbox": [x1, y1, width, height], #if adjusted above
            "bbox": [x_center, y_center, width, height],
            "area": area,
            "score": entry["score"],
            "iscrowd": 0,
        })

        #Increment counters
        annotation_id_counter += 1

    # Create a mapping dictionary for images
    image_id_mapping = {image['id']: image['file_name'] for image in coco_data["images"]}

    # Add ground truth info (for each row in metadata dataframe)
    ground_truth_id_counter = 1

    for _, ground_truth_entry in img_metadata.iterrows():
        # file_name = f"{image_name}.JPG" #just filename
        # file_name = '/' + ground_truth_entry['full_path'] #data/and path
        file_name = ground_truth_entry['full_path'].split('data/')[1] #just path

        for image_id, mapped_file_name in image_id_mapping.items():
             if file_name == mapped_file_name:
                  category_id = category_name_to_id.get(ground_truth_entry.get("species", ""), None)

                  if category_id is not None:
                      coco_data["ground_truth"].append({
                          "id": ground_truth_id_counter,
                          "image_id": image_id,
                          "name": ground_truth_entry.get("species", ""),
                          "category_id": category_id,
                          "bbox": [float(ground_truth_entry.get('X', 0)), float(ground_truth_entry.get('Y', 0)),
                                   float(ground_truth_entry.get('W', 0)), float(ground_truth_entry.get('H', 0))], #will need to do some math on these
                      })

                      #Increment counter
                      ground_truth_id_counter += 1
    
    return coco_data


def main(yolo_path, yaml_path, csv_path, out_dir):
    #load files
    with open(yolo_path, 'r') as json_file:
        yolo_data = json.load(json_file)
    with open(yaml_path, 'r') as yaml_file:
        yaml_data = yaml.safe_load(yaml_file)
        class_ids = yaml_data.get('names', {})
    img_metadata = pd.read_csv(csv_path)

    #convert to COCO format
    coco_data = convert_to_coco(yolo_data, class_ids, img_metadata)

    #output the COCO format to a JSON file
    json_output_path = os.path.join(out_dir, "labels.json")
    with open(json_output_path, "w") as json_output_file:
        json.dump(coco_data, json_output_file, indent=4)

    #also save a version with just the "ground_truth" annotations
    json_gt_output_path = os.path.join(out_dir, "labels_gt.json")
    coco_data_gt = coco_data
    if 'annotations' in coco_data_gt:
        del coco_data_gt['annotations']
        coco_data_gt['annotations'] = coco_data_gt.pop('ground_truth')
    with open(json_gt_output_path, "w") as json_output_file_gt:
        json.dump(coco_data_gt, json_output_file_gt, indent=4)

    print(f"Converted data saved to {json_output_path} and {json_gt_output_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert YOLO predictions to COCO format")
    parser.add_argument("--yolo_path", type=str)
    parser.add_argument("--yaml_path", type=str)
    parser.add_argument("--csv_path", type=str)
    parser.add_argument("--out_dir", type=str)
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.yolo_path, args.yaml_path, args.csv_path, args.out_dir)

