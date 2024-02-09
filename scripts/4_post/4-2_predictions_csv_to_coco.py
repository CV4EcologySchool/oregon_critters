## Convert YOLO predictions CSV to COCO json format (used to create a FiftyOne dataset or visualize with 2_prep/6_visualize_boxes.ipynb)
## -- can also be used for Timelapse?

import pandas as pd
import json
import yaml
import argparse
import os
import ast

# define function to format predicted labels
def format_preds(pred_data):

    #read as dataframe and extract image name
    pred_df = pd.DataFrame(pred_data)
    pred_df['image_name'] = pred_df['path'].apply(lambda x: os.path.splitext(os.path.basename(x))[0])

    #separate predicted box dimensions into individual columns
    pred_df['box_coord'] = pred_df['box_coord'].apply(ast.literal_eval)
    pred_df[['x_center', 'y_center', 'w_pred', 'h_pred']] = pred_df['box_coord'].apply(pd.Series)

    #convert coordinates from center of box to top left origin
    pred_df['x_pred'] = pred_df['x_center'] - (pred_df['w_pred'] / 2)
    pred_df['y_pred'] = pred_df['y_center'] - (pred_df['h_pred'] / 2)

    return(pred_df)


# Define function to convert predictions to COCO (with ground-truth also)
def convert_to_coco(preds_labels, class_ids, img_metadata):

    # Read in dataframes
    img_metadata = pd.DataFrame(img_metadata)

    # Create empty COCO dictionaries
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

    # Merge metadata with predictions to get info needed below
    preds_labels['image_name'] = preds_labels['image_name'] + '.JPG'
    cols_to_merge = ['image_name','full_path','img_width','img_height','datetime']
    preds_labels_merged = pd.merge(preds_labels, img_metadata[cols_to_merge], on = 'image_name', how='left')

    # For each entry (row), extract 'images' and 'annotations' info
    for pred_index, pred_row in preds_labels_merged.iterrows():

        #get file path (everything after 'data/')
        filepath = pred_row.full_path.split('data/')[1]

        # Add image info (first check if it has been added already)
        if pred_row['image_name'] not in unique_image_ids:
            coco_data["images"].append({
                "id": image_id_counter,
                "file_name": filepath,
                "width": int(pred_row['img_width']),
                "height": int(pred_row['img_height']),
                "date_captured": pred_row['datetime'],
            })

            # Increment counters and mark image_id as complete
            image_id_counter += 1
            unique_image_ids.add(pred_row['image_name'])

        # Add annotation info
        coco_data["annotations"].append({
            "id": annotation_id_counter,
            "image_id": image_id_counter - 1,
            "name": pred_row["class_name"],
            "category_id": pred_row["class_id"],
            "bbox": [pred_row['x_pred'], pred_row['y_pred'], pred_row['w_pred'], pred_row['h_pred']],
            "area": float(pred_row['w_pred']*pred_row['h_pred']),
            "score": pred_row["confidence"],
            "iscrowd": 0,
        })

        #Increment counters
        annotation_id_counter += 1

    # Create a mapping dictionary for images
    image_id_mapping = {image['id']: image['file_name'] for image in coco_data["images"]}

    # Add ground truth info (for each row in metadata dataframe)
    ground_truth_id_counter = 1

    for _, ground_truth_entry in img_metadata.iterrows():
        file_name = ground_truth_entry['full_path'].split('data/')[1]

        for image_id, mapped_file_name in image_id_mapping.items():
             if file_name == mapped_file_name:
                  category_id = category_name_to_id.get(ground_truth_entry.get("CName", ""), None)

                  if category_id is not None:
                      coco_data["ground_truth"].append({
                          "id": ground_truth_id_counter,
                          "image_id": image_id,
                          "name": ground_truth_entry.get("CName", ""),
                          "category_id": category_id,
                          "bbox": [float(ground_truth_entry.get('X', 0)), float(ground_truth_entry.get('Y', 0)),
                                   float(ground_truth_entry.get('W', 0)), float(ground_truth_entry.get('H', 0))], #will need to do some math on these
                      })

                      #Increment counter
                      ground_truth_id_counter += 1
    
    return coco_data


def main(predictions_csv, yaml_path, groundtruth_csv, out_dir):
    
    #load predicted labels
    pred_csv = pd.read_csv(predictions_csv, low_memory=False)
    pred_labels = format_preds(pred_csv)
    
    #load yaml file
    with open(yaml_path, 'r') as yaml_file:
        yaml_data = yaml.safe_load(yaml_file)
        class_ids = yaml_data.get('names', {})

    #load metadata
    img_metadata = pd.read_csv(groundtruth_csv, low_memory=False) #won't determine column type by first rows (useful for NAs here)

    #convert predictions to COCO format
    coco_data = convert_to_coco(pred_labels, class_ids, img_metadata)

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
    parser.add_argument("predictions_csv", type=str, help="Path to predictions output, e.g. 'pred_both35_both_predictions.csv")
    parser.add_argument("yaml_path", type=str, help="Path to project yaml file with classes, e.g.,'dataset_both.yaml")
    parser.add_argument("groundtruth_csv", type=str, help="Path to ground truth labels, e.g. 'both.csv'")
    parser.add_argument("--out_dir", type=str, default='.',help="Desired directory for outputs")
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.predictions_csv, args.yaml_path, args.groundtruth_csv, args.out_dir)

