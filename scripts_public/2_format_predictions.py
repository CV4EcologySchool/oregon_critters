## Convert YOLO predictions CSV to COCO json format (used to create a FiftyOne dataset or visualize with 2_prep/6_visualize_boxes.ipynb)
## -- can also be used for Timelapse?

import pandas as pd
import json
import argparse
import os
import ast
import datetime
from PIL import Image

# Define function to get image dimensions
def get_image_dimensions(filepath):
    with Image.open(filepath) as img:
        return img.size
    
# Define function to extract image birthtime (*adds to runtime*)
def get_birthtime(filepath):
    birth_time = os.stat(filepath).st_birthtime
    birth_time_date = datetime.datetime.fromtimestamp(birth_time)
    return(birth_time_date)

# Define function to format predicted labels
def format_preds(pred_data):

    #read as dataframe and extract image name
    pred_df = pd.DataFrame(pred_data)
    pred_df['image_name'] = pred_df['path'].apply(lambda x: os.path.splitext(os.path.basename(x))[0])

    #separate predicted box dimensions into individual columns (now handles NAs for empty images)
    pred_df['box_coord'] = pred_df['box_coord'].apply(lambda x: pd.NA if pd.isna(x) else ast.literal_eval(x) if isinstance(x, str) else pd.NA)
    pred_df[['x_center', 'y_center', 'w_pred', 'h_pred']] = pred_df['box_coord'].apply(pd.Series)

    #convert coordinates from center of box to top left origin
    pred_df['x_pred'] = pred_df['x_center'] - (pred_df['w_pred'] / 2)
    pred_df['y_pred'] = pred_df['y_center'] - (pred_df['h_pred'] / 2)

    #convert NAs to 'None' for JSON format
    pred_df = pred_df.where(pd.notna(pred_df), None)

    return(pred_df)


# Define function to convert predictions to COCO
def convert_to_coco(preds_labels, class_ids):

    # Create empty COCO dictionaries
    coco_data = {
        "info": {},
        "licenses": {},
        "categories": [],
        "images": [],
        "annotations": []
    }

    # Format 'categories' dictionary
    class_ids['id'] = range(len(class_ids))
    class_ids['supercategory'] = 'object' 
    coco_data['categories'] = class_ids.to_dict('records')

    # Initialize counters
    image_id_counter = 1
    annotation_id_counter = 1

    # Keep track of unique image_ids
    unique_image_ids = set()
    
    #need this still? it was for merging before but might be useful
    preds_labels['image_name'] = preds_labels['image_name'] + '.JPG' 

    # For each entry (row), extract 'images' and 'annotations' info
    for pred_index, pred_row in preds_labels.iterrows():

        #extract image dimensions and datetime
        dimensions = get_image_dimensions(pred_row.path)
        birthtime = get_birthtime(pred_row.path)

        # Add image info (first check if it has been added already)
        if pred_row['image_name'] not in unique_image_ids:
            coco_data["images"].append({
                "id": image_id_counter,
                "file_name": pred_row.path.split('data/')[1],
                "width": int(dimensions[0]),
                "height": int(dimensions[1]),
                "date_captured": str(birthtime),
            })

            # Increment counters and mark image_id as complete
            image_id_counter += 1
            unique_image_ids.add(pred_row['image_name'])

        # Add annotation info (only if image has a predicted box)
        if pred_row['class_name'] == 'empty':
            coco_data["annotations"].append(None)   
        else:
            coco_data["annotations"].append({
                "id": annotation_id_counter,
                "image_id": image_id_counter - 1,
                "name": pred_row["class_name"],
                "category_id": int(pred_row["class_id"]),
                "bbox": [pred_row['x_pred'], pred_row['y_pred'], pred_row['w_pred'], pred_row['h_pred']],
                "area": float(pred_row['w_pred']*pred_row['h_pred']),
                "score": pred_row["confidence"],
                "iscrowd": 0,
            })
        #remove empty annotation entries
        coco_data["annotations"] = [ann for ann in coco_data['annotations'] if ann is not None]

        #Increment counters
        annotation_id_counter += 1
    
    return coco_data

    
# Define function to convert prediction to Njobvu TXT input 
def convert_to_njobvu(entry):

    class_id = entry["class_id"]
    class_name = entry["class_name"]

    #extract filepath and dimensions
    filepath = entry['path']
    dimensions = get_image_dimensions(filepath)

    njobvu_data = {
        "frame_id": 1,
        "filename": filepath.split('data/')[1],
        "objects": []
    }

    # Add object info (only if image has a predicted box)
    if  entry['class_name'] == 'empty':
        njobvu_data["objects"].append(None)   
    else:
        njobvu_data["objects"].append({
                "class_id": int(class_id),
                "name": class_name,
                "image_width": int(dimensions[0]),
                "image_height": int(dimensions[1]),
                "relative_coordinates": {
                    "center_x": entry["x_center"] / int(dimensions[0]), #check math here
                    "center_y": entry["y_center"] / int(dimensions[1]),
                    "width": entry["w_pred"] / int(dimensions[0]),
                    "height": entry["h_pred"] / int(dimensions[1]),
                },
                "confidence": float(entry["confidence"])
            })
    #remove empty annotation entries
    njobvu_data["objects"] = [ann for ann in njobvu_data['objects'] if ann is not None]

    return njobvu_data


# Main
def main(preds_path):
    
    #load predictions and format them
    pred_csv = pd.read_csv(preds_path, low_memory=False)
    pred_labels = format_preds(pred_csv)
    
    #load classes (looks for classes.csv in current directory)
    if os.path.exists('classes.csv'):
        class_ids = pd.read_csv('classes.csv', header=None, names=['name'])
    else:
        print('classes.csv not found')

    ## Convert to COCO format and save as JSON
    coco_data = convert_to_coco(pred_labels, class_ids)
    coco_output_path = os.path.join(args.out_dir, "labels.json")
    with open(coco_output_path, "w") as coco_output_file:
        json.dump(coco_data, coco_output_file, indent=4)

    ## Convert to NJOBVU format and save as TXT
    yolo_data = pred_labels
    yolo_data['dict_entry'] = yolo_data.apply(convert_to_njobvu, axis=1)
    dict_entries = yolo_data['dict_entry'].tolist() #convert dictionary entries column to a list for saving

    text_output_path = os.path.join(args.out_dir, "labels_for_njobvu.txt")
    with open(text_output_path, 'w') as txt_file:
        txt_file.write("[\n")  
        for i, entry in enumerate(dict_entries):
            txt_file.write(json.dumps(entry, indent=2))  
            if i < len(dict_entries) - 1:
                txt_file.write(',\n')  
            else:
                txt_file.write('\n')  
        txt_file.write("]\n") 

    ##Save NJOBVU output as JSON (not necessary)
    # json_output_path = os.path.join(out_dir, "labels_for_njobvu.json")
    # with open(json_output_path, 'w') as json_file:
    #     json.dump(dict_entries, json_file)

    print(f"Converted data saved to {coco_output_path} and {text_output_path}")    


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert YOLO predictions to JSON and TXT files")
    parser.add_argument("preds_path", type=str, help="Path to predictions output, e.g. 'pred_both35_both_predictions.csv")
    parser.add_argument("--out_dir", type=str, default='.', help="Desired directory for outputs")
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.preds_path)

