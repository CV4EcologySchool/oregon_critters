# Computes YOLOv8 model performance on test set
# Takes CSV from "yolo_2_predict.py", formats output, and compares with ground-truth labels

import os
import pandas as pd
import json
import argparse
import ast

# define function to format prediction labels
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


# define function to format ground truth labels
def format_gt(gt_data):
    
    #read in true bounding boxes, keep only columns we need, and remove '.JPG' from image name
    true_df = gt_data[['full_path','CName', 'X','Y','W','H', 'cam_type', 'day_night', 'project', 'group', 'img_width', 'img_height', 'sequence_id']]
    true_df['image_name'] = true_df['full_path'].apply(lambda path: os.path.splitext(os.path.basename(path))[0])
 
    #rename columns
    true_df.rename(columns={"CName": "class_true", "X":"x_gt", "Y":"y_gt", "W":"w_gt", "H":"h_gt"}, inplace=True)
    
    return(true_df)


#define function to calculate intersection over union (IOU) of predicted and true boxes
def calculate_iou(x1, y1, w1, h1, x2, y2, w2, h2):

    # Define bboxes
    box1 = [x1, y1, w1, h1]
    box2 = [x2, y2, w2, h2]

    # Convert bounding box coordinates to the format (x1, y1, x2, y2)
    box1 = [box1[0], box1[1], box1[0] + box1[2], box1[1] + box1[3]]
    box2 = [box2[0], box2[1], box2[0] + box2[2], box2[1] + box2[3]]

    # Calculate the coordinates of the intersection rectangle
    x_intersection = max(box1[0], box2[0])
    y_intersection = max(box1[1], box2[1])
    w_intersection = min(box1[2], box2[2]) - x_intersection
    h_intersection = min(box1[3], box2[3]) - y_intersection

    # Calculate the area of intersection
    area_intersection = max(0, w_intersection) * max(0, h_intersection)

    # Calculate the area of the union
    area_box1 = (box1[2] - box1[0]) * (box1[3] - box1[1])
    area_box2 = (box2[2] - box2[0]) * (box2[3] - box2[1])
    area_union = area_box1 + area_box2 - area_intersection

    # Calculate IoU
    iou = area_intersection / area_union

    return iou 


#define function to compare predicted boxes and ground truth
def compare_boxes(pred_data, gt_data):
    
    thresh = 0.5 #set IoU threshold for matches
    results_list = [] #initialize list to store results

    #iterate over predicted boxes 
    for pred_index, pred_row in pred_data.iterrows(): 

        pred_image_name = pred_row['image_name'] 
        matching_rows = gt_data[gt_data['image_name'] == pred_image_name] #find boxes in matching image
        
        #iterate over true boxes on same image
        for true_index, true_row in matching_rows.iterrows(): 
                
            #call function to calculate IoU 
            iou_result = calculate_iou(pred_row['x_pred'], pred_row['y_pred'], pred_row['w_pred'], pred_row['h_pred'],
                                    true_row['x_gt'], true_row['y_gt'], true_row['w_gt'], true_row['h_gt'])
            
            #if predicted and true boxes overlap, call it a match
            if iou_result > thresh:
                match = True
                true_class = true_row['class_true']
                
            else: 
                match = False
                true_class = 'background'

            #append a row for this predicted box
            results_list.append({'pred_image': pred_image_name,
                                 'pred_box_index': pred_index,
                                 'pred_class': pred_row['class_name'],
                                 'confidence': pred_row['confidence'], 
                                 'match': match, 
                                 'true_box_index': true_index, 
                                 'true_class': true_class, 
                                 'iou': iou_result,
                                 'pred_box': [pred_row['x_pred'], pred_row['y_pred'], pred_row['w_pred'], pred_row['h_pred']],
                                 'gt_box': [true_row['x_gt'], true_row['y_gt'], true_row['w_gt'], true_row['h_gt']]})
            
    #create dataframe from the results list
    results_df = pd.DataFrame(results_list)

    return results_df


# define main 
def main(preds_csv, groundtruth_csv, out_dir):

    #load ground truth labels and format them
    gt_labels = pd.read_csv(groundtruth_csv, low_memory=False)
    gt_labels = format_gt(gt_labels)

    #load predicted labels and format them
    pred_labels = pd.read_csv(preds_csv, low_memory=False)
    pred_labels = format_preds(pred_labels)
    
    #compare true and predicted boxes for overlap
    label_matches = compare_boxes(pred_labels, gt_labels)

    #now compare for class matches
    label_matches['class_match'] = label_matches['pred_class'] == label_matches['true_class']
    label_matches.head()

    #add metadata back
    results_df_merged = label_matches.merge(gt_labels[['image_name', 'full_path', 'sequence_id', 'cam_type', 'project', 'day_night', 'group']], left_on = 'pred_image', right_on = 'image_name', how = 'left')

    #now get false negatives by extracting indices of all true boxes that weren't matched
    false_neg = gt_labels.query('index not in @label_matches.true_box_index')
    false_neg.head(11)

    #save evaluation results as CSV
    results_path = os.path.join(out_dir, "test_set_evaluation.csv")
    results_df_merged.to_csv(results_path)
    
    #save false negatives list as CSV
    false_neg_path = os.path.join(out_dir, "test_set_false_negatives.csv")
    false_neg.to_csv(false_neg_path)

    print(f"Output saved to {results_path} and {false_neg_path}")


# run main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Evaluate YOLOv8 model on 'test' set")
    parser.add_argument("--preds_csv", type=str, help="Path to predictions output, e.g. 'pred_both35_both_predictions.csv")
    parser.add_argument("--groundtruth_csv", type=str, help="Path to ground truth labels, e.g. 'both.csv'")
    parser.add_argument("--out_dir", type=str)
    args = parser.parse_args()

    # Call the main function with the provided arguments
    main(args.preds_csv, args.groundtruth_csv, args.out_dir)
