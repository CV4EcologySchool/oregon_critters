# Generate predictions from trained YOLOv8 model

from ultralytics import YOLO
import argparse
import os
import pandas as pd
import wandb

# Parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to run predictions from trained YOLOv8 model")

    #required (positional) arguments
    parser.add_argument("model", type=str, help="Path and name to pretrained model (.pt file)")
    parser.add_argument("name", type=str, help="Provide a name for the model run")
   
    #optional arguments
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--conf", type=int, default=0.1, help="Confidence threshold")
    parser.add_argument("--iou", type=int, default=0.7, help="IOU threshold for bboxes")
    parser.add_argument("--device", type=str, default=0, help="Device(s), e.g., 0 or 1 or 'cpu' or 'mps'")
    parser.add_argument("--save_imgs", action='store_true', help="Save images with predicted boxes?")
    parser.add_argument("--save_txt", action='store_true', help="Save individual text files with predictions?")

    return parser.parse_args()


# Define function to list all image files in '/data/' folder (subdirectories okay)
def list_files(image_dir):
    file_paths = []
    for root, _, files in os.walk(image_dir):
        for file in files:
            if not file.startswith('.'): #ignore hidden files (e.g., .DS_Store)
                # file_path = os.path.join(root, file) #for absolute file paths
                # file_paths.append(relative_path)
                relative_path = os.path.relpath(os.path.join(root, file), 'data') #for relative file paths
                file_paths.append(f"./{'data'}/{relative_path}")
    
    #save image list as text file
    txt_output_path = os.path.join(os.getcwd(), "image_list.txt")
    with open(txt_output_path, 'w') as file:
        for path in file_paths:
            file.write(path + '\n')
    print('Created "image_list.txt"')
    return file_paths


# Define function to check if predictions file already exists (if so, append a suffix)
def generate_preds_filename(base_filename):
    if not os.path.exists(base_filename):
        return base_filename
    suffix = 1
    while True:
        new_filename = f"{os.path.splitext(base_filename)[0]}_{suffix}.csv"
        if not os.path.exists(new_filename):
            return new_filename
        suffix += 1


# Define function to run predictions
def main():

    #parse args
    args = parse_args()

    #start wandb run
    wandb.init(entity = 'oregon-critters',
	       project = 'COA_predictions',
               name = args.name,
               save_code = True,
               job_type = 'val')
    
    #load the trained model (.pt file)
    model = YOLO(args.model)

    #create the image list, unless one already exists
    if os.path.exists('image_list.txt'):
        with open('image_list.txt', 'r') as f:
            print('Existing "image_list.txt" file found')
            filepaths = [line.strip() for line in f] #load existing file, if found
    else:
        image_dir = os.path.join(os.getcwd(), 'data/')
        filepaths = list_files(image_dir)     

    n_images = len(filepaths)
    print(f"Generating predictions on {n_images} images...")

    #run predictions
    results = model.predict(filepaths,
                            stream=True, 
                            save=args.save_imgs,
                            save_conf=args.save_imgs,
                            save_txt=args.save_txt, 
                            imgsz=args.imgsz, 
                            conf=args.conf, 
                            iou=args.iou, 
                            # device = 'mps',
                            device=args.device)

    #convert results to dataframe (code from lonnylundsten on github.com/ultralytics/ultralytics/issues/2143)
    list = []
    for result in results:
            boxes = result.boxes.cpu().numpy()
            for box in boxes:
                cls = int(box.cls[0])
                path = result.path
                class_name = model.names[cls]
                conf = int(box.conf[0]*100)
                bx = box.xywh.tolist()
                df = pd.DataFrame({'path': path,'class_name': class_name, 'class_id': cls, 'confidence': conf, 'box_coord': bx})
                list.append(df)
    results_df = pd.concat(list) 

    #add empty images to dataframe:

    #turn empties onto a df
    empty_df = pd.DataFrame({'path': filepaths, 'class_name': 'empty', 'class_id': None, 'confidence': None, 'box_coord': None}, index=filepaths)
    #find only the ones not already in results df
    filtered_empties = empty_df[~empty_df['path'].isin(results_df['path'])]
    #now add those to results_df
    df = pd.concat([results_df, filtered_empties], ignore_index=True, sort=False)

    #save predictions as CSV
    csv_output = "predictions.csv"
    csv_output_path = generate_preds_filename(csv_output) #if 'predictions.csv' exists, create a new filename
    df.to_csv(csv_output_path, index=False)

    print(f"Saved predictions to {csv_output_path} in current directory")

if __name__ == "__main__":
    main()
