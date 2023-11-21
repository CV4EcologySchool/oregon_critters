# Run predictions on validation set from trained YOLO model

from ultralytics import YOLO
import wandb
import argparse
import os
import json

# With argparse:
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to run predictions from trained YOLOv8 model on validation set")

    #set required (positional) arguments
    parser.add_argument("model", type=str, help="Path and name to pretrained model")
    #parser.add_argument("data", type=str, help="Path and name of dataset.yaml file")
    parser.add_argument("val_images", type=str, help="Path and name of val.txt file")
    parser.add_argument("name", type=str, help="Provide a name for the model run")
   
    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--conf", type=int, default=0.1, help="Confidence threshold")
    parser.add_argument("--iou", type=int, default=0.7, help="IOU threshold for bboxes")

    return parser.parse_args()

# Now set up predictions
def main():

    #parse args
    args = parse_args()

    #start wandb run
#    wandb.init(entity = 'oregon-critters', 
#	       project = 'demo_predict',
#               name = args.name,
#               save_code = True,
#               job_type = 'pred')

    #load my trained model
    model = YOLO(args.model) #e.g., runs/detect/7_mc_ground_med/weights/best.pt
                             #or see gruncrow: model = YOLO(get_best_model_weights("batch_name"))

    #import val images
    with open(args.val_images, 'r') as f:
        image_paths = f.read().splitlines()

    #run predictions
    results = model.predict(image_paths, save=True, save_conf=True, imgsz=args.imgsz, save_txt=True, conf=args.conf, iou=args.iou)

    #save as json
    results_json = results[0].tojson()

    cwd = os.getcwd()
    output_path = os.path.join(cwd, 'predictions.json')

    print(cwd)
    print(output_path)

    with open(output_path, "w") as output_file:
        json.dump(results_json, output_file, indent=4)

if __name__ == "__main__":
    main()
