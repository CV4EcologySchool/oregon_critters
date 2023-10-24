# Validate YOLO model

from ultralytics import YOLO
import wandb
import argparse

def parse_args():
    parser = argparse.ArgumentParser(description = "Script to validate YOLOv8 model using ultralytics")

    #set required (positional) arguments
    parser.add_argument("model", type=str, help="Path and name to pretrained model")
    parser.add_argument("data", type=str, help="Path and name of dataset.yaml file")
   
    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")

    return parser.parse_args()
    
def main():

    #parse args
    args = parse_args()

    #start wandb run
    wandb.init(project = 'test_name')

    #load my model
    model = YOLO(args.model)  #e.g., runs/detect/7_mc_ground_med/weights/best.pt
                              #or see gruncrow: model = YOLO(get_best_model_weights("batch_name"))

    #validate
    metrics = model.val(data = args.data,
                        imgsz = args.imgsz,
                        save_json = True)

    metrics.box.map
    metrics.box.map50
    metrics.box.map75
    metrics.box.maps
    
if __name__ == "__main__":
    main()
