# Train YOLO model

from ultralytics import YOLO
import wandb
import argparse

# With sys:
#import sys

# Parse arguments
#DATASET = sys.argv[1]
#NAME = sys.argv[2]

# With argparse:
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to train YOLOv8 model using ultralytics")

    #set required (positional) arguments
    parser.add_argument("data", type=str, help="Path and name of dataset.yaml file")
    parser.add_argument("model", type=str, help="Pretrained model name, e.g., yolov8n.pt")
    parser.add_argument("name", type=str, help="Provide a name for the model run")

    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--epochs", type=int, default=100, help="Number of training epochs")
    #parser.add_arguments("--devices")

    return parser.parse_args()

# Now set up training
def main():

    #Parse args
    args = parse_args()

    # Start new wandb run
    wandb.init(project = 'test_name')         #set name for wandb project

    # Load pretrained model
    model = YOLO(args.model)

    # Train 
    results = model.train(data = args.data, 
                          epochs = args.epochs, 
                          imgsz = args.imgsz, 
                          name = args.name)

if __name__ == "__main__":
    main()

