# Train YOLO model

from ultralytics import YOLO
import wandb

# Start new wandb run
wandb.init(
    project = 'test_name',         #set name
    config={                       #track hyperparameters/metadata
        "dataset":"COA_demo"
        "architecture":"YOLOv8"
    }
)

# Load pretrained model
model = YOLO('yolov8n.pt') 

# Train 
results = model.train(data = 'dataset_demo.yaml', 
                      epochs = 100, 
                      imgsz = 640)

# results = model.train(data = DATASET, 
#                       epochs = 100, 
#                       imgsz = 640, 
#                       name = NAME)

print('finished training')