#Run validation on trained yolo model

import wandb
from ultralytics import yolo

#Start a W&B run
wandb.init(project='yolo_0_val')

#Load the model
model = YOLO('/home/cara/oregon_critters/runs/detect/train15/weights/best.pt')

#Validate the model
metrics = model.val(data=dataset.yaml, save_json=True)  # no arguments needed, dataset and settings remembered
metrics.box.map    # map50-95
metrics.box.map50  # map50
metrics.box.map75  # map75
metrics.box.maps   # a list contains map50-95 of each category