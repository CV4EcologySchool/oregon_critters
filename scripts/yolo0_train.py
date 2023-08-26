#train yolo model

import ultralytics

yolo detect train data=dataset_ground.yaml model=yolov8n.pt epochs=100 imgsz=640