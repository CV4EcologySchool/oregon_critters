#train yolo model

import ultralytics

#first cd into location where yaml is stored
#make sure wandb is connected
#start a screen (e.g., screen -S cara_train_1)
#and activate environment (conda activate cara_cv4e)

##then run:
#yolo detect train data=dataset_ground.yaml model=yolov8n.pt epochs=100 imgsz=640 name=3_mc_ground

##for single-class, add:
#single_cls=True

##to select specific GPU:
#CUDA_VISIBLE_DEVICES=6 yolo detect train ...