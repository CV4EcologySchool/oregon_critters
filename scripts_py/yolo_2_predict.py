# Run predictions from trained YOLOv8 model

from ultralytics import YOLO
import wandb
import argparse
import os
import json
import pandas as pd

# With argparse:
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to run predictions from trained YOLOv8 model")

    #set required (positional) arguments
    parser.add_argument("model", type=str, help="Path and name to pretrained model")
    parser.add_argument("image_list", type=str, help="Path and name of text file with images, e.g., 'val.txt'")
    parser.add_argument("name", type=str, help="Provide a name for the model run")
   
    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--conf", type=int, default=0.1, help="Confidence threshold")
    parser.add_argument("--iou", type=int, default=0.7, help="IOU threshold for bboxes")
    parser.add_argument("--device", type=str, default=0, help="Device(s), e.g., 0 or 'cpu'")

    return parser.parse_args()

# Now set up predictions
def main():

    #parse args
    args = parse_args()

    #start wandb run
    wandb.init(entity = 'oregon-critters', 
	       project = 'YOLO_full',
               name = args.name,
               save_code = True,
               job_type = 'pred',
               config={
                       "conf": args.conf,
                       "iou": args.iou,
                       "model": args.model,
                   })

    #load the trained model
    model = YOLO(args.model) #e.g., runs/detect/7_mc_ground_med/weights/best.pt

    #run predictions
    results = model.predict(args.image_list,
                            stream=True, 
                            save=True, 
                            save_conf=True, 
                            imgsz=args.imgsz, 
                            save_txt=True, 
                            conf=args.conf, 
                            iou=args.iou, 
                            device = 'cpu')

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
    df = pd.concat(list)

    #save as CSV
    cwd = os.getcwd()
    output_path = os.path.join(cwd, args.name + "_predictions.csv")
    df.to_csv(output_path, index=False)

    print("Saved predictions to:")
    print(output_path)

if __name__ == "__main__":
    main()
