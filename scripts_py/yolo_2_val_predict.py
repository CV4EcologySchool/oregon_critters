# Run predictions from trained YOLO model

from ultralytics import YOLO
import wandb
import argparse
import os
import json

# With argparse:
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to run predictions from trained YOLOv8 model")

    #set required (positional) arguments
    parser.add_argument("model", type=str, help="Path and name to pretrained model")
    #parser.add_argument("data", type=str, help="Path and name of dataset.yaml file")
    parser.add_argument("image_list", type=str, help="Path and name of text file with images, e.g., 'val.txt'")
    parser.add_argument("name", type=str, help="Provide a name for the model run")
   
    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--conf", type=int, default=0.1, help="Confidence threshold")
    parser.add_argument("--iou", type=int, default=0.7, help="IOU threshold for bboxes")
    parser.add_argument("--device", type=int, default=0, help="Device(s), e.g., 0 or cpu")

    return parser.parse_args()

# Now set up predictions
def main():

    #parse args
    args = parse_args()

    #start wandb run
    wandb.init(entity = 'oregon-critters', 
	       project = 'YOLOv8_full',
               name = args.name,
               save_code = True,
               job_type = 'pred',
               config={
                       "conf": args.conf,
                       "iou": args.iou,
                       "model": args.model,
                   })

    #load my trained model
    model = YOLO(args.model) #e.g., runs/detect/7_mc_ground_med/weights/best.pt

    #import images
    with open(args.image_list, 'r') as f:
        image_paths = f.read().splitlines()

    #run predictions
    results = model.predict(image_paths, save=True, save_conf=True, imgsz=args.imgsz, save_txt=True, conf=args.conf, iou=args.iou, device = args.device)

    #save as json
    #results_json = results[0].tojson()
    results_json = json.loads(results[0].tojson())    

    results_numpy = results[0].numpy()

    #testing code from UL github to convert results to CSV (issue 2143)
    list = []
    for result in results:
        boxes =

    print(results_numpy)

    cwd = os.getcwd()
    output_path = os.path.join(cwd, args.name + "_predictions.json")

    print("Saved predictions to:")
    print(output_path)

    with open(output_path, "w") as output_file:
        json.dump(results_json, output_file, indent=2)

if __name__ == "__main__":
    main()
