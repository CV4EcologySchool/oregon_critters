
# Run YOLO predictions as if in command line

import subprocess
import argparse
import os
import wandb

# Parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to run predictions from trained YOLOv8 model")
    parser.add_argument("model", type=str, help="Path and name to pretrained model (.pt file)")
    parser.add_argument("name", type=str, help="Provide a name for the model run")
    parser.add_argument("--source", type=str, default='image_list.txt', help="Image list text file")
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--conf", type=int, default=0.1, help="Confidence threshold")
    parser.add_argument("--iou", type=int, default=0.7, help="IOU threshold for bboxes")
    parser.add_argument("--device", type=str, default=0, help="Device(s), e.g., 0 or 1 or 'cpu' or 'mps'")
    parser.add_argument("--save_imgs", action='store_true', help="Save images with predicted boxes?")
    parser.add_argument("--save_txt", action='store_true', help="Save individual text files with predictions?")

    return parser.parse_args()

# Define main function
def main():

    #Parse arguments
    args = parse_args()

    #Start wandb run
    wandb.init(entity = 'oregon-critters',
	       project = 'COA_predictions',
               name = args.name,
               save_code = True,
               job_type = 'pred')

    #Locate image input list
    with open(args.source, 'r') as file:
        filepaths = [line.strip() for line in file]

    n_images = len(filepaths)
    print(f'Generating predictions on {n_images} images...')

    #Run predictions on each file separately (skipping if there's an error)
    for file in filepaths:
        try:
            command = f"yolo predict model={args.model} source={file} save_txt={args.save_txt} save={args.save_imgs} save_conf={args.save_txt} imgsz={args.imgsz} iou={args.iou} conf={args.conf} device={args.device}"
            result = subprocess.run(command, shell=True, check=True, capture_output=True)
            print(f"Processed image: {file}")
        except Exception as e:
            print(f"Skipping image {file}: {e}")

    print("Predictions complete", result.stdout.decode())
    print("Results text files saved to runs/detect/predictX")

if __name__ == "__main__":
    main()