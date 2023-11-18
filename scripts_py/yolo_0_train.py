#!/raid1/home/fw/appelc/local/envs/yolov8/bin/python3

# Train YOLO model

from ultralytics import YOLO
import wandb
import argparse

# Arguments
def parse_args():
    parser = argparse.ArgumentParser(description = "Script to train YOLOv8 model using ultralytics")

    #set required (positional) arguments
    parser.add_argument("data", type=str, help="Path and name of dataset.yaml file")
    parser.add_argument("model", type=str, help="Pretrained model name, e.g., yolov8n.pt")
    #parser.add_argument("project", type=str, help="Project name")
    parser.add_argument("name", type=str, help="Provide a name for the model run")

    #set optional arguments and their defaults
    parser.add_argument("--imgsz", type=int, default=640, help="Image dimensions, e.g., 320 or 640")
    parser.add_argument("--batch", type=int, default=16, help="Batch size, e.g., 16 or 32")
    parser.add_argument("--epochs", type=int, default=100, help="Number of training epochs")
    parser.add_argument("--device", type=int, default=0, help="Device(s), e.g. 0, [0,1], or cpu")
    #parser.add_argument("--single_cls", type=int, default=False, help="To train as single-class, use True")

    return parser.parse_args()

# Now set up training
def main():

    #Parse args
    args = parse_args()

    # Start new wandb run
    wandb.init(entity = 'oregon-critters',
               project = 'demo_train',
	       name = args.name,
               save_code = True,
               job_type = 'train',
	       config={
	           #"learning_rate": lr,
                   "batch_size": args.batch,
                   "epochs": args.epochs,
               })

    # Load pretrained model
    model = YOLO(args.model)

    # Train 
    results = model.train(data = args.data, 
                          epochs = args.epochs, 
                          imgsz = args.imgsz, 
			  batch = args.batch,
                          name = args.name,
                          device = args.device)

if __name__ == "__main__":
    main()

