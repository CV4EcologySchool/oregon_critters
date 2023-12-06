## Script to create a FiftyOne dataset from YOLO predictions using a JSON file

#you do not need to be in the dataset_dir to run this script
#make sure dataset_dir has the following structure:
#/demo2/data: where the images are stored (subfolders are OK)
#/demo2/labels.json: the json file
#note the "file_name" field in the "images" directory in labels.json should have paths relative to /demo2/data/ (e.g., "file_name": "COA_2020/.../images/filename.JPG")

import fiftyone as fo

def main(args):
    dataset_dir = args.dir
    name = args.name

    #name = 'demo2_test_full6'
    #dataset_dir = '/Volumes/Cara_cam_traps/CV4E/data_cleaned/demo2/'

    # Create the dataset
    dataset = fo.Dataset.from_dir(
        dataset_dir=dataset_dir,
        dataset_type=fo.types.COCODetectionDataset,
        label_field='predictions', #this is what to call it, not what it's called in the JSON
        name = name
    )

    # Create separate ground truth dataset
    dataset2 = fo.Dataset.from_dir(
        data_path=dataset_dir + 'data/',
        labels_path=dataset_dir + 'labels_gt.json',
        dataset_type=fo.types.COCODetectionDataset,
        label_field='ground_truth',
    )

    # Merge the two
    dataset.merge_samples(dataset2)

    # Launch app
    session = fo.launch_app(dataset)
    session.wait()

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--dir", type=str, default='.')
    parser.add_argument("--name", type=str, default="test_dataset")
    args = parser.parse_args()
    main(args)