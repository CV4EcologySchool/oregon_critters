# oregon_critters

A computer vision model for classifying camera trap images in western Oregon (created by Cara Appel)

Below are steps to run predictions on your images and view them using several options for external software.

### Requirements
This model uses python scripts. ...

### Workflow overview
1. Rename your images, if necessary
2. Set up directory with your images
3. Run predictions script
4. Run formatting script to generate inputs for various post-processing programs

#### File organization
These scripts use the following directory structure:
project / data (put your images here. subfolders are ok, e.g. site_1, site_2)
        / classes.csv
        / model_file.pt

Filenames must be unique within a folder, so rename them if you need to. I like the renaming workflow in the camtrapR package.

#### Post-processing options
1. Njobvu-AI (open-source browser-based tool)
2. FiftyOne (browser-based tool)
3. Timelapse (local software - PC only)
