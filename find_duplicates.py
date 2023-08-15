import os
import glob
import numpy as np
import matplotlib.pyplot as plt

# get all your images and find how many duplicate names you have
print("Hello")

folders = [dir for dir in os.listdir("../data/") if os.path.isdir(f"../data/{dir}")]

file_names = []
full_names = []

for folder in folders:
    filenames = os.listdir(f"../data/{folder}/images")
    #imagenames = filenames.sort
    imagenames = [f"{folder}/{name}" for name in filenames]
    file_names.append(filenames)
    full_names.append(imagenames)

    #print(filenames[0:5])
    #print(imagenames[0:5])


file_names = [i for sublist in file_names for i in sublist]
file_names = np.array(file_names)

full_names = [j for sublist in full_names for j in sublist]
full_names = np.array(full_names)

print(file_names.shape)
print(np.unique(file_names).shape)

uniq_names, uniq_in, uniq_counts = np.unique(file_names, return_index = True, return_counts = True)

dup_ind = np.where(uniq_counts > 1)[0]
dup_names = uniq_names[dup_ind]

bool_index = [True if file_name in dup_names else False for file_name in file_names]

full_names_dups = np.array(full_names)[bool_index]

np.savetxt('dup_names', full_names_dups, delimiter=',', fmt='%s')

# indices = np.where(file_names[])

print("Done")
print(full_names_dups)
