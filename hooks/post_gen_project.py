import os, sys
import pandas as pd

# Read data from wfs request
dataf = pd.read_csv("{{cookiecutter.data_wfs_request}}")

# Get path to file
path = os.path.join(os.getcwd(), "data", "raw_data",'data.csv')

# Save data in raw_data directory
dataf.to_csv(path, encoding='utf-8')

# Check if file was downloaded.
try:
    f = open(path)
except IOError:
    raise FileNotFoundError("An error has occured while downloading the data. Please send Traceback to bio@emodnet.eu")
else: 
    print("Data successfully downloaded from https://emodnet-biology.eu/ in: " + path)
