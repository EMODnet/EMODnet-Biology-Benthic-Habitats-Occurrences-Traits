import os, sys
import pandas as pd

# Read data from wfs request
dataf = pd.read_csv("{{cookiecutter.data_wfs_request}}")

# Save data in raw_data directory
dataf.to_csv(os.path.join(os.getcwd(), "data", "raw_data",'data.csv'), encoding='utf-8')

# Check if file was downloaded.
try:
    path = os.path.join(os.getcwd(), "data", "raw_data", "data.csv")
    f = open(path)
except IOError:
    raise FileNotFoundError("An error has occured. Please send Traceback to bio@emodnet.eu")
else: 
    print("Data successfully downloaded from https://emodnet-biology.eu/ in: " + path)



