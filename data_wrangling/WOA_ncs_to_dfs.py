###########################################################################
#
# Converting downloaded WOA data to useable format
# Author: Tormey Reimer
# Date: 2024-08-27
# This script consolidates the data already downloaded from WOA by 
# downloading_WOA.R and converts it into useful files.
#
###########################################################################

# Setting up
import xarray as xr
import pandas as pd
# import numpy as np
import datetime as dt
import glob
import dask
import os
import netCDF4 as nc
from dask.distributed import Client
client = Client(threads_per_worker=1)

def get_absolute_paths(filenames, directory):
    absolute_paths = []
    for filename in filenames:
        absolute_path = os.path.abspath(os.path.join(directory, filename))
        absolute_paths.append(absolute_path)
    return absolute_paths

# Get all files within the WOA_data directory
WOA_path = "FishMIP_Input_Explorer/example_data/WOA_data"
WOA_files = get_absolute_paths(os.listdir(WOA_path), WOA_path)

# Seperate by variable
WOA_temp = [k for k in WOA_files if 'decav81B0_t' in k]
WOA_sali = [k for k in WOA_files if 'decav81B0_s' in k]

WOA_temp = WOA_temp[:3] # Just for now while the downloads are incomplete

# Open and combine files
temp = xr.open_mfdataset(WOA_temp, engine = "h5netcdf", decode_times = False)

# Fix time coordinate variable
units, reference_date = temp.time.attrs['units'].split('since')
temp['time'] = pd.date_range(start = reference_date, periods = temp.sizes['time'], freq='MS')

# Isolate mean decadal monthly temperature
df_temp = temp['t_an']

# Have a look
# df_temp

# For grouping by depth and saving
# df_temp = df_temp.chunk({'time': 12, 'depth': 2, 'lat': 720, 'lon': 1440})
# for dep, group in df_temp.groupby('depth'):
#     group.to_parquet(WOA_path + '/' + 'WOA_temperature_decav_monthly_' + f'{dep}.parquet', 
#                      mode = 'w+') # Overwrites existing files

df = pd.read_csv("FishMIP_Input_Explorer/example_data/FishMIP_regions_bbox.csv")

for i in range(36):
    df_1 = df_temp.where(
        (df.iloc[i].xmin < df_temp.lon) & (df_temp.lon < df.iloc[i].xmax) & 
        (df.iloc[i].ymin < df_temp.lat) & (df_temp.lat < df.iloc[i].ymax), 
        drop=True
    ).to_dataframe()
    filename = f'{df.iloc[i].region.replace("i'i", "ii")}.parquet'
    filename = f'{df.iloc[i].region.replace(" ", "_")}.parquet'
    df_1.to_parquet(WOA_path + '/' + 'WOA_temperature_decav_monthly_' + ) # Overwrites existing files
