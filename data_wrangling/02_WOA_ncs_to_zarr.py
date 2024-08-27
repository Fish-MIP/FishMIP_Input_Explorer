###########################################################################
#
# Converting downloaded WOA data to useable format
# Author: Tormey Reimer
# Date: 2024-08-27
# This script consolidates the data already downloaded from WOA by the
# downloading_WOA.R script and converts it into cloud-friendly zarr files.
#
###########################################################################

# Setting up
import xarray as xr
import pandas as pd
import datetime as dt
import glob
import dask
import os
import netCDF4 as nc
import zarr
#from dask.distributed import Client
#client = Client(threads_per_worker=1)

def get_absolute_paths(filenames, directory):
    absolute_paths = []
    for filename in filenames:
        absolute_path = os.path.abspath(os.path.join(directory, filename))
        absolute_paths.append(absolute_path)
    return absolute_paths

# Get all files within the WOA_data directory
WOA_path = "example_data/WOA_data/"
WOA_files = get_absolute_paths(os.listdir(WOA_path), WOA_path)

# Seperate by variable
WOA_temp = [k for k in WOA_files if 'decav81B0_t' in k]
WOA_sali = [k for k in WOA_files if 'decav81B0_s' in k]

# Open and combine files
temp = xr.open_mfdataset(WOA_temp, engine = "netcdf4", decode_times = False)

# Fix time coordinate variable
units, reference_date = temp.time.attrs['units'].split('since')
temp['time'] = pd.date_range(start = reference_date, periods = temp.sizes['time'], freq='MS')

# Isolate mean decadal monthly temperature
df_temp = temp['t_an']
df_temp = df_temp.chunk({'time': 12, 'depth': 57, 'lat': 50, 'lon': 100})
df_temp.to_zarr("example_data/WOA_data/WOA_temperature.zarr", mode="w")

# Clear temp variable to free memory
temp = []

# Open and combine salinity files
temp = xr.open_mfdataset(WOA_sali, engine = "netcdf4", decode_times = False)

# Fix time coordinate variable
units, reference_date = temp.time.attrs['units'].split('since')
temp['time'] = pd.date_range(start = reference_date, periods = temp.sizes['time'], freq='MS')

# Isolate mean decadal monthly temperature
df_temp = temp['s_an']
df_temp = df_temp.chunk({'time': 12, 'depth': 57, 'lat': 50, 'lon': 100})
df_temp.to_zarr("example_data/WOA_data/WOA_salinity.zarr", mode="w")

