###########################################################################
#
# Converting downloaded WOA data to useable format
# Author: Tormey Reimer
# Date: 2024-08-27
# Edited by: Denisse Fierro Arcos
# Edited on: 2024-09-04
# 
# This script consolidates the data already downloaded from WOA by the
# downloading_WOA.R script and converts it into cloud-friendly zarr files.
#
###########################################################################

# Calling relevant libraries
import xarray as xr
import zarr
from dask.distributed import Client
import pandas as pd
import datetime as dt
from glob import glob
import os

# Starting a cluster
client = Client(threads_per_worker = 1)

# Defining function to save netCDF files to zarr files
def netcdf_to_zarr(list_files, var_name, file_out):
    '''
    Inputs:
    list_files (character list) Full paths to WOA files that need to be 
    merged and save as a single zarr collection. All files must contain 
    the same variable of interest.
    var_name (character) Name of variable to be saved as zarr file.
    file_out (character) Full file path where zarr file will be saved.

    Outputs:
    None. This function saves a zarr file to disk.
    '''
    da = xr.open_mfdataset(list_files, decode_times = False)[var_name]
    
    # Fix time coordinate variable - Keep name of month only
    units, reference_date = da.time.attrs['units'].split('since')
    da['time'] = pd.date_range(start = reference_date, periods = da.sizes['time'], 
                               freq = 'MS').strftime('%B')
    da = da.rename({'time': 'month'})
    
    #Rechunk data 
    da_rechunk = da.chunk({'month': 12, 'depth': 57, 'lat': 120, 'lon': 240})

    #Save data, but ensure directory exists
    out_dir = os.path.dirname(file_out)
    if not os.path.exists(out_dir):
        os.makedirs(out_dir)
    da_rechunk.to_zarr(file_out, consolidated = True, mode = 'w')

# Defining path to WOA_data directory
WOA_path = '/g/data/vf71/WOA_data/global/'

# Save WOA temperature files as zarr files
WOA_temp = sorted(glob(os.path.join(WOA_path, '*/*_t*.nc')))
temp_out = os.path.join(WOA_path, 'woa23_month_clim_mean_temp_1981-2010.zarr')
netcdf_to_zarr(WOA_temp, 't_an', temp_out)

# Save WOA salinity files as zarr files
WOA_sal = sorted(glob(os.path.join(WOA_path, '*/*_s*.nc')))
sal_out = os.path.join(WOA_path, 'woa23_month_clim_mean_sal_1981-2010.zarr')
netcdf_to_zarr(WOA_sal, 's_an', sal_out)

