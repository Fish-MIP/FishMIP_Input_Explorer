###########################################################################
#
# Converting downloaded WOA data to useable format
# Author: Tormey Reimer
# Date: 2024-08-27
# This script consolidates the zarr data and converts it into parquets.
#
###########################################################################

import xarray as xr
import pandas as pd
import datetime as dt
import dask
import os
import zarr

# Defines region bounding boxes
df = pd.read_csv("example_data/FishMIP_regions_bbox.csv")

df_temp = xarray.open_zarr("example_data/WOA_data/WOA_temperature.zarr")

# For grouping by depth and saving (no longer in use)
# df_temp = df_temp.chunk({'time': 12, 'depth': 2, 'lat': 720, 'lon': 1440})
# for dep, group in df_temp.groupby('depth'):
#     group.to_parquet(WOA_path + '/' + 'WOA_temperature_decav_monthly_' + f'{dep}.parquet', 
#                      mode = 'w+') # Overwrites existing files

# For grouping by region and saving
for i in range(36):
    df_1 = df_temp.where(
        (df.iloc[i].xmin < df_temp.lon) & (df_temp.lon < df.iloc[i].xmax) & 
        (df.iloc[i].ymin < df_temp.lat) & (df_temp.lat < df.iloc[i].ymax), 
        drop=True
    ).to_dataframe()
    filename = f'{df.iloc[i].region.replace("i'i", "ii")}.parquet'
    filename = f'{df.iloc[i].region.replace(" ", "_")}.parquet'
    df_1.to_parquet(WOA_path + 'WOA_temperature_decav81B0_monthly_' + filename) # Overwrites existing files

df_temp = xarray.open_zarr("example_data/WOA_data/WOA_salinity.zarr")

# Save by region
for i in range(36):
    df_1 = df_temp.where(
        (df.iloc[i].xmin < df_temp.lon) & (df_temp.lon < df.iloc[i].xmax) &
        (df.iloc[i].ymin < df_temp.lat) & (df_temp.lat < df.iloc[i].ymax),
        drop=True
    ).to_dataframe()
    filename = f'{df.iloc[i].region.replace("i'i", "ii")}.parquet'
    filename = f'{df.iloc[i].region.replace(" ", "_")}.parquet'
    df_1.to_parquet(WOA_path + 'WOA_salinity_decav81B0_monthly_' + filename) # Overwrites existing files
