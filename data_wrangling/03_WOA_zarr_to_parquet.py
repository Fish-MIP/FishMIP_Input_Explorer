###########################################################################
#
# Converting downloaded WOA data to useable format
# Author: Tormey Reimer
# Date: 2024-08-27
# This script turns the cloud-friendly zarr data into shiny-friendly 
# parquets files.
#
###########################################################################

import xarray as xr
import pandas as pd
import datetime as dt
import dask
import os
import zarr
import math

WOA_path = "example_data/WOA_data/"

# Defines region bounding boxes
df = pd.read_csv("example_data/FishMIP_regions_bbox.csv")

df_temp = xr.open_zarr(WOA_path + "WOA_temperature.zarr")

# For grouping by region and saving.
# Some lines are commented out because removing Nan values here does not make the files smaller.
for i in range(36):
    df_1 = df_temp.where(
        (df.iloc[i].xmin < df_temp.lon) & (df_temp.lon < df.iloc[i].xmax) &
        (df.iloc[i].ymin < df_temp.lat) & (df_temp.lat < df.iloc[i].ymax),
        drop=True
    ).to_dataframe().reset_index()
    df_1 = pd.melt(df_1, id_vars = ['time', 'depth', 'lat', 'lon'], value_vars = 't_an', var_name = 'variable', value_name = 'value').set_index(['time', 'depth', 'lat', 'lon'])
    # df_2 = df_1[df_1['value'].notnull()]
    filename = f'{df.iloc[i].region}'
    filename = filename.replace("i'i", "ii").replace(" ", "_")
    df_1.to_parquet(WOA_path + 'WOA_temperature_decav81B0_monthly_' + filename + '.parquet') # Overwrites existing files
    # df_2.to_parquet(WOA_path + 'WOA_temperature_decav81B0_monthly_' + filename + '_NoNans.parquet') # Overwrites existing files


df_temp = xr.open_zarr("example_data/WOA_data/WOA_salinity.zarr")

# Save by region
for i in range(36):
    df_1 = df_temp.where(
        (df.iloc[i].xmin < df_temp.lon) & (df_temp.lon < df.iloc[i].xmax) &
        (df.iloc[i].ymin < df_temp.lat) & (df_temp.lat < df.iloc[i].ymax),
        drop=True
    ).to_dataframe().reset_index()
    df_1 = pd.melt(df_1, id_vars = ['time', 'depth', 'lat', 'lon'], value_vars = 's_an', var_name = 'variable', value_name = 'value').set_index(['time', 'depth', 'lat', 'lon'])
    # df_2 = df_1[df_1['value'].notnull()]
    filename = f'{df.iloc[i].region}'
    filename = filename.replace("i'i", "ii").replace(" ", "_")
    df_1.to_parquet(WOA_path + 'WOA_salinity_decav81B0_monthly_' + filename + '.parquet') # Overwrites existing files
    # df_2.to_parquet(WOA_path + 'WOA_salinity_decav81B0_monthly_' + filename + '_NoNans.parquet') # Overwrites existing files

