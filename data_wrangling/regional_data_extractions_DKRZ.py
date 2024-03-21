#!/usr/bin/env python3

#Extracting ESM data using regional model boundaries.
#Author: Denisse Fierro Arcos
#Last updated: 2024-02-14
#This script needs to be run whenever there is an update of regional boundaries
#or ESM outputs.

#Libraries
import xarray as xr
import os
import re
from glob import glob
import pandas as pd
import numpy as np
from datetime import datetime

#FishMIP region mask
mask_ras = xr.open_dataset('FishMIP_regional_mask_025deg.nc')
mask_ras = mask_ras.rename({'latitude': 'lat', \
                            'longitude': 'lon', \
                            'time': 'id'})

#FishMIP keys to interpret mask
regions = pd.read_csv('FishMIP_regions_keys.csv')

#Base folder containing Earth System Model (ESM) data
base_dir = '/work/bb0820/ISIMIP/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2'
#List only files at 0.25 deg resolution
list_files = glob(os.path.join(base_dir, "*15arcmin*"))

#Folder for outputs
base_out = 'regions'
os.makedirs(base_out, exist_ok = True)

#Loop through all files
for f in list_files:
  #Base file name out
  base_file_out = re.sub(".nc$", ".csv", os.path.split(f)[-1])
  
  #Open data array
  da = xr.open_dataarray(f)
  #Change format of time dimension
  new_time = da.indexes['time'].to_datetimeindex()
  #Showing as month_year
  new_time = [datetime.strftime(t, format = "%b_%Y") for t in new_time]
  da['time'] = new_time
  #Keep data array attributes to be recorded in final data frame
  da_attrs = da.attrs
  da_attrs = pd.DataFrame([da_attrs])
  #Checking if there is a depth dimension
  if len(da.dims) > 3:
    [depth_name] = [d for d in da.dims if d not in ['time', 'lat', 'lon']]
    #Changing depth dimension name
    da = da.rename({depth_name: 'depth_m'})
    ind_wider = ['lat', 'lon', 'depth_m']
  else:
    ind_wider = ['lat', 'lon']
  
  #Extract data for each region included in the regional mask
  for i in regions.index:
    #Get name of region
    reg_name = regions.loc[i, 'region'].lower().replace(" ", "-")
    #Get ID for region
    id_reg = regions.loc[i, 'id']
    #Load mask
    mask = mask_ras.region.sel(id = id_reg)
    #Check if depth dimension exists
    if 'depth_m' in da.dims:
      df_all_depths = []
      #Apply mask to each depth bin
      for dbin, databin in da.groupby('depth_m'):
        dmask = databin.where(np.isfinite(mask)).drop_vars('id')
        #Remove rows (latitude dimension) where all cells are NAs
        dmask = dmask.dropna(dim = 'lat', how = 'all')
        df_mask = dmask.to_series().to_frame().reset_index()
        df_mask['depth_m'] = dbin
        df_mask = df_mask.pivot(index = ind_wider, columns = 'time', 
                                values = dmask.name).reset_index()
        #Include original dataset attributes
        df_mask =  pd.concat([df_mask, da_attrs], axis = 1)
        df_all_depths.append(df_mask)
      #Concatenate all depth data frames
      df_all_depths = pd.concat(df_all_depths, axis = 0)
      #File name out - Replacing "global" for region name
      file_out = re.sub('global', reg_name, file_out)
      #Saving data frame
      df_all_depths.to_csv(os.path.join(base_out, file_out), index = False)
    #If depth is not present, then apply mask directly
    else:
      #Apply mask to ESM data
      da_mask = da.where(np.isfinite(mask)).drop_vars('id')
      #Remove rows (latitude dimension) where all cells are NAs
      da_mask = da_mask.dropna(dim = 'lat', how = 'all')
      #Turn extracted data into data frame
      df = da_mask.to_series().to_frame().reset_index().\
                    pivot(index = ind_wider, columns = 'time', 
                          values = da.name).reset_index()
      #Include original dataset attributes
      df = pd.concat([df, da_attrs], axis = 1)
      #File name out - Replacing "global" for region name
      file_out = re.sub("global", reg_name, base_file_out)
      #Saving data frame
      df.to_csv(os.path.join(base_out, file_out), index = False)
  

