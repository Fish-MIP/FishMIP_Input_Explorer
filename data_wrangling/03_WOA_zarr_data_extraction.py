#!/usr/bin/env python3

# Extracting WOA data using FishMIP regional model boundaries
# Author: Denisse Fierro Arcos
# Date: 2024-09-04


import xarray as xr
import zarr
import os
from glob import glob
import pandas as pd
import numpy as np
from datetime import datetime
import geopandas as gpd
import rioxarray

#Defining functions
def mask_boolean_ard_data(file_path, boolean_mask_ras):
    '''
    Open netCDF files in analysis ready data (ARD) format. That is apply chunks
    that make data analysis easier.
    
    Inputs:
    file_path (character): Full file path where netCDF file is stored.
    boolean_mask_ras (boolean data array): Data array to be used as initial mask
    to decrease the size of the original dataset. This mask makes no distinction
    between regional models, it simply identifies grid cells within regional 
    model boundaries with the value of 1.
    
    Outputs:
    da (data array): ARD data array containing data only for grid cells within
    regional model boundaries.
    '''

    #Getting chunks from gridded mask to apply it to model data array
    [lat_chunk] = np.unique(boolean_mask_ras.chunksizes['lat'])
    [lon_chunk] = np.unique(boolean_mask_ras.chunksizes['lon'])
    
    #Open data array
    da = xr.open_zarr(file_path)
    [var] = list(da.keys())
    da = da[var]
    #Ensure chunks are the same as mask
    if 'month' in da.dims:
        da = da.chunk({'month': 12, 'depth': 57, 'lat': lat_chunk, 'lon': lon_chunk})
        #Make sure months are in correct format
        da['month'] = pd.date_range(start = '2010-01-01', periods = 12, 
                                    freq = 'MS').strftime('%B')
    else:
        da = da.chunk({'depth': 57, 'lat': lat_chunk, 'lon': lon_chunk})
        
    #Apply mask for all regions to decrease dataset size
    da = da.where(boolean_mask_ras == 1)
    
    #Add spatial information to dataset
    da.rio.set_spatial_dims(x_dim = 'lon', y_dim = 'lat', inplace = True)
    da.rio.write_crs('epsg:4326', inplace = True)

    return da

def mask_ard_data(ard_da, shp_mask, file_out):
    '''
    Open netCDF files in analysis ready data (ARD) format. That is apply chunks
    that make data analysis easier.
    
    Inputs:
    ard_da (data array): Analysis ready data (ARD) data array produced by the 
    function "open_ard_data"
    shp_mask (shapefile): Shapefile containing the boundaries of regional models
    file_out (character): Full file path where masked data should be stored.
    
    Outputs:
    No data is returned, but masked file will be stored in specified file path.
    '''

    #Clip data using regional shapefile
    da_mask = ard_da.rio.clip(shp_mask.geometry, shp_mask.crs, drop = True, 
                              all_touched = True)
    #Remove spatial information
    da_mask = da_mask.drop_vars('crs')
    da_mask.encoding = {}

    #Check file extension included in path to save data
    if file_out.endswith('zarr'):
        for i, c in enumerate(da_mask.chunks):
            if len(c) > 1 and len(set(c)) > 1:
                print(f'Rechunking {file_out}.')
                print(f'Dimension "{da_mask.dims[i]}" has unequal chunks.')
                da_mask = da_mask.chunk({da_mask.dims[i]: '200MB'})
        da_mask.to_zarr(file_out, consolidated = True, mode = 'w')
    if file_out.endswith('parquet'):
        #Keep data array attributes to be recorded in final data frame
        da_attrs = ard_da.attrs
        da_attrs = pd.DataFrame([da_attrs])
        ind_wider = ['lat', 'lon', 'depth', 'vals']
        #Turn extracted data into data frame and remove rows with NA values
        df = da_mask.to_series().to_frame().reset_index().dropna()
        #Changing column name to standardise across variables
        df = df.rename(columns = {ard_da.name: 'vals'}).reset_index(drop = True)
        #Reorganise data
        df = df[ind_wider]
        #Include original dataset attributes
        df = pd.concat([df, da_attrs], axis = 1)
        #Saving data frame
        df.to_parquet(file_out)

#Loading FishMIP regional models shapefile
rmes = gpd.read_file('/g/data/vf71/shared_resources/FishMIP_regional_models/FishMIP_regional_models.shp')

#Loading FishMIP regional models gridded mask
mask_ras = xr.open_dataarray(os.path.join('/g/data/vf71/shared_resources/FishMIPMasks/',\
        'merged_regional_fishmip/gfdl-mom6-cobalt2_areacello_15arcmin_fishMIP_regional_merged.nc'))
#Rechunking data to make it more manageable
mask_ras = mask_ras.chunk({'lat': 144, 'lon': 288})

# Defining path to WOA_data directory
WOA_zarr = glob('/g/data/vf71/WOA_data/global/*.zarr')

#Define (or create) folders where outputs will be stored
base_out_clim = '/g/data/vf71/WOA_data/regional/climatology'
os.makedirs(base_out_clim, exist_ok = True)
base_out_month = '/g/data/vf71/WOA_data/regional/monthly'
os.makedirs(base_out_month, exist_ok = True)

#Applying functions to WOA files
for f in WOA_zarr:
    #Open data array as ARD
    da = mask_boolean_ard_data(f, mask_ras)   
    
    #Create full file path
    if 'month' in f:
         #Adding output folder to create full file path
        full_file_out = os.path.join(base_out_month, os.path.basename(f))
    else:
        full_file_out = os.path.join(base_out_clim,
                                     os.path.basename(f).replace('zarr', 'parquet'))

    #Extract data for each region included in the regional mask
    for i in rmes.region:
        #Get polygon for each region
        mask = rmes[rmes.region == i]
        #Get name of region and clean it for use in output file
        reg_name = mask['region'].values[0].lower().replace(" ", "-").replace("'", "")
        #File name out - Replacing "global" for region name
        file_out = full_file_out.replace('woa23_', f'woa23_{reg_name}_')
        #Extract data and save masked data - but only if file does not already exist
        if os.path.isdir(file_out) | os.path.isfile(file_out):
            continue
        mask_ard_data(da, mask, file_out)
