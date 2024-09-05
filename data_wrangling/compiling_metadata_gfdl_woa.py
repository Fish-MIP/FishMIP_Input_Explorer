#!/usr/bin/env python3

# Compiling metadata for GFDL outputs in a single file
# Author: Denisse Fierro Arcos
# Date: 2024-09-05
# 
# This script collects metadata for all GFDL-MOM6-COBALT2 files and creates
# a data frame

# Load libraries
import netCDF4
import os
from glob import glob
import pandas as pd

#Define variables and attributes that should be ignored
not_var = ['time', 'lon', 'lat', 'lev', 'lev2']
not_att = ['_FillValue', 'missing_value', 'coordinates', 'grid_mapping', 'cell_methods']

#Define base directory for GFDL files
base_dir = '/g/data/vf71/fishmip_inputs/ISIMIP3a/global_inputs/obsclim/025deg'
#Get a list of all files containing monthly ESM outputs (depth is excluded)
list_files = glob(os.path.join(base_dir, '*monthly*.nc'))
#Create empty dictionary to store metadata
var_dict = {}
#Get metadata for all GFDL files
for f in list_files:
    net = netCDF4.Dataset(f, 'r')
    [var] = [i for i in net.variables.keys() if i not in not_var]
    var_dict[var] = {}
    net = net.variables[var]
    attrs = [i for i in net.ncattrs() if i not in not_att]
    for a in attrs:
        var_dict[var].update({a: net.getncattr(a)})

#Create a dataframe from dictionary
f_out = '/g/data/vf71/la6889/FishMIP_Input_Explorer/Masks_netcdf_csv/gfdl_var_keys.csv'
var_df = pd.DataFrame(var_dict).transpose().reset_index(names = 'short_name')
var_df.to_csv(f_out, index = False)

#Getting list of WOA files - Selecting only one averaging period as all shared metadata
list_files = glob('/g/data/vf71/WOA_data/global/*/*00*.nc')
#Create empty dictionary to store metadata
woa_dict = {}
#Get metadata for all GFDL files
for f in list_files:
    net = netCDF4.Dataset(f, 'r')
    #Only two variables are used in the shiny app
    [var] = [i for i in net.variables.keys() if i in ['t_an', 's_an']]
    woa_dict[var] = {}
    net = net.variables[var]
    attrs = [i for i in net.ncattrs() if i not in not_att]
    for a in attrs:
        woa_dict[var].update({a: net.getncattr(a)})

#Create a dataframe from dictionary
f_out = '/g/data/vf71/la6889/FishMIP_Input_Explorer/Masks_netcdf_csv/woa_var_keys.csv'
woa_df = pd.DataFrame(woa_dict).transpose().reset_index(names = 'short_name')
woa_df.to_csv(f_out, index = False)
