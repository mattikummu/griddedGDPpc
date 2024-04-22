# Downscaled gridded global dataset for Gross Domestic Product (GDP) per capita at purchasing power parity (PPP) over 1990-2022

These codes were used to create the 'Downscaled gridded global dataset for Gross Domestic Product (GDP) per capita at purchasing power parity (PPP) over 1990-2022' that is published in Kummu et al (2024). Please do cite this paper if using the codes. 

Kummu et al. 2024. Downscaled gridded global dataset for Gross Domestic Product (GDP) per capita at purchasing power parity (PPP) over 1990-2022. Preprint (submitted to Scientific Data). 

The input data for the code (code_input_data.zip), as well as the resulted output data, is available at: 
http://doi.org/10.5281/zenodo.10976734

The code is numbered with the order it should be run. Below each code is briefly explained:
1_gdp_prepare_adm0.R: puts together admin 0 level data, interpolates and extrapolates the missing values
2_gdp_prep_adm1.R: puts together admin 1 level data, interpolates and extrapolates the missing values
3_gdp_prep_spatial.R: combines the admin 0 and admin 1 level to a global grid and gpkg file
4_downscaling_train.R: prepares the training data for downscaling model
Downscaling_Matlab_Codes (under this folder, with own readme file): downscaling admin 1 level data to admin 2 level
5_downscaling_predict.R: prepares the data for downscaling prediction to admin 2 level
6_gdpTotal.R: estimates the total GDP PPP, using admin 2 level GDP per capita (from 5_downscaling_predict.R) and population count data
7_gdp_plot_maps.R: script to plot maps shown in the manuscript
8_gdp_metadata_collect.R: puts together the metadata for the study
9_adm2_validation.R: validation of admin 2 level data

To run the code, please extract the folders in code_input_data.zip (available from http://doi.org/10.5281/zenodo.10976734) under the same folder with the code. 

For more information, please contact Matti Kummu (matti.kummu@aalto.fi)
