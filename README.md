# Downscaling Sentinel-3 data
The provided code in R is designed to downscale Sentinel-3 Land Surface Temperature (LST) data using Sentinel-2 Normalized Difference Vegetation Index (NDVI) data through an Area-to-Area CoKriging approach. The study area for this project is a region above Venice, Italy. 


![marina+map](https://github.com/user-attachments/assets/dead698b-6fd1-466f-8930-489fe8f15b6d)

_study area for the project_


Here's a summary of the key steps and components of the code:

**Library Imports**: 
The code imports several libraries necessary for data handling, geostatistical analysis, and visualization, including raster, terra, ggplot2, sp, gstat, and atakrig.

**Data Import**:

The NDVI and LST raster files are loaded using their respective file paths.
The desired resolution for the downscaled LST is defined.

**Data Preparation**:

The raster files are converted to dataframes for easier manipulation and analysis.
Some initial statistics and descriptions of the rasters are printed to understand the data better.

**Geostatistical Analysis**:

The code likely involves creating variograms, which are essential for understanding the spatial relationships within the data.
The Area-to-Area CoKriging method is employed to downscale the LST data, utilizing the NDVI data to improve the resolution while preserving the spatial nature of the data.
Techniques to handle anomalies and improve the prediction accuracy are applied.

**Results and Validation**:

The final downscaled data is validated to ensure the method's effectiveness in improving spatial resolution without losing the statistical integrity of the original data.
Overall, the code facilitates a detailed geostatistical approach to enhance the resolution of LST data using correlated NDVI data, specifically tailored for an area above Venice, Italy.
