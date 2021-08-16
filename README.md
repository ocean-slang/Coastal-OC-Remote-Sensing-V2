# Modeling Coastal Water Clarity Using Landsat-8 and Sentinel-2
Tools and resources for ocean color remote sensing of the Virginia Coast Reserve. Can be modified for your water body of interest. Folder contains code for analysis and modeling, resources, code for processing Level-1 Landsat-8/Sentinel-2 images, etc.

Link to paper: 

Overview of workflow:
1. Download Level-1 Landsat-8 (Collection 1) and Sentinel-2
2. Process with l2gen in NASA SeaDAS or REMSEM ACOLITE to get Level-2 images
3. Use Level-2 NetCDF images as inputs to MATLAB code. Code extracts data from images and implements ocean color algorithms (eg. Secchi disk depth, Zsd)
4. Code (R) for modeling and data analysis available. Empirical fit used to improve satellite Secchi depth estimates

Credits:
bash code for l2gen written by Sarah Lang, with help from NASA OC forum
https://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?tid=8666 
https://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?tid=8810 
https://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?tid=8654 

MATLAB code written by Kelly Luis, modified by Sarah Lang

R code written by Sarah Lang
