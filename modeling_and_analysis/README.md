Order of methods:

1. atmo_correction_comparison.R: compare ACOLITE vs. SeaDAS by seeing how well each can predict Secchi depth
2. l8_vs_s2_comparison.R: compare Landsat-8 and Sentinel-2 Secchi depths to see how well they agree (and if separate algorithm adjustments are needed)
3. modeling.R: once decision made about atmospheric correction software and modeling decisions, can create regional algorithm adjustments via simple empirical corrections
4. boostrapping.R: test robustness of model coefficients via bootstrapping
5. temporal_analysis: analyze combined satellite (with regional empirical adjustments applied) and in situ time series
6. insitu_WQ_analysis: analyze in situ water quality parameters
7. supplementary_averaging_comparisons: compare satellite Secchi depth estimates at 10 m resolution (Sentinel-2 only), 30 m resolution (Landsat-8 and Sentinel-2), and 90 m resolution (Landsat-8 and Sentinel-2) where there are in situ match-ups. Purpose was to see whether spatial averaging decisions affected match-ups (they did not).
8. By spatial averaging decisions, we mean using a 3x3 box vs. single pixel for match-ups, and down-scaling S2 resolution to L8. 
