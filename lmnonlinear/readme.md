Functions and methods via:
Glover, D. M., Doney, S. C., & Jenkins, W. J. (Eds.). (2011). Least squares and regression techniques, goodness of fit and tests, and nonlinear least squares techniques. In Modeling Methods for Marine Science (pp. 49–74). Cambridge University Press; Cambridge Core. https://doi.org/10.1017/CBO9780511975721.004

MATLAB code written by Sarah Lang (slang@uri.edu)

Modifies Lee et al., 2016 Landsat-8 Secchi depth algorithm (https://doi.org/10.1016/j.rse.2016.02.033) by adjusting free parameters

Note: Please use good judgement when implementing this procedure. Procedure might push parameters well outside the plausible range for the original parameters that were developed from mechanistic models of the underwater light field. Use caution, especially if free parameters are shifted a degree of magnitude. 

For example, in our case, we found that adjustments for Sentinel-2 were more realistic than Landsat-8. This could be an alternate procedure used in future remote sensing studies to evaluate the applicability of the Lee et al. 2016 model for Sentinel-2.
