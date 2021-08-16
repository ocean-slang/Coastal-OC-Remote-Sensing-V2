function [ LinFit, N, Rsq, RMSE, MAPD, bias] = validation_stats(derived_value,insitu_value)
% Statistics for Same Day Validation: Linear Fit, Number of Observations
% ..., Rsq, root mean square error, and mean absolute percent difference

LinFit = fitlm(derived_value,insitu_value);
N = LinFit.NumObservations;
Rsq = round(LinFit.Rsquared.Ordinary,2);
RMSE = round(sqrt(nanmean((derived_value-insitu_value(:)).^2)),2);
MAPD = round((nanmean((derived_value-insitu_value)./insitu_value).*100),2);
bias = sum(derived_value-insitu_value)./N;

end
