function boxedP = extractC_default(parameter, slat, slon, coord)
% Description: This function extracts a 3x3 box centered on coordinates
% of interest
%
% Input:
% parameter = Level 2 product parameter matrix (i.e. Chlorophyll) 
% slat = Sensor latitude matrix
% slon = Sensor longitude matrix
% coord = Coordinates of interest [Latitude Longitude]
%
% Output:
% boxedP = Array of 3x3 box of output parameter
% 
% Toolboxes:
% None required
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for site = 1:length(coord) 
            if isnan(coord(site)) 
                boxedP(:,site) = nan;
            else
                index = knnsearch(coord(site,1), coord(site,2), [slon slat]);
                [row(site), col(site)] = ind2sub(size(parameter), index); 
                boxedP(:,site) = reshape(parameter((row(site)-1:row(site)+1),(col(site)-1:col(site)+1)),9,1);
            end
    end

end

