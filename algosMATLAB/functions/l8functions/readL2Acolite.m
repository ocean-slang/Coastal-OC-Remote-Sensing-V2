function struct = readL2(filename)
% Description: This function reads and formats date, coordinates, and
% geophysical parameter information 
%
% Input:
% filename: Level 2 product from SeaDAS; generally ends with '.L2_LAC_OC'
% 
% Output:
% struct = Matlab Structure with SeaDAS variables and metadata
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % Read and Format Date 
 %   year = unique(ncread(filename,'/scan_line_attributes/year'));
  % doy = unique(ncread(filename,'/scan_line_attributes/day'));
  %  struct.date = datetime(year,1,0) + caldays(doy);

    % Read Coordinates
    struct.Lat = ncread(filename,'lat');
    struct.Lon = ncread(filename,'lon');
    
    % Read Geophysical Parameters
    struct.rrs_443 = ncread(filename,'Rrs_443');
    struct.rrs_482 = ncread(filename,'Rrs_483');
    struct.rrs_561 = ncread(filename,'Rrs_561');
    struct.rrs_655 = ncread(filename,'Rrs_655');
   % struct.chl = ncread(filename,'/geophysical_data/chlor_a');
    struct.kd_490 = ncread(filename,'kd490_qaasw');
   % struct.aot_865 = ncread(filename,'/geophysical_data/aot_865');
   % struct.angstrom = ncread(filename,'/geophysical_data/angstrom');
    struct.l2_flags = ncread(filename,'l2_flags');
    
end

