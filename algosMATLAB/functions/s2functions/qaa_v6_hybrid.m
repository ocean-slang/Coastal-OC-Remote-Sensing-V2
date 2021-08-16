function [a, bb, apg, bbp, id_ref] = qaa_v6_hybrid(Rrs, wl)
% qaa_v6_l8-Computes absorption and backscattering properties according to Lee et al.(2002) 
%
% Syntax: [a, bb, apg, bbp, id_ref] = qaa_v6_l8(Rrs, wl)
%
% Inputs:
%    Rrs - Remote sensing reflectance (sr^-1) from Landsat 8
%    wl - Wavelengths corresponding to Landsat 8 Rrs
%
% Outputs:
%    a - Total absorption (1/m) from QAAv6 (Lee et al. 2002)
%    bb - Total backscattering (1/m) from QAAv6 (Lee et al. 2002)
%    apg - Absorption by pighments (1/m) from QAAv6 (Lee et al. 2002)
%    bbp - Backscattering by particles (1/m) from QAAv6 (Lee et al. 2002)
%    id_ref - Reference wavelength (nm) used for QAAv6 (Lee et al. 2002)
%
% Examples: 
%    Example 1: All qaa_v6_hybrid output 
%    [a, bb, apg, bbp, id_ref] = qaa_v6_hybrid(Rrs, wl)
%    
%    Example 2: Absorption and backscattering coefficients qaa_v6_l8 output 
%    [a, bb, ~, ~, ~] = qaa_v6_hybrid(Rrs, wl)
%
% Other m-files required: qaa_v6_hybrid.m and h20_iops.m
% Subfunctions: none
% MAT-files required: none
% See also: none
%
% Author: Kelly Luis
% Email: kelly.luis001@umb.edu or m11keluis@gmail.com
% Website: http://www.github.com/m11keluis
% March 8, 2019
% ************************************************************************

% Find L8 wavelengths closest to 443, 490, 555, 670, etc.
id443 = find(abs(wl-443)==min(abs(wl-443)));   % Band 1: 443nm
id490 = find(abs(wl-482)==min(abs(wl-482)));   % Band 2: 482nm 
id555 = find(abs(wl-554)==min(abs(wl-554)));   % Band 3: 554nm
id670 = find(abs(wl-655)==min(abs(wl-655)));   % Band 4: 655nm
id704 = find(abs(wl-704)==min(abs(wl-704)));   % Band 5: 704nm
id740 = find(abs(wl-740)==min(abs(wl-740)));   % Band 6: 740nm
id783 = find(abs(wl-783)==min(abs(wl-783)));   % Band 7: 783nm

% Replace Rrs(670) with Rrs(655), by Wei. 
Rrs(id670) = Rrs(id670).*0.762+(4.52e-5);


% ******************** aw & bbw, refer to Lee et al.(2016), RSE; **********
aw = h2o_iops(wl,'a');
bbw= h2o_iops(wl,'b');

%% Step 1: Calculate subsurface remote sensing reflectance 
rrs = Rrs./(0.52+1.7*Rrs);

g0 = 0.089;
g1 = 0.125;
u  = (-g0 + (g0^2 + 4*g1*rrs).^0.5)/(2*g1);

%% Step 2: Determine Reference Wavelength
        wl_ref = wl(id740);
        id_ref = id740;
        rrs_ref = rrs(id_ref);
        a_ref = aw;
%% Step 3: Backscattering by Particles at Reference Wavelength
bbp_ref = u(id_ref).*a_ref/(1-u(id_ref)) - bbw(id_ref);

%% Step 4
beta=log10(u(id_ref)/u(id783));
Y = (-372.99*(beta^2))+(37.286*beta)+0.84;

%% Step 5 & Step 6: Use reference information to calculate IOPs
bbp = bbp_ref.*((wl_ref./wl).^Y);
a = (1-u).*(bbw + bbp)./u;

% for i = 1 : length(wl)
% 
%    %bbp(1,i) = bbp_ref*(wl_ref/wl(i))^Y;
%    bbp(1,i) = bbp_ref.*((wl_ref./wl(i)).^Y);
%    a(1,i) = (1-u(i)).*(bbw(i) + bbp(i))./u(i); 
% end

% Total Backscattering
bb = bbp + bbw;

apg = a - aw; %h2o_iops(wl,'a');


end


