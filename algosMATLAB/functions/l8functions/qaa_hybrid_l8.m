function [a, bb, apg, bbp, id_ref] = qaa_hybrid_l8(Rrs, wl)
% qaa_hybrid_l8-Computes absorption and backscattering properties according to Lee et al.(2002) 
%
% Syntax: [a, bb, apg, bbp, id_ref] = qaa_hybrid_l8(Rrs, wl)
%
% Inputs:
%    Rrs - Remote sensing reflectance (sr^-1) from Landsat 8
%    wl - Wavelengths corresponding to Landsat 8 Rrs
%
% Outputs:
%    a - Total absorption (1/m) from QAAhybrid (Lee et al. 2002)
%    bb - Total backscattering (1/m) from QAAhybrid (Lee et al. 2002)
%    apg - Absorption by pighments (1/m) from QAAhybrid (Lee et al. 2002)
%    bbp - Backscattering by particles (1/m) from QAAhybrid (Lee et al. 2002)
%    id_ref - Reference wavelength (nm) used for QAAhybrid (Lee et al. 2002)
%
% Examples: 
%    Example 1: All qaa_hybrid_l8 output 
%    [a, bb, apg, bbp, id_ref] = qaa_hybrid_l8(Rrs, wl)
%    
%    Example 2: Absorption and backscattering coefficients qaa_hybrid_l8 output 
%    [a, bb, ~, ~, ~] = qaa_hybrid_l8(Rrs, wl)
%
% Other m-files required: qaa_hybrid_l8.m and h20_iops.m
% Subfunctions: none
% MAT-files required: none
% See also: none
%
% Author: Kelly Luis
% Email: kelly.luis001@umb.edu or m11keluis@gmail.com
% Website: http://www.github.com/m11keluis
% March 8, 2019
% ************************************************************************

% Find L8 wavelengths closest to 443, 490, 555, and 670
id443 = find(abs(wl-443)==min(abs(wl-443)));   % Band 1: 443nm
id490 = find(abs(wl-482)==min(abs(wl-482)));   % Band 2: 482nm 
id555 = find(abs(wl-554)==min(abs(wl-554)));   % Band 3: 554nm
id670 = find(abs(wl-655)==min(abs(wl-655)));   % Band 4: 655nm
id754 = find(abs(wl-754)==min(abs(wl-754)));
id779 = find(abs(wl-779)==min(abs(wl-779)));

% Replace Rrs(670) with Rrs(655), by Wei. 
Rrs(id670) = Rrs(id670).*0.762+(4.52e-5);


% ******************** aw & bbw, of pure sea water, refer to Lee et al.(2016), RSE; **********
aw  = [0.005 0.011 0.064 0.368];
bbw = [0.0021 0.0014 0.0008 0.0004];

%% Step 1: Calculate subsurface remote sensing reflectance 
rrs = Rrs./(0.52+1.7*Rrs);

g0 = 0.089;
g1 = 0.125;
u  = (-g0 + (g0^2 + 4*g1*rrs).^0.5)/(2*g1);

%% Step 2: Determine Reference Wavelength
    if Rrs(id670) < 0.0015

        wl_ref = wl(id555);
        id_ref = id555;

        rrs_ref = rrs(id_ref);

        ki = log10((rrs(id443)+rrs(id490))/(rrs(id_ref) + 5*rrs(id670)*rrs(id670)/rrs(id490)));
        a_ref = aw(3) + 10.^(-1.146 - 1.366 * ki - 0.469*ki.^2);

    else
        wl_ref = 670;
        id_ref = id670;
        a_ref = aw(4) + 0.39 * (rrs(id670)/(rrs(id443) + rrs(id490)))^1.14;
    end


%% Step 3: Backscattering by Particles at Reference Wavelength
bbp_ref = u(id_ref).*a_ref/(1-u(id_ref)) - bbw(3);

%% Step 4
Y = 2.0*(1-1.2*exp(-0.9*rrs(id443)/rrs(id555)));

%% Step 5 & Step 6: Use reference information to caluclate IOPs
    for i = 1 : length(wl)
       bbp(1,i) = bbp_ref *(wl_ref/wl(i))^Y;
       a(1,i)   = (1-u(i)).*(bbw(i) + bbp(i))./u(i);      
    end

% Total Backscattering
bb = bbp + bbw;

apg = a - aw; %h2o_iops(wl,'a');


end


