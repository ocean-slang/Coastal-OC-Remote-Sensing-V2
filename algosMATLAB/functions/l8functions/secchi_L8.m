function [Kdminwl, Secchi, Kdmin, maxRrs] = secchi_L8(Rrs, Kd, wl)
% Landsat8 Secchi Disk Depth 
% Input: (All input as row vectors)
% rrs = remote sensing reflectance
% kd = diffuse attenuation coefficient
% wl = wavelength
% Output: 
% secchi = secchi disk depth
% Citation: Lee et al.(2016),RSE
% Created: Feb-06-2018, by Kelly Luis, UMass Boston

% ************************************************************************
[Kdmin, Index] = min(Kd);%Default omit nan
maxRrs = max(Rrs);
Kdminwl = wl(Index); %Find the wavelength with the minimum kd

    if Kdminwl == 530 % use max rrs value in rrs vector for secchi if wl is 530
        Secchi = 1./(2.5.*Kdmin).*log(abs(0.14-maxRrs)./0.013);
    else
    Secchi = 1./(2.5.*Kdmin).*log(abs(0.14-Rrs(Index))./0.013);
    end
    
end
    
