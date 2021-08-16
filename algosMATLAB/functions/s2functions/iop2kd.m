function kd =iop2kd(a,bb,bbw,sa)

    % iop2kd-Computes diffuse attenuation coefficient (Lee et al. 2013)
    %
    % Syntax: kd = iop2kd(a,bb,bbw,sa)
    %
    % Inputs:
    %    a - Total absorption (1/m) from QAAv6 (Lee et al. 2002)
    %    bb - Total backscattering (1/m) from QAAv6 (Lee et al. 2002)
    %    bbw - Pure water backscattering coefficients [0.002 0.0014 0.0008
    %    0.004] from Lee et al. (2016)
    %    sa - Solar Zenith Angle [30 degrees]
    %
    % Outputs:
    %    kd - Diffuse attenuation coefficient (1/m) from Lee et al. (2013)
    %
    % Examples: 
    %    Example 1: 
    %    kd =iop2kd(a,bb,bbw,sa)
    % 
    % Other m-files required: qaa_v6_l8.m and h20_iops
    % Subfunctions: none
    % MAT-files required: none
    % See also: none
    %
    % Author: Kelly Luis
    % Email: kelly.luis001@umb.edu or m11keluis@gmail.com
    % Website: http://www.github.com/m11keluis
    % March 8, 2019
    % ************************************************************************

    % Contribution from moleculuar backscattering to total backscattering
    nw = bbw./bb;

    % Compute Diffuse Attenuation Coefficient
    kd = (1+0.005*sa).*a+(1-0.265*nw)*4.259.*(1-0.52*exp(-10.8*a)).*bb;

end

