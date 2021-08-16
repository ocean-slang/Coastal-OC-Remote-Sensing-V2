function lMask(fig,c)
oldcmap = colormap;
mycmap = get(fig,'Colormap');
mycmap(1,:) = [0.5 0.5 0.5];
mycmap(end,:) = [0 0 0];
set(fig,'Colormap',flipud(mycmap));
set(c, 'Colormap',flipud(oldcmap),'YDir','reverse');
ylabel(c,'L8 Derived Z_S_D (m)')
end

