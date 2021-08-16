function sMasks = maskL2(zsd, l2flags)

zsd = zsd(:);
flags = fliplr(dec2bin(l2flags(:),32)-'0'); %Converts to Binary Numbers

for i = 1:length(flags)
    if flags(i,2) == 1 && flags(i,17) == 1
        sMasks(i,:) = 0;
    elseif flags(i,17) == 1
        sMasks(i,:) = 0;
    elseif flags(i,2) == 1 
        sMasks(i,:) = 9;
    else
        sMasks(i,:) = zsd(i,:);
    end
end

    sMasks(sMasks > 9) = nan;
    
end
