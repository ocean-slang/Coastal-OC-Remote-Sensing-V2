function out=modfunc(x,a)
out = 1./(a(1)*x(:,1).*log(abs(a(2)-x(:,2))./a(3)));