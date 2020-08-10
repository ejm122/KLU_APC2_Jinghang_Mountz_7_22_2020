%AI Function: Calculates the hemispheric activation asymmetry index
% for a given ROI.  Yields number between -1 and 1, 0 = symmetric
% activation, negative value = right dominated activation, positive
%value = left dominated activation.
%-- Ellie Mountz (ejm122@pitt.edu) and Jinghang Li (jil202@pitt.edu)
%Last updated: 8/5/2020
function index = AI(l,r) 
   index = (l-r) ./ (abs(l) + abs(r));
   %index = (l-r)./(0.5*(l+r));
end