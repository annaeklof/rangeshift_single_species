%Gets the dispersal kernel from dispersal data
%Explanation:
% First normalize the in-data. For example, if in-data dispersal is 
% 17.353 and the landscape size is 100x100 km then 
% normalized dispersal is 0.1735 in a 1x1 landscape.
% The dispersal is for 99 % of the individuals.
%Returned value is the value of a in dispersal kernel function. d =
%e^-(a/b) if b = 1.
%
% NatalDisp can be vector or scalar
function [A_values] = CalculateDispersalKernel(NatalDisp,LandscapeSize)

A_values = zeros(length(NatalDisp),1);
%Dispersal of 17km and a landscape of 100x100 km
%NatalDisp = 17;
%LandscapeSize = 100;

for i = 1:length(NatalDisp)
    %Normalize to a 1x1 landscape
    DispersalNormalized = NatalDisp(i) / LandscapeSize;
    LandscapeSizeNormalized = LandscapeSize / LandscapeSize;


    d = num2str(DispersalNormalized);
    LSize = (LandscapeSizeNormalized);

    %F_x - F_0 = (F_1 + F_0) * 0.99
    F_1 = ['(-a*exp(-(' num2str(LSize*300) '/a))']; %LSize*300 då dispkern kan sträcka sig längre än LandscapeSize....
    F_0 = 'a';
    F_x = ['(-a*exp(-( ' d ' /a))']; 
    %x = d;


    
    LoadString = (['numeric::solve(' F_x ' + ' F_0 ') == ' F_1 '  + ' F_0 ')*0.99, a = 0..25)']);
    save
    a = evalin(symengine, LoadString);
    A_values(i) = double(a);


    %DispKernel = exp(-(d./a));
%     x = 0:0.1:3;
%     y = exp(-x./a);
%     figure
%     plot(x,y)


    clear a
end
end