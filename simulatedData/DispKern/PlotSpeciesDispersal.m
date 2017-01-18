d = 0:0.001:2;
figure
hold on

% for i = 1:length(ALSize50km)
%     a = ALSize50km(i);
%     kern = exp(-d/a);
%     plot(d,kern)
%     
%     
% end

for i = 1:length(aList)
    a = aList(i);
    kern = exp(-d/a);
    plot(d,kern)
    
    
end