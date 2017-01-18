 function [  ] = extractresults(continuity,contrast)
%EXTRACTRESULTS Summary of this function goes here
%   ex. extractresults(0,5,'mean')
 %REMEMBER TO DELETE LINE
 %type should be "mean"
%% INIT
foldername = 'res'; %Local folder with resultfiles in it.
doplot = 'true'; %Set to 'true' to plot and print plots
type = 'mean'; %Some silly option for plots
PaddingTime = 100 + 100; %Time before and after climatic shift
DensTypes = 'ABCD';

A=zeros(15,1);
B=A;
C=A;
D=A;
if ispc
    disp('kolla paths!')
    addpath([pwd '\exportfigure\'])
%     respath = (['Z:\Current work\Resfromtrio\Sliding25feb' '\' foldername '\']);
    respath = ([pwd '\' foldername '\']);
    addpath (respath)
    load('Listofruns.mat')
else
    addpath([pwd '/exportfigure'])
    respath = (['/nobackup/global/x_jonar' '/' foldername '/']);
    addpath (respath)
    load('Listofruns.mat')
end
Simlength = unique(params.data(:,params.Simlength));
%continuity = [0 0 0 1 1 2 2];
%contrast = [1 3 5 3 5 3 5];
a = unique(params.data(:,params.a));
a_m_fetch = unique(params.data(:,params.a_m))';

TotalCombinations = length(a) * length(Simlength);


TotalTime = Simlength(end) + PaddingTime;

%% Get results
for typeID = 1:length(DensTypes)

dataexport_pops = zeros(TotalTime,TotalCombinations*2); % 12*15 is combinations of a * 2 * length(Simlength)
dataexport_besatta = zeros(TotalTime,TotalCombinations*2);
dataexport_proportions = zeros(2,TotalCombinations);
dataexport_existingpops = zeros(TotalTime,TotalCombinations*2);
dataexport_survivingreps = zeros(1,TotalCombinations);
dataexport_besatta = zeros(TotalTime,TotalCombinations*2);
dataexport_replicates_extinct_over_time = zeros(TotalTime,TotalCombinations);

for k = 1:length(a_m_fetch)
    %HÃ¤mta alla dÃ¤r cont = 0 contrast = 1 och a_m(j)
    x1 = find(params.data(:,2) == continuity);
    x2=find(params.data(:,3) == contrast);
    x3=find(params.data(:,params.a_m) == a_m_fetch(k));
    found = intersect(x1,x2);
    found = intersect(found,x3);
    %Combinations = kombinationer av a och a_m
    combinations = length(found)/length(Simlength); % length(found) = 90, 90/6 = 15 (eftersom a_m inte varierar så egentligen length(a))
    %X: kolumn är olika Simlength och rader olika a
    X = vec2mat(found,combinations)'; %snyggare lÃ¶sning

%     H = figure('Color','w');
%     set(H, 'Outerposition',[ 50          95        1580         900])
%     hold on



    for i = 1:length(Simlength)
    
        switch DensTypes(typeID)
            case 'D'
                close all
                H = figure('Color','w');
                set(H, 'Outerposition',[ 50          95        1580         900])
                hold on
        end
    
        for i2 = 1:combinations
            %Laddar alla replikat från en kombination av parametrar, ex från '0_1_10_N_3.mat'
            filename = [num2str(continuity) '_' num2str(contrast) '_' num2str(Simlength(i)) '_N_'  num2str(X(i2,i)) '.mat'];
            load(strcat(respath,filename),'results','reps') %ex '0_1_10_N_3.mat'
            
            %Vi ska sortera landskap efter densitet i mitten
            %Limits ska definieras efter densitet A,B,C där B är nånting
            %~förväntad densitet (0.33)
            switch DensTypes(typeID)
                case 'A'
                    XLowerLimit = 0;
                    XUpperLimit = 0.2333;
                case 'B'
                    XLowerLimit = 0.2333;
                    XUpperLimit = 0.4333;
                case 'C'
                    XLowerLimit = 0.4333;
                    XUpperLimit = 1.01;
                case 'D'
                    XLowerLimit = -0.01;
                    XUpperLimit = 1.01;
                otherwise
                    disp('Invalid case. Dreadful feeling!')
                
            end

            
            XIndex = zeros(reps,1);
            for i3 = 1:reps
                XDensity = find(results.X(:,i3) > 2/3 & results.X(:,i3) < 4/3);
                XDensity = length(XDensity)/1000;
                if (XDensity > XLowerLimit && XDensity < XUpperLimit)
                    XIndex(i3) = 1;
                else
                    XIndex(i3) = 0;
                end
            end
            
            %I *think* this is old test code
            if(i==2)
               switch DensTypes(typeID)
                case 'A'
                    %disp(['Case A: ' num2str(sum(XIndex))])
                    A(i2) = sum(XIndex);
                case 'B'
                    %disp(['Case B: ' num2str(sum(XIndex))])
                    B(i2) = sum(XIndex);
                case 'C'
                    %disp(['Case C: ' num2str(sum(XIndex))])
                    C(i2) = sum(XIndex);
                case 'D'
                    %disp(['Case D: ' num2str(sum(XIndex))])
                    D(i2) = sum(XIndex);
               end
            end
            
            
            
            
            XIndex = logical(XIndex);
            
            if(sum(XIndex) > 0)
                pops_mean = mean(results.pops(XIndex,:),1);
                pops_std = std(results.pops(XIndex,:));
                besatta_mean = mean(results.besatta(:,XIndex)');
                besatta_std = std(results.besatta(:,XIndex)');    
                existingIndex = (results.pops(:,end)>2) > 0;
                existing_pops = results.pops(XIndex & existingIndex,:); 
                existing_pops_mean = mean(existing_pops,1);
                existing_pops_std = std(existing_pops);

                replicates_survived = length(find((results.pops(XIndex,end)>5) > 0)) / sum(XIndex);
                replicates_extinct_over_time = sum( results.besatta(1:end,XIndex) < 2 ,2 ) / sum(XIndex);


               % save('test.mat','pops_mean','pops_std','i','Simlength','i2','dataexport')
                dataexport_pops(1:Simlength(i)+PaddingTime,-1+2*i2 + (i-1)*combinations*2 )     = pops_mean';
                dataexport_pops(1:Simlength(i)+PaddingTime,2*i2 + (i-1)*combinations*2 )        = pops_std';
                dataexport_besatta(1:Simlength(i)+PaddingTime,-1+2*i2 + (i-1)*combinations*2 )  = besatta_mean';
                dataexport_besatta(1:Simlength(i)+PaddingTime,2*i2 + (i-1)*combinations*2 )     = besatta_std';
                dataexport_proportions(1,(i-1)*combinations+i2) = results.middle_mean ./ results.init_mean;
                dataexport_proportions(2,(i-1)*combinations+i2) = results.end_mean ./ results.init_mean;
                dataexport_existingpops(1:Simlength(i)+PaddingTime,-1+2*i2 + (i-1)*combinations*2 )     = existing_pops_mean';
                dataexport_existingpops(1:Simlength(i)+PaddingTime,2*i2 + (i-1)*combinations*2 )        = existing_pops_std';
                dataexport_survivingreps( (i-1)*combinations+i2) = replicates_survived;
                dataexport_replicates_extinct_over_time(1:Simlength(i)+PaddingTime, (i-1)*combinations+i2) = replicates_extinct_over_time;
                
                switch DensTypes(typeID)
                    case 'D'
                        switch type
                          case 'mean'
        %                     axis([0 100+Simlength(i) 0 100000])
                            H(i2) = subplot(3,combinations,i2);
                            title(['Simlength = ' num2str(Simlength(i))    ]);
                            hold on
                             x = 1:length(Simlength);
                             y1 = pops_mean;
                             y2 = pops_mean - pops_std;
                             y3 = pops_mean + pops_std;
                             plot(y1,'r')
                             plot(y2,'k')
                             plot(y3,'k')
                            axis([0 PaddingTime+Simlength(i) 0 300000])
                        end
                end
            end

            
        end
        %% GIVE TITLE AND EXPORT FIG
          switch DensTypes(typeID)
          case 'D'
              switch doplot
                    case 'true'
                        suptitle(sprintf(['Continuity=' num2str(continuity) '  \n' 'Contrast=' num2str(contrast) ' \n' 'Simlength =' num2str(Simlength(i))]));
                    %     if ispc
                    %         filename = [pwd '\Figures\' num2str(continuity) '_' num2str(contrast) '_' 'a_m=' num2str(a_m_fetch(k)) '.mat'];
                    %     else
                    %         filename = [pwd '/Figures/' num2str(continuity) '_' num2str(contrast) '_' 'a_m=' num2str(a_m_fetch(k)) '.mat'];
                        if ispc
                            dirname = [pwd '\Figures\' num2str(continuity) '_' num2str(contrast) '\'];
                            filename = horzcat(dirname, [type '_' 'Simlength=' num2str(Simlength(i)) '.mat']);
                            Exist = exist(dirname,'dir');
                            if Exist == 7
                                export_fig(filename,-opengl)
                                close all
                            else 
                                mkdir(dirname)
                                export_fig(filename,-opengl)
                                close all
                            end


                        else
                            disp('ajajlinux!')
                            break
                        end
                    otherwise
                        %disp('Plots disabled in script!')
              end  
          end

    end
    
    

    
end
    %% GIVE TITLE AND EXPORT DATA
% save([pwd '\Rexport\dataexport_pops_' num2str(continuity) '_' num2str(contrast) '.mat'],'dataexport_pops')
% save([pwd '\Rexport\dataexport_besatta_' num2str(continuity) '_' num2str(contrast) '.mat'],'dataexport_besatta')
% save([pwd '\Rexport\dataexport_proportions_' num2str(continuity) '_' num2str(contrast) '.mat'],'dataexport_proportions')
csvwrite([pwd '\Rexport\dataexport_pops_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_pops)
csvwrite([pwd '\Rexport\dataexport_besatta_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_besatta)
csvwrite([pwd '\Rexport\dataexport_proportions_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_proportions)
csvwrite([pwd '\Rexport\dataexport_existingpops_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_existingpops)
csvwrite([pwd '\Rexport\dataexport_survivingreps_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_survivingreps)
csvwrite([pwd '\Rexport\dataexport_ext_over_time_' num2str(continuity) '_' num2str(contrast) '_' DensTypes(typeID) '.csv'],dataexport_replicates_extinct_over_time)


end
savename = ['ABCD_' num2str(continuity) '_' num2str(contrast) '.mat']
save(savename,'A','B','C','D')
save


%Also make a .csv file that includes Simlength and a and PaddingTime
Headers = {'Simlength','A','Paddingtime'};
parameters{3} = PaddingTime;
parameters{2} = a;
parameters{1} = Simlength;
%Print headers 
fid = fopen('parameters.csv', 'w') ;
  fprintf(fid, '%s,', Headers{1,1:end-1}) ;
  fprintf(fid, '%s\n', Headers{1,end}) ;
  MaxRows = max(cellfun('length',parameters));
    for i = 1:MaxRows
        if i <= length(parameters{1})
            fprintf(fid, '%f,', parameters{1}(i)) ;
        else
            fprintf(fid, '%f,', '');
        end
        if i <= length(parameters{2})
            fprintf(fid, '%f,', parameters{2}(i));
        else
            fprintf(fid, '%f,', '');
        end
        if i <= length(parameters{3})
            fprintf(fid, '%f\n', parameters{3}(i));
        else
            fprintf(fid, '%f\n', '');
        end
    end
fclose(fid) ;
end
% %reference purpose
% a = [0.001 0.002 .004 0.008 0.016 0.032 0.064 0.128 0.256 0.512 1.024 2.048 4.096 8.192 16.3840
% ];
% a_m = [0.001 0.002 .004 0.008 0.016 0.032 0.064 0.128 0.256 0.512 1.024 2.048 4.096 8.192 16.3840
% ];