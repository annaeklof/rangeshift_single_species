%ReadDispersalData('DispersaldataMammals.csv',50) t.ex. använts
function [] = ReadDispersalData(filename, LSize)
%Takes excelfile with dispersal data and adds kernel column

Format = '%s %s %s %f';
fileID = fopen(filename);
ReadFromFile = textscan(fileID,Format,'HeaderLines',1,'Delimiter',';');
fclose(fileID);


%LSize = 50; %Landscape size 50x50 km
A = CalculateDispersalKernel(ReadFromFile{4},LSize);

CellToWrite = ReadFromFile;
CellToWrite{5} = A;
RowsToWrite = max(cellfun('length',CellToWrite));
ColumnsToWrite = length(CellToWrite);
NewHeader = (['A (LSize ' num2str(LSize) ' km)']);

Headers = {'Taxon','sp_code','Species','Natal_dispersal_km',NewHeader};

fid = fopen('Output.csv', 'w') ;
  fprintf(fid, '%s;', Headers{1,1:end-1}) ;
  fprintf(fid, '%s\n', Headers{1,end}) ;
    for i = 1:RowsToWrite
        for j = 1:ColumnsToWrite
            if j < ColumnsToWrite
                if j <= 3
                    fprintf(fid, '%s;', CellToWrite{j}{i}) ;
                else
                    fprintf(fid, '%f;', CellToWrite{j}(i)) ;
                end   
            else
                fprintf(fid, '%f\n', CellToWrite{j}(i)) ;
            end
        end
    end

fclose(fid) ;
