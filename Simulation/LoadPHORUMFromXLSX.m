function PHORUMdata = LoadPHORUMFromXLSX(settings,dirString)
warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames');
fileSuffix = strcat(string(settings.year));
inDir = fileSuffix;
if strlength(settings.suffix)>0; inDir = strcat(inDir,'_',settings.suffix); end
inputFolder = strcat('Inputs\',inDir,'\');
inputFileName = strcat(inputFolder,'PHORUMdata',fileSuffix,'.xlsx');
%% Load and Clean genData
[~, ~, tempGenData] = xlsread(inputFileName, 'genData');
% Remove header row
tempGenData(1,:) = [];

% Remove unwanted generators, identified by a '0' in column A
index = 1;
while index <= size(tempGenData,1)
    if cell2mat(tempGenData(index,1)) == 0
        tempGenData(index,:) = [];
    else
        index = index + 1;
    end
end

genData.PlantCode = tempGenData(:,4);
genData.HeatRate = cell2mat(tempGenData(:, 16))*1000/1000000;  %convert from btu/kWh -> MMbtu/MWh
genData.CapacitySummer = cell2mat(tempGenData(:, 49));
genData.CapacityWinter = cell2mat(tempGenData(:, 50));
genData.gen_capacity_needs = max(genData.CapacitySummer,genData.CapacityWinter);
genData.VarOM = cell2mat(tempGenData(:, 51));
genData.FuelPrice = cell2mat(tempGenData(:,52:63));
genData.RampRate = cell2mat(tempGenData(:,64));
genData.MinUp = round(cell2mat(tempGenData(:,65)));    % Need hours in integers
genData.MinDown = round(cell2mat(tempGenData(:,66)));  % Need hours in integers
genData.StartupCost = cell2mat(tempGenData(:,67));
genData.Min = cell2mat(tempGenData(:, 68));
genData.EAF = cell2mat(tempGenData(:,69:80))./100;
genData.Zone = tempGenData(:,81);
% TODO convert Zone to TCR

% Emission rates (lb/MWh)
genData.NOX = cell2mat(tempGenData(:,85));
genData.SO2 = cell2mat(tempGenData(:,86));
genData.N2O = cell2mat(tempGenData(:,87));
genData.CO2 = cell2mat(tempGenData(:,88));
genData.CO2eqv = cell2mat(tempGenData(:,90));
genData.CO = cell2mat(tempGenData(:,91));
genData.NH3 = cell2mat(tempGenData(:,92));
genData.PM10 = cell2mat(tempGenData(:,93));
genData.PM25 = cell2mat(tempGenData(:,94));
genData.VOC = cell2mat(tempGenData(:,95));

% Marginal Damages ($1999/MWh)
genData.MDNH3 = cell2mat(tempGenData(:,103));
genData.MDSO2 = cell2mat(tempGenData(:,104));
genData.MDVOC = cell2mat(tempGenData(:,105));
genData.MDNOX = cell2mat(tempGenData(:,106));
genData.MDPM25 = cell2mat(tempGenData(:,107));
genData.MDPM10 = cell2mat(tempGenData(:,108));

% No load costs
genData.NLC = cell2mat(tempGenData(:,110));

% Assign generator to a Transmission Constrained Region (TCR) based on zone
for index = 1 : size(genData.Zone,1)
    if(strcmp(genData.Zone(index),'AEP')) | (strcmp(genData.Zone(index),'COMED')) | (strcmp(genData.Zone(index),'APS')) | (strcmp(genData.Zone(index),'DUQ')) | (strcmp(genData.Zone(index),'DAY')) | (strcmp(genData.Zone(index),'PENELEC'))| (strcmp(genData.Zone(index),'ATSI'))
        genData.TCR(index,1) = 1;
    elseif(strcmp(genData.Zone(index),'BGE')) | (strcmp(genData.Zone(index),'PEPCO'))
        genData.TCR(index,1) = 2;
    elseif(strcmp(genData.Zone(index),'PPL')) | (strcmp(genData.Zone(index),'METED'))
        genData.TCR(index,1) = 3;
    elseif(strcmp(genData.Zone(index),'JCPL')) | (strcmp(genData.Zone(index),'PECO')) | (strcmp(genData.Zone(index),'PSEG')) | (strcmp(genData.Zone(index),'AECO')) | (strcmp(genData.Zone(index),'DPL')) | (strcmp(genData.Zone(index),'RECO'))
        genData.TCR(index,1) = 4;
    elseif(strcmp(genData.Zone(index),'DOM'))
        genData.TCR(index,1) = 5;
    end
end
%clear tempGenData

%% Load & Clean load
disp('Loading load data...');

% load from xls
loadData = readtable(inputFileName,'Sheet','Load');
importData = readtable(inputFileName,'Sheet','Imports');
transferLimitData = readtable(inputFileName,'Sheet','Transmission Constraints');
renewablesData = readtable(inputFileName,'Sheet','Wind');

loadData.CE = loadData.COMED*0.0;
loadData.AP = loadData.COMED*0.0;

% Transmission constraints
transferLimitData.TI12 = transferLimitData.AverageWesternTransferLimit/4; % TI12maxGDX.val = WImax/4;
transferLimitData.TI13 = transferLimitData.AverageWesternTransferLimit/2; % TI13maxGDX.val = WImax/2;
transferLimitData.TI15 = transferLimitData.AP_SOUTHBBLACKOAKAEP_DOM_DOM_; % TI15maxGDX.val = DOMImax;
transferLimitData.TI23 = transferLimitData.AverageCentralTransferLimit; % TI23maxGDX.val = CImax;
transferLimitData.TI25 = transferLimitData.AverageWesternTransferLimit/4; % TI52maxGDX.val = WImax/4;
transferLimitData.TI34 = transferLimitData.AverageEasternTransferLimit; % TI34maxGDX.val = EImax;

renewablesData.windTCR2 = renewablesData.windTCR1*0.0;
renewablesData.windTCR4 = renewablesData.windTCR1*0.0;
renewablesData.windTCR5 = renewablesData.windTCR1*0.0;
renewablesData.solarTCR1 = renewablesData.windTCR1*0.0;
renewablesData.solarTCR2 = renewablesData.windTCR1*0.0;
renewablesData.solarTCR3 = renewablesData.windTCR1*0.0;
renewablesData.solarTCR4 = renewablesData.windTCR1*0.0;
renewablesData.solarTCR5 = renewablesData.windTCR1*0.0;
clear load imports LMPs transmissionConstraints wind
%% Load & Clean Storage
disp('Loading storage data...');

% load from xls
[~,~,tempStorageData] = xlsread(inputFileName, 'storageData');

% delete headers
tempStorageData(1,:) = [];

storageData.TCR = cell2mat(tempStorageData(:,3));
storageData.PowerCapacity = cell2mat(tempStorageData(:,4));
storageData.ChargeEfficiency = cell2mat(tempStorageData(:,5));
storageData.DischargeEfficiency = cell2mat(tempStorageData(:,6));
storageData.EnergyCapacity = cell2mat(tempStorageData(:,4)).*cell2mat(tempStorageData(:,7)); % PowerCapacity * Duration
storageData.VariableCost = cell2mat(tempStorageData(:,8));

clear tempStorageData

%% Save data to disk
PHORUMdata.genData = genData;
PHORUMdata.loadData = loadData;
PHORUMdata.importData = importData;
PHORUMdata.transferLimitData = transferLimitData;
PHORUMdata.renewablesData = renewablesData;
PHORUMdata.storageData = storageData;
save(strcat('Cases\',dirString,'\PHORUMdata_',fileSuffix),'PHORUMdata');
warning('ON', 'MATLAB:table:ModifiedAndSavedVarnames')
end