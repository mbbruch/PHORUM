function PHORUMdata = LoadPHORUMFromCSV(settings,dirString)
warning('OFF', 'MATLAB:table:ModifiedAndSavedVarnames');
fileSuffix = strcat(string(settings.year));
inDir = fileSuffix;
if strlength(settings.suffix)>0; inDir = strcat(inDir,'_',settings.suffix); end

%% Load and Clean genData
inputFolder = strcat('Inputs\',inDir,'\');
genData = readtable(strcat(inputFolder,'phorum_',fileSuffix,'.csv'));
genData.FuelPrice = table2array(genData(:,find(string(genData.Properties.VariableNames) == "FuelPrice_01"):find(string(genData.Properties.VariableNames) == "FuelPrice_12"))); % from Jan to Dec
genData(:,find(string(genData.Properties.VariableNames) == "FuelPrice_01"):find(string(genData.Properties.VariableNames) == "FuelPrice_12")) = [];
genData.EAF = table2array(genData(:,find(string(genData.Properties.VariableNames) == "EAF_01"):find(string(genData.Properties.VariableNames) == "EAF_12"))); % from Jan to Dec
genData(:,find(string(genData.Properties.VariableNames) == "EAF_01"):find(string(genData.Properties.VariableNames) == "EAF_12")) = [];

PHORUMdata.genData = genData;
PHORUMdata.loadData = readtable(strcat(inputFolder,'load_',fileSuffix,'.csv'));
PHORUMdata.importData = readtable(strcat(inputFolder,'imports_',fileSuffix,'.csv'));
PHORUMdata.transferLimitData = readtable(strcat(inputFolder,'transferLimits_',fileSuffix,'.csv'));
PHORUMdata.renewablesData = readtable(strcat(inputFolder,'windSolar_',fileSuffix,'.csv'));
PHORUMdata.storageData = readtable(strcat(inputFolder,'storage_',fileSuffix,'.csv'));
save(strcat('Cases\',dirString,'\PHORUMdata_',fileSuffix),'PHORUMdata');
warning('ON', 'MATLAB:table:ModifiedAndSavedVarnames')
end