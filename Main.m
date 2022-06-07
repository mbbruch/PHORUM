% This is the primary PHORUM Matlab function.  The function makes 
% several sub-function calls, and is responsible for calling GAMS. 
% The function loads data from the database file and settings file.

function Main(settingsFileSuffix) %ex: UCtesla_2019
%% Load settings
load(strcat('Inputs\Settings\settings',settingsFileSuffix));
addpath(settings.GAMSpath);
thisDir = pwd();
%% Set up a working directory for this test case
dirStringPrefix = settings.EVdataFile;
if strlength(dirStringPrefix)==0 dirStringPrefix ='NoEV'; end
dirString = strcat(dirStringPrefix,'_CC',string(settings.isControlledCharging), '_',string(settings.year),'_Txn',string(settings.isTransmissionConstraints));
if strlength(settings.suffix)>0; dirString = strcat(dirString,'_',settings.suffix); end
mkdir(strcat('Cases\',dirString));
mkdir(strcat('Cases\',dirString,'\solver_logs'));
%% Save an archive of the code that's currently running this test case
zip(strcat('CodeBackup_',dirString),{'*.m','Models'}); movefile(strcat('CodeBackup_',dirString,'.zip'),strcat('Cases\',dirString));
%% Move the GAMS model we will use into the working directory
if settings.isTransmissionConstraints txnString = 'Trans'; else txnString = 'NoTrans'; end
if settings.isControlledCharging chgString = 'CC'; else chgString = 'NoCC'; end
modelFile = strcat('PHORUM_',txnString,'_',chgString);
copyfile(strcat('Models\',modelFile,'.gms'),strcat('Cases\',dirString));
copyfile(strcat('Models\*.opt'),strcat('Cases\',dirString));
%% Load input data
PHORUMdata = LoadPHORUMFromCSV(settings,dirString);
[PHORUMdata, EVdata] = MakeEVData(PHORUMdata,settings,2019,'2019',dirString);
% Initialize all results structures
[totalResults, prevDayResults] = InitializeResultsStruct(settings);

%% Daily GAMS loop
totalRuntime = 0;
for rangeIndex = 1 : settings.numDateRanges
    for day = settings.dStart(rangeIndex) : 1: settings.dEnd(rangeIndex) - 1

        % Status
        tic;
        disp(['Running GAMS, day: ', num2str(day)]);
        
        % Create GDX files
        CreateGDX(day, PHORUMdata, settings, prevDayResults, EVdata, dirString);
        % Run GAMS
        cd(strcat(thisDir,'\Cases\',dirString));
        gams(strcat(modelFile,' logoption=2'));
        movefile(strcat(modelFile,'.log'),strcat('solver_logs\',modelFile,'_',string(day),'.log'));
        delete('matdata.g*')
        cd('..\..');
        % [status,cmdout] = system(strcat(settings.callGAMS," ",pwd(),"\Models\",modelFile," workdir=",pwd(),"\Cases\",dirString),'-echo');

        % Parse daily results
        [totalResults, prevDayResults] = ParseOutputs(totalResults, PHORUMdata, day, settings.dEnd(rangeIndex), totalRuntime, tic, settings, EVdata, dirString);

    end
end

disp(['Saving results to ', settings.outputFileName, '...']);
% Create all outputs as specified by settings
outputs = SaveResults(totalResults, settings, PHORUMdata,dirString);



end