 function [outputsHourly, outputsDaily, outputsTotal] = SaveResults(totalResults, settings, PHORUMdata, dirString)
 
 %% Load data from PHORUMdata
genData = PHORUMdata.genData;
storageData = PHORUMdata.storageData;

% Emission rates (lb/MWh)
gNOX = genData.NOX;
gSO2 = genData.SO2;
gN2O = genData.N2O;
gCO2 = genData.CO2;
gCO2eqv = genData.CO2eqv;
gCO = genData.CO;
gNH3 = genData.NH3;
gPM10 = genData.PM10;
gPM25 = genData.PM25;
gVOC = genData.VOC;

% Marginal Damages ($2010/MWh)
gMDNH3 = genData.MDNH3;
gMDSO2 = genData.MDSO2;
gMDVOC = genData.MDVOC;
gMDNOX = genData.MDNOX;
gMDPM25 = genData.MDPM25;
gMDPM10 = 0;

% Startup emissions (lbs) and marginal damages ($2010)
% gStartupNOX = genData.gStartupNOX*2000;
% gStartupCO2 = genData.gStartupCO2*2000;
% gStartupSO2 = genData.gStartupSO2*2000;
% gStartupMDNOX = genData.gStartupMDNOX;
% gStartupMDSO2 = genData.gStartupMDSO2;

% Generator costs
gStartupCost = genData.StartupCost;
gNLC = 0;
 
% Hours
counter = 1;
for dayIndex = 1 : size(totalResults.date,2)
    for hourIndex = 1 : 24
        hour(counter) = 24*(dayIndex-1) + hourIndex;
        counter = counter + 1;
    end
end
numDays = size(totalResults.date,2)-1;

%% Daily outputs
gLevel = totalResults.gLevel;
gRuntime = totalResults.gRuntime;
gStartups = totalResults.gStartup;

% Generator costs & net revenue
totalResults.gStartup
repmat(gStartupCost,1,numDays)
gStartupC = totalResults.gStartup .* repmat(gStartupCost,1,numDays);
gVC = gLevel .* totalResults.gVC;
gNLC = totalResults.gRuntime .* repmat(gNLC,1,numDays);
%gNetRevenue = totalResults.gGrossRevenue - gStartupC - gVC - gNLC;

% Generator emissions
gNOX = gLevel .* repmat(gNOX,1,numDays);
gSO2 = gLevel .* repmat(gSO2,1,numDays);
gN2O = gLevel .* repmat(gN2O,1,numDays);
gCO2 = gLevel .* repmat(gCO2,1,numDays);
gCO2eqv = gLevel .* repmat(gCO2eqv,1,numDays);
gCO = gLevel .* repmat(gCO,1,numDays);
gNH3 = gLevel .* repmat(gNH3,1,numDays);
gPM10 = gLevel .* repmat(gPM10,1,numDays);
gPM25 = gLevel .* repmat(gPM25,1,numDays);
gVOC = gLevel .* repmat(gVOC,1,numDays);
gMDNH3 = gLevel .* repmat(gMDNH3,1,numDays);
gMDSO2 = gLevel .* repmat(gMDSO2,1,numDays);
gMDVOC = gLevel .* repmat(gMDVOC,1,numDays);
gMDNOX = gLevel .* repmat(gMDNOX,1,numDays);
gMDPM25 = gLevel .* repmat(gMDPM25,1,numDays);
gMDPM10 = gLevel .* repmat(gMDPM10,1,numDays);

% gStartupCO2 = totalResults.gStartup .* repmat(gStartupCO2,1,numDays);
% gStartupNOX = totalResults.gStartup .* repmat(gStartupNOX,1,numDays);
% gStartupSO2 = totalResults.gStartup .* repmat(gStartupSO2,1,numDays);
% gStartupMDNOX = totalResults.gStartup .* repmat(gStartupMDNOX,1,numDays);
% gStartupMDSO2 = totalResults.gStartup .* repmat(gStartupMDSO2,1,numDays);
% 
% gMDtotal = gMDNH3 + gMDSO2 + gMDVOC + gMDNOX + gMDPM25 + gMDPM10 + gStartupMDNOX + gStartupMDSO2;
%% Total Outputs

tSysCost = sum(totalResults.sysCost,2);
tGNetRevenue = sum(0,2);
tGVC = sum(gVC,2);
tGLevel = sum(gLevel,2);
tGRuntime = sum(gRuntime,2);
tGStartups = sum(gStartups,2);
tGStartupC = sum(gStartupC,2);
tGNLC = sum(gNLC,2);
tNOX = sum(gNOX,2);
tSO2 = sum(gSO2,2);
tN2O = sum(gN2O,2);
tCO2 = sum(gCO2,2);
tCO2eqv = sum(gCO2eqv,2);
tCO = sum(gCO,2);
tNH3 = sum(gNH3,2);
tPM10 = sum(gPM10,2);
tPM25 = sum(gPM25,2);
tVOC = sum(gVOC,2);
tMDNH3 = sum(gMDNH3,2);
tMDSO2 = sum(gMDSO2,2);
tMDVOC = sum(gMDVOC,2);
tMDNOX = sum(gMDNOX,2);
tMDPM25 = sum(gMDPM25,2);
tMDPM10 = sum(gMDPM10,2);
% tMDtotal = sum(gMDtotal,2);
tSCharge = sum(totalResults.sCharge,2);
tSDischarge = sum(totalResults.sDischarge,2);
tSNetRevenue = sum(totalResults.sNetRevenue,2);

%% Create outputs structures as requested in settings

% Hourly output structure
outputsHourly.hour = hour;
if settings.load == 1
    outputsHourly.loadTCR1 = totalResults.loadTCR1;
    outputsHourly.loadTCR2 = totalResults.loadTCR2;
    outputsHourly.loadTCR3 = totalResults.loadTCR3;
    outputsHourly.loadTCR4 = totalResults.loadTCR4;
    outputsHourly.loadTCR5 = totalResults.loadTCR5;
end

% vehicle outputs
if settings.isControlledCharging == 1
%    outputsHourly.vSOC = totalResults.vSOC;
    outputsHourly.vCharge = totalResults.vCharge;
end

%% Daily output structure
outputsDaily.day = totalResults.date;
if (settings.systemCosts == 2),     outputsDaily.sysCost = totalResults.sysCost; end
if (settings.gLevel == 2),      outputsDaily.gLevel = gLevel; end
if (settings.gRuntime == 2),    outputsDaily.gRuntime = gRuntime; end
if (settings.gStartups == 2),   outputsDaily.gStartups = gStartups; end
% if (settings.gNetRevenue == 2), outputsDaily.gNetRevenue = gNetRevenue; end
if (settings.gVC == 2),         outputsDaily.gVC = gVC; end
if (settings.gStartupC == 2),   outputsDaily.gStartupC = gStartupC; end
if (settings.sCharge == 2),     outputsDaily.sCharge = totalResults.sCharge; end
if (settings.sDischarge == 2),  outputsDaily.sDischarge = totalResults.sDischarge; end
% if (settings.sNetRevenue == 2), outputsDaily.sNetRevenue = totalResults.sNetRevenue; end
if (settings.gNOX == 2),        outputsDaily.gNOX = gNOX; end
if (settings.gSO2 == 2),        outputsDaily.gSO2 = gSO2; end
if (settings.gN2O == 2),        outputsDaily.gN2O = gN2O; end
if (settings.gCO2 == 2),        outputsDaily.gCO2 = gCO2; end
if (settings.gCO2eqv == 2),     outputsDaily.gCO2eqv = gCO2eqv; end
if (settings.gCO == 2),         outputsDaily.gCO = gCO; end
if (settings.gNH3 == 2),        outputsDaily.gNH3 = gNH3; end
if (settings.gPM10 == 2),       outputsDaily.gPM10 = gPM10; end
if (settings.gPM25 == 2),       outputsDaily.gPM25 = gPM25; end
if (settings.gVOC == 2),        outputsDaily.gVOC = gVOC; end
% if (settings.MDtotal == 2),    outputsDaily.gMDtotal = gMDtotal; end
if (settings.MDNH3 == 2),      outputsDaily.gMDNH3 = gMDNH3; end
if (settings.MDSO2 == 2),      outputsDaily.gMDSO2 = gMDSO2; end
if (settings.MDVOC == 2),      outputsDaily.gMDVOC = gMDVOC; end
if (settings.MDNOX == 2),      outputsDaily.gMDNOX = gMDNOX; end
if (settings.MDPM25 == 2),     outputsDaily.gMDPM25 = gMDPM25; end
if (settings.MDPM10 == 2),     outputsDaily.gMDPM10 = gMDPM10; end

% if (settings.gStartupNOX == 2),     outputsDaily.gStartupNOX = gStartupNOX; end
% if (settings.gStartupCO2 == 2),     outputsDaily.gStartupCO2 = gStartupCO2; end
% if (settings.gStartupSO2 == 2),     outputsDaily.gStartupSO2 = gStartupSO2; end
% if (settings.StartupMDPM25 == 2),     outputsDaily.gMDPM25 = gMDPM25; end
% if (settings.StartupMDPM10 == 2),     outputsDaily.gMDPM10 = gMDPM10; end
 if (settings.wind == 2)
     outputsDaily.windTCR1 = totalResults.windTCR1;
     outputsDaily.windTCR2 = totalResults.windTCR2;
     outputsDaily.windTCR3 = totalResults.windTCR3;
     outputsDaily.windTCR4 = totalResults.windTCR4;
     outputsDaily.windTCR5 = totalResults.windTCR5;
end
if (settings.isControlledCharging == 1)
    outputsDaily.vCharge = totalResults.vCharge;
    outputsDaily.vDischarge = totalResults.vDischarge;
end

% Total output structure

if (settings.systemCosts == 3),     outputsTotal.sysCost = tSysCost; end
if (settings.gLevel == 3),      outputsTotal.gLevel = tGLevel; end
if (settings.gRuntime == 3),    outputsTotal.gRuntime = tGRuntime; end
if (settings.gStartups == 3),   outputsTotal.gStartups = tGStartups; end
% if (settings.gNetRevenue == 3), outputsTotal.gNetRevenue = tGNetRevenue; end
if (settings.gVC == 3),         outputsTotal.gVC = tGVC; end
if (settings.gNLC == 3),         outputsTotal.gNLC = tGNLC; end
if (settings.gStartupC == 3),   outputsTotal.gStartupC = tGStartupC; end
if (settings.sCharge == 3),     outputsTotal.sCharge = tSCharge; end
if (settings.sDischarge == 3),  outputsTotal.sDischarge = tSDischarge; end
% if (settings.sNetRevenue == 3), outputsTotal.sNetRevenue = tSNetRevenue; end
if (settings.gNOX == 3),        outputsTotal.gNOX = tNOX; end
if (settings.gSO2 == 3),        outputsTotal.gSO2 = tSO2; end
if (settings.gN2O == 3),        outputsTotal.gN2O = tN2O; end
if (settings.gCO2 == 3),        outputsTotal.gCO2 = tCO2; end
if (settings.gCO2eqv == 3),     outputsTotal.gCO2eqv = tCO2eqv; end
if (settings.gCO == 3),         outputsTotal.gCO = tCO; end
if (settings.gNH3 == 3),        outputsTotal.gNH3 = tNH3; end
if (settings.gPM10 == 3),       outputsTotal.gPM10 = tPM10; end
if (settings.gPM25 == 3),       outputsTotal.gPM25 = tPM25; end
if (settings.gVOC == 3),        outputsTotal.gVOC = tVOC; end
% if (settings.MDtotal == 3),    outputsTotal.gMDtotal = tMDtotal; end
if (settings.MDNH3 == 3),      outputsTotal.gMDNH3 = tMDNH3; end
if (settings.MDSO2 == 3),      outputsTotal.gMDSO2 = tMDSO2; end
if (settings.MDVOC == 3),      outputsTotal.gMDVOC = tMDVOC; end
if (settings.MDNOX == 3),      outputsTotal.gMDNOX = tMDNOX; end
if (settings.MDPM25 == 3),     outputsTotal.gMDPM25 = tMDPM25; end
if (settings.MDPM10 == 3),     outputsTotal.gMDPM10 = tMDPM10; end
 if (settings.wind == 3)
     outputsTotal.windTCR1 = sum(totalResults.windTCR1);
     outputsTotal.windTCR2 = sum(totalResults.windTCR2);
     outputsTotal.windTCR3 = sum(totalResults.windTCR3);
     outputsTotal.windTCR4 = sum(totalResults.windTCR4);
     outputsTotal.windTCR5 = sum(totalResults.windTCR5);
end


if exist('outputsHourly'), outputs.outputsHourly = outputsHourly; end
if exist('outputsDaily'), outputs.outputsDaily = outputsDaily; end
if exist('outputsTotal'), outputs.outputsTotal = outputsTotal; end
save(strcat('Cases\',dirString,'\Outputs_',dirString,'.mat'));
