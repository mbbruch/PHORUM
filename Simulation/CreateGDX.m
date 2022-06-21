function CreateGDX(day, PHORUMdata, settings, prevDayResults, EVdata, dirString)
%% Settings
EAFderating = 0.5;

%% Load data from PHORUMdata structure
genData = PHORUMdata.genData;
loadData = PHORUMdata.loadData;
storageData = PHORUMdata.storageData;

% Set range of modeled hours based on starting day
tStart = day * 24 + 1 - 24;
tEnd = tStart + (settings.optWindow - 1);

% Assign summer/winter capacity based on start date
if tStart > 2900 && tStart < 6588
    Capacity = genData.CapacitySummer;
else
    Capacity = genData.CapacityWinter;
end

% Assign fuel costs and EAF based on start date
if (tStart >=1 && tStart < 745)        % Jan
    FuelPrice = genData.FuelPrice(:,1); 
    EAF = genData.EAF(:,1);
end
if (tStart >=745 && tStart < 1417)     % Feb
    FuelPrice = genData.FuelPrice(:,2); 
    EAF = genData.EAF(:,2);
end
if (tStart >=1417 && tStart < 2161)    % Mar 
    FuelPrice = genData.FuelPrice(:,3); 
    EAF = genData.EAF(:,3);
end
if (tStart >=2161 && tStart < 2881)    % Apr
    FuelPrice = genData.FuelPrice(:,4); 
    EAF = genData.EAF(:,4);
end
if (tStart >=2881 && tStart < 3625)    % May
    FuelPrice = genData.FuelPrice(:,5); 
    EAF = genData.EAF(:,5);
end
if (tStart >=3625 && tStart < 4345)    % June
    FuelPrice = genData.FuelPrice(:,6); 
    EAF = genData.EAF(:,6);
end
if (tStart >=4345 && tStart < 5089)    % July
    FuelPrice = genData.FuelPrice(:,7); 
    EAF = genData.EAF(:,7);
end
if (tStart >=5089 && tStart < 5833)    % Aug
    FuelPrice = genData.FuelPrice(:,8); 
    EAF = genData.EAF(:,8);
end
if (tStart >=5833 && tStart < 6553)    % Sept
    FuelPrice = genData.FuelPrice(:,9); 
    EAF = genData.EAF(:,9);
end
if (tStart >=6553 && tStart < 7297)    % Oct
    FuelPrice = genData.FuelPrice(:,10); 
    EAF = genData.EAF(:,10);
end
if (tStart >=7297 && tStart < 8017)    % Nov
    FuelPrice = genData.FuelPrice(:,11); 
    EAF = genData.EAF(:,11);
end
if (tStart >=8017)                     % Dec
    FuelPrice = genData.FuelPrice(:,12);      
    EAF = genData.EAF(:,12);
end               

% Apply EAF derating
if(max(EAF) > 1.001) EAF = EAF/100; end
EAF = EAF + (1 - EAF).*EAFderating;
Capacity = Capacity .* EAF;
% Calculate variable cost.  Include emission prices and marginal damages as 
% specified by settings. Also include 10% cost adder on fuel price and variable O&M as allowed by PJM.
VC = 1.1*(genData.HeatRate.*FuelPrice + genData.VarOM);

StartupCost = genData.StartupCost;

if settings.isEmissionsPrice == 1
    EmissionPrice = (genData.NOX*settings.priceNOX + genData.SO2*settings.priceSO2 + genData.N2O*settings.priceN2O + genData.CO2*settings.priceCO2 + genData.CO2eqv*settings.priceCO2eqv + genData.CO*settings.priceCO + genDta.NH3*settings.priceNH3 + genData.PM10*settings.pricePM10 + genData.PM25*settings.pricePM25 + genData.VOC*settings.priceVOC)/2000;
    MDPrice = genData.MDNH3*settings.isMDNH3 + genData.MDSO2*settings.isMDSO2 + genData.MDVOC*settings.isMDVOC + genData.MDNOX*settings.isMDNOX + genData.MDPM25*settings.isMDPM25 + genData.MDPM10*settings.isMDPM10;
    StartupEmissionPrice = (genData.StartupNOX*settings.priceNOX + genData.StartupSO2*settings.priceSO2 + genData.StartupCO2*settings.priceCO2);
    StartupCost = StartupCost + genData.StartupEmissionPrice + genData.StartupMDNOX*settings.isMDNOX + genData.StartupMDSO2*settings.isMDSO2;
    VC = VC + EmissionPrice + StartupEmissionPrice + MDPrice;
end

%% Create generator GDX files

% Set up GAMS subsets of generators for each TCR
gens = [];
gensTCR1 = [];
gensTCR2 = [];
gensTCR3 = [];
gensTCR4 = [];
gensTCR5 = [];
countTCR1 = 1;
countTCR2 = 1;
countTCR3 = 1;
countTCR4 = 1;
countTCR5 = 1;
for g = 1 : size(genData.TCR,1)
    gens{g} = strcat('g',num2str(g));
    if genData.TCR(g) == 1 
        gensTCR1{countTCR1} = strcat('g',num2str(g));
        countTCR1 = countTCR1+1;
    elseif genData.TCR(g) == 2 
        gensTCR2{countTCR2} = strcat('g',num2str(g));
        countTCR2 = countTCR2+1;
    elseif genData.TCR(g) == 3 
        gensTCR3{countTCR3} = strcat('g',num2str(g));
        countTCR3 = countTCR3+1;
    elseif genData.TCR(g) == 4 
        gensTCR4{countTCR4} = strcat('g',num2str(g));
        countTCR4 = countTCR4+1;
    elseif genData.TCR(g) == 5 
        gensTCR5{countTCR5} = strcat('g',num2str(g));
        countTCR5 = countTCR5+1;
    end
end

% Create GDX structures
gensGDX.name = 'g';
gensGDX.type = 'set';
gensGDX.uels = gens;

gensTCR1GDX.name = 'gTCR1';
gensTCR1GDX.type = 'set';
gensTCR1GDX.uels = gensTCR1;

gensTCR2GDX.name = 'gTCR2';
gensTCR2GDX.type = 'set';
gensTCR2GDX.uels = gensTCR2;

gensTCR3GDX.name = 'gTCR3';
gensTCR3GDX.type = 'set';
gensTCR3GDX.uels = gensTCR3;

gensTCR4GDX.name = 'gTCR4';
gensTCR4GDX.type = 'set';
gensTCR4GDX.uels = gensTCR4;

gensTCR5GDX.name = 'gTCR5';
gensTCR5GDX.type = 'set';
gensTCR5GDX.uels = gensTCR5;

CapacityGDX.name = 'gCapacity';
CapacityGDX.type = 'parameter';
CapacityGDX.uels = gens;
CapacityGDX.form = 'full';
CapacityGDX.dim = 1;
CapacityGDX.val = Capacity;

RampRateGDX.name = 'gRampRate';
RampRateGDX.type = 'parameter';
RampRateGDX.uels = gens;
RampRateGDX.form = 'full';
RampRateGDX.dim = 1;
RampRateGDX.val = genData.RampRate;

MinCapacityGDX.name = 'gMinCapacity';
MinCapacityGDX.type = 'parameter';
MinCapacityGDX.uels = gens;
MinCapacityGDX.form = 'full';
MinCapacityGDX.dim = 1;
MinCapacityGDX.val = genData.Min.*Capacity;

MinUpGDX.name = 'gMinUp';
MinUpGDX.type = 'parameter';
MinUpGDX.uels = gens;
MinUpGDX.form = 'full';
MinUpGDX.dim = 1;
MinUpGDX.val = genData.MinUp;

MinDownGDX.name = 'gMinDown';
MinDownGDX.type = 'parameter';
MinDownGDX.uels = gens;
MinDownGDX.form = 'full';
MinDownGDX.dim = 1;
MinDownGDX.val = genData.MinDown;

StartupCostGDX.name = 'gStartupC';
StartupCostGDX.type = 'parameter';
StartupCostGDX.uels = gens;
StartupCostGDX.form = 'full';
StartupCostGDX.dim = 1;
StartupCostGDX.val = StartupCost;

VCGDX.name = 'gVC';
VCGDX.type = 'parameter';
VCGDX.uels = gens;
VCGDX.form = 'full';
VCGDX.dim = 1;
VCGDX.val = VC;

NLCGDX.name = 'gNLC';
NLCGDX.type = 'parameter';
NLCGDX.uels = gens;
NLCGDX.form = 'full';
NLCGDX.dim = 1;
NLCGDX.val = genData.NLC;
if settings.isEmissionsPrice == 1
	emissionRate.name = 'emissionRate';
	emissionRate.type = 'parameter';
	emissionRate.uels = gens;
	emissionRate.form = 'full';
	emissionRate.dim = 1;
	emissionRate.val = genData.CO2;
end

% Calculate initial state and runtime/offtime from previous run.  Load to
% GDX.
Ontime = prevDayResults.Ontime;
Downtime = prevDayResults.Downtime;
InitGen = prevDayResults.InitGen;
InitState = prevDayResults.InitState;

if isempty(Ontime)
    Ontime = zeros(length(Capacity),1);
else
    for index = 1 : length(Ontime)
        if Ontime(index) ~= 0
            Ontime(index) = genData.MinUp(index) - Ontime(index);
            if Ontime(index) < 0
                Ontime(index) = 0;
            end
        end
    end
end
if isempty(Downtime)
    Downtime = zeros(length(Capacity),1);
else
    for index = 1 : length(Downtime)
        if Downtime(index) ~= 0
            Downtime(index) = genData.MinDown(index) - Downtime(index);
            if Downtime(index) < 0
                Downtime(index) = 0;
            end
        end
    end
end

OntimeGDX.name = 'gOntime';
OntimeGDX.type = 'parameter';
OntimeGDX.uels = gens;
OntimeGDX.form = 'full';
OntimeGDX.dim = 1;
OntimeGDX.val = Ontime;

DowntimeGDX.name = 'gDowntime';
DowntimeGDX.type = 'parameter';
DowntimeGDX.uels = gens;
DowntimeGDX.form = 'full';
DowntimeGDX.dim = 1;
DowntimeGDX.val = Downtime;

if isempty(InitState)
    InitState = zeros(size(gens,2),1);
end
InitStateGDX.name = 'gInitState';
InitStateGDX.type = 'parameter';
InitStateGDX.uels = gens;
InitStateGDX.form = 'full';
InitStateGDX.dim = 1;
InitStateGDX.val = InitState;

if isempty(InitGen)
    InitGen = zeros(1,size(gens,2));
end

% If the init gen is larger than max gen, due to offline gen issues, set
% init gen equal to max gen
for index = 1 : size(InitGen, 2)
    if InitGen(1,index) > Capacity(index,1)
        InitGen(1,index) = Capacity(index,1);
    end
    if InitGen(1,index) > 0 && InitGen(1,index) < genData.Min(index,1)*Capacity(index,1)
        InitGen(1,index) = genData.Min(index,1)*Capacity(index,1);
    end
end

InitGenGDX.name = 'gInitGen'; 
InitGenGDX.type = 'parameter';
InitGenGDX.uels = gens;
InitGenGDX.form = 'full';
InitGenGDX.dim = 1;
InitGenGDX.val = InitGen;

% Write GDX file
if settings.isEmissionsPrice == 1
wgdx('GenData', gensGDX, gensTCR1GDX, gensTCR2GDX, gensTCR3GDX, gensTCR4GDX, gensTCR5GDX, InitStateGDX, InitGenGDX, OntimeGDX, DowntimeGDX, CapacityGDX, VCGDX, RampRateGDX, MinCapacityGDX, MinUpGDX, MinDownGDX, StartupCostGDX, NLCGDX, emissionRate); 
else
wgdx('GenData', gensGDX, gensTCR1GDX, gensTCR2GDX, gensTCR3GDX, gensTCR4GDX, gensTCR5GDX, InitStateGDX, InitGenGDX, OntimeGDX, DowntimeGDX, CapacityGDX, VCGDX, RampRateGDX, MinCapacityGDX, MinUpGDX, MinDownGDX, StartupCostGDX, NLCGDX); 
end

%Load-serving utilities
utilitiesTCR1 = ["AEP","AEPAPT","AEPIMP","AEPKPT","AEPOPT","AP","APS","COMED","CE","DAY","DEOK","DUQ","EKPC","OE","OVEC","PAPWR","PENELEC","PN"];
utilitiesTCR2 = ["BC","BGE","PEPCO","SMECO"];
utilitiesTCR3 = ["ME","METED","PLCO","PPL","UGI"];
utilitiesTCR4 = ["AECO","DPL","DPLCO","EASTON","JC","JCPL","PE","PECO","PS","PSEG","RECO","VMEU"];
utilitiesTCR5 = ["DOM"];
utilities = cat(2,utilitiesTCR1,utilitiesTCR2,utilitiesTCR3,utilitiesTCR4,utilitiesTCR5);
for n = 1:length(utilities); AddLoadUnit(loadData,'',utilities(n),tStart,tEnd); end


TCRs = ["1","2","3","4","5"];
if sum(sum(AEP))==0
    AEP = AEPAPT + AEPIMP + AEPKPT + AEPOPT;
end

AEPAPT = 0; AEPIMP = 0; AEPKPT = 0; AEPOPT = 0;

% Imports / exports
importsAEP = ["ALTE","ALTW","CPLW","CWLP","DUK","EKPC","IPL","LAGN","LGEE","MEC","MECS","NIPS","OVEC","SIGE","TVA","WEC"];
importsCE = ["AMIL","MDU"];
importsDAY = ["CIN"];
importsDOM = ["CPLE"];
importsPENELEC = ["FE"];
importsPSEG = ["HUDS","LIND","NEPT","NYIS"];
importExportAreas = cat(2,importsAEP,importsCE,importsDAY,importsDOM,importsPENELEC,importsPSEG);
for n = 1:length(importExportAreas); AddLoadUnit(PHORUMdata.importData,'',importExportAreas(n),tStart,tEnd); end

% Wind / solar
TCRs = ["1","2","3","4","5"];
for n = 1:length(TCRs); AddLoadUnit(PHORUMdata.renewablesData,'windTCR',TCRs(n),tStart,tEnd); end
for n = 1:length(TCRs); AddLoadUnit(PHORUMdata.renewablesData,'solarTCR',TCRs(n),tStart,tEnd); end

% Assign imports to utility zones
for n = 1:length(importsAEP); AEP = AEP - eval(importsAEP(n)); end
for n = 1:length(importsCE); CE = CE - eval(importsCE(n)); end
for n = 1:length(importsDAY); DAY = DAY - eval(importsDAY(n)); end
for n = 1:length(importsDOM); DOM = DOM - eval(importsDOM(n)); end
for n = 1:length(importsPENELEC); PENELEC = PENELEC - eval(importsPENELEC(n)); end
for n = 1:length(importsPSEG); PSEG = PSEG - eval(importsPSEG(n)); end

% Assign utility zones to TCRs
loadTCR1 = zeros(settings.optWindow,1); loadTCR2 = loadTCR1; loadTCR3 = loadTCR1; loadTCR4 = loadTCR1; loadTCR5 = loadTCR1;
for n = 1:length(utilitiesTCR1); loadTCR1 = loadTCR1 + eval(utilitiesTCR1(n)); end
for n = 1:length(utilitiesTCR2); loadTCR2 = loadTCR2 + eval(utilitiesTCR2(n)); end
for n = 1:length(utilitiesTCR3); loadTCR3 = loadTCR3 + eval(utilitiesTCR3(n)); end
for n = 1:length(utilitiesTCR4); loadTCR4 = loadTCR4 + eval(utilitiesTCR4(n)); end
for n = 1:length(utilitiesTCR5); loadTCR5 = loadTCR5 + eval(utilitiesTCR5(n)); end
  
% 
% Allocate synchronized reserve.  Reserve requirements equal to largest 
% single generator in TCR1, TCRs2-4,and TCR5
loadTCR1 = loadTCR1 + max(genData.gen_capacity_needs(genData.TCR==1));
loadTCR2 = loadTCR2 + max(genData.gen_capacity_needs(genData.TCR==2 | genData.TCR==3 | genData.TCR==4));
loadTCR3 = loadTCR3 + max(genData.gen_capacity_needs(genData.TCR==2 | genData.TCR==3 | genData.TCR==4));
loadTCR4 = loadTCR4 + max(genData.gen_capacity_needs(genData.TCR==2 | genData.TCR==3 | genData.TCR==4));
loadTCR5 = loadTCR5 + max(genData.gen_capacity_needs(genData.TCR==5));

% Setup for initial run - 
% Add time period 0, which is hour 24 from the previous optimization.
% Because ramping and supply/demand constraints don't hold for this hour,
% set load equal to t=1
loadTCR1 = [loadTCR1(1); loadTCR1];
loadTCR2 = [loadTCR2(1); loadTCR2];
loadTCR3 = [loadTCR3(1); loadTCR3];
loadTCR4 = [loadTCR4(1); loadTCR4];
loadTCR5 = [loadTCR5(1); loadTCR5];
windTCR1 = [windTCR1(1); windTCR1];
windTCR2 = [windTCR2(1); windTCR2];
windTCR3 = [windTCR3(1); windTCR3];
windTCR4 = [windTCR4(1); windTCR4];
windTCR5 = [windTCR5(1); windTCR5];
solarTCR1 = [solarTCR1(1); solarTCR1];
solarTCR2 = [solarTCR2(2); solarTCR2];
solarTCR3 = [solarTCR3(3); solarTCR3];
solarTCR4 = [solarTCR4(4); solarTCR4];
solarTCR5 = [solarTCR5(1); solarTCR5];

% Transmission constraints
if settings.isTransmissionConstraints == 1
	TI12 = PHORUMdata.transferLimitData.TI12(tStart:tEnd);
	TI13 = PHORUMdata.transferLimitData.TI13(tStart:tEnd);
	TI15 = PHORUMdata.transferLimitData.TI15(tStart:tEnd);
	TI23 = PHORUMdata.transferLimitData.TI23(tStart:tEnd);
	TI25 = PHORUMdata.transferLimitData.TI25(tStart:tEnd);
	TI34 = PHORUMdata.transferLimitData.TI34(tStart:tEnd);
	
	TI12 = [TI12(1); TI12];
	TI13 = [TI13(1); TI13];
	TI15 = [TI15(1); TI15];
	TI23 = [TI23(1); TI23];
	TI25 = [TI25(1); TI25];
	TI34 = [TI34(1); TI34];
end
%% Create load GDX files

% Create GDX structures
timeVars = {};
for t = 1 : length(loadTCR1)
    timeVars{t} = strcat('t',num2str(t));
end
time.name = 't';
time.type = 'set';
time.uels = timeVars;


if settings.isEmissionsPrice == 1
	loadObjective.name = 'objective';
	loadObjective.val = objective;
	loadObjective.type = 'parameter';
	loadObjective.form = 'full';
	loadObjective.dim = 0;
end

loadTCR1GDX.name = 'loadTCR1';
loadTCR1GDX.val = loadTCR1;
loadTCR1GDX.type = 'parameter';
loadTCR1GDX.uels = timeVars;
loadTCR1GDX.form = 'full';
loadTCR1GDX.dim = 1;

loadTCR2GDX.name = 'loadTCR2';
loadTCR2GDX.val = loadTCR2;
loadTCR2GDX.type = 'parameter';
loadTCR2GDX.uels = timeVars;
loadTCR2GDX.form = 'full';
loadTCR2GDX.dim = 1;

loadTCR3GDX.name = 'loadTCR3';
loadTCR3GDX.val = loadTCR3;
loadTCR3GDX.type = 'parameter';
loadTCR3GDX.uels = timeVars;
loadTCR3GDX.form = 'full';
loadTCR3GDX.dim = 1;

loadTCR4GDX.name = 'loadTCR4';
loadTCR4GDX.val = loadTCR4;
loadTCR4GDX.type = 'parameter';
loadTCR4GDX.uels = timeVars;
loadTCR4GDX.form = 'full';
loadTCR4GDX.dim = 1;

loadTCR5GDX.name = 'loadTCR5';
loadTCR5GDX.val = loadTCR5;
loadTCR5GDX.type = 'parameter';
loadTCR5GDX.uels = timeVars;
loadTCR5GDX.form = 'full';
loadTCR5GDX.dim = 1;

if settings.isTransmissionConstraints == 1
	TI12maxGDX.name = 'TI12max';
	TI12maxGDX.val = TI12;
	TI12maxGDX.type = 'parameter';
	TI12maxGDX.uels = timeVars;
	TI12maxGDX.form = 'full';
	TI12maxGDX.dim = 1;
	 
	TI13maxGDX.name = 'TI13max';
	TI13maxGDX.val = TI13;
	TI13maxGDX.type = 'parameter';
	TI13maxGDX.uels = timeVars;
	TI13maxGDX.form = 'full';
	TI13maxGDX.dim = 1;
	 
	TI15maxGDX.name = 'TI15max';
	TI15maxGDX.val = TI15;
	TI15maxGDX.type = 'parameter';
	TI15maxGDX.uels = timeVars;
	TI15maxGDX.form = 'full';
	TI15maxGDX.dim = 1;
	 
	TI52maxGDX.name = 'TI52max';
	TI52maxGDX.val = TI25;
	TI52maxGDX.type = 'parameter';
	TI52maxGDX.uels = timeVars;
	TI52maxGDX.form = 'full';
	TI52maxGDX.dim = 1;
	 
	TI23maxGDX.name = 'TI23max';
	TI23maxGDX.val = TI23;
	TI23maxGDX.type = 'parameter';
	TI23maxGDX.uels = timeVars;
	TI23maxGDX.form = 'full';
	TI23maxGDX.dim = 1;
	 
	TI34maxGDX.name = 'TI34max';
	TI34maxGDX.val = TI34;
	TI34maxGDX.type = 'parameter';
	TI34maxGDX.uels = timeVars;
	TI34maxGDX.form = 'full';
	TI34maxGDX.dim = 1;
end

windSolarTCR1GDX.name = 'windSolarTCR1';
windSolarTCR1GDX.val = windTCR1 + solarTCR1;
windSolarTCR1GDX.type = 'parameter';
windSolarTCR1GDX.uels = timeVars;
windSolarTCR1GDX.form = 'full';
windSolarTCR1GDX.dim = 1;
 
windSolarTCR2GDX.name = 'windSolarTCR2';
windSolarTCR2GDX.val = windTCR2 + solarTCR2;
windSolarTCR2GDX.type = 'parameter';
windSolarTCR2GDX.uels = timeVars;
windSolarTCR2GDX.form = 'full';
windSolarTCR2GDX.dim = 1;
 
windSolarTCR3GDX.name = 'windSolarTCR3';
windSolarTCR3GDX.val = windTCR3 + solarTCR3;
windSolarTCR3GDX.type = 'parameter';
windSolarTCR3GDX.uels = timeVars;
windSolarTCR3GDX.form = 'full';
windSolarTCR3GDX.dim = 1;
 
windSolarTCR4GDX.name = 'windSolarTCR4';
windSolarTCR4GDX.val = windTCR4 + solarTCR4;
windSolarTCR4GDX.type = 'parameter';
windSolarTCR4GDX.uels = timeVars;
windSolarTCR4GDX.form = 'full';
windSolarTCR4GDX.dim = 1;
 
windSolarTCR5GDX.name = 'windSolarTCR5';
windSolarTCR5GDX.val = windTCR5 + solarTCR5;
windSolarTCR5GDX.type = 'parameter';
windSolarTCR5GDX.uels = timeVars;
windSolarTCR5GDX.form = 'full';
windSolarTCR5GDX.dim = 1;

% Create load GDX file
if settings.isTransmissionConstraints
    wgdx('LoadData', time, loadTCR1GDX, loadTCR2GDX, loadTCR3GDX, loadTCR4GDX, loadTCR5GDX, windSolarTCR1GDX, windSolarTCR2GDX, windSolarTCR3GDX, windSolarTCR4GDX, windSolarTCR5GDX, TI12maxGDX, TI13maxGDX, TI15maxGDX, TI52maxGDX, TI23maxGDX, TI34maxGDX); %loadObjective;
else
    wgdx('LoadData', time, loadTCR1GDX, loadTCR2GDX, loadTCR3GDX, loadTCR4GDX, loadTCR5GDX, windSolarTCR1GDX, windSolarTCR2GDX, windSolarTCR3GDX, windSolarTCR4GDX, windSolarTCR5GDX);
end

% Set initial SOC.  If this is the first day, set equal to 1/2 capacity.
% If this is not the first day, use results from the previous day.
if isempty(prevDayResults.sInitSOC)
    sInitSOC = storageData.EnergyCapacity/2;
else
    sInitSOC = prevDayResults.sInitSOC;
end

% Set the max SOC (capacity in MWh)
sSOCmax = storageData.EnergyCapacity;

% Set up GAMS subsets of generators for each TCR.  If a TCR does not
% contain any storage, direct it to the 'blank' entry in the storage
% database.
stors = [];
storsTCR1 = [];
storsTCR2 = [];
storsTCR3 = [];
storsTCR4 = [];
storsTCR5 = [];
countTCR1 = 1;
countTCR2 = 1;
countTCR3 = 1;
countTCR4 = 1;
countTCR5 = 1;
for s = 1 : size(storageData.TCR,1)
    stors{s} = strcat('s',num2str(s));
    if storageData.TCR(s) == 1 
        storsTCR1{countTCR1} = strcat('s',num2str(s));
        countTCR1 = countTCR1+1;
    elseif storageData.TCR(s) == 2 
        storsTCR2{countTCR2} = strcat('s',num2str(s));
        countTCR2 = countTCR2+1;
    elseif storageData.TCR(s) == 3 
        storsTCR3{countTCR3} = strcat('s',num2str(s));
        countTCR3 = countTCR3+1;
    elseif storageData.TCR(s) == 4 
        storsTCR4{countTCR4} = strcat('s',num2str(s));
        countTCR4 = countTCR4+1;
    elseif storageData.TCR(s) == 5 
        storsTCR5{countTCR5} = strcat('s',num2str(s));
        countTCR5 = countTCR5+1;
    end
end

storsGDX.name = 's';
storsGDX.type = 'set';
storsGDX.uels = stors;

storsTCR1GDX.name = 'sTCR1';
storsTCR1GDX.type = 'set';
if isempty(storsTCR1) == 0
    storsTCR1GDX.uels = storsTCR1;   
else
    storsTCR1GDX.uels = {'s1'};
end
storsTCR2GDX.name = 'sTCR2';
storsTCR2GDX.type = 'set';
if isempty(storsTCR2) == 0
    storsTCR2GDX.uels = storsTCR2;
else
    storsTCR2GDX.uels = {'s1'};
end
storsTCR3GDX.name = 'sTCR3';
storsTCR3GDX.type = 'set';
if isempty(storsTCR3) == 0
    storsTCR3GDX.uels = storsTCR3;
else
    storsTCR3GDX.uels = {'s1'};
end
storsTCR4GDX.name = 'sTCR4';
storsTCR4GDX.type = 'set';
if isempty(storsTCR4) == 0
    storsTCR4GDX.uels = storsTCR4;
else
    storsTCR4GDX.uels = {'s1'};
end
storsTCR5GDX.name = 'sTCR5';
storsTCR5GDX.type = 'set';
if isempty(storsTCR5) == 0
    storsTCR5GDX.uels = storsTCR5;
else
    storsTCR5GDX.uels = {'s1'};
end

sRampRateGDX.name = 'sRampRate';
sRampRateGDX.type = 'parameter';
sRampRateGDX.val = storageData.PowerCapacity;
sRampRateGDX.uels = stors;
sRampRateGDX.form = 'full';
sRampRateGDX.dim = 1;

sSOCmaxGDX.name = 'sSOCmax';
sSOCmaxGDX.type = 'parameter';
sSOCmaxGDX.val = sSOCmax;
sSOCmaxGDX.uels = stors;
sSOCmaxGDX.form = 'full';
sSOCmaxGDX.dim = 1;

sChargeEffGDX.name = 'sChargeEff';
sChargeEffGDX.type = 'parameter';
sChargeEffGDX.val = storageData.ChargeEfficiency;
sChargeEffGDX.uels = stors;
sChargeEffGDX.form = 'full';
sChargeEffGDX.dim = 1;

sDischargeEffGDX.name = 'sDischargeEff';
sDischargeEffGDX.type = 'parameter';
sDischargeEffGDX.val = storageData.DischargeEfficiency;
sDischargeEffGDX.uels = stors;
sDischargeEffGDX.form = 'full';
sDischargeEffGDX.dim = 1;

sVCGDX.name = 'sMargCost';
sVCGDX.type = 'parameter';
sVCGDX.val = storageData.VariableCost;
sVCGDX.uels = stors;
sVCGDX.form = 'full';
sVCGDX.dim = 1;

sInitSOCGDX.name = 'sInitSOC';
sInitSOCGDX.type = 'parameter';
sInitSOCGDX.val = sInitSOC;
sInitSOCGDX.uels = stors;
sInitSOCGDX.form = 'full';
sInitSOCGDX.dim = 1;

wgdx('StorageData', storsGDX, storsTCR1GDX, storsTCR2GDX, storsTCR3GDX, storsTCR4GDX, storsTCR5GDX, sRampRateGDX, sSOCmaxGDX, sChargeEffGDX, sDischargeEffGDX, sVCGDX, sInitSOCGDX);

if settings.isControlledCharging
    % Set initial SOC.  If this is the first day, set equal to 1/2 capacity.
    % If this is not the first day, use results from the previous day.
    if isempty(prevDayResults.vInitSOC)
        vInitSOC = EVdata.initialSOC.*EVdata.number*0.001;
    else
        vInitSOC = prevDayResults.vInitSOC;
    end

    for v = 1 : length(EVdata.number(:,1))
        vehProfiles{v} = strcat('v',num2str(v));
    end
    vehiclesGDX.name = 'v';
    vehiclesGDX.type = 'set';
    vehiclesGDX.uels = vehProfiles;

    for a = 1 : length(EVdata.number(1,:))
        transAreas{a} = strcat('a',num2str(a));
    end
    areasGDX.name = 'a';
    areasGDX.type = 'set';
    areasGDX.uels = transAreas;

    vMilesGDX.name = 'vMiles';
    vMilesGDX.type = 'parameter';
    vMilesGDX.val = [EVdata.miles_battery(24,:);EVdata.miles_battery(tStart:tEnd,:)];
    vMilesGDX.uels = {timeVars,vehProfiles}; 
    vMilesGDX.form = 'full';
    vMilesGDX.dim = 2;

    vAvailableGDX.name = 'vAvailable';
    vAvailableGDX.type = 'parameter';
    vAvailableGDX.val = EVdata.available(tStart:tEnd,:);
    vAvailableGDX.uels = {timeVars,vehProfiles}; 
    vAvailableGDX.form = 'full';
    vAvailableGDX.dim = 2;

    vNumGDX.name = 'vNum';
    vNumGDX.type = 'parameter';
    vNumGDX.val = EVdata.number;
    vNumGDX.uels = {vehProfiles,transAreas}; 
    vNumGDX.form = 'full';
    vNumGDX.dim = 2;

    vInitSOCGDX.name = 'vInitSOC';
    vInitSOCGDX.type = 'parameter';
    vInitSOCGDX.val = vInitSOC;
    vInitSOCGDX.uels = {vehProfiles,transAreas}; 
    vInitSOCGDX.form = 'full';
    vInitSOCGDX.dim = 2;

    vBatteryGDX.name = 'vBattery';
    vBatteryGDX.type = 'parameter';
    vBatteryGDX.val = EVdata.vBattery;
    vBatteryGDX.uels = vehProfiles;
    vBatteryGDX.form = 'full';
    vBatteryGDX.dim = 1;

    vCRGDX.name = 'vCR';
    vCRGDX.type = 'parameter';
    vCRGDX.val = EVdata.vCR;
    vCRGDX.uels = vehProfiles;
    vCRGDX.form = 'full';
    vCRGDX.dim = 1;

    vEffGDX.name = 'vEff';
    vEffGDX.type = 'parameter';
    vEffGDX.val = EVdata.vEff;
    vEffGDX.uels = vehProfiles;
    vEffGDX.form = 'full';
    vEffGDX.dim = 1;

    wgdx('VehData',vehiclesGDX, areasGDX,vMilesGDX,vAvailableGDX,vNumGDX,vInitSOCGDX,vBatteryGDX,vCRGDX,vEffGDX)
end
movefile('*.gdx',strcat('Cases\',dirString))
end
