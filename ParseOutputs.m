 function [totalResults, prevDayResults] = ParseOutputs(totalResults, PHORUMdata, day, dEnd, totalRuntime, tic, settings, EVdata, dirString)

% This function parses the results from the GAMS run.  It pulls
% results from results.GDX, cleans the data, and saves it to the
% totalResults structure.  Cross-day variables are saved to the
% prevDayResults structure. 
numGens = length(PHORUMdata.genData.HeatRate);
numStors = length(PHORUMdata.storageData.PowerCapacity);
isEVanalysis = (strlength(settings.EVdataFile)>0);
% Check if this is the last day of the range.  If so, save results for all 48 hours.
 hour = 24;
 lastDay = 0;
 if day == dEnd - 1
     hour = settings.optWindow;
     lastDay = 1;
 end
  
% Save date and runtime
totalResults.date = [totalResults.date, day];
if lastDay == 1
    totalResults.date = [totalResults.date, day+1];
end    

runtime = toc;
totalRuntime = sum(totalResults.runtime) + runtime;
totalResults.runtime = [totalResults.runtime,runtime];
disp(['Day complete.  Runtime: ', num2str(runtime/60), ', Total runtime: ', num2str(totalRuntime/60)]);

GAMSoutput.form = 'full';
GAMSoutput.compress = 'false';
gdxFileString = convertStringsToChars(strcat(pwd(),'\Cases\',dirString,'\results.gdx'));

GAMSoutput.name = 'solveStatus'; output = rgdx(gdxFileString, GAMSoutput);
solveStatus = output.val;
totalResults.solveStatus = [totalResults.solveStatus, output.val];
GAMSoutput.name = 'modelStatus'; output = rgdx(gdxFileString, GAMSoutput);
totalResults.modelStatus = [totalResults.modelStatus, output.val];
GAMSoutput.name = 'gap'; output = rgdx(gdxFileString, GAMSoutput);
totalResults.gap = [totalResults.gap, output.val];

%% If optimization did not execute correctly, set results = 0 and return
if solveStatus > 3 %1=normal completion, 2=iteration interrupt, 3=resource interrupt (time limit)
    totalResults.loadTCR1 = [totalResults.loadTCR1, zeros(1,hour)];
    totalResults.loadTCR2 = [totalResults.loadTCR2, zeros(1,hour)];
    totalResults.loadTCR3 = [totalResults.loadTCR3, zeros(1,hour)];
    totalResults.loadTCR4 = [totalResults.loadTCR4, zeros(1,hour)];
    totalResults.loadTCR5 = [totalResults.loadTCR5, zeros(1,hour)];

%   Storage
    totalResults.sSOC = [totalResults.sSOC, zeros(numStors,hour)];

    % Transmission
%    totalResults.tLevelTI12 = [totalResults.tLevelTI12, zeros(1,hour)];
%    totalResults.tLevelTI13 = [totalResults.tLevelTI13, zeros(1,hour)];
%    totalResults.tLevelTI15 = [totalResults.tLevelTI15, zeros(1,hour)];
%    totalResults.tLevelTI52 = [totalResults.tLevelTI52, zeros(1,hour)];
%    totalResults.tLevelTI23 = [totalResults.tLevelTI23, zeros(1,hour)];
%    totalResults.tLevelTI34 = [totalResults.tLevelTI34, zeros(1,hour)];

%    totalResults.tMaxTI12 = [totalResults.tMaxTI12, zeros(1,hour)];
%    totalResults.tMaxTI13 = [totalResults.tMaxTI13, zeros(1,hour)];
%    totalResults.tMaxTI15 = [totalResults.tMaxTI15, zeros(1,hour)];
%    totalResults.tMaxTI52 = [totalResults.tMaxTI52, zeros(1,hour)];
%    totalResults.tMaxTI23 = [totalResults.tMaxTI23, zeros(1,hour)];
%    totalResults.tMaxTI34 = [totalResults.tMaxTI34, zeros(1,hour)];

    % LMPs
%    totalResults.LMPTCR1 = [totalResults.LMPTCR1, zeros(1,hour)];
%    totalResults.LMPTCR2 = [totalResults.LMPTCR2, zeros(1,hour)];
%    totalResults.LMPTCR3 = [totalResults.LMPTCR3, zeros(1,hour)];
%    totalResults.LMPTCR4 = [totalResults.LMPTCR4, zeros(1,hour)];
%    totalResults.LMPTCR5 = [totalResults.LMPTCR5, zeros(1,hour)];
%    totalResults.LMPTCR1actual = [totalResults.LMPTCR1actual, zeros(1,hour)];
%    totalResults.LMPTCR2actual = [totalResults.LMPTCR2actual, zeros(1,hour)];
%    totalResults.LMPTCR3actual = [totalResults.LMPTCR3actual, zeros(1,hour)];
%    totalResults.LMPTCR4actual = [totalResults.LMPTCR4actual, zeros(1,hour)];
%    totalResults.LMPTCR5actual = [totalResults.LMPTCR5actual, zeros(1,hour)];

    % System Cost
    totalResults.sysCost = [totalResults.sysCost, 0];
    totalResults.gLevel = [totalResults.gLevel, zeros(numGens,1)];
    totalResults.gRuntime = [totalResults.gRuntime, zeros(numGens,1)];
    totalResults.gStartup = [totalResults.gStartup, zeros(numGens,1)];
%    totalResults.gGrossRevenue = [totalResults.gGrossRevenue, zeros(numGens,1)];
    totalResults.gVC = [totalResults.gVC, zeros(numGens,1)];
%    totalResults.wind = [totalResults.wind, 0];
%    totalResults.windSolarTCR1 = [totalResults.windSolarTCR1, 0];
%    totalResults.windSolarTCR2 = [totalResults.windSolarTCR2, 0];
%    totalResults.windSolarTCR3 = [totalResults.windSolarTCR3, 0];
%    totalResults.windSolarTCR4 = [totalResults.windSolarTCR4, 0];
%    totalResults.windSolarTCR5 = [totalResults.windSolarTCR5, 0];

%   totalResults.sNetRevenue = [totalResults.sNetRevenue, zeros(numStors,1)];
    totalResults.sCharge = [totalResults.sCharge, zeros(numStors,1)];
    totalResults.sDischarge = [totalResults.sDischarge, zeros(numStors,1)];

    if isEVanalysis == 1   
        totalResults.vSOC = [totalResults.vSOC, zeros(size(EVdata.prevSOC,1),hour,size(EVdata.prevSOC,2))];
        totalResults.vCharge = [totalResults.vCharge, zeros(size(EVdata.prevSOC,1),1,size(EVdata.prevSOC,2))];
        totalResults.vDischarge = [totalResults.vDischarge, zeros(size(EVdata.prevSOC,1),1,size(EVdata.prevSOC,2))];    
    end

% Set prevDayResults to null
     prevDayResults.Ontime = [];
     prevDayResults.Downtime = [];
     prevDayResults.InitState = [];
     prevDayResults.InitGen = [];
     prevDayResults.sInitSOC = [];
     if isEVanalysis == 1, prevDayResults.vInitSOC = []; end

     save('totalResults', 'totalResults');
    return;
end


%% Pull outputs from GAMS


% Wind
% GAMSoutput.name = 'wind';
% output = rgdx(gdxFileString,GAMSoutput);
% wind = output.val;


% Generation
GAMSoutput.name = 'gLevel';
output = rgdx(gdxFileString, GAMSoutput);
gLevel = output.val;

% Variable costs
GAMSoutput.name = 'gVC';
output = rgdx(gdxFileString, GAMSoutput);
gVC = output.val;

% Load
GAMSoutput.name = 'loadTCR1';
output = rgdx(gdxFileString, GAMSoutput);
loadTCR1 = output.val;
GAMSoutput.name = 'loadTCR2';
output = rgdx(gdxFileString, GAMSoutput);
loadTCR2 = output.val;
GAMSoutput.name = 'loadTCR3';
output = rgdx(gdxFileString, GAMSoutput);
loadTCR3 = output.val;
GAMSoutput.name = 'loadTCR4';
output = rgdx(gdxFileString, GAMSoutput);
loadTCR4 = output.val;
GAMSoutput.name = 'loadTCR5';
output = rgdx(gdxFileString, GAMSoutput);
loadTCR5 = output.val;


% Storage
GAMSoutput.compress='false';
GAMSoutput.name = 'sSOC';
output  = rgdx(gdxFileString,GAMSoutput);
sSOC = output.val;
GAMSoutput.compress='false';

GAMSoutput.name = 'sDischarge';
output = rgdx(gdxFileString,GAMSoutput);
sDischarge = output.val;
GAMSoutput.name = 'sCharge';
output = rgdx(gdxFileString,GAMSoutput);
sCharge = output.val;

% Hourly system cost
GAMSoutput.name = 'HourlyCost';
output = rgdx(gdxFileString, GAMSoutput);
sysCost = output.val;

% If running EVs, pull EV data
if settings.isControlledCharging == 1
    GAMSoutput.name = 'vSOC';
    GAMSoutput.compress = 'true';
    GAMSoutput.form = 'full';
    output = rgdx(gdxFileString, GAMSoutput);
    vSOC = output.val;
%    GAMSoutput.name = 'vCharge';
%    output = rgdx(gdxFileString, GAMSoutput);
%    vCharge = output.val;
end

% LMPs
% GAMSoutput.compress = 'false';
% GAMSoutput.field = 'm';
% GAMSoutput.name = 'SUPPLY';
% output = rgdx(gdxFileString, GAMSoutput);
% LMPTCR1 = -output.val;


%% Clean result structures


% Remove extraneous values returned by GAMS which are not needed
numStorageUnits = size(PHORUMdata.storageData.TCR,1);
numGens = size(PHORUMdata.genData.TCR,1);
numResults = size(gLevel,1);

% % Storage - calculate sCharge & sDischarge

 if solveStatus > 3
    sSOC = zeros(numStorageUnits,hour);
    sDischarge = zeros(numStorageUnits,hour);
    sCharge = zeros(numStorageUnits,hour);
    sDiff = zeros(numStorageUnits,hour);
 else
    sDiff = diff(sSOC,1,2);
    sDischarge = sDischarge(numResults-(numStorageUnits-1):numResults, 2:hour+1);
    % sDischarge = sDischarge(numGens+1: numGens+6, 2:hour+1);
    sCharge = sCharge(numResults-(numStorageUnits-1):numResults, 2:hour+1);
    sSOC = sSOC(numResults-(numStorageUnits-1):numResults, 2:hour+1);
 end


sCharge = [];
sDischarge = [];
for y = 1 : size(sSOC,1)
    for i = 1 : hour
        if sDiff(y,i) >= 0
            sCharge(y,i) = sDiff(y,i);
            sDischarge(y,i) = 0;
        else
            sCharge(y,i) = 0;
            sDischarge(y,i) = -sDiff(y,i);
        end
    end
end
        

% Sys Cost 
sysCost = sysCost(2:hour+1);

% Generators
gLevel = gLevel(numResults - (numStorageUnits) - (numGens)+1 : numResults - numStorageUnits, 2:hour+1);
gVC = gVC(numResults - (numStorageUnits) - (numGens)+1 : numResults - numStorageUnits, 1);

% Load
loadTCR1 = loadTCR1(2:hour+1);
loadTCR2 = loadTCR2(2:hour+1);
loadTCR3 = loadTCR3(2:hour+1);
loadTCR4 = loadTCR4(2:hour+1);
loadTCR5 = loadTCR5(2:hour+1);

% wind = wind(2:hour+1);

% EVs
 if settings.isControlledCharging == 1
    % Track vSOC
    if solveStatus > 3 % If the day did not solve
       vSOC = zeros(size(EVdata.number,1),hour,size(EVdata.number,2));
       vDiff = zeros(size(EVdata.number,1),hour,size(EVdata.number,2));
    else
        vDiff = diff(vSOC,1,2);
        vSOC = vSOC(:,2:hour+1,:);
    end
    % Track vCharge & vDischarge
    vCharge = [];
    vDischarge = [];
    for y = 1 : size(vSOC,1)
        for z = 1 : size(vSOC,3)
            for i = 1 : hour
                if vDiff(y,i,z) >= 0
                    vCharge(y,i, z) = vDiff(y,i, z);
                    vDischarge(y,i, z) = 0;
                else
                    vCharge(y,i, z) = 0;
                    vDischarge(y,i, z) = -vDiff(y,i, z);
                end
            end
        end
    end
end

%% Load prevDayResults

% How long generators have been on / off
for index = 1 : size(gLevel,1)
    hourCounter = 0;
    prevDayResults.Ontime(index) = 0;
    prevDayResults.Downtime(index) = 0;
    if gLevel(index, hour) > 0
        while gLevel(index, hour - hourCounter) > 0 && hourCounter < hour - 1
            prevDayResults.Ontime(index) = prevDayResults.Ontime(index) + 1;
            hourCounter = hourCounter + 1;
        end
    end
    if gLevel(index, hour) == 0
        while gLevel(index, hour - hourCounter) == 0 && hourCounter < hour - 1
            prevDayResults.Downtime(index) = prevDayResults.Downtime(index) + 1;
            hourCounter = hourCounter + 1;
        end
    end        
end

% Generator level and state (on/off)
for index = 1 : size(gLevel,1)
    prevDayResults.InitGen(index) = gLevel(index,hour);
    if gLevel(index,hour) > 0
        prevDayResults.InitState(index) = 1;
    else
        prevDayResults.InitState(index) = 0;
    end
end

% Storage state of charge
for index = 1 : size(sSOC,1)
    prevDayResults.sInitSOC(index) = sSOC(index,hour);
end

% Vehicle state of charge
if settings.isControlledCharging == 1
    prevDayResults.vInitSOC = zeros(size(EVdata.number,1),1,size(EVdata.number,2));
    for index = 1:size(EVdata.number,1) 
        prevDayResults.vInitSOC(index,1:size(vSOC,3)) = squeeze(vSOC(index,hour,1:size(vSOC,3)));
    end 
    prevDayResults.vInitSOC = squeeze(prevDayResults.vInitSOC);
end 


 %% Add the day's results to totalResults

% First, hourly results.

% Load
totalResults.loadTCR1 = [totalResults.loadTCR1, loadTCR1'];
totalResults.loadTCR2 = [totalResults.loadTCR2, loadTCR2'];
totalResults.loadTCR3 = [totalResults.loadTCR3, loadTCR3'];
totalResults.loadTCR4 = [totalResults.loadTCR4, loadTCR4'];
totalResults.loadTCR5 = [totalResults.loadTCR5, loadTCR5'];

% Storage
totalResults.sSOC = [totalResults.sSOC, sSOC(1:numStorageUnits,:)];

% Transmission
%totalResults.tLevelTI12 = [totalResults.tLevelTI12, tLevelTI12'];
%totalResults.tLevelTI13 = [totalResults.tLevelTI13, tLevelTI13'];
%totalResults.tLevelTI15 = [totalResults.tLevelTI15, tLevelTI15'];
%totalResults.tLevelTI52 = [totalResults.tLevelTI52, tLevelTI52'];
%totalResults.tLevelTI23 = [totalResults.tLevelTI23, tLevelTI23'];
%totalResults.tLevelTI34 = [totalResults.tLevelTI34, tLevelTI34'];

%totalResults.tMaxTI12 = [totalResults.tMaxTI12, tMaxTI12'];
%totalResults.tMaxTI13 = [totalResults.tMaxTI13, tMaxTI13'];
%totalResults.tMaxTI15 = [totalResults.tMaxTI15, tMaxTI15'];
%totalResults.tMaxTI52 = [totalResults.tMaxTI52, tMaxTI52'];
%totalResults.tMaxTI23 = [totalResults.tMaxTI23, tMaxTI23'];
%totalResults.tMaxTI34 = [totalResults.tMaxTI34, tMaxTI34'];

% LMPs
%totalResults.LMPTCR1 = [totalResults.LMPTCR1, LMPTCR1'];
%totalResults.LMPTCR2 = [totalResults.LMPTCR2, LMPTCR2'];
%totalResults.LMPTCR3 = [totalResults.LMPTCR3, LMPTCR3'];
%totalResults.LMPTCR4 = [totalResults.LMPTCR4, LMPTCR4'];
%totalResults.LMPTCR5 = [totalResults.LMPTCR5, LMPTCR5'];

%totalResults.LMPTCR1actual = [totalResults.LMPTCR1actual, LMPTCR1actual'];
%totalResults.LMPTCR2actual = [totalResults.LMPTCR2actual, LMPTCR2actual'];
%totalResults.LMPTCR3actual = [totalResults.LMPTCR3actual, LMPTCR3actual'];
%totalResults.LMPTCR4actual = [totalResults.LMPTCR4actual, LMPTCR4actual'];
%totalResults.LMPTCR5actual = [totalResults.LMPTCR5actual, LMPTCR5actual'];

% Next, daily outputs
% System Cost
totalResults.sysCost = [totalResults.sysCost, sum(sysCost)'];

% Generation
gRuntime = zeros(size(gLevel,1),size(gLevel,2));
gStartup = zeros(size(gLevel,1),size(gLevel,2));
%gGrossRevenue = zeros(size(gLevel,1),size(gLevel,2));

gTCR = PHORUMdata.genData.TCR;

for rowIndex = 1 : size(gLevel,1)
    for colIndex = 1 : size(gLevel,2)
        % Generator runtime
        if gLevel(rowIndex,colIndex) > 0
            gRuntime(rowIndex,colIndex) = 1;
            % Generator startup
            if colIndex > 1 
                if gLevel(rowIndex,colIndex - 1) == 0
                    gStartup(rowIndex,colIndex) = 1;
                end
            end
        end
%{
        % Generator gross revenue
        if (gTCR(rowIndex) == 1) 
            gGrossRevenue(rowIndex,colIndex) = LMPTCR1(colIndex)*gLevel(rowIndex,colIndex);
        elseif (gTCR(rowIndex) == 2) 
            gGrossRevenue(rowIndex,colIndex) = LMPTCR2(colIndex)*gLevel(rowIndex,colIndex);
        elseif (gTCR(rowIndex) == 3) 
            gGrossRevenue(rowIndex,colIndex) = LMPTCR3(colIndex)*gLevel(rowIndex,colIndex);
        elseif (gTCR(rowIndex) == 4) 
            gGrossRevenue(rowIndex,colIndex) = LMPTCR4(colIndex)*gLevel(rowIndex,colIndex);
        elseif (gTCR(rowIndex) == 5) 
            gGrossRevenue(rowIndex,colIndex) = LMPTCR5(colIndex)*gLevel(rowIndex,colIndex);
        end 
   %}
    end
end
totalResults.gLevel = [totalResults.gLevel, sum(gLevel,2)];
totalResults.gRuntime = [totalResults.gRuntime, sum(gRuntime,2)];
totalResults.gStartup = [totalResults.gStartup, sum(gStartup,2)];
% totalResults.gGrossRevenue = [totalResults.gGrossRevenue, sum(gGrossRevenue,2)];
totalResults.gVC = [totalResults.gVC, gVC];
% totalResults.wind = [totalResults.wind,sum(wind)];

% Storage gross revenue
%{
sTCR = PHORUMdata.storageData.sTCR;
sNetRevenue = zeros(size(sSOC,1),size(sSOC,2));
for rowIndex = 1 : size(sSOC,1)
    for colIndex = 1 : size(sSOC,2)
        if (sTCR(rowIndex) == 1) 
            sNetRevenue(rowIndex,colIndex) = LMPTCR1(colIndex)*(sDischarge(rowIndex,colIndex)-sCharge(rowIndex,colIndex));
        elseif (sTCR(rowIndex) == 2) 
            sNetRevenue(rowIndex,colIndex) = LMPTCR2(colIndex)*(sDischarge(rowIndex,colIndex)-sCharge(rowIndex,colIndex));
        elseif (sTCR(rowIndex) == 3) 
            sNetRevenue(rowIndex,colIndex) = LMPTCR3(colIndex)*(sDischarge(rowIndex,colIndex)-sCharge(rowIndex,colIndex));
        elseif (sTCR(rowIndex) == 4) 
            sNetRevenue(rowIndex,colIndex) = LMPTCR4(colIndex)*(sDischarge(rowIndex,colIndex)-sCharge(rowIndex,colIndex));
        elseif (sTCR(rowIndex) == 5) 
            sNetRevenue(rowIndex,colIndex) = LMPTCR5(colIndex)*(sDischarge(rowIndex,colIndex)-sCharge(rowIndex,colIndex));
        end
    end
end

totalResults.sNetRevenue = [totalResults.sNetRevenue, sum(sNetRevenue,2)];
%}
% totalResults.sCharge = [totalResults.sCharge, sum(sCharge,2)];
% totalResults.sDischarge = [totalResults.sDischarge, sum(sDischarge,2)];

% EV outputs -- outputting HOURLY outputs.
if settings.isControlledCharging == 1
    totalResults.vSOC = [totalResults.vSOC, vSOC];
    totalResults.vCharge = [totalResults.vCharge, sum(vCharge,2)];
    totalResults.vDischarge = [totalResults.vDischarge, sum(vDischarge,2)];    
end

save(strcat('Cases\',dirString,'\totalResults.mat'),'totalResults');
save(strcat('Cases\',dirString,'\prevDayResults.mat'),'prevDayResults');
end