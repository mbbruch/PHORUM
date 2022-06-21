function [totalResults, prevDayResults] = InitializeResultsStruct(settings)

% Structure to store results from all days
totalResults.gap= [];
totalResults.objective=[];
totalResults.modelStatus= [];
totalResults.solveStatus= [];
totalResults.emissions=[];
totalResults.allgen=[];
totalResults.gLevel = [];
totalResults.gVC = [];
totalResults.gRuntime = [];
totalResults.gStartup = [];
totalResults.gGrossRevenue = [];
totalResults.sysCost = [];
totalResults.tLevelTI12 = [];
totalResults.tLevelTI13 = [];
totalResults.tLevelTI15 = [];
totalResults.tLevelTI52 = [];
totalResults.tLevelTI23 = [];
totalResults.tLevelTI34 = [];
totalResults.tMaxTI12 = [];
totalResults.tMaxTI13 = [];
totalResults.tMaxTI15 = [];
totalResults.tMaxTI52 = [];
totalResults.tMaxTI23 = [];
totalResults.tMaxTI34 = [];
totalResults.LMPTCR1 = [];
totalResults.LMPTCR2 = [];
totalResults.LMPTCR3 = [];
totalResults.LMPTCR4 = [];
totalResults.LMPTCR5 = [];
totalResults.LMPTCR1actual = [];
totalResults.LMPTCR2actual = [];
totalResults.LMPTCR3actual = [];
totalResults.LMPTCR4actual = [];
totalResults.LMPTCR5actual = [];
totalResults.sCharge = [];
totalResults.sDischarge = [];
totalResults.sSOC = [];
totalResults.sNetRevenue = [];
totalResults.date = [];
totalResults.runtime = [];
totalResults.loadTCR1 = [];
totalResults.loadTCR2 = [];
totalResults.loadTCR3 = [];
totalResults.loadTCR4 = [];
totalResults.loadTCR5 = [];
totalResults.windTCR1 = [];
totalResults.windTCR2 = [];
totalResults.windTCR3 = [];
totalResults.windTCR4 = [];
totalResults.windTCR5 = [];

% Structure to track cross-day variables
prevDayResults.Ontime = [];
prevDayResults.Downtime = [];
prevDayResults.InitState = [];
prevDayResults.InitGen = [];
prevDayResults.sInitSOC = [];

if settings.isControlledCharging
    totalResults.vSOC = [];
    totalResults.vCharge = [];
    totalResults.vDischarge = [];
    totalResults.vChargeH=[];
    prevDayResults.vInitSOC = [];
end
