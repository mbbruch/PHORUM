function [PHORUMdata, EVdata] = MakeEVData(PHORUMdata,settings,year,fileSuffix,dirString)
EVdata = struct;
EVModel = settings.EVdataFile;
if strlength(EVModel) == 0
    return;
end

inputFolder = strcat('.\Inputs\DrivingProfiles\');
load(strcat('.\Inputs\',fileSuffix,'\PJMpop'));
load(strcat(inputFolder,'hourDataCharge.mat'));
EVcharacteristics = readtable(strcat(inputFolder,'EVcharacteristics.csv'));
kWhPer100Mile = EVcharacteristics{strcmp(EVcharacteristics.modelName,EVModel),"kWhPer100Mile"};
range = EVcharacteristics{strcmp(EVcharacteristics.modelName,EVModel),"range"};

switch year
   case 2010
      usCars = 130892000; % https://www.bts.gov/content/automobile-profile
      usPop = 308745538; 
      totalPop = sum(PJMpop);
      PJMpop = PJMpop/totalPop;      
   case 2019
      usCars = 108547710; % https://www.bts.gov/content/automobile-profile
      usPop = 328239523; % https://www.census.gov/newsroom/press-releases/2019/popest-nation.html
      totalPop = 65000000; % PJM annual report
      %Note: PJMpop is already normalized for 2019
end 

vehPerCap = usCars/usPop;
kWhPerMile = kWhPer100Mile/100;
MWhPerMile = kWhPerMile/1000;
weights = weights/sum(weights);

chargeRate = .0074;
chargerEfficiency = .85;
percent = .1;
totalEVcount = totalPop*percent*vehPerCap;
areaEVCounts = PJMpop*totalEVcount;
initialSOC = ones(20,1)*kWhPerMile*range*1.0; %starts at full charge

%Create EVdata.mat and PHORUMData from hourDataCharge.mat for a 
%given percent of electric vehicle penetration
EVdata.model = EVModel;
EVdata.vBattery = ones(20,1)*MWhPerMile*range;
EVdata.vEff = ones(20,1)*MWhPerMile; 
EVdata.available = home_clean;
EVdata.number = weights'*areaEVCounts';
EVdata.initialSOC = initialSOC;
EVdata.vCR = ones(20,1)*chargeRate;


num_hours = size(miles_total,1);
num_profiles = size(miles_total,2);
EVdata.miles_battery = zeros(num_hours,num_profiles);
EVdata.miles_gas = zeros(num_hours,num_profiles);
EVdata.chargeAtHoursEnd = zeros(num_hours,num_profiles);
EVdata.chargeLoad_car_kWh = zeros(num_hours,num_profiles);
prevSOC = EVdata.initialSOC';
for i = 1:num_hours
    maxPossibleCharging = home_clean(i,:)*chargeRate*1000;
    drivingLoss = min(prevSOC,miles_total(i,:)*kWhPerMile);
    socPostDrivingLoss = prevSOC - drivingLoss;
    maxChargeToAddPostDrivingLoss = initialSOC'-socPostDrivingLoss;
    newChargeAdded = (prevSOC<initialSOC').*min(maxPossibleCharging,maxChargeToAddPostDrivingLoss);
    EVdata.chargeAtHoursEnd(i,:) = socPostDrivingLoss + newChargeAdded;
    EVdata.chargeLoad_car_kWh(i,:) = newChargeAdded/chargerEfficiency;
    EVdata.miles_battery(i,:) = drivingLoss/kWhPerMile;
    EVdata.miles_gas(i,:) = miles_total(i,:) - EVdata.miles_battery(i,:);
    prevSOC = EVdata.chargeAtHoursEnd(i,:);
end

%Get rid of zeros:
EVdata.miles_battery= EVdata.miles_battery(:,any(EVdata.number'));
EVdata.miles_gas= EVdata.miles_gas(:,any(EVdata.number'));
EVdata.available= EVdata.available(:,any(EVdata.number'));
EVdata.chargeAtHoursEnd= EVdata.chargeAtHoursEnd(:,any(EVdata.number'));
EVdata.chargeLoad_car_kWh= EVdata.chargeLoad_car_kWh(:,any(EVdata.number'));
EVdata.initialSOC(all(EVdata.number==0,2))=[];
EVdata.vBattery(all(EVdata.number==0,2),:)=[];
EVdata.vEff(all(EVdata.number==0,2),:)=[];
EVdata.vCR(all(EVdata.number==0,2),:)=[];
EVdata.number(all(EVdata.number==0,2),:)=[];
EVdata.chargeLoad_TCR_MWh = EVdata.chargeLoad_car_kWh*EVdata.number*0.001;

if settings.isControlledCharging==0
    % assign EV's added load to existing loads to show the charging activities
    % vehLoad = totalEVcount*staticChargeAll*.001;
    % total MWh (calc'ed via hourly electric driving drains) 
    % divided by total MWh (calc'ed via staticChargeAll)
    % ratio = sum(sum(EVdata.miles*EVdata.number,2)*(MWhPerMile/chargerEfficiency))/sum(vehLoad);

    PHORUMdata.loadData.AECO = PHORUMdata.loadData.AECO+EVdata.chargeLoad_TCR_MWh(:,1);
    PHORUMdata.loadData.CE = PHORUMdata.loadData.CE+EVdata.chargeLoad_TCR_MWh(:,2);
    PHORUMdata.loadData.PEPCO = PHORUMdata.loadData.PEPCO+EVdata.chargeLoad_TCR_MWh(:,3);
    PHORUMdata.loadData.AP = PHORUMdata.loadData.AP+EVdata.chargeLoad_TCR_MWh(:,4);
    PHORUMdata.loadData.DOM = PHORUMdata.loadData.DOM+EVdata.chargeLoad_TCR_MWh(:,5);
elseif settings.isTransmissionConstraints==0
    EVdata.number(:,1) = sum(EVdata.number,2);
    EVdata.number(:,2:5) = 0;
	EVdata.chargeLoad_TCR_MWh(:,1) = sum(EVdata.chargeLoad_TCR_MWh,2);
    EVdata.chargeLoad_TCR_MWh(:,2:5) = 0;
end

save(strcat('Cases\',dirString,'\EVdata_',EVModel,'_',string(year),'.mat'),'EVdata')
end