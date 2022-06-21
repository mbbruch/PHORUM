
base_dir <<- "C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\"
script_dir <<- paste0(base_dir,"ETL_Functions\\")
source(paste0(script_dir,"setup.R"))
files = list.files(path=script_dir, pattern="*.R",full.names=TRUE,recursive=FALSE)
files <- files[!(files %like% "data_build.R") & !(files %like% "setup.R")]
lapply(files,source)
modelingYear <- 2025
coalRetirePct <- 0.25
renewablePct <- 0.0# 0.22 #only used if year==2035
phorum.old <- importOldPHORUM();
queue <- importQueue(modelingYear);
importTransferLimits(modelingYear);
importInOuts(modelingYear);
load <- importLoad(modelingYear);
importRenewables(modelingYear,renewablePct,load,queue,phorum.old);
needs <- importNEEDS(modelingYear);
eia860 <- importEIA860(modelingYear);
merged <- needs[eia860,':='(summer.factor=summer.factor, 
                                        winter.factor=winter.factor,
                                        gen.capacity.eia=gen.capacity.eia,
                                        gen.type.eia=gen.type.eia,
                            CapacitySummer=gen.capacity.summer,
                            CapacityWinter=gen.capacity.winter),
                      on=.(plant.id=plant.id,gen.id=gen.id)]
merged[,rowcount.needs:=.N,by=plant.id]
eia860.rows <- eia860[,.(rowcount=.N),by=plant.id]
merged <- merged[eia860.rows,rowcount.eia:=rowcount,on=.(plant.id=plant.id)]
onerow.eia <- merged[rowcount.needs>1 & rowcount.eia==1]
merged <- merged[!(rowcount.needs>1 & rowcount.eia==1)]
onerow.eia <- onerow.eia[eia860,':='(summer.factor=i.summer.factor, 
                          winter.factor=i.winter.factor,
                          gen.capacity.eia=i.gen.capacity.eia,
                          gen.type.eia=i.gen.type.eia,
                          CapacitySummer=i.gen.capacity.summer,
                          CapacityWinter=i.gen.capacity.winter),
              on=.(plant.id=plant.id)]
onerow.eia <- onerow.eia[, .SD[1], plant.id][,gen.capacity.needs:=gen.capacity.eia]
merged <- rbindlist(list(merged,onerow.eia),use.names=TRUE)
merged[,':='(rowcount.eia=NULL,rowcount.needs=NULL)]
merged <- appendTCRs(merged, phorum.old);
merged <- derateHydro(merged,modelingYear);
#merged <- combineGenerators(merged);
merged <- appendCapacities(merged, eia860);
merged <- appendVarOMs(merged, phorum.old);
merged <- appendEAFs(merged, phorum.old);
merged <- appendOperationsVars(merged);
buildStorage(merged,modelingYear);
merged <- merged[!(fuel %like% "Storage")]
merged <- appendFuelPrices(merged,modelingYear);
merged <- appendDamages(merged);
merged <- retireCoal(merged,coalRetirePct)
merged <- merged[CapacitySummer >= 5 | CapacityWinter >= 5]
#merged[,num.like.this:=.N,by=.(VarOM,HeatRate,RampRate,MinUp,MinDown,StartupCost,Min)]
#num.with.ties <- merged[,.N]
#merged[num.like.this>1,VarOM:=VarOM+runif(.N,-0.01,0.01)]
merged[,':='(
  StartupCost=StartupCost*y10.to.y19,
  VarOM=VarOM*y10.to.y19
)]
merged[,NLC:=0]
write.csv(merged,paste0(out_dir,"phorum_",modelingYear,"25PctLessCoal_20220618.csv"))

PHORUMdata2021 <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\results_01132022\\phorum",modelingYear,".csv")

getPHORUMresults <- function(casename) {
  theseResults <- readMat(paste0("C:\\Users\\Matthew\\Desktop\\PJM\\results_01132022\\",casename,".mat"))[[1]][[2]]
  gLevels <- theseResults[[which(dimnames(theseResults)[[1]]=='gLevel')]]
  return(rowSums(gLevels))
}

CCphev10NoTrans2021 <- getPHORUMresults("CCphev10NoTrans2021")
UCphev10NoTrans2021 <- getPHORUMresults("UCphev10NoTrans2021")
CCphev35NoTrans2021 <- getPHORUMresults("CCphev35NoTrans2021")
UCphev35NoTrans2021 <- getPHORUMresults("UCphev35NoTrans2021")
CCbev265NoTrans2021 <- getPHORUMresults("CCTeslaNoTrans2021")
UCbev265NoTrans2021 <- getPHORUMresults("UCTeslaNoTrans2021")
NoPEV <- getPHORUMresults("NoVehNoTrans2021")

PHORUMdata2021 <- merged
PHORUMdata2021[,':='(
  nh3.md=MDNH3,
  so2.md=MDSO2,
  voc.md=MDVOC,
  nox.md=MDNOX,
  pm25.md=MDPM25,
  co2eq.rate=CO2eqv
)]

PHORUMdata2021[is.na(co2eq.rate),co2eq.rate:=0]
PHORUMdata2021[,total.md:=nh3.md+so2.md+voc.md+nox.md+pm25.md+carbon.price*co2eq.rate*tonne.per.lb]
md.NoPEV=sum(NoPEV*PHORUMdata2021$total.md)
md.UCbev265=sum(UCbev265NoTrans2021*PHORUMdata2021$total.md)
md.CCbev265=sum(CCbev265NoTrans2021*PHORUMdata2021$total.md)
md.UCphev10=sum(UCphev10NoTrans2021*PHORUMdata2021$total.md)
md.CCphev10=sum(CCphev10NoTrans2021*PHORUMdata2021$total.md)
md.UCphev35=sum(UCphev35NoTrans2021*PHORUMdata2021$total.md)
md.CCphev35=sum(CCphev35NoTrans2021*PHORUMdata2021$total.md)
md.UCbev265.diff=(md.UCbev265 - md.NoPEV)/total.cars.switched
md.CCbev265.diff=(md.CCbev265 - md.NoPEV)/total.cars.switched
md.UCphev10.diff=(md.UCphev10 - md.NoPEV)/total.cars.switched
md.CCphev10.diff=(md.CCphev10 - md.NoPEV)/total.cars.switched
md.UCphev35.diff=(md.UCphev35 - md.NoPEV)/total.cars.switched
md.CCphev35.diff=(md.CCphev35 - md.NoPEV)/total.cars.switched
PHORUM.veh[type %in% c("CV","HEV"),grid.per.car.uc:=0]
PHORUM.veh[type %in% c("CV","HEV"),grid.per.car.cc:=0]
PHORUM.veh[type %in% c("PHEV10"),':='(
  grid.per.car.uc=md.UCphev10.diff,
  grid.per.car.cc=md.CCphev10.diff
)]
PHORUM.veh[type %in% c("PHEV35"),':='(
  grid.per.car.uc=md.UCphev35.diff,
  grid.per.car.cc=md.CCphev35.diff
)]
PHORUM.veh[type %in% c("BEV265"),':='(
  grid.per.car.uc=md.UCbev265.diff,
  grid.per.car.cc=md.CCbev265.diff
)]
PHORUM.veh[is.na(PHORUM.veh)] <- 0
PHORUM.veh.fixed = rbindlist(list(PHORUM.veh,PHORUM.veh[type %in% c("PHEV10","PHEV35","BEV265")]))
PHORUM.veh.fixed[(.N-5):(.N-3),type:=paste0(type," (UC)")]
PHORUM.veh.fixed[(.N-2):.N,type:=paste0(type," (CC)")]
PHORUM.veh.fixed[str_count(type,"UC")>0,grid.per.car:=grid.per.car.uc]
PHORUM.veh.fixed[str_count(type,"CC")>0,grid.per.car:=grid.per.car.cc]
costs <- rbindlist(list(
  data.table(Powertrain=PHORUM.veh.fixed$type,Stage="Manufacture",Costs=PHORUM.veh.fixed$usd.per.car.upfront),
  data.table(Powertrain=PHORUM.veh.fixed$type,Stage="Upstream",Costs=PHORUM.veh.fixed$usd.per.car.upstream),
  data.table(Powertrain=PHORUM.veh.fixed$type,Stage="Tailpipe",Costs=PHORUM.veh.fixed$usd.per.car.tailpipe),
  data.table(Powertrain=PHORUM.veh.fixed$type,Stage="Grid",Costs=PHORUM.veh.fixed$grid.per.car)
))
costs$Stage = factor(costs$Stage, levels=c("Grid","Tailpipe","Upstream","Manufacture"))
costs$Powertrain = factor(costs$Powertrain, 
                          levels=c("CV","HEV","PHEV10 (UC)","PHEV10 (CC)",
                                   "PHEV35 (UC)", "PHEV35 (CC)",
                                   "BEV265 (UC)", "BEV265 (CC)"))

ggplot(costs,aes(x=Powertrain,y=Costs,fill=Stage))+geom_col(width=0.75) +
  xlab("Powertrain") + ylab("Air emissions costs (annual $/car)") +
  scale_x_discrete(limits = rev(levels(costs$Powertrain))) +
  scale_y_continuous(labels=scales::dollar_format(),limits=c(0,800),breaks=c(0,200,400,600,800), position="right") +
  scale_fill_brewer(palette = "Dark2", guide = guide_legend(reverse = TRUE)) +
  theme_light() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        axis.title.y = element_blank(), 
  plot.margin = unit(c(0,0,0,0), "cm")) +
  coord_flip()

PHORUMdata2021$change.uc <- UCbev265NoTrans2021 - NoPEV
PHORUMdata2021$change.cc <- CCbev265NoTrans2021 - UCbev265NoTrans2021
change.uc <- PHORUMdata2021[,.(Change=sum(change.uc)),by=fuel]
change.cc <- PHORUMdata2021[,.(Change=sum(change.cc)),by=fuel]
ggplot(change.uc, aes(x=fuel, y=Change)) + 
  geom_point() + coord_flip() + theme_light() +ylab("Change (MWh, BEV265 (UC) - CV)")
ggplot(change.cc, aes(x=fuel, y=Change)) + 
  geom_point() + coord_flip() + theme_light() +ylab("Change (MWh, BEV265 (CC) - BEV265 (UC))")

#(usd/tonne)*(tonne/g)*(g/mi)*(mpg/mpg)*(mi/km)
wtp.usd.per.km <- wtp.g.per.mi[,.(
  VOC=(refineries[pollutant=="voc",damage]/g.per.tonne)*VOC*(mpg.greet/mpg)*mi.per.km,
  NOx=(refineries[pollutant=="nox",damage]/g.per.tonne)*NOx*(mpg.greet/mpg)*mi.per.km,
  SOx=(refineries[pollutant=="so2",damage]/g.per.tonne)*SOx*(mpg.greet/mpg)*mi.per.km,
  pm25=(refineries[pollutant=="pm25",damage]/g.per.tonne)*(PM2.5.BrakesTires+PM2.5.Combustion*mpg.greet/mpg)*mi.per.km,
  ghg.g.per.km=mi.per.km*GHG*mpg.greet/mpg
)]
operation.g.per.km <- vehicle.emissions.chicago[Stage %in% c("Operation"),.(
  VOC.per.km=mi.per.km*VOC*mpg.greet/mpg,
  NOx.per.km=mi.per.km*NOx*mpg.greet/mpg,
  PM10.per.km=mi.per.km*(PM10.BrakesTires+PM10.Combustion*mpg.greet/mpg),
  PM2.5.per.km=mi.per.km*(PM2.5.BrakesTires+PM2.5.Combustion*mpg.greet/mpg),
  SOx.per.km=mi.per.km*SOx*mpg.greet/mpg,
  GHG.per.km=mi.per.km*GHG*mpg.greet/mpg)]





eia.one <- eia860.names[gen.count==1]
needs.one <- needs.names[gen.count==1]

needs.rollup.temp <- needs[,.N,by=.(`ORIS Plant Code`,`Plant Name`,`ORIS Plant Code`, PlantType)]


merged <- eia.one[needs.one,on=.(plant.id=plant.id)][length(overall.type.eia)>0][,
                                                                                 ':='(summer.ratio=gen.capacity.summer/gen.capacity.needs,
                                                                                      winter.ratio=gen.capacity.winter/gen.capacity.needs,
                                                                                      total.ratio=gen.capacity.eia/gen.capacity.needs)]

temp.1 <- needs.names[,sum(capacity,na.rm=TRUE), by=fuel]
temp.2 <- eia860.names[,sum(capacity,na.rm=TRUE), by=fuel]