
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
