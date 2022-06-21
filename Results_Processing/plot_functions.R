
getPHORUMgendata <- function(year,runString){
     coalSuffix <- str_extract(runString,"[0-9]{1,3}PctLessCoal")
     if(is.na(coalSuffix)){
          PHORUMdata <- fread(paste0(results_dir,"phorum_",year,".csv"))
     } else{
          PHORUMdata <- fread(paste0(results_dir,"phorum_",year,"_",coalSuffix,".csv"))
     }
     PHORUMdata[,':='(
          nh3.md=MDNH3,
          so2.md=MDSO2,
          voc.md=MDVOC,
          nox.md=MDNOX,
          pm25.md=MDPM25,
          co2eq.rate=CO2eqv
     )]
     
     PHORUMdata[is.na(co2eq.rate),co2eq.rate:=0]
     PHORUMdata[,total.md:=(nh3.md+so2.md+voc.md+nox.md+pm25.md)+carbon.price*co2eq.rate*tonne.per.lb]
     if(year==2010){
          PHORUMdata$gen.type.eia=as.character(NA)
     }
     return(PHORUMdata)
}

fixup2010 <- function(){
     damages <- fread(paste0(etl_dir,"county_data\\caces.damages.all.counties.csv"))
     phorum <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\results_20220607\\phorum_2010_prelim.csv")

     phorum <- phorum[,.(
          fuel.type,
          detailed.type,
          fips,
          NH3,
          SO2=parse_number(SO2),
          VOC,
          NOX=parse_number(NOX),
          PM25,
          CO2eqv=parse_number(CO2eqv)
     )]
     phorum[fuel.type %like% "NG",fuel_group:="Natural Gas"]
     phorum[fuel.type %like% "Petroleum Coke",fuel_group:="Petroleum Coke"]
     phorum[fuel.type %like% "BIT" | fuel.type %like% "SUB" | fuel.type %like% "Coal",fuel_group:="Coal"]
     phorum[fuel.type %like% "RFO" | fuel.type %like% "DFO",fuel_group:="Petroleum"]
     phorum[fuel.type %like% "MSW" | fuel.type %like% "Biomass" | fuel.type %like% "Landfill" | 
                     fuel.type %like% "Fossil Waste" | fuel.type %like% "NUC" | 
                     fuel.type %like% "Hydro", fuel_group:="Free"]
     
     phorum[damages[model=='INMAP' & pollutant=='pm25'],MDPM25:=damage.acs.stacklevel*PM25*tonne.per.lb*acs.to.pope2019*vsl,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='so2'],MDSO2:=damage.acs.stacklevel*SO2*tonne.per.lb*acs.to.pope2019*vsl,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='voc'],MDVOC:=damage.acs.stacklevel*VOC*tonne.per.lb*acs.to.pope2019*vsl,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='nh3'],MDNH3:=damage.acs.stacklevel*NH3*tonne.per.lb*acs.to.pope2019*vsl,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='nox'],MDNOX:=damage.acs.stacklevel*NOX*tonne.per.lb*acs.to.pope2019*vsl,on=.(fips=fips)]
     
     phorum[is.na(MDPM25),MDPM25:=0]
     phorum[is.na(MDSO2),MDSO2:=0]
     phorum[is.na(MDVOC),MDVOC:=0]
     phorum[is.na(MDNH3),MDNH3:=0]
     phorum[is.na(MDNOX),MDNOX:=0]
     write.csv(phorum,"C:\\Users\\Matthew\\Desktop\\PJM\\results_20220607\\phorum_2010.csv")
}


getPHORUMGridEmissions <- function(fullString) {
     theseResults <- readMat(paste0(fullString,"\\totalResults.mat"))[[1]]
     genMWh <- rowSums(theseResults[[which(dimnames(theseResults)[[1]]=='gLevel')]],na.rm=TRUE)
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString,"_")>3) {
          detail <- substr(runString,str_locate(runString, "_Txn[0-1]")[2]+2,str_length(runString))
     }
     thisPHORUMdata <- getPHORUMgendata(year,runString)
     thisPHORUMdata$genMWh <- genMWh
     out1 <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(thisPHORUMdata[,sum(genMWh*NH3*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*SO2*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*VOC*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*NOX*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*PM25*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*co2eq.rate*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*CO*tonne.per.lb,na.rm=TRUE)]))
     out1[,stage:="grid"]
     
     thisPHORUMdata[fuel_group %in% c("Petroleum Coke","Free"),fuel_group:="Other"]
     thisPHORUMdata[fuel=="NUC",fuel_group:="Nuclear"]
     thisPHORUMdata[,type_for_upstream:="Default"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combined Cycle",type_for_upstream:="CC"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combustion Turbine" & (is.na(gen.type.eia) | gen.type.eia!="IC"),type_for_upstream:="GT"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combustion Turbine" & gen.type.eia=="IC",type_for_upstream:="ICE"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type %like% "Steam",type_for_upstream:="Steam"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combined Cycle",type_for_upstream:="CC"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combustion Turbine" & (is.na(gen.type.eia) | gen.type.eia!="IC"),type_for_upstream:="GT"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combustion Turbine" & gen.type.eia=="IC",type_for_upstream:="ICE"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type %like% "Steam",type_for_upstream:="Steam"]
     gen.by.fuel <- thisPHORUMdata[fuel_group!="Other",.(genMWh=sum(genMWh,na.rm=TRUE)),by=.(fuel_group,type_for_upstream)]
     feedstock.emissions <- fread(paste0(etl_dir,"misc\\Greet_Grid_Upstream_Emissions_20220608.csv"))
     gen.by.fuel <- gen.by.fuel[feedstock.emissions,on=.(fuel_group=Fuel,type_for_upstream=Type)]
     out2 <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(gen.by.fuel[,sum(genMWh*0*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*SOx*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*VOC*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*NOx*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*PM2.5*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*GHGs*tonne.per.lb,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*CO*tonne.per.lb,na.rm=TRUE)]))
     out2[,stage:="upstream_grid"]
     out <- rbindlist(list(out1,out2))
     return(out[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

getPHORUMGridDamages <- function(fullString) {
     theseResults <- readMat(paste0(fullString,"\\totalResults.mat"))[[1]]
     genMWh <- rowSums(theseResults[[which(dimnames(theseResults)[[1]]=='gLevel')]],na.rm=TRUE)
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString,"_")>3) {
          detail <- word(runString,-1,sep="_")
     }
     thisPHORUMdata <- getPHORUMgendata(year,runString)
     thisPHORUMdata$genMWh <- genMWh
     out1 <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(thisPHORUMdata[,sum(genMWh*nh3.md,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*so2.md,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*voc.md,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*nox.md,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*pm25.md,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*carbon.price*co2eq.rate*tonne.per.lb,na.rm=TRUE)],
                               thisPHORUMdata[,sum(genMWh*CO*co.cost,na.rm=TRUE)]))
     out1[,stage:="grid"]
     
     thisPHORUMdata[fuel_group %in% c("Petroleum Coke","Free"),fuel_group:="Other"]
     thisPHORUMdata[fuel=="NUC",fuel_group:="Nuclear"]
     thisPHORUMdata[,type_for_upstream:="Default"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combined Cycle",type_for_upstream:="CC"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combustion Turbine" & (is.na(gen.type.eia) | gen.type.eia!="IC"),type_for_upstream:="GT"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combustion Turbine" & gen.type.eia=="IC",type_for_upstream:="ICE"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type %like% "Steam",type_for_upstream:="Steam"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combined Cycle",type_for_upstream:="CC"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combustion Turbine" & (is.na(gen.type.eia) | gen.type.eia!="IC"),type_for_upstream:="GT"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type=="Combustion Turbine" & gen.type.eia=="IC",type_for_upstream:="ICE"]
     thisPHORUMdata[fuel_group=="Petroleum" & detailed.type %like% "Steam",type_for_upstream:="Steam"]
     gen.by.fuel <- thisPHORUMdata[fuel_group!="Other",.(genMWh=sum(genMWh,na.rm=TRUE)),by=.(fuel_group,type_for_upstream)]
     feedstock.emissions <- fread(paste0(etl_dir,"misc\\Greet_Grid_Upstream_Emissions_20220608.csv"))
     gen.by.fuel <- gen.by.fuel[feedstock.emissions,on=.(fuel_group=Fuel,type_for_upstream=Type)]
     feedstock.damages.oil <- getOilFeedstockDamages()
     feedstock.damages.gas <- getNatGasFeedstockDamages()
     feedstock.damages.coal <- getCoalFeedstockDamages()
     gen.by.fuel[,':='(nh3.md=0.0,so2.md=0.0,voc.md=0.0,nox.md=0.0,pm25.md=0.0)]
     gen.by.fuel[fuel_group=="Petroleum",':='(
          nh3.md=feedstock.damages.oil[pollutant=="nh3",]$damage,
          so2.md=feedstock.damages.oil[pollutant=="so2",]$damage,
          voc.md=feedstock.damages.oil[pollutant=="voc",]$damage,
          nox.md=feedstock.damages.oil[pollutant=="nox",]$damage,
          pm25.md=feedstock.damages.oil[pollutant=="pm25",]$damage
     )]
     gen.by.fuel[fuel_group=="Natural Gas",':='(
          nh3.md=feedstock.damages.gas[pollutant=="nh3",]$damage,
          so2.md=feedstock.damages.gas[pollutant=="so2",]$damage,
          voc.md=feedstock.damages.gas[pollutant=="voc",]$damage,
          nox.md=feedstock.damages.gas[pollutant=="nox",]$damage,
          pm25.md=feedstock.damages.gas[pollutant=="pm25",]$damage
     )]
     gen.by.fuel[fuel_group=="Coal",':='(
          nh3.md=feedstock.damages.coal[pollutant=="nh3",]$damage,
          so2.md=feedstock.damages.coal[pollutant=="so2",]$damage,
          voc.md=feedstock.damages.coal[pollutant=="voc",]$damage,
          nox.md=feedstock.damages.coal[pollutant=="nox",]$damage,
          pm25.md=feedstock.damages.coal[pollutant=="pm25",]$damage
     )]
     out2 <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(gen.by.fuel[,sum(genMWh*0*tonne.per.lb*nh3.md,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*SOx*tonne.per.lb*so2.md,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*VOC*tonne.per.lb*voc.md,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*NOx*tonne.per.lb*nox.md,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*PM2.5*tonne.per.lb*pm25.md,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*GHGs*tonne.per.lb*carbon.price,na.rm=TRUE)],
                               gen.by.fuel[,sum(genMWh*CO*co.cost,na.rm=TRUE)]))
     out2[,stage:="upstream_grid"]
     out <- rbindlist(list(out1,out2))
     return(out[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

getTotalMiles_Old <- function(){
     driving.profiles <- fread(paste0(etl_dir,"vehicle_profiles\\vehicle_profile_details.csv"))
     total.cars.switched <- max(driving.profiles[,sum(cars),by=.(powertrain)]$V1)
     total.miles <- driving.profiles[,.(demand_miles_fleet=sum(demand_miles_fleet,na.rm=TRUE),
                                        combustion_miles_fleet=sum(combustion_miles_fleet,na.rm=TRUE),
                                        battery_miles_fleet=sum(battery_miles_fleet,na.rm=TRUE)),by=powertrain]
     total.miles[,avg.lifespan:=(miles.greet*total.cars.switched/demand_miles_fleet)]
     return(total.miles)
}

getDiscountRateMultiplier <- function(){
     profiles <- fread(paste0(etl_dir,"vehicle_profiles\\vehicle_profile_details.csv"))
     profiles <- profiles[powertrain=="CV" & cars > 0,.(
          profile,
          cars,
          weight=cars/sum(cars),
          avmt=demand_miles_car,
          lifespan=miles.greet/demand_miles_car)]
     profiles[,':='(
          full.years=floor(lifespan),
          part.year.fraction=lifespan-floor(lifespan)
     )]
     profiles$npv <- 0
     for(row in 1:nrow(profiles)){
          profiles[row,]$npv <- -1*NPV(cf0=-1,
                                       cf=c(rep(-1,times=profiles[row,]$full.years-1),-profiles[row,]$part.year.fraction),
                                       times=c(1:profiles[row,]$full.years),
                                       i=0.05) #3% discount rate with 2% inflation
     }
     
     return(weighted.mean(profiles$npv,profiles$weight))
}

getPJMCounties <- function(){
     county_data <- fread(paste0(etl_dir,"county_data\\county_2019_populations.csv"))
     needs <- fread(paste0(etl_dir,"generators\\needs_20201006.csv"))
     region_fips <- needs[,.(units=.N),by=.(FIPS5,`Region Name`)
     ][,
       county_max_region:=max(units,na.rm=FALSE),by=.(FIPS5)
     ][units==county_max_region]
     region_fips <- region_fips[,':='(max_region=max(`Region Name`)),by=.(FIPS5)]
     region_fips <- region_fips[max_region==`Region Name`]
     region_fips[county_data,':='(lat=INTPTLAT,long=INTPTLONG),on=.(FIPS5=GEOID)]
     
     index <- get.knnx(region_fips[,.(long,lat)], county_data[,.(INTPTLONG,INTPTLAT)],1)$nn.index #TODO use lat-longs!
     county_data$region <- region_fips[index,`Region Name`]
     county_data <- county_data[substr(region,1,3)=='PJM']
     county_data <- county_data[,.(fips=GEOID,pop_pct=`2019_Population`/sum(`2019_Population`,na.rm=TRUE))]
     return(county_data)
}

getTailpipeDamages_AllCounties <- function(){
     county_data <- getPJMCounties()
     tailpipe.damages <- county_data[caces.county,on=.(fips=fips)][!is.na(pop_pct)]
     tailpipe.damages <- 
          tailpipe.damages[,.(
               damage=weighted.mean(damage.acs.groundlevel*acs.to.pope2019*vsl,w=pop_pct,na.rm=TRUE)
          ),by=.(pollutant)]
}

getTailpipeDamages <- function(specific.county=-1){
     if(specific.county==-1){
          tailpipe.damages <- getTailpipeDamages_AllCounties()
     }
     else{
          tailpipe.damages <- caces.county[fips==specific.county,
                                           .(damage=mean(damage.acs.groundlevel*acs.to.pope2019*vsl)),
                                           by=.(pollutant)]
     }
     return(tailpipe.damages)
}

getManufactureDamages <- function(){
     manufacture.counties <- fread(paste0(etl_dir,"county_data\\manufacture_counties.csv"))
     manufacture.counties <- manufacture.counties[caces.county,on=.(county=fips)]
     manufacture.counties[is.na(manufacture.counties)] <- 0
     manufacture.damages <- 
          manufacture.counties[,.(
               other=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=auto.cbp.pct,na.rm=TRUE),
               cu.prod=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=NiCuCo.Prod.cbp.pct,na.rm=TRUE),
               cu.mine=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=NiCu.Mine.cbp.pct,na.rm=TRUE),
               co.mine=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=CoMn.Mine.cbp.pct,na.rm=TRUE),
               li.prod=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=Li.Prod.cbp.pct,na.rm=TRUE),
               graphite.prod=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=Graphite.Prod.cbp.pct,na.rm=TRUE)
          ),by=.(pollutant)]
     return(manufacture.damages)
}

getOilFeedstockDamages <- function(){
     #Per https://michaelminn.net/tutorials/data-sources/2017-refineries-metadata.html
     #Atmospheric Distillation Mbpd (AD_Mbpd):
     #Usually a good proxy for general facility capacity. The volume of crude oil, 
     #measured in thousands of barrels per day (Mb/d), processed by the atmospheric 
     #distillation chamber of a refinery. The refining process of separating crude oil 
     #components at atmospheric pressure by heating to temperatures of about 600º to 750º F 
     #(depending on the nature of the crude oil and desired products) and subsequent condensing 
     #of the fractions by cooling.
     county_data <- fread(paste0(etl_dir,"county_data\\county_2019_populations.csv"))
     sites <- fread(paste0(etl_dir,"county_data\\Petroleum_Refineries.csv"))
     total.cap <- sum(sites$AD_Mbpd,na.rm=TRUE)
     index <- get.knnx(county_data[,.(INTPTLONG,INTPTLAT)],sites[,.(Longitude,Latitude)],1)$nn.index #TODO use lat-longs!
     sites$FIPS <- county_data[index,GEOID]
     sites <- sites[,.(percent=sum(AD_Mbpd,na.rm=TRUE)/total.cap),by=FIPS][percent > 0]
     damages <- sites[caces.county,on=.(FIPS=fips),.(fips=fips,
                                                     damage.acs.stacklevel=damage.acs.stacklevel,
                                                     pollutant=pollutant,
                                                     model=model,
                                                     season=season,
                                                     percent=percent)][!is.na(percent)] 
     damages <- damages[,.(damage=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=percent)
     ),by=.(pollutant)]
     return(damages)
}
getNatGasFeedstockDamages <- function(){
     county_data <- fread(paste0(etl_dir,"county_data\\county_2019_populations.csv"))
     sites <- fread(paste0(etl_dir,"county_data\\Natural_Gas_Processing_Plants.csv"))
     total.cap <- sum(sites$Plant_Flow,na.rm=TRUE)
     index <- get.knnx(county_data[,.(INTPTLONG,INTPTLAT)],sites[,.(Longitude,Latitude)],1)$nn.index #TODO use lat-longs!
     sites$FIPS <- county_data[index,GEOID]
     sites <- sites[,.(percent=sum(Plant_Flow,na.rm=TRUE)/total.cap),by=FIPS][percent > 0]
     damages <- sites[caces.county,on=.(FIPS=fips),.(fips=fips,
                                                     damage.acs.stacklevel=damage.acs.stacklevel,
                                                     pollutant=pollutant,
                                                     model=model,
                                                     season=season,
                                                     percent=percent)][!is.na(percent)] 
     damages <- damages[,.(damage=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=percent)
     ),by=.(pollutant)]
     return(damages)
}
getCoalFeedstockDamages <- function(){
     county_data <- fread(paste0(etl_dir,"county_data\\county_2019_populations.csv"))
     sites <- fread(paste0(etl_dir,"county_data\\Coal_Mines.csv"))
     sites[,capacity:=parse_number(tot_prod)]
     total.cap <- sum(sites$capacity,na.rm=TRUE)
     sites[,FIPS:=mstafips*1000+mctyfips]
     sites <- sites[,.(percent=sum(capacity,na.rm=TRUE)/total.cap),by=FIPS][percent > 0]
     damages <- sites[caces.county,on=.(FIPS=fips),.(fips=fips,
                                                     damage.acs.stacklevel=damage.acs.stacklevel,
                                                     pollutant=pollutant,
                                                     model=model,
                                                     season=season,
                                                     percent=percent)][!is.na(percent)] 
     damages <- damages[,.(damage=weighted.mean(damage.acs.stacklevel*acs.to.pope2019*vsl,w=percent)
     ),by=.(pollutant)]
     return(damages)
}

getManufactureEmissions <- function(vehType){
     vehicle.emissions <- fread(paste0(etl_dir,"misc\\Greet_Vehicle_Emissions_20220615.csv"))
     vehicle.emissions <- vehicle.emissions[!(Model %like% "BEV" | Model %like% "CD_Electric") | Stage!="Fuel"]
     vehTypeShort = word(vehType,1,sep="_")
     temp <- vehicle.emissions[Model==vehTypeShort]
     veh <- data.table(vehType=vehType)
     veh[,':='(
          Car.Cu.Mine.ghg=temp[Stage=="Car.Cu.Mine"]$GHG,
          Car.Cu.Mine.nox=temp[Stage=="Car.Cu.Mine"]$NOx,
          Car.Cu.Mine.so2=temp[Stage=="Car.Cu.Mine"]$SOx,
          Car.Cu.Mine.pm25=temp[Stage=="Car.Cu.Mine"]$PM2.5,
          Car.Cu.Mine.voc=temp[Stage=="Car.Cu.Mine"]$VOC,
          Car.Cu.Mine.co=temp[Stage=="Car.Cu.Mine"]$CO,
          Car.Cu.Prod.ghg=temp[Stage=="Car.Cu.Prod"]$GHG,
          Car.Cu.Prod.nox=temp[Stage=="Car.Cu.Prod"]$NOx,
          Car.Cu.Prod.so2=temp[Stage=="Car.Cu.Prod"]$SOx,
          Car.Cu.Prod.pm25=temp[Stage=="Car.Cu.Prod"]$PM2.5,
          Car.Cu.Prod.voc=temp[Stage=="Car.Cu.Prod"]$VOC,
          Car.Cu.Prod.co=temp[Stage=="Car.Cu.Prod"]$CO,
          Car.Remainder.ghg=temp[Stage=="Car.Remainder"]$GHG,
          Car.Remainder.nox=temp[Stage=="Car.Remainder"]$NOx,
          Car.Remainder.so2=temp[Stage=="Car.Remainder"]$SOx,
          Car.Remainder.pm25=temp[Stage=="Car.Remainder"]$PM2.5,
          Car.Remainder.voc=temp[Stage=="Car.Remainder"]$VOC,
          Car.Remainder.co=temp[Stage=="Car.Remainder"]$CO,
          Battery.NiCu.Mine.ghg=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(GHG)],
          Battery.NiCu.Mine.nox=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(NOx)],
          Battery.NiCu.Mine.so2=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(SOx)],
          Battery.NiCu.Mine.pm25=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(PM2.5)],
          Battery.NiCu.Mine.voc=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(VOC)],
          Battery.NiCu.Mine.co=temp[Stage %in% c('Battery.Ni.Mine','Battery.Cu.Mine'),sum(CO)],
          Battery.CoMn.Mine.ghg=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(GHG)],
          Battery.CoMn.Mine.nox=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(NOx)],
          Battery.CoMn.Mine.so2=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(SOx)],
          Battery.CoMn.Mine.pm25=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(PM2.5)],
          Battery.CoMn.Mine.voc=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(VOC)],
          Battery.CoMn.Mine.co=temp[Stage %in% c('Battery.Co.Mine','Battery.Mn.Mine'),sum(CO)],
          Battery.NiCuCo.Prod.ghg=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(GHG)],
          Battery.NiCuCo.Prod.nox=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(NOx)],
          Battery.NiCuCo.Prod.so2=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(SOx)],
          Battery.NiCuCo.Prod.pm25=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(PM2.5)],
          Battery.NiCuCo.Prod.voc=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(VOC)],
          Battery.NiCuCo.Prod.co=temp[Stage %in% c('Battery.Ni.Prod','Battery.Cu.Prod','Battery.Co.Prod'),sum(CO)],
          Battery.Li.Prod.ghg=temp[Stage=="Battery.Li.Prod"]$GHG,
          Battery.Li.Prod.nox=temp[Stage=="Battery.Li.Prod"]$NOx,
          Battery.Li.Prod.so2=temp[Stage=="Battery.Li.Prod"]$SOx,
          Battery.Li.Prod.pm25=temp[Stage=="Battery.Li.Prod"]$PM2.5,
          Battery.Li.Prod.voc=temp[Stage=="Battery.Li.Prod"]$VOC,
          Battery.Li.Prod.co=temp[Stage=="Battery.Li.Prod"]$CO,
          Battery.Graphite.Prod.ghg=temp[Stage=="Battery.Graphite.Prod"]$GHG,
          Battery.Graphite.Prod.nox=temp[Stage=="Battery.Graphite.Prod"]$NOx,
          Battery.Graphite.Prod.so2=temp[Stage=="Battery.Graphite.Prod"]$SOx,
          Battery.Graphite.Prod.pm25=temp[Stage=="Battery.Graphite.Prod"]$PM2.5,
          Battery.Graphite.Prod.voc=temp[Stage=="Battery.Graphite.Prod"]$VOC,
          Battery.Graphite.Prod.co=temp[Stage=="Battery.Graphite.Prod"]$CO,
          Battery.Remainder.ghg=temp[Stage=="Battery.Remainder"]$GHG,
          Battery.Remainder.nox=temp[Stage=="Battery.Remainder"]$NOx,
          Battery.Remainder.so2=temp[Stage=="Battery.Remainder"]$SOx,
          Battery.Remainder.pm25=temp[Stage=="Battery.Remainder"]$PM2.5,
          Battery.Remainder.voc=temp[Stage=="Battery.Remainder"]$VOC,
          Battery.Remainder.co=temp[Stage=="Battery.Remainder"]$CO)]
     return(veh)
}
getOperationsEmissions <- function(fullString,vehType){
     runString <- word(fullString,-1,sep=fixed("\\"))
     plugin <- str_length(vehType)>3 #not 'HEV' or 'CV'
     if(plugin){ 
          evDataString <- paste0(fullString,"\\EVdata_",vehType,"_2019.mat")
     } else{
          defaultVeh <- "BEV300_2021GREET"
          evDataString <- paste0(gsub(runString,"",fullString),defaultVeh,"_CC0_2019_Txn1\\EVdata_",defaultVeh,"_2019.mat")
     }
     EVdata <- readMat(evDataString)[[1]]
     miles.battery <-colSums(EVdata[[which(dimnames(EVdata)[[1]]=='miles.battery')]])
     miles.gas <-colSums(EVdata[[which(dimnames(EVdata)[[1]]=='miles.gas')]])
     ev.number <- rowSums(EVdata[[which(dimnames(EVdata)[[1]]=='number')]])
     miles.battery <- sum(miles.battery*ev.number)
     miles.gas <- sum(miles.gas*ev.number)
     if(!plugin){
          miles.gas <- miles.gas + miles.battery
          miles.battery <- 0.0
     }
     total.miles <- miles.battery+miles.gas
     vehicle.emissions <- fread(paste0(etl_dir,"misc\\Greet_Vehicle_Emissions_20220615.csv"))
     vehicle.emissions <- vehicle.emissions[!(Model %like% "BEV" | Model %like% "CD_Electric") | Stage!="Fuel"]
     vehTypeShort = word(vehType,1,sep="_")
     temp <- vehicle.emissions[Model==vehTypeShort]
     veh <- data.table(vehType=vehType)
     if(vehTypeShort %like% "BEV"){
          # Fuel and feedstock are defined by grid-side data, not GREET data (see getPHORUMGridDamages)
          veh[, ':='(
               Tailpipe.ghg=temp[Stage=="Operation"]$GHG*total.miles,
               Tailpipe.nox=temp[Stage=="Operation"]$NOx*total.miles,
               Tailpipe.so2=temp[Stage=="Operation"]$SOx*total.miles,
               Tailpipe.pm25=temp[Stage=="Operation"]$PM2.5*total.miles,
               Tailpipe.voc=temp[Stage=="Operation"]$VOC*total.miles,
               Tailpipe.co=temp[Stage=="Operation"]$CO*total.miles,
               Upstream.ghg=0.0,
               Upstream.nox=0.0,
               Upstream.so2=0.0,
               Upstream.pm25=0.0,
               Upstream.voc=0.0,
               Upstream.co=0.0)]
     } else if(vehTypeShort %like% "PHEV"){
          cd_tailpipe <- vehicle.emissions[Model %like% paste0(vehTypeShort,"_CD_") & Stage=="Operation"]
          cs_tailpipe <- vehicle.emissions[Model %like% paste0(vehTypeShort,"_CS_") & Stage=="Operation"]
          cd_upstream <- vehicle.emissions[Model==paste0(vehTypeShort,"_CD_Gasoline") & Stage %in% c("Fuel","Feedstock")]
          cs_upstream <- vehicle.emissions[Model==paste0(vehTypeShort,"_CS_Gasoline") & Stage %in% c("Fuel","Feedstock")]
          veh[, ':='(
               Tailpipe.ghg= cd_tailpipe[,sum(GHG,  na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(GHG,  na.rm=TRUE)]*miles.gas,
               Tailpipe.nox= cd_tailpipe[,sum(NOx,  na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(NOx,  na.rm=TRUE)]*miles.gas,
               Tailpipe.so2= cd_tailpipe[,sum(SOx,  na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(SOx,  na.rm=TRUE)]*miles.gas,
               Tailpipe.pm25=cd_tailpipe[,sum(PM2.5,na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(PM2.5,na.rm=TRUE)]*miles.gas,
               Tailpipe.voc= cd_tailpipe[,sum(VOC,  na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(VOC,  na.rm=TRUE)]*miles.gas,
               Tailpipe.co=  cd_tailpipe[,sum(CO,   na.rm=TRUE)]*miles.battery + cs_tailpipe[,sum(CO,   na.rm=TRUE)]*miles.gas,
               Upstream.ghg= cd_upstream[,sum(GHG,  na.rm=TRUE)]*miles.battery + cs_upstream[,sum(GHG,  na.rm=TRUE)]*miles.gas,
               Upstream.nox= cd_upstream[,sum(NOx,  na.rm=TRUE)]*miles.battery + cs_upstream[,sum(NOx,  na.rm=TRUE)]*miles.gas,
               Upstream.so2= cd_upstream[,sum(SOx,  na.rm=TRUE)]*miles.battery + cs_upstream[,sum(SOx,  na.rm=TRUE)]*miles.gas,
               Upstream.pm25=cd_upstream[,sum(PM2.5,na.rm=TRUE)]*miles.battery + cs_upstream[,sum(PM2.5,na.rm=TRUE)]*miles.gas,
               Upstream.voc= cd_upstream[,sum(VOC,  na.rm=TRUE)]*miles.battery + cs_upstream[,sum(VOC,  na.rm=TRUE)]*miles.gas,
               Upstream.co=  cd_upstream[,sum(CO,   na.rm=TRUE)]*miles.battery + cs_upstream[,sum(CO,   na.rm=TRUE)]*miles.gas
          )] 
     } else{
          veh[, ':='(
               Tailpipe.ghg=temp[Stage=="Operation"]$GHG*total.miles,
               Tailpipe.nox=temp[Stage=="Operation"]$NOx*total.miles,
               Tailpipe.so2=temp[Stage=="Operation"]$SOx*total.miles,
               Tailpipe.pm25=temp[Stage=="Operation"]$PM2.5*total.miles,
               Tailpipe.voc=temp[Stage=="Operation"]$VOC*total.miles,
               Tailpipe.co=temp[Stage=="Operation"]$CO*total.miles,
               Upstream.ghg=temp[Stage %in% c("Fuel","Feedstock"),sum(GHG,na.rm=TRUE)]*total.miles,
               Upstream.nox=temp[Stage %in% c("Fuel","Feedstock"),sum(NOx,na.rm=TRUE)]*total.miles,
               Upstream.so2=temp[Stage %in% c("Fuel","Feedstock"),sum(SOx,na.rm=TRUE)]*total.miles,
               Upstream.pm25=temp[Stage %in% c("Fuel","Feedstock"),sum(PM2.5,na.rm=TRUE)]*total.miles,
               Upstream.voc=temp[Stage %in% c("Fuel","Feedstock"),sum(VOC,na.rm=TRUE)]*total.miles,
               Upstream.co=temp[Stage %in% c("Fuel","Feedstock"),sum(CO,na.rm=TRUE)]*total.miles)]
     }
     return(veh)
}

getGREETEmissions <- function(fullString){
     runString <- word(fullString,-1,sep=fixed("\\"))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     operations.emissions <- getOperationsEmissions(fullString,vehType)
     out1 <- getGREETEmissionsExceptTailpipe(fullString,operations.emissions)
     out2 <- getGREETEmissionsTailpipe(-1,fullString,operations.emissions)
     return(rbindlist(list(out1,out2)))
}

getGREETDamages <- function(fullString){
     runString <- word(fullString,-1,sep=fixed("\\"))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     operations.emissions <- getOperationsEmissions(fullString,vehType)
     out1 <- getGREETDamagesExceptTailpipe(fullString,operations.emissions)
     out2 <- getGREETDamagesTailpipe(-1,fullString,operations.emissions)
     return(rbindlist(list(out1,out2)))
}

getGREETEmissionsTailpipe <- function(specific.county,fullString,operations.emissions){
     
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString2 <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString2,"_")>3) {
          detail <- word(runString2,-1,sep="_")
     }
     
     tailpipe.damages <- getTailpipeDamages(specific.county)
     operations <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                              cost=c(0.0,
                                     operations.emissions[,Tailpipe.so2]/g.per.tonne,
                                     operations.emissions[,Tailpipe.voc]/g.per.tonne,
                                     operations.emissions[,Tailpipe.nox]/g.per.tonne,
                                     operations.emissions[,Tailpipe.pm25]/g.per.tonne,
                                     operations.emissions[,Tailpipe.ghg]/g.per.tonne,
                                     operations.emissions[,Tailpipe.co]/g.per.tonne))
     operations[,stage:="operations"]
     return(operations[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

getGREETDamagesTailpipe <- function(specific.county,fullString,operations.emissions){
     
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString2 <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString2,"_")>3) {
          detail <- word(runString2,-1,sep="_")
     }
     
     tailpipe.damages <- getTailpipeDamages(specific.county)
     operations <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                              cost=c(0.0,
                                     operations.emissions[,Tailpipe.so2]*tailpipe.damages[pollutant=='so2',damage]/g.per.tonne,
                                     operations.emissions[,Tailpipe.voc]*tailpipe.damages[pollutant=='voc',damage]/g.per.tonne,
                                     operations.emissions[,Tailpipe.nox]*tailpipe.damages[pollutant=='nox',damage]/g.per.tonne,
                                     operations.emissions[,Tailpipe.pm25]*tailpipe.damages[pollutant=='pm25',damage]/g.per.tonne,
                                     operations.emissions[,Tailpipe.ghg]*carbon.price/g.per.tonne,
                                     operations.emissions[,Tailpipe.co]*co.cost/g.per.tonne))
     operations[,stage:="operations"]
     return(operations[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

getGREETEmissionsExceptTailpipe <- function(fullString,operations.emissions,specific.county=-1){
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString2 <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString2,"_")>3) {
          detail <- word(runString2,-1,sep="_")
     }
     
     
     manufacture.emissions <- getManufactureEmissions(vehType)
     manufacture.damages <- getManufactureDamages()
     refining.damages <- getOilFeedstockDamages()
     
     car.manufacture <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                                   cost=c(0.0,
                                          manufacture.emissions[,((Car.Cu.Mine.so2) +
                                                                       (Car.Cu.Prod.so2) + 
                                                                       (Car.Remainder.so2))/g.per.tonne],
                                          manufacture.emissions[,((Car.Cu.Mine.voc) +
                                                                       (Car.Cu.Prod.voc) + 
                                                                       (Car.Remainder.voc))/g.per.tonne],
                                          manufacture.emissions[,((Car.Cu.Mine.nox) +
                                                                       (Car.Cu.Prod.nox) + 
                                                                       (Car.Remainder.nox))/g.per.tonne],
                                          manufacture.emissions[,((Car.Cu.Mine.pm25) +
                                                                       (Car.Cu.Prod.pm25) + 
                                                                       (Car.Remainder.pm25))/g.per.tonne],
                                          manufacture.emissions[,(Car.Cu.Mine.ghg + Car.Cu.Prod.ghg + Car.Remainder.ghg)/g.per.tonne],
                                          manufacture.emissions[,((Car.Cu.Mine.co) +
                                                                       (Car.Cu.Prod.co) + 
                                                                       (Car.Remainder.co))/g.per.tonne]))
     car.manufacture[,stage:="car_manufacture"]
     
     battery.manufacture <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                                       cost=c(0.0,
                                              manufacture.emissions[,((Battery.NiCu.Mine.so2) +
                                                                           (Battery.NiCuCo.Prod.so2) + 
                                                                           (Battery.Remainder.so2) +
                                                                           Battery.CoMn.Mine.so2 +
                                                                           Battery.Li.Prod.so2 +
                                                                           Battery.Graphite.Prod.so2)/g.per.tonne],
                                              manufacture.emissions[,((Battery.NiCu.Mine.voc)+
                                                                           (Battery.NiCuCo.Prod.voc) + 
                                                                           (Battery.Remainder.voc) +
                                                                           Battery.CoMn.Mine.voc +
                                                                           Battery.Li.Prod.voc +
                                                                           Battery.Graphite.Prod.voc)/g.per.tonne],
                                              manufacture.emissions[,((Battery.NiCu.Mine.nox) +
                                                                           (Battery.NiCuCo.Prod.nox) + 
                                                                           (Battery.Remainder.nox) +
                                                                           Battery.CoMn.Mine.nox +
                                                                           Battery.Li.Prod.nox +
                                                                           Battery.Graphite.Prod.nox)/g.per.tonne],
                                              manufacture.emissions[,((Battery.NiCu.Mine.pm25) +
                                                                           (Battery.NiCuCo.Prod.pm25) + 
                                                                           (Battery.Remainder.pm25) +
                                                                           Battery.CoMn.Mine.pm25 +
                                                                           Battery.Li.Prod.pm25 +
                                                                           Battery.Graphite.Prod.pm25)/g.per.tonne],
                                              manufacture.emissions[,(Battery.NiCu.Mine.ghg + Battery.CoMn.Mine.ghg + 
                                                                           Battery.NiCuCo.Prod.ghg + Battery.Li.Prod.ghg + Battery.Graphite.Prod.ghg + Battery.Remainder.ghg)/g.per.tonne],
                                              manufacture.emissions[,(Battery.CoMn.Mine.co +
                                                                           Battery.Li.Prod.co +
                                                                           Battery.Graphite.Prod.co)/g.per.tonne]))
     battery.manufacture[,stage:="battery_manufacture"]
     
     upstream <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                            cost=c(0.0,
                                   operations.emissions[,Upstream.so2]/g.per.tonne,
                                   operations.emissions[,Upstream.voc]/g.per.tonne,
                                   operations.emissions[,Upstream.nox]/g.per.tonne,
                                   operations.emissions[,Upstream.pm25]/g.per.tonne,
                                   operations.emissions[,Upstream.ghg]/g.per.tonne,
                                   operations.emissions[,Upstream.co]/g.per.tonne))
     upstream[,stage:="upstream_gasoline"]
     
     out <- rbindlist(list(car.manufacture,battery.manufacture,upstream))
     return(out[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

getGREETDamagesExceptTailpipe <- function(fullString,operations.emissions,specific.county=-1){
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString2 <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString2,"_")>3) {
          detail <- word(runString2,-1,sep="_")
     }

     
     manufacture.emissions <- getManufactureEmissions(vehType)
     manufacture.damages <- getManufactureDamages()
     refining.damages <- getOilFeedstockDamages()
     
     car.manufacture <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(0.0,
                               manufacture.emissions[,((Car.Cu.Mine.so2)*manufacture.damages[pollutant=='so2',cu.mine] +
                                     (Car.Cu.Prod.so2)*manufacture.damages[pollutant=='so2',cu.prod] + 
                                     (Car.Remainder.so2)*manufacture.damages[pollutant=='so2',other])/g.per.tonne],
                               manufacture.emissions[,((Car.Cu.Mine.voc)*manufacture.damages[pollutant=='voc',cu.mine] +
                                     (Car.Cu.Prod.voc)*manufacture.damages[pollutant=='voc',cu.prod] + 
                                     (Car.Remainder.voc)*manufacture.damages[pollutant=='voc',other])/g.per.tonne],
                               manufacture.emissions[,((Car.Cu.Mine.nox)*manufacture.damages[pollutant=='nox',cu.mine] +
                                     (Car.Cu.Prod.nox)*manufacture.damages[pollutant=='nox',cu.prod] + 
                                     (Car.Remainder.nox)*manufacture.damages[pollutant=='nox',other])/g.per.tonne],
                               manufacture.emissions[,((Car.Cu.Mine.pm25)*manufacture.damages[pollutant=='pm25',cu.mine] +
                                     (Car.Cu.Prod.pm25)*manufacture.damages[pollutant=='pm25',cu.prod] + 
                                     (Car.Remainder.pm25)*manufacture.damages[pollutant=='pm25',other])/g.per.tonne],
                               manufacture.emissions[,(Car.Cu.Mine.ghg + Car.Cu.Prod.ghg + Car.Remainder.ghg)*carbon.price/g.per.tonne],
                               manufacture.emissions[,((Car.Cu.Mine.co) +
                                     (Car.Cu.Prod.co) + 
                                     (Car.Remainder.co))*co.cost/g.per.tonne]))
     car.manufacture[,stage:="car_manufacture"]

     battery.manufacture <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                                   cost=c(0.0,
                                          manufacture.emissions[,((Battery.NiCu.Mine.so2)*manufacture.damages[pollutant=='so2',cu.mine] +
                                                                       (Battery.NiCuCo.Prod.so2)*manufacture.damages[pollutant=='so2',cu.prod] + 
                                                                       (Battery.Remainder.so2)*manufacture.damages[pollutant=='so2',other] +
                                                                       Battery.CoMn.Mine.so2*manufacture.damages[pollutant=='so2',co.mine] +
                                                                       Battery.Li.Prod.so2*manufacture.damages[pollutant=='so2',li.prod] +
                                                                       Battery.Graphite.Prod.so2*manufacture.damages[pollutant=='so2',graphite.prod])/g.per.tonne],
                                          manufacture.emissions[,((Battery.NiCu.Mine.voc)*manufacture.damages[pollutant=='voc',cu.mine] +
                                                                       (Battery.NiCuCo.Prod.voc)*manufacture.damages[pollutant=='voc',cu.prod] + 
                                                                       (Battery.Remainder.voc)*manufacture.damages[pollutant=='voc',other] +
                                                                       Battery.CoMn.Mine.voc*manufacture.damages[pollutant=='voc',co.mine] +
                                                                       Battery.Li.Prod.voc*manufacture.damages[pollutant=='voc',li.prod] +
                                                                       Battery.Graphite.Prod.voc*manufacture.damages[pollutant=='voc',graphite.prod])/g.per.tonne],
                                          manufacture.emissions[,((Battery.NiCu.Mine.nox)*manufacture.damages[pollutant=='nox',cu.mine] +
                                                                       (Battery.NiCuCo.Prod.nox)*manufacture.damages[pollutant=='nox',cu.prod] + 
                                                                       (Battery.Remainder.nox)*manufacture.damages[pollutant=='nox',other] +
                                                                       Battery.CoMn.Mine.nox*manufacture.damages[pollutant=='nox',co.mine] +
                                                                       Battery.Li.Prod.nox*manufacture.damages[pollutant=='nox',li.prod] +
                                                                       Battery.Graphite.Prod.nox*manufacture.damages[pollutant=='nox',graphite.prod])/g.per.tonne],
                                          manufacture.emissions[,((Battery.NiCu.Mine.pm25)*manufacture.damages[pollutant=='pm25',cu.mine] +
                                                                       (Battery.NiCuCo.Prod.pm25)*manufacture.damages[pollutant=='pm25',cu.prod] + 
                                                                       (Battery.Remainder.pm25)*manufacture.damages[pollutant=='pm25',other] +
                                                                       Battery.CoMn.Mine.pm25*manufacture.damages[pollutant=='pm25',co.mine] +
                                                                       Battery.Li.Prod.pm25*manufacture.damages[pollutant=='pm25',li.prod] +
                                                                       Battery.Graphite.Prod.pm25*manufacture.damages[pollutant=='pm25',graphite.prod])/g.per.tonne],
                                          manufacture.emissions[,(Battery.NiCu.Mine.ghg + Battery.CoMn.Mine.ghg + 
                                                                       Battery.NiCuCo.Prod.ghg + Battery.Li.Prod.ghg + Battery.Graphite.Prod.ghg + Battery.Remainder.ghg)*carbon.price/g.per.tonne],
                                          manufacture.emissions[,(Battery.CoMn.Mine.co +
                                                                       Battery.Li.Prod.co +
                                                                       Battery.Graphite.Prod.co)*co.cost/g.per.tonne]))
     battery.manufacture[,stage:="battery_manufacture"]
     
     upstream <- data.table(pollutant=c("nh3","so2","voc","nox","pm25","ghg","co"),
                        cost=c(0.0,
                               operations.emissions[,Upstream.so2]*refining.damages[pollutant=='so2',damage]/g.per.tonne,
                               operations.emissions[,Upstream.voc]*refining.damages[pollutant=='voc',damage]/g.per.tonne,
                               operations.emissions[,Upstream.nox]*refining.damages[pollutant=='nox',damage]/g.per.tonne,
                               operations.emissions[,Upstream.pm25]*refining.damages[pollutant=='pm25',damage]/g.per.tonne,
                               operations.emissions[,Upstream.ghg]*carbon.price/g.per.tonne,
                               operations.emissions[,Upstream.co]*co.cost/g.per.tonne))
     upstream[,stage:="upstream_gasoline"]
    
     out <- rbindlist(list(car.manufacture,battery.manufacture,upstream))
     return(out[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,stage=stage,pollutant,cost)])
}

plotEvolution <- function(damages){
     
     rollup <- damages[!(Powertrain %like% "CC"),.(Costs=sum(cost)),by=.(Powertrain,Year)]
     baseline.grid.damages <- damages[Powertrain %like% "ICEV" & stage %in% c("grid","upstream_grid"),.(bau_grid_cost=sum(cost)),by=Year]
     rollup[baseline.grid.damages,Costs:=Costs-bau_grid_cost,on=.(Year)]
     rollup[Year=="2035_10PctRE",Year:="Future,\n10% Wind\n+Solar"]
     rollup[Year=="2035_22PctRE",Year:="Future,\n22% Wind\n+Solar"]
     rollup[,`PJM Generator Fleet`:=Year]
     rollup[,Powertrain:=word(Powertrain,1,sep="-")]
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, c("ICEV","HEV","PHEV20","BEV300"))
     ggplot(rollup, aes(x = `PJM Generator Fleet`, 
                        y = Costs, 
                        group = Powertrain, 
                        col = Powertrain,
                        shape = Powertrain)) +           # Draw line plot with ggplot2
          geom_line(linetype = "dotted",size=1.0, alpha=0.5) + 
          geom_point(size=3.5) +
          scale_y_continuous(labels=scales::dollar_format(), 
                             #limits=c(0,10000),
                             #breaks=seq(0,10000,2000), 
                             #minor_breaks = seq(0, 10000, 1000),
                             position="left") + #,
                             #expand = c(0,0)) + 
          ylab("Life cycle air emissions externalities ($/car)") +
          scale_x_discrete(expand = c(0.05,0.05)) +
          theme_light() + scale_color_brewer(palette = "Dark2") + 
          theme(legend.position="right",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 11),
                strip.text = element_text(size = 11),
                axis.text = element_text(size=11),
                axis.title = element_text(size=11),
                panel.grid.major.x = element_blank() ,
                # explicitly set the horizontal lines (or they will disappear too)
                panel.grid.major.y = element_line( size=.075, color="grey90" ),
                panel.grid.minor.y = element_line( size=.075, color="grey90" ) )
     ggsave(paste0(results_dir,"TimeSeries.pdf"), width=6.5, height=3.85,units="in")
     
}


plotEvolution_GhGCriteria <- function(damages){
     
     rollup <- damages[!(Powertrain %like% "CC"),.(Costs=sum(cost)),by=.(Powertrain,Year,
                                                Type=ifelse(pollutant=="GHGs","GHGs","Criteria Pollutants"))]
     baseline.grid.damages <- damages[Powertrain %like% "ICEV" & stage %in% c("grid","upstream_grid"),.(bau_grid_cost=sum(cost)),
                                      by=.(Year,Type=ifelse(pollutant=="GHGs","GHGs","Criteria Pollutants"))]
     rollup[baseline.grid.damages,Costs:=Costs-bau_grid_cost,on=.(Year,Type)]
     rollup[Year=="2035_10PctRE",Year:="Future,\n10% Wind\n+Solar"]
     rollup[Year=="2035_22PctRE",Year:="Future,\n22% Wind\n+Solar"]
     rollup[,`PJM Generator Fleet`:=Year]
     rollup[,Powertrain:=word(Powertrain,1,sep="-")]
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, c("ICEV","HEV","PHEV20","BEV300"))
     
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, c("ICEV","HEV","PHEV20-UC","PHEV20-CC","BEV300-UC","BEV300-CC"))
     ggplot(rollup, aes(x = `PJM Generator Fleet`, 
                        y = Costs, 
                        group = Powertrain, 
                        col = Powertrain,
                        shape = Powertrain)) +           # Draw line plot with ggplot2
          geom_line(linetype = "dotted",size=1.0, alpha=0.5) + 
          geom_point(size=3.5) +
          scale_y_continuous(limits = c(0,NA),labels=scales::dollar_format(), 
                             position="left", expand=c(0.1,0.1)) + 
          ylab("Life cycle air emissions externalities ($/car)") +
          scale_x_discrete(expand = c(0.1,0.1)) +
          theme_light() + scale_color_brewer(palette = "Dark2") + 
          theme(legend.position="right",
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 11),
                strip.text = element_text(size = 12),
                axis.text = element_text(size=11),
                axis.title = element_text(size=12)) +
          facet_grid(Type ~ .,scales="free")
     ggsave(paste0(results_dir,"TimeSeries_Detail.pdf"), width=6.5, height=6.5,units="in")
}
plotLifeCycleBreakdown <- function(damages,yearToPlot, detailToPlot=""){
     rollup <- damages[year==yearToPlot & detail==detailToPlot,.(Costs=sum(cost)),by=.(Powertrain,Stage)]
     baseline.grid.damages <- rollup[Powertrain %like% "ICEV" & Stage=="Electricity\nProduction",Costs]
     rollup <- rollup[Powertrain %like% "BEV" | Powertrain %like% "PHEV" | !(Stage =="Electricity\nProduction")]
     rollup[Stage=="Electricity\nProduction",Costs:=Costs-baseline.grid.damages]
     rollup$Stage <- fct_relevel(rollup$Stage, c("Vehicle\nUse","Electricity\nProduction","Gasoline\nProduction","Battery\nProduction","Vehicle\nProduction"))
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, c("ICEV","HEV","PHEV20-UC","PHEV20-CC","BEV300-UC","BEV300-CC"))
     
     ggplot(rollup,aes(x=Powertrain,y=Costs,fill=Stage))+geom_col(width=0.75) +
          xlab("Powertrain") + ylab("Life cycle air emissions externalities ($/car)") +
          scale_x_discrete(limits = rev(levels(rollup$Powertrain))) +
          scale_y_continuous(labels=scales::dollar_format(), limits=c(0,10000),breaks=c(0,2000,4000,6000,8000), position="right") + 
          scale_fill_manual(values = getFiveColors(), guide = guide_legend(reverse = TRUE)) + 
          #scale_fill_brewer(palette = "Dark2", guide = guide_legend(reverse = TRUE)) +
          theme_light() + 
          theme(legend.position="bottom",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 10),
                strip.text = element_text(size = 12),
                axis.text = element_text(size=12),
                axis.title = element_text(size=12),
                axis.title.y = element_blank(), 
                plot.margin = unit(c(0,0,0,0), "cm")) +
          coord_flip()  
     suffix <- ifelse(detailToPlot=="","",paste0("_",detailToPlot))
     ggsave(paste0(results_dir,"DamagesByStage_",yearToPlot,suffix,".pdf"), width=6.5, height=4.0,units="in")
}

getFiveColors <- function(){
     colors <- RColorBrewer::brewer.pal(5,'Dark2')
     colors <- colors[c(2,1,3,4,5)]
}

getCountyLeaders <- function(){
     files = list.files(path=results_dir, pattern="*CC0_2019*",full.names=TRUE)
     pjm_counties <- getPJMCounties()[,fips]
     for(thisFile in files){
          runString <- word(thisFile,-1,sep=fixed("\\"))
          vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
          operations.emissions <- getOperationsEmissions(thisFile,vehType)
          out1 <- getGREETDamagesExceptTailpipe(thisFile,operations.emissions)
          out1[!(stage %like% "manufacture"),cost:=cost*discounted.cash.flow.multiplier/total.cars.switched]
          out1 <- out1[,.(Costs=sum(cost)),by=.(year,cc,veh,txn,detail)]
          
          out2 <- rbindlist(lapply(pjm_counties,getGREETDamagesTailpipe,
                                   fullString=thisFile,
                                   operations.emissions=operations.emissions))
          out2$fips <- rep(pjm_counties,times=nrow(out2)/length(pjm_counties))
          out2[!(stage %like% "manufacture"),cost:=cost*discounted.cash.flow.multiplier/total.cars.switched]
          out2 <- out2[,.(Costs=sum(cost)),
                       by=.(year,cc,veh,txn,detail,fips)]
          out2$Costs <- out2$Costs + out1$Costs
          if(exists("out")){
               out <- rbindlist(list(out,out2));
          } else{
               out <- out2;
          }
     }     
     damagesGrid <- rbindlist(lapply(files,getPHORUMGridDamages))
     damagesGrid <- damagesGrid[,.(cost=sum(cost)),by=.(year,cc,veh,txn,detail)]
     baseline.grid.damages <- damagesGrid[veh %like% "ICEV",sum(cost)]
     damagesGrid[,cost:=cost-baseline.grid.damages]
     damagesGrid[,grid.cost:=cost*discounted.cash.flow.multiplier/total.cars.switched]
     out[damagesGrid,grid.cost:=grid.cost,on=.(veh)]
     out[,total.cost:=Costs+grid.cost]
     out[,veh:=word(veh,1,sep="_")]
     out[,min.cost:=min(total.cost),by=fips][total.cost==min.cost,best.veh:=1]
     out <- out[best.veh==1]
}

plotDamages <- function(damages,yearToPlot,detailToPlot="",yAxisSuffix,topMargin,bottomMargin){
     rollup <- damages[year==yearToPlot & detail==detailToPlot  & pollutant !="nh3" & pollutant !="CO"]
     rollup <- rollup[,.(Costs=sum(cost)),by=.(Year,Powertrain,Stage,Pollutant=pollutant)]

     baseline.grid.cost <- rollup[Powertrain %like% "ICEV" & Stage =="Electricity\nProduction",.(bau.grid.cost=sum(Costs)),by=.(Pollutant)]
     rollup[baseline.grid.cost,bau.grid.cost:=bau.grid.cost,on=.(Pollutant)]
     rollup[Stage=="Electricity\nProduction",Costs:=Costs-bau.grid.cost]
     rollup$Stage <- fct_relevel(rollup$Stage, c("Vehicle\nUse","Electricity\nProduction","Gasoline\nProduction","Battery\nProduction","Vehicle\nProduction"))
     rollup <- rollup[Powertrain %like% "BEV" | Powertrain %like% "PHEV" | Stage != "Electricity\nProduction"]
     rollup$Pollutant <- fct_relevel(rollup$Pollutant, c("GHGs","SO2","NOx","PM2.5","VOC","CO"))
     rollup[Powertrain=="BEV300-CC",Powertrain:="BEV\nCC"]
     rollup[Powertrain=="BEV300-UC",Powertrain:="BEV\nUC"]
     rollup[Powertrain=="PHEV20-CC",Powertrain:="PHEV\nCC"]
     rollup[Powertrain=="PHEV20-UC",Powertrain:="PHEV\nUC"]
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, 
                                      c("BEV\nCC","BEV\nUC","PHEV\nCC","PHEV\nUC","HEV","ICEV"))
     
     plot <- ggplot(rollup,aes(x=Powertrain,y=Costs,fill=Stage))+geom_col(width=0.75) +
          xlab("Powertrain") + ylab(paste0("Life cycle air emissions externalities ($/car)")) +
          scale_x_discrete(limits = rev(levels(rollup$Powertrain))) +
          scale_fill_manual(values = getFiveColors(), guide = guide_legend(reverse = FALSE)) + 
          #scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
          theme_light() + ggtitle(yAxisSuffix) +
          theme(title = element_text(size=12),
                legend.position="right",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 10),
                strip.text = element_text(size = 10,margin = margin(b = 0.1)),
                axis.text = element_text(size=7),
                axis.title = element_text(size=11),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 10), 
                plot.margin = unit(c(topMargin,0,bottomMargin,0), "cm"),
                panel.margin.y = unit(0, "lines")) +
          facet_wrap(. ~ Pollutant,scales="free",ncol=2)
}

makeDamagePlots <- function(){
     files = list.files(path=results_dir, pattern="*_C{2}",full.names=TRUE)
     damagesGrid <- rbindlist(lapply(files,getPHORUMGridDamages))
     damagesVeh <- rbindlist(lapply(files,getGREETDamages))
     damages <- rbindlist(list(damagesGrid,damagesVeh))
     damages[!(stage %like% "manufacture"),cost:=cost*discounted.cash.flow.multiplier/total.cars.switched]
     damages[,Powertrain:=word(veh,1,sep="_")]
     damages[Powertrain %like% "PHEV" | Powertrain %like% "BEV",
             Powertrain:=paste0(Powertrain,"-",ifelse(cc==0,"UC","CC"))]
     damages[Powertrain=="CV",Powertrain:="ICEV"]
     damages[,Year:=ifelse(detail=="",year,paste0(year,"_",detail))]
     damages[pollutant=="co",pollutant:="CO"]
     damages[pollutant=="ghg",pollutant:="GHGs"]
     damages[pollutant=="nox",pollutant:="NOx"]
     damages[pollutant=="pm25",pollutant:="PM2.5"]
     damages[pollutant=="so2",pollutant:="SO2"]
     damages[pollutant=="voc",pollutant:="VOC"]
     damages <- damages[!(detail %like% "LessCoal")]
     damages[stage=="upstream_grid",Stage:="Electricity\nProduction"]
     damages[stage=="upstream_gasoline",Stage:="Gasoline\nProduction"]
     damages[stage=="operations",Stage:="Vehicle\nUse"]
     damages[stage=="grid",Stage:="Electricity\nProduction"]
     damages[stage=="car_manufacture",Stage:="Vehicle\nProduction"]
     damages[stage=="battery_manufacture",Stage:="Battery\nProduction"]
     
     plotEvolution(damages)
     plotEvolution_GhGCriteria(damages)
     plotLifeCycleBreakdown(damages,yearToPlot=2010,detailToPlot="")
     plotLifeCycleBreakdown(damages,yearToPlot=2019,detailToPlot="")
     plotLifeCycleBreakdown(damages,yearToPlot=2025,detailToPlot="")
     plotLifeCycleBreakdown(damages,yearToPlot=2035,detailToPlot="10PctRE")
     plotLifeCycleBreakdown(damages,yearToPlot=2035,detailToPlot="22PctRE")
     plot1 <- plotDamages(damages,yearToPlot=2019,detailToPlot="",
                            yAxisSuffix="(a) 2019 Grid",topMargin=0,bottomMargin=0.25)
     plot2 <- plotDamages(damages,yearToPlot=2025,detailToPlot="",
                            yAxisSuffix="(b) 2025 Grid",topMargin=0,bottomMargin=0.25)
     plot3 <- plotDamages(damages,yearToPlot=2035,detailToPlot="10PctRE",
                            yAxisSuffix="(c) Future Grid (10% Wind+Solar)",topMargin=0.25,bottomMargin=0)
     plot4 <- plotDamages(damages,yearToPlot=2035,detailToPlot="22PctRE",
                            yAxisSuffix="(d) Future Grid (22% Wind+Solar)",topMargin=0.25,bottomMargin=0)
     ggsave(paste0(results_dir,"DamagesA.pdf"), grid.arrange(plot1,plot2,ncol=1), width=6.5, height=7.75,units="in")
     ggsave(paste0(results_dir,"DamagesB.pdf"), grid.arrange(plot3,plot4,ncol=1), width=6.5, height=7.75,units="in")

}

makeEmissionsPlots <- function(){
     files = list.files(path=results_dir, pattern="*_C{2}",full.names=TRUE)
     emissionsGrid <- rbindlist(lapply(files,getPHORUMGridEmissions))
     emissionsVeh <- rbindlist(lapply(files,getGREETEmissions))
     emissions <- rbindlist(list(emissionsGrid,emissionsVeh))
     emissions[!(stage %like% "manufacture"),cost:=cost*discounted.cash.flow.multiplier/total.cars.switched]
     emissions[,Powertrain:=word(veh,1,sep="_")]
     emissions[Powertrain %like% "PHEV" | Powertrain %like% "BEV",
             Powertrain:=paste0(Powertrain,"-",ifelse(cc==0,"UC","CC"))]
     emissions[Powertrain=="CV",Powertrain:="ICEV"]
     emissions[,Year:=ifelse(detail=="",year,paste0(year,"_",detail))]
     emissions[pollutant=="co",pollutant:="CO"]
     emissions[pollutant=="ghg",pollutant:="GHGs"]
     emissions[pollutant=="nox",pollutant:="NOx"]
     emissions[pollutant=="pm25",pollutant:="PM2.5"]
     emissions[pollutant=="so2",pollutant:="SO2"]
     emissions[pollutant=="voc",pollutant:="VOC"]
     emissions <- emissions[pollutant !="nh3"]
     emissions[stage=="upstream_grid",Stage:="Electricity\nProduction"]
     emissions[stage=="upstream_gasoline",Stage:="Gasoline\nProduction"]
     emissions[stage=="operations",Stage:="Vehicle\nUse"]
     emissions[stage=="grid",Stage:="Electricity\nProduction"]
     emissions[stage=="car_manufacture",Stage:="Vehicle\nProduction"]
     emissions[stage=="battery_manufacture",Stage:="Battery\nProduction"]
     plot1 <- plotEmissions(emissions,yearToPlot=2019,detailToPlot="",
                            yAxisSuffix="(a) 2019 Grid",topMargin=0,bottomMargin=0.25)
     plot2 <- plotEmissions(emissions,yearToPlot=2025,detailToPlot="",
                            yAxisSuffix="(b) 2025 Grid",topMargin=0,bottomMargin=0.25)
     plot3 <- plotEmissions(emissions,yearToPlot=2035,detailToPlot="10PctRE",
                            yAxisSuffix="(c) Future Grid (10% Wind+Solar)",topMargin=0.25,bottomMargin=0)
     plot4 <- plotEmissions(emissions,yearToPlot=2035,detailToPlot="22PctRE",
                            yAxisSuffix="(d) Future Grid (22% Wind+Solar)",topMargin=0.25,bottomMargin=0)
     ggsave(paste0(results_dir,"EmissionsA.pdf"), grid.arrange(plot1,plot2,ncol=1), width=6.5, height=7.75,units="in")
     ggsave(paste0(results_dir,"EmissionsB.pdf"), grid.arrange(plot3,plot4,ncol=1), width=6.5, height=7.75,units="in")
}

plotEmissions <- function(emissions,yearToPlot,detailToPlot="",yAxisSuffix,topMargin,bottomMargin){
     rollup <- emissions[year==yearToPlot & detail==detailToPlot & pollutant !="nh3",.(lbs=sum(cost)/tonne.per.lb),by=.(Powertrain,Stage,Pollutant=pollutant)]
     baseline.grid.emissions <- rollup[Powertrain %like% "ICEV" & Stage =="Electricity\nProduction",.(bau.grid.lbs=sum(lbs)),by=.(Pollutant)]
     rollup[baseline.grid.emissions,bau.grid.lbs:=bau.grid.lbs,on=.(Pollutant)]
     rollup[Stage=="Electricity\nProduction",lbs:=lbs-bau.grid.lbs]
     rollup$Stage <- fct_relevel(rollup$Stage, c("Vehicle\nUse","Electricity\nProduction","Gasoline\nProduction","Battery\nProduction","Vehicle\nProduction"))
     rollup <- rollup[Powertrain %like% "BEV" | Powertrain %like% "PHEV" | Stage != "Electricity\nProduction"]
     rollup$Pollutant <- fct_relevel(rollup$Pollutant, c("GHGs","SO2","NOx","PM2.5","VOC","CO"))
     rollup[Powertrain=="BEV300-CC",Powertrain:="BEV\nCC"]
     rollup[Powertrain=="BEV300-UC",Powertrain:="BEV\nUC"]
     rollup[Powertrain=="PHEV20-CC",Powertrain:="PHEV\nCC"]
     rollup[Powertrain=="PHEV20-UC",Powertrain:="PHEV\nUC"]
     rollup$Powertrain <- fct_relevel(rollup$Powertrain, 
                                 c("BEV\nCC","BEV\nUC","PHEV\nCC","PHEV\nUC","HEV","ICEV"))
     
     plot <- ggplot(rollup,aes(x=Powertrain,y=lbs,fill=Stage))+geom_col(width=0.75) +
          xlab("Powertrain") + ylab(paste0("Life cycle emissions (lb/car)")) +
          scale_x_discrete(limits = rev(levels(rollup$Powertrain))) +
          scale_fill_manual(values = getFiveColors(), guide = guide_legend(reverse = FALSE)) + 
          #scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
          theme_light() + ggtitle(yAxisSuffix) +
          theme(title = element_text(size=12),
                legend.position="right",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 10),
                strip.text = element_text(size = 10,margin = margin(b = 0.1)),
                axis.text = element_text(size=7),
                axis.title = element_text(size=11),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 10), 
                plot.margin = unit(c(topMargin,0,bottomMargin,0), "cm"),
                panel.margin.y = unit(0, "lines")) +
          facet_wrap(. ~ Pollutant,scales="free",ncol=2)
     return(plot)
}

getGenByType <- function(fullString){
     theseResults <- readMat(paste0(fullString,"\\totalResults.mat"))[[1]]
     genMWh <- rowSums(theseResults[[which(dimnames(theseResults)[[1]]=='gLevel')]],na.rm=TRUE)
     files = list.files(path=results_dir, pattern="*_C{2}",full.names=TRUE)
     theseResults <- readMat(paste0(fullString,"\\totalResults.mat"))[[1]]
     genMWh <- rowSums(theseResults[[which(dimnames(theseResults)[[1]]=='gLevel')]],na.rm=TRUE)
     runString <- word(fullString,-1,sep=fixed("\\"))
     year <- parse_number(gsub("_","",str_extract(runString, "_(2)[0-9]{3}_")))
     cc <- parse_number(substr(str_extract(runString, "_CC[0-1]_"),4,4))
     vehType <- substr(runString,1,str_locate(runString, "_CC[0-1]_")[1]-1)
     runString <- gsub(vehType,"",runString)
     txn <- parse_number(substr(str_extract(runString, "_Txn[0-1]"),5,5))
     detail <- ""
     if(str_count(runString,"_")>3) {
          detail <- word(runString,-1,sep="_")
     }
     thisPHORUMdata <- getPHORUMgendata(year,runString)
     thisPHORUMdata$genMWh <- genMWh  
     thisPHORUMdata[,type:=fuel_group]
     thisPHORUMdata[detailed.type=="Nuclear",type:="Other"]
     thisPHORUMdata[detailed.type!="Nuclear" & 
                         fuel_group %in% c("Free","Petroleum Coke"),type:="Other"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type=="Combined Cycle",
                    type:="Combined Cycle Natural Gas"]
     thisPHORUMdata[fuel_group=="Natural Gas" & detailed.type!="Combined Cycle",
                    type:="Other Natural Gas"]
     out <- thisPHORUMdata[,.(MWh=sum(genMWh)),
                           by=.(type)]
     return(out[,.(year=year,cc=cc,veh=vehType,txn=txn,detail=detail,type,MWh)])
     
}

plotGenerationEvolutionNoEV <- function(){
     files = list.files(path=results_dir, pattern="CV_",full.names=TRUE)
     genByType <- rbindlist(lapply(files,getGenByType))
     bau <- genByType[year==2019][,bau.gen:=MWh]
     genByType[bau,bau.gen:=bau.gen,on=.(type)]
     genByType[,change:=MWh-bau.gen]
     genByType$Technology <- fct_relevel(genByType$type, 
                                         c("Combined Cycle Natural Gas","Coal","Other Natural Gas","Petroleum","Other"))
     genByType[,year:=paste0(year,"_",detail)]
     ggplot(genByType,aes(x=Technology,y=change/1000,group=as.factor(year),fill=as.factor(year)))+geom_col(width=0.5,position = position_dodge()) +
          xlab("Technology") + ylab(paste0("Generation changes vs. 2019)")) +
          scale_x_discrete(limits = rev(levels(genByType$Technology))) + 
          scale_y_continuous(position="right") +
          scale_fill_brewer(palette = "Dark2", guide = guide_legend(reverse = TRUE)) +
          theme_light() + 
          theme(legend.position="right",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 11),
                strip.text = element_text(size = 12),
                axis.text = element_text(size=12),
                axis.title = element_text(size=12),
                axis.title.y = element_blank(), 
                plot.margin = unit(c(0,0,0,0), "cm")) +
          coord_flip()  
}

plotGenerationChange <- function(yearToPlot,detailToPlot=""){
     files = list.files(path=results_dir, pattern="*_C{2}",full.names=TRUE)
     if(detailToPlot==""){
          files <- files[files %like% yearToPlot]
          ylab = paste0(yearToPlot," grid")
          
     } else{
          files <- files[files %like% paste0(yearToPlot,"_Txn1_",detailToPlot)]
          ylabel = paste0(yearToPlot," grid, ",detailToPlot)
     }     
     genByType <- rbindlist(lapply(files,getGenByType))
     bau <- genByType[veh=="ICEV"][,bau.gen:=MWh]
     genByType[bau,bau.gen:=bau.gen,on=.(type)]
     genByType[,change:=MWh-bau.gen]
     genByType <- genByType[veh %in% c("BEV300_2021GREET")]
     genByType[,`Charge Scheme`:=ifelse(cc==0,"Uncontrolled","Controlled")]
     genByType$`Charge Scheme` <- fct_relevel(genByType$`Charge Scheme`, c("Controlled","Uncontrolled"))
     genByType$Technology <- fct_relevel(genByType$type, 
                                         c("Combined Cycle Natural Gas","Coal","Other Natural Gas","Petroleum","Other"))

     ggplot(genByType,aes(x=Technology,y=change/1000,group=`Charge Scheme`,fill=`Charge Scheme`))+geom_col(width=0.75,position = position_dodge()) +
          xlab("Technology") + ylab(paste0("Generation induced by charging\n(GWh per year, BEV300, ",ylabel,")")) +
          scale_x_discrete(limits = rev(levels(genByType$Technology))) + 
          scale_y_continuous(position="right") +
          scale_fill_brewer(palette = "Dark2", guide = guide_legend(reverse = TRUE)) +
          theme_light() + 
          theme(legend.position="right",
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 11),
                strip.text = element_text(size = 12),
                axis.text = element_text(size=12),
                axis.title = element_text(size=12),
                axis.title.y = element_blank(), 
                plot.margin = unit(c(0,0,0,0), "cm")) +
          coord_flip()  
     suffix <- ifelse(detailToPlot=="",year,paste0(yearToPlot,"_",detailToPlot))
     ggsave(paste0(results_dir,"GenChanges_",suffix,".pdf"), width=6.5, height=3,units="in")
     
}
