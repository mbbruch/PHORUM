fixupBadHours <- function(data,columnString){
        cols.to.fixup <- colnames(data)[grep(columnString,colnames(data))]
        for(i in cols.to.fixup){
                #Missing data 
                data[date=='2019-03-10' & hour.ending==3,c(i)] <- 
                        (data[date=='2019-03-10' & hour.ending==2,..i] +
                                 data[date=='2019-03-10' & hour.ending==4,..i])/2
                #Daylight savings time makes most (but not all!) variables doubled
                data[date=='2019-11-03' & hour.ending==2,c(i)] <- 
                        (data[date=='2019-11-03' & hour.ending==1,..i] +
                                 data[date=='2019-11-03' & hour.ending==3,..i])/2
        }
        return(data)
}

importRenewables <- function(modelingYear,renewablePct,load,queue,phorum_old){
        wind <- fread(paste0(dataminer_dir,"wind_gen.csv"))
        solar <- fread(paste0(dataminer_dir,"solar_gen.csv"))
        wind[area=="WEST",TCR:=1][
                area=="MIDATL",TCR:=3][
                        area=="SOUTH",TCR:=5][
                                area=="OTHER",TCR:=1]
        solar[area=="WEST",TCR:=1][
                area=="MIDATL",TCR:=3][
                        area=="SOUTH",TCR:=5][
                                area=="OTHER",TCR:=1]
        wind1 <- wind[area=='WEST'][,':='(TCR=1,wind_generation_mw=0)]
        wind2 <- wind[area=='WEST'][,':='(TCR=2,wind_generation_mw=0)]
        wind3 <- wind[area=='WEST'][,':='(TCR=3,wind_generation_mw=0)]
        wind4 <- wind[area=='WEST'][,':='(TCR=4,wind_generation_mw=0)]
        wind5 <- wind[area=='WEST'][,':='(TCR=5,wind_generation_mw=0)]
        wind <- rbindlist(list(
                wind,
                wind1,
                wind2,
                wind3,
                wind4,
                wind5
        ))
        solar1 <- solar[area=='WEST'][,':='(TCR=1,solar_generation_mw=0)]
        solar2 <- solar[area=='WEST'][,':='(TCR=2,solar_generation_mw=0)]
        solar3 <- solar[area=='WEST'][,':='(TCR=3,solar_generation_mw=0)]
        solar4 <- solar[area=='WEST'][,':='(TCR=4,solar_generation_mw=0)]
        solar5 <- solar[area=='WEST'][,':='(TCR=5,solar_generation_mw=0)]
        solar <- rbindlist(list(
                solar,
                solar1,
                solar2,
                solar3,
                solar4,
                solar5
        ))
        
        if(modelingYear %in% c(2025,2035)){
                queue[TCR %in% c(2,4),TCR:=3]
                solar.shares <- queue[Fuel %like% 'Solar',.(capacity.nameplate=sum(MFO,na.rm=TRUE)),by=TCR][,.(TCR,TCR.share=capacity.nameplate/sum(capacity.nameplate,na.rm=TRUE))]
                wind.shares <- queue[Fuel %like% 'Wind',.(capacity.nameplate=sum(MFO,na.rm=TRUE)),by=TCR][,.(TCR,TCR.share=capacity.nameplate/sum(capacity.nameplate,na.rm=TRUE))]
                offshore.wind.shares <- queue[Fuel %like% 'Offshore',.(capacity.nameplate=sum(MFO,na.rm=TRUE)),by=TCR][,.(TCR,TCR.share=capacity.nameplate/sum(capacity.nameplate,na.rm=TRUE))]
                onshore.wind.shares <- queue[(Fuel %like% 'Wind') & !(Fuel %like% 'Offshore'),.(capacity.nameplate=sum(MFO,na.rm=TRUE)),by=TCR][,.(TCR,TCR.share=capacity.nameplate/sum(capacity.nameplate,na.rm=TRUE))]
                wind.shares[offshore.wind.shares,TCR.share.offshore:=i.TCR.share,on=.(TCR=TCR)][is.na(TCR.share.offshore),TCR.share.offshore:=0]
                wind.shares[onshore.wind.shares,TCR.share.onshore:=i.TCR.share,on=.(TCR=TCR)][is.na(TCR.share.onshore),TCR.share.onshore:=0]
                needs_2019 <- fread(paste0(gen_dir,"needs_20220124_active.csv"))
                needs_2019 <- needs_2019[substr(`Region Name`,1,3)=='PJM'][ 
                        (`On Line Year` <= (2019) | `On Line Year`==9999) &
                                (`Retirement Year` >= (2019+1) | `Retirement Year`==9999) & 
                                PlantType %in% c('Solar PV','Onshore Wind','Offshore Wind')]
                needs_2019[,subregion:=toupper(word(needs_2019$`Region Name`,2,2,sep="_"))]
                counties <- fread(paste0(county_dir,"county_2019_populations.csv"))
                needs_2019[counties,':='(LAT=INTPTLAT,LON=INTPTLONG),on=.(FIPS5=GEOID)]
                needs_2019 <- appendWindSolarTCRs(needs_2019,phorum_old)
                needs_2019[TCR %in% c(2,4),TCR:=3]
                needs_installed_renewables <- needs_2019[,.(installed_capacity=sum(`Capacity (MW)`,na.rm=TRUE)),by=.(PlantType,TCR)][TCR %in% c(1,3,5)]
                solar_mw_2019 <- needs_installed_renewables[PlantType=='Solar PV']
                solar_mwh_2019 <- solar[TCR %in% c(1,3,5),.(mwh=sum(solar_generation_mw,na.rm=TRUE)),by=TCR]
                solar_mwh_2019[solar_mw_2019,installed_capacity:=installed_capacity,on=.(TCR=TCR)][,mwh.per.mw:=mwh/installed_capacity]
                wind_mw_2019 <- needs_installed_renewables[PlantType %like% 'Wind']
                wind_mwh_2019 <- wind[TCR %in% c(1,3,5),.(mwh=sum(wind_generation_mw,na.rm=TRUE)),by=TCR]
                wind_mwh_2019[wind_mw_2019,installed_capacity:=installed_capacity,on=.(TCR=TCR)][,mwh.per.mw:=mwh/installed_capacity]
                
                solar.shares[solar_mwh_2019,':='(mwh.per.mw=mwh.per.mw,mwh=mwh),on=.(TCR=TCR)]
                wind.shares[wind_mwh_2019,':='(mwh.per.mw=mwh.per.mw,mwh=mwh),on=.(TCR=TCR)]
                offshore.wind.shares[wind_mwh_2019,':='(mwh.per.mw=mwh.per.mw,mwh=mwh),on=.(TCR=TCR)]
                onshore.wind.shares[wind_mwh_2019,':='(mwh.per.mw=mwh.per.mw,mwh=mwh),on=.(TCR=TCR)]
                
                if(modelingYear==2025){
                        #2025: add everything suggested by: IHS Markit (for solar), recent year installation trends from EIA860 (for wind)
                        future_solar <- fread(paste0(misc_dir,"future_solar.csv"))
                        newSolarMW <- future_solar[year >= 2020 & year<=modelingYear,sum(solar_additions_mw)]
                        future_wind <- fread(paste0(misc_dir,"future_wind.csv"))
                        newWindMW <- future_wind[year==modelingYear,]$wind_mw_adjusted - future_wind[year==2019,]$wind_mw_adjusted
                        wind.shares <- wind.shares[,.(TCR, mwh.per.mw, mwh, installations=newWindMW*TCR.share)]
                } else if(modelingYear==2035){
                        total.load.2019 <- sum(load[,(which(colnames(load)=='hour')+1):ncol(load)])
                        renewable.mwh.needed <- total.load.2019*renewablePct
                        #shares by fuel taken from PJM "energy transition in PJM: frameworks for analysis" study addendum
                        if(renewablePct == 0.10){
                                offshoreMW = 260; onshoreMW=11194; solarMW=3977;
                        } else if(renewablePct ==0.22){
                                offshoreMW = 11701; onshoreMW=18524; solarMW=24020;
                        }
                        totalCapacityMW = offshoreMW+onshoreMW+solarMW
                        offshoreShareMW = offshoreMW/totalCapacityMW
                        onshoreShareMW = onshoreMW/totalCapacityMW
                        windShareMW = offshoreShareMW + onshoreShareMW
                        solarShareMW = 1-offshoreShareMW-onshoreShareMW
                        mwh.per.mw.offshore <- offshore.wind.shares[,sum(TCR.share*mwh.per.mw,na.rm=TRUE)]
                        mwh.per.mw.onshore <- onshore.wind.shares[,sum(TCR.share*mwh.per.mw,na.rm=TRUE)]
                        mwh.per.mw.solar <- solar.shares[,sum(TCR.share*mwh.per.mw,na.rm=TRUE)]
                        mwh.per.mw.total <- mwh.per.mw.offshore*offshoreShareMW+mwh.per.mw.onshore*onshoreShareMW+mwh.per.mw.solar*solarShareMW
                        renewable.mw.needed <- renewable.mwh.needed/mwh.per.mw.total
                        newWindMW = renewable.mw.needed*windShareMW
                        newOffshoreMW = renewable.mw.needed*offshoreShareMW
                        newOnshoreMW = renewable.mw.needed*onshoreShareMW
                        newSolarMW = renewable.mw.needed*solarShareMW
                        wind.shares <- wind.shares[,.(TCR, mwh.per.mw, mwh, installations=newOffshoreMW*TCR.share.offshore+newOnshoreMW*TCR.share.onshore)]
                }
                wind.shares[,total.new.mwh:=installations*mwh.per.mw]
                solar.shares <- solar.shares[,.(TCR, mwh.per.mw, mwh, installations=newSolarMW*TCR.share)]
                solar.shares[,total.new.mwh:=installations*mwh.per.mw]
                
                solar.shares[,multiplier:=(1+total.new.mwh/mwh)]
                wind.shares[,multiplier:=(1+total.new.mwh/mwh)]
                solar[solar.shares,multiplier:=multiplier,on=.(TCR=TCR)][TCR %in% c(2,4),multiplier:=0][,solar_generation_mw:=solar_generation_mw*multiplier]
                wind[wind.shares,multiplier:=multiplier,on=.(TCR=TCR)][TCR %in% c(2,4),multiplier:=0][,wind_generation_mw:=wind_generation_mw*multiplier]
        }
        
        intermittents <- rbindlist(list(wind[!is.na(TCR),.(type="windTCR",datetime=datetime_beginning_ept,TCR,MW=wind_generation_mw)],
                                        solar[!is.na(TCR),.(type="solarTCR",datetime=datetime_beginning_ept,TCR,MW=solar_generation_mw)]))
        intermittents = data.table(dcast(intermittents, datetime ~ type + TCR, value.var = c("MW"), fun.aggregate=sum))
        
        names(intermittents)<-gsub("TCR\\_","TCR",names(intermittents))
        
        intermittents <- intermittents[,datetime:=lubridate::parse_date_time(datetime, 
                                                                             '%m/%d/%Y %I:%M:%S %p')]
        intermittents[,':='(date=date(datetime),
                            hour.ending=hour(datetime)+1)][,datetime:=NULL]
        intermittents <- select(intermittents, date, hour.ending, everything())
        intermittents <- rbindlist(list(intermittents,
                                        data.table(date=as.Date('2019-03-10'),
                                                   hour.ending=3)),
                                   fill=TRUE)
        intermittents[is.na(intermittents)] <- 0
        intermittents <- intermittents[order(date,hour.ending)][,hour:=.I]
        intermittents <- select(intermittents, date, hour.ending, hour, everything())
        
        intermittents <- fixupBadHours(intermittents,"^[A-Za-z]{4,5}_[0-9]{1}")
        if(renewablePct > 0){
                write.csv(intermittents,paste0(out_dir,"windSolar_",modelingYear,"_",renewablePct*100,"PctRE.csv"))
        } else{
                write.csv(intermittents,paste0(out_dir,"windSolar_",modelingYear,".csv"))
        }
        return(intermittents)
}

importTransferLimits <- function(modelingYear){
        txn <- fread(paste0(dataminer_dir,"transfer_limits_and_flows.csv"))
        areas_to_tcr <- fread(paste0(base_dir,"crosswalks\\TransferLimitArea_To_TCRLink.csv"))
        txn <- txn[areas_to_tcr,on=.(transfer_limit_area=TransferLimitArea),allow.cartesian=TRUE][!is.na(TCRLink)][,transfer_limit:=transfer_limit*Multiplier]
        txn[,TCRLink:=paste0("TI",TCRLink)]
        txn = data.table(dcast(txn, datetime_beginning_ept ~ TCRLink, value.var = c("transfer_limit"), fun.aggregate=sum))
        txn <- txn[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                         '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
        txn[,':='(date=date(datetime),
                  hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
        txn <- select(txn, date, hour.ending, everything())
        txn <- rbindlist(list(txn,
                              data.table(date=as.Date('2019-03-10'),
                                         hour.ending=3)),
                         fill=TRUE)
        txn <- txn[order(date,hour.ending)][year(date)==2019][,hour:=.I]
        txn <- select(txn, date, hour.ending, hour, everything())
        txn <- fixupBadHours(txn,"[A-Z]{1,}")
        write.csv(txn,paste0(out_dir,"transferLimits_",modelingYear,".csv"))
        return(txn)
}

importInOuts <- function(modelingYear){
        imports <- fread(paste0(dataminer_dir,"act_sch_interchange.csv"))
        imports = data.table(dcast(imports, datetime_beginning_ept ~ tie_line, value.var = c("actual_flow"), fun.aggregate=sum))
        imports <- imports[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                                 '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
        imports[,':='(date=date(datetime),
                      hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
        imports <- select(imports, date, hour.ending, everything())
        imports <- rbindlist(list(imports,
                                  data.table(date=as.Date('2019-03-10'),
                                             hour.ending=3)),
                             fill=TRUE)
        imports <- imports[order(date,hour.ending)][year(date)==2019][,hour:=.I]
        imports <- select(imports, date, hour.ending, hour, everything())
        imports <- fixupBadHours(imports,"[A-Z]{1,}")
        
        write.csv(imports,paste0(out_dir,"imports_",modelingYear,".csv"))
        return(imports)
}

importLoad <- function(modelingYear){
        
        load <- fread(paste0(dataminer_dir,"hrl_load_metered.csv"))
        load = data.table(dcast(load, datetime_beginning_ept ~ load_area, value.var = c("mw"), fun.aggregate=sum))
        load <- load[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                           '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
        load[,':='(date=date(datetime),
                   hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
        load <- select(load, date, hour.ending, everything())
        load <- rbindlist(list(load,
                               data.table(date=as.Date('2019-03-10'),
                                          hour.ending=3)),
                          fill=TRUE)
        load <- load[order(date,hour.ending)][year(date)==2019][,hour:=.I]
        load <- select(load, date, hour.ending, hour, everything())
        load$RTO <- NULL
        load <- fixupBadHours(load,"[A-Z]{2,}")
        write.csv(load,paste0(out_dir,"load_",modelingYear,".csv"))
        return(load)
}