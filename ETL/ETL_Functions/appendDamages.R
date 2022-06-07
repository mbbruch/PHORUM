appendDamages <- function(phorum){
     egrid <- fread(paste0(gen_dir,"egrid2019_data.csv"))
     egrid <- egrid[,.(ORISPL,LAT,LON,PLNGENAN, PLNOXRTA, PLSO2RTA, PLN2ORTA, PLCO2RTA, PLC2ERTA)]
     damages <- fread(paste0(base_dir,"county_data\\caces.damages.all.counties.csv"))
     county_data <- fread(paste0(base_dir,"county_data\\county_2019_populations.csv"))
     epa_aem_2019 <- fread(paste0(gen_dir,"2019_inventory_point_20210914\\egu_SmokeFlatFile_2019NEI_POINT_20210914_16sep2021_v0.csv"), skip=15)
     epa_aem_2019 <- epa_aem_2019[poll %in% c("VOC","CO","PM25-PRI","SO2","NOX","PM10-PRI","NH3") & ipm_yn !='AKHIPRVI']
     epa_aem_2019[,oris_facility_code:=as.numeric(oris_facility_code)]
     epa_aem_2019[,ipm_yn_orispl:=as.numeric(word(ipm_yn,1,1,sep="_"))]
     epa_aem_2019[,ipm_yn_subnumber:=fifelse(str_count(ipm_yn,"_")>0,
                                             as.numeric(parse_number(word(ipm_yn,-1,sep="_"))),
                                             as.numeric(NA))]
     epa_aem_2019[,max_orispl:=max(oris_facility_code,na.rm=TRUE),by=facility_name]
     epa_aem_2019[,max_ipm_yn_orispl:=max(ipm_yn_orispl,na.rm=TRUE),by=facility_name]
     epa_aem_2019[is.na(max_orispl) | max_orispl < 1,max_orispl:=max_ipm_yn_orispl]
     epa_aem_2019[is.na(oris_facility_code),oris_facility_code:=max_orispl]
     epa_aem_2019 <- dcast(epa_aem_2019, facility_name + oris_facility_code ~ poll, value.var="ann_value",fun.aggregate = sum, na.rm=TRUE)
     
     #lbs/MWh
     egrid <- egrid[str_count(PLNGENAN,"\\(")==0 & str_count(PLNGENAN,"\\)")==0 & PLNGENAN !="0",positive.gen:=1][is.na(positive.gen),positive.gen:=0]
     egrid <- egrid[positive.gen==1]
     egrid[,':='(
          PLNGENAN=parse_number(PLNGENAN),
          PLNOXRTA=parse_number(PLNOXRTA),
          PLSO2RTA=parse_number(PLSO2RTA),
          #PLN2ORTA=parse_number(PLN2ORTA), already numeric
          PLCO2RTA=parse_number(PLCO2RTA),
          PLC2ERTA=parse_number(PLC2ERTA)
     )]
     egrid[epa_aem_2019, ':='(
          CO=CO,NH3=NH3,PM10=`PM10-PRI`,PM25=`PM25-PRI`,VOC=VOC
     ),on=.(ORISPL=oris_facility_code)]
     egrid <- egrid[!is.na(PLNGENAN) | 
                         !is.na(PLNOXRTA) | 
                         !is.na(PLSO2RTA) |
                         !is.na(PLN2ORTA) | 
                         !is.na(PLCO2RTA) | 
                         !is.na(PLC2ERTA)]
     phorum <- phorum[egrid,':='(
          NOX=PLNOXRTA,
          SO2=PLSO2RTA,
          N2O=PLN2ORTA,
          CO2=PLCO2RTA,
          CO2eqv=PLC2ERTA,
          CO=2000*CO/PLNGENAN,
          NH3=2000*NH3/PLNGENAN,
          PM10=2000*PM10/PLNGENAN,
          PM25=2000*PM25/PLNGENAN,
          VOC=2000*VOC/PLNGENAN
     ),on=.(plant.id=ORISPL)]
     phorum.95th <- phorum[,.(NOX.95=quantile(NOX,0.95,na.rm=TRUE),
                              SO2.95=quantile(SO2,0.95,na.rm=TRUE),
                              N2O.95=quantile(N2O,0.95,na.rm=TRUE),
                              CO2.95=quantile(CO2,0.95,na.rm=TRUE),
                              CO2eqv.95=quantile(CO2eqv,0.95,na.rm=TRUE),
                              CO.95=quantile(CO,0.95,na.rm=TRUE),
                              NH3.95=quantile(NH3,0.95,na.rm=TRUE),
                              PM10.95=quantile(PM10,0.95,na.rm=TRUE),
                              PM25.95=quantile(PM25,0.95,na.rm=TRUE),
                              VOC.95=quantile(VOC,0.95,na.rm=TRUE)),
                           by=.(overall.type.needs)]
     phorum <- phorum[phorum.95th,on=.(overall.type.needs)]
     phorum[,':='(
          NOX=pmin(NOX,NOX.95),
          SO2=pmin(SO2,SO2.95),
          N2O=pmin(N2O,N2O.95),
          CO2=pmin(CO2,CO2.95),
          CO2eqv=pmin(CO2eqv,CO2eqv.95),
          CO=pmin(CO,CO.95),
          NH3=pmin(NH3,NH3.95),
          PM10=pmin(PM10,PM10.95),
          PM25=pmin(PM25,PM25.95),
          VOC=pmin(VOC,VOC.95)
     )]
     phorum.averages <- phorum[,.(NOX.avg=mean(NOX,na.rm=TRUE),
                                  SO2.avg=mean(SO2,na.rm=TRUE),
                                  N2O.avg=mean(N2O,na.rm=TRUE),
                                  CO2.avg=mean(CO2,na.rm=TRUE),
                                  CO2eqv.avg=mean(CO2eqv,na.rm=TRUE),
                                  CO.avg=mean(CO,na.rm=TRUE),
                                  NH3.avg=mean(NH3,na.rm=TRUE),
                                  PM10.avg=mean(PM10,na.rm=TRUE),
                                  PM25.avg=mean(PM25,na.rm=TRUE),
                                  VOC.avg=mean(VOC,na.rm=TRUE)),
                               by=.(overall.type.needs)]
     
     phorum <- phorum[phorum.averages,':='(
          NOX=fifelse(is.na(NOX),NOX.avg,NOX),
          SO2=fifelse(is.na(SO2),SO2.avg,SO2),
          N2O=fifelse(is.na(N2O),N2O.avg,N2O),
          CO2=fifelse(is.na(CO2),CO2.avg,CO2),
          CO2eqv=fifelse(is.na(CO2eqv),CO2eqv.avg,CO2eqv),
          CO=fifelse(is.na(CO),CO.avg,CO),
          NH3=fifelse(is.na(NH3),NH3.avg,NH3),
          PM10=fifelse(is.na(PM10),PM10.avg,PM10),
          PM25=fifelse(is.na(PM25),PM25.avg,PM25),
          VOC=fifelse(is.na(VOC),VOC.avg,VOC)
     ),
     on=.(overall.type.needs)]
     phorum[damages[model=='INMAP' & pollutant=='pm25'],MDPM25:=damage.acs.stacklevel*PM25*tonne.per.lb,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='so2'],MDSO2:=damage.acs.stacklevel*SO2*tonne.per.lb,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='voc'],MDVOC:=damage.acs.stacklevel*VOC*tonne.per.lb,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='nh3'],MDNH3:=damage.acs.stacklevel*NH3*tonne.per.lb,on=.(fips=fips)]
     phorum[damages[model=='INMAP' & pollutant=='nox'],MDNOX:=damage.acs.stacklevel*NOX*tonne.per.lb,on=.(fips=fips)]
     
     phorum[is.na(MDPM25),MDPM25:=0]
     phorum[is.na(MDSO2),MDSO2:=0]
     phorum[is.na(MDVOC),MDVOC:=0]
     phorum[is.na(MDNH3),MDNH3:=0]
     phorum[is.na(MDNOX),MDNOX:=0]
     
     return(phorum)
}
