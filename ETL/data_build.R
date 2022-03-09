library(data.table)

phorum.old <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\PHORUMdata2011.csv")
needs <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\needs_20201006.csv")
eia860 <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\eia860_2019.csv")
egrid <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\egrid2018_data_v2.csv")
egrid <- egrid[,.(ORISPL,PLNOXRTO, PLSO2RTA, PLN2ORTA, PLCO2RTA, PLC2ERTA)]
damages <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\caces.damages.all.counties.csv")

setnames(phorum.old,"Variable O&M Cost ($/MWh)", "om.cost")
setnames(phorum.old,"Startup cost adder ($)", "startup.cost")
setnames(phorum.old,"Boiler/Generator/Committed Unit","boiler.gen.or.committed")
setnames(phorum.old,"Combustion Turbine/IC Engine","combustion.turbine.or.engine")
setnames(phorum.old,"Modeled Fuels", "modeled.fuels")
setnames(phorum.old,"Capacity (MW) (Summer)", "gen.capacity.summer")
setnames(phorum.old,"Capacity (MW) (Winter)", "gen.capacity.winter")
setnames(phorum.old,"Min uptime (hrs)","min.uptime")
setnames(phorum.old,"Min downtime (hrs)","min.downtime")
setnames(phorum.old,"Minimum Generation (% of max)","min.generation.pct")
setnames(needs,"Boiler/Generator/Committed Unit","boiler.gen.or.committed")
setnames(needs,"Combustion Turbine/IC Engine","combustion.turbine.or.engine")
setnames(needs,"Modeled Fuels", "modeled.fuels")
setnames(needs,"Capacity (MW)", "capacity.needs")
setnames(eia860,"Nameplate Capacity (MW)", "gen.capacity.eia")
setnames(eia860,"Summer Capacity (MW)", "gen.capacity.summer")
setnames(eia860,"Winter Capacity (MW)", "gen.capacity.winter")
setnames(eia860,"Minimum Load (MW)", "min.load")

phorum.old[om.cost %in% c(4.25,4.29),om.cost:=4.29] #fix of a random typo in XLSX
phorum.old[,':='(
        startup.cost=as.numeric(gsub("\\,", "", startup.cost)),
        min.generation.pct=as.numeric(gsub("%", "", min.generation.pct)))]
egrid[,':='(
        PLNOXRTO=as.numeric(gsub("\\,", "", PLNOXRTO)),
        PLSO2RTA=as.numeric(gsub("\\,", "", PLSO2RTA)),
        PLN2ORTA=as.numeric(gsub("\\,", "", PLN2ORTA)),
        PLCO2RTA=as.numeric(gsub("\\,", "", PLCO2RTA)),
        PLC2ERTA=as.numeric(gsub("\\,", "", PLC2ERTA))
)]
eia860[,':='(
        gen.capacity.eia = as.numeric(gsub("\\,", "", gen.capacity.eia)),
        gen.capacity.summer = as.numeric(gsub("\\,", "", gen.capacity.summer)),
        gen.capacity.winter = as.numeric(gsub("\\,", "", gen.capacity.winter)),
        min.load = as.numeric(gsub("\\,", "", min.load)))]
needs[,modeled.fuels:=gsub("Natural Gas","NG", modeled.fuels)][
        ,modeled.fuels:=gsub("Distillate Fuel Oil","DFO", modeled.fuels)   
        ][,modeled.fuels:=gsub("Residual Fuel Oil","RFO", modeled.fuels) 
        ][,modeled.fuels:=gsub("Bituminous","BIT", modeled.fuels) 
        ][,modeled.fuels:=gsub("Subbituminous","SUB", modeled.fuels) 
        ][,modeled.fuels:=gsub("Nuclear Fuel","NUC", modeled.fuels)
        ]
needs[modeled.fuels=="NG, DFO",modeled.fuels:="NG"][modeled.fuels=="NG, RFO",modeled.fuels:="NG"]
needs <- needs[!(modeled.fuels %in% c("Wind","Solar","Energy Storage"))]

om.costs <- unique(phorum.old[,.(PlantType,
                                 boiler.gen.or.committed,
                                 combustion.turbine.or.engine,
                                 modeled.fuels,
                                 om.cost)])
om.costs.single <- unique(om.costs[,unique.vals:=uniqueN(om.cost),by=PlantType][unique.vals==1][,.(PlantType,om.cost)])
om.costs.single <- rbindlist(list(om.costs.single,
                                data.table(PlantType="Non-Fossil Waste",om.cost=om.costs.single[PlantType=="Biomass",om.cost]),
                                data.table(PlantType="Tires",om.cost=om.costs.single[PlantType=="Biomass",om.cost])))
needs[om.costs,om.cost.1:=om.cost, on=.(boiler.gen.or.committed,
                                                          combustion.turbine.or.engine,
                                                          modeled.fuels)]
needs[om.costs.single,om.cost.2:=om.cost, on=.(PlantType)]
needs[,om.cost:=ifelse(is.na(om.cost.1),om.cost.2,om.cost.1)][,':='(om.cost.1=NULL,om.cost.2=NULL)]

needs[PlantType=="Combustion Turbine",startup.cost:=capacity.needs*25
      ][PlantType=="Nuclear",startup.cost:=capacity.needs*500
        ][PlantType=="Combined Cycle",startup.cost:=capacity.needs*50
          ][PlantType %like% "Coal" | PlantType %like% "Waste" | 
                    PlantType %like% "Bio" | PlantType %like% "Steam", startup.cost:=capacity.needs*100]

phorum.old[,':='(size.bucket=fifelse(pmax(gen.capacity.winter,gen.capacity.summer)>=500,
                                     "500+",fifelse(pmax(gen.capacity.winter,gen.capacity.summer)>=250,"250-499","0-250")
))]
eafs.jan <- phorum.old[,.(eaf=mean(`EAF Jan-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.feb <- phorum.old[,.(eaf=mean(`EAF Feb-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.mar <- phorum.old[,.(eaf=mean(`EAF Mar-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.apr <- phorum.old[,.(eaf=mean(`EAF Apr-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.may <- phorum.old[,.(eaf=mean(`EAF May-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.jun <- phorum.old[,.(eaf=mean(`EAF Jun-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.jul <- phorum.old[,.(eaf=mean(`EAF Jul-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.aug <- phorum.old[,.(eaf=mean(`EAF Aug-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.sep <- phorum.old[,.(eaf=mean(`EAF Sept-10`),na.rm=TRUE),by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.oct <- phorum.old[,.(eaf=mean(`EAF Oct-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.nov <- phorum.old[,.(eaf=mean(`EAF Nov-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
eafs.dec <- phorum.old[,.(eaf=mean(`EAF Dec-10`),na.rm=TRUE), by=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]

needs[,':='(
        eaf.jan=100,
        eaf.feb=100,
        eaf.mar=100,
        eaf.apr=100,
        eaf.may=100,
        eaf.jun=100,
        eaf.jul=100,
        eaf.aug=100,
        eaf.sep=100,
        eaf.oct=100,
        eaf.nov=100,
        eaf.dec=100,
        size.bucket=fifelse(capacity.needs>=500,
                            "500+",fifelse(capacity.needs>=250,"250-499","0-250")
                            ))]
needs[eafs.jan,eaf.jan:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.feb,eaf.feb:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.mar,eaf.mar:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.apr,eaf.apr:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.may,eaf.may:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.jun,eaf.jun:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.jul,eaf.jul:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.aug,eaf.aug:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.sep,eaf.sep:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.oct,eaf.oct:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.nov,eaf.nov:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]
needs[eafs.dec,eaf.dec:=eaf,on=.(size.bucket,PlantType, boiler.gen.or.committed, combustion.turbine.or.engine, modeled.fuels)]


needs.names <- needs[substr(`Region Name`,1,3)=='PJM'][ 
        (`On Line Year` < 2020 | `On Line Year`==9999) &
                (`Retirement Year` >= 2020 | `Retirement Year`==9999)][
                        ,.(plant.id = `ORIS Plant Code`, 
                          plant.name = `Plant Name`,
                          plant.id = `ORIS Plant Code`, 
                          gen.id = `Unit ID`,
                          unique.id = UniqueID_Final,
                          fips=FIPS5,
                          gen.capacity.needs = capacity.needs,
                          heat.rate.btu.kwh = `Heat Rate (Btu/kWh)`,
                          gen.type.needs = combustion.turbine.or.engine, 
                          unit.type = boiler.gen.or.committed,
                          detailed.type = PlantType,
                          overall.type.needs = paste0(boiler.gen.or.committed,"_", substr(combustion.turbine.or.engine,1,2),"_",PlantType,"_",modeled.fuels),
                          fuel = modeled.fuels,
                          combustion = (combustion.turbine.or.engine=='IC Engine'),
                          year.on = ifelse(`On Line Year` < 9999, `On Line Year`,NA),
                          year.off = ifelse(`Retirement Year` < 9999,`Retirement Year`,NA),
                          om.cost,
                          eaf.jan,
                          eaf.feb,
                          eaf.mar,
                          eaf.apr,
                          eaf.may,
                          eaf.jun,
                          eaf.jul,
                          eaf.aug,
                          eaf.sep,
                          eaf.oct,
                          eaf.nov,
                          eaf.dec)
                ][,':='(gen.count=.N,
                        overall.type.count.needs=uniqueN(overall.type.needs),
                        plant.capacity=sum(gen.capacity.needs,na.rm=TRUE)),
                  by=.(plant.id,combustion)]


eia860.names <- eia860[
        is.na(`Planned Retirement Year`) | `Planned Retirement Year`>2020 |
                (`Planned Retirement Year`==2020) & `Planned Retirement Month` >= 5
        ][,
             .(plant.id = `Plant Code`,
               plant.name = `Plant Name`,
               gen.id = `Generator ID`, 
               gen.capacity.eia,
               gen.capacity.summer,
               gen.capacity.winter,
               min.load,
               gen.power.factor = `Nameplate Power Factor`,
               gen.type.eia = `Prime Mover`, 
               tech = `Technology`,
               fuel = `Energy Source 1`,
               overall.type.eia = paste0(`Prime Mover`,"_", `Technology`,"_",`Energy Source 1`),
               combustion = (`Prime Mover`=='IC' & Technology!='Landfill Gas'),
               year.on = `Operating Year`,
               year.off = `Planned Retirement Year`,
               month.on = `Operating Month`,
               month.off = `Planned Retirement Month`)][
                       plant.id==2847 & gen.type.eia=='IC',':='(combustion=TRUE)
               ][plant.id==2847 & gen.id %in% c('GT4','GT5','GT6','GT7'),':='(plant.id=55248)][
                       plant.id %in% c(58177,58326,58980),combustion:=FALSE
               ][,':='(gen.count=.N,
                       overall.type.count.eia=uniqueN(overall.type.eia),
                       plant.capacity=sum(gen.capacity.eia,na.rm=TRUE)),
                 by=.(plant.id,combustion)][
                         ,':='(
                                 summer.factor = gen.capacity.summer/gen.capacity.eia,
                                 winter.factor = gen.capacity.winter/gen.capacity.eia,
                                 winter.summer = gen.capacity.winter/gen.capacity.summer
                         )
                 ]
                                                                                           
eia860.names <- eia860.names[needs.names[,.(gens=.N),by=.(plant.id,plant.name,combustion)],':='(gens.needs=i.gens), on=.(plant.id=plant.id,combustion=combustion)][
        is.na(year.off) | year.off!=2020 | month.off > 5 | (!is.na(gens.needs) & gens.needs>=gen.count)]

#TODO: merge all of this over as proportions, not levels; some are e.g. 60x1 vs 15x4
merged <- needs.names[eia860.names,':='(gen.capacity.summer.1=gen.capacity.summer,
                                        gen.capacity.winter.1=gen.capacity.winter,
                                        gen.type.eia=gen.type.eia),
                      on=.(plant.id=plant.id,gen.id=gen.id)]

merged[detailed.type=="Combustion Turbine" | gen.type.eia=="IC" | fuel %in% "Landfill Gas",ramp.rate:=gen.capacity.needs*0.33
        ][gen.type.eia=="GT",ramp.rate:=gen.capacity.needs*0.34
        ][detailed.type %like% "Steam" | detailed.type %in% c("Nuclear","Fuel Cell") | gen.type.eia=="ST" | fuel %in% c("MSW","Biomass") | fuel %like% "Waste",ramp.rate:=gen.capacity.needs*0.14
        ][detailed.type=="Combined Cycle" | gen.type.eia %in% c("CA","CC","CS","CT"),ramp.rate:=gen.capacity.needs*0.22
        ][detailed.type %like% "Hydro" & ((!is.na(gen.capacity.summer.1) & gen.capacity.summer.1>0) | 
                                              (!is.na(gen.capacity.winter.1) & gen.capacity.winter.1>0)),ramp.rate:=30]

eia860.names[,':='(unique.summer.capacities=uniqueN(gen.capacity.summer),
                                                    unique.winter.capacities=uniqueN(gen.capacity.winter)),by=plant.id]
eia860.consistent.summer <- eia860.names[!is.na(gen.capacity.summer) & unique.summer.capacities==1,.(summer=mean(gen.capacity.summer)),by=.(plant.id)]
eia860.consistent.winter <- eia860.names[!is.na(gen.capacity.winter) & unique.winter.capacities==1,.(winter=mean(gen.capacity.winter)),by=.(plant.id)]
eia860.averages <- eia860.names[,.(summer=mean(gen.capacity.summer,na.rm=TRUE),
                                   winter=mean(gen.capacity.winter,na.rm=TRUE)),
                                by=plant.id]
merged[ eia860.consistent.summer,':='(gen.capacity.summer.2=summer),on=.(plant.id=plant.id)]
merged[eia860.consistent.winter,':='(gen.capacity.winter.2=winter),on=.(plant.id=plant.id)]
merged[eia860.averages,':='(gen.capacity.summer.3=summer,
                            gen.capacity.winter.3=winter),
       on=.(plant.id=plant.id)]
merged[,':='(
        gen.capacity.summer=fifelse(is.na(gen.capacity.summer.1),
                                    fifelse(is.na(gen.capacity.summer.2),
                                            gen.capacity.summer.3,gen.capacity.summer.2),
                                    gen.capacity.summer.1)
)]
merged[,':='(
        gen.capacity.winter=fifelse(is.na(gen.capacity.winter.1),
                                    fifelse(is.na(gen.capacity.winter.2),
                                            gen.capacity.summer.3,gen.capacity.winter.2),
                                    gen.capacity.winter.1)
)]

merged[,':='(gen.capacity.summer.1=NULL,gen.capacity.summer.2=NULL,gen.capacity.summer.3=NULL,
             gen.capacity.winter.1=NULL,gen.capacity.winter.2=NULL,gen.capacity.winter.3=NULL)]
merged[,':='(overall.type.count.needs=NULL,gen.count=NULL)]
merged.averages <- merged[,.(mean.summer=mean(gen.capacity.summer,na.rm=TRUE),
                             mean.winter=mean(gen.capacity.winter,na.rm=TRUE)),by=overall.type.needs]
merged <- merged[merged.averages,':='(
        gen.capacity.summer=fifelse(is.na(gen.capacity.summer),mean.summer,gen.capacity.summer),
        gen.capacity.winter=fifelse(is.na(gen.capacity.winter),mean.winter,gen.capacity.winter)),
        on=.(overall.type.needs=overall.type.needs)]

merged[detailed.type %in% c("Biomass","Combined Cycle","Landfill Gas","Municipal Solid Waste"),':='(min.uptime=5,min.downtime=4)
        ][detailed.type %in% c("Fossil Waste","Non-Fossil Waste","Hydro"),':='(min.uptime=10,min.downtime=8)
        ][detailed.type =="O/G Steam",':='(min.uptime=9,min.downtime=7)
        ][detailed.type =="Pumped Storage",':='(min.uptime=0,min.downtime=0)
        ][detailed.type %in% c("Nuclear","Fuel Cell"),':='(min.uptime=20,min.downtime=20)
        ][detailed.type=="Combustion Turbine",':='(min.uptime=fifelse(gen.capacity.needs>70,3,2),min.downtime=fifelse(gen.capacity.needs>70,2,1))
        ][detailed.type=="Coal Steam",':='(min.uptime=fifelse(gen.capacity.needs>=150,15,6),min.downtime=fifelse(gen.capacity.needs>70,12,6))]
merged[detailed.type %in% c("Nuclear","Fuel Cell"),startup.cost:=gen.capacity.needs*500
        ][detailed.type %in% c("Coal Steam","O/G Steam","Municipal Solid Waste","Fossil Waste","Non-Fossil Waste","Biomass"),startup.cost:=gen.capacity.needs*100
        ][detailed.type=="Combustion Turbine",startup.cost:=gen.capacity.needs*25
        ][detailed.type %in% c("Combined Cycle","Landfill Gas") | gen.type.eia %in% c("CA","CC","CS","CT"),startup.cost:=gen.capacity.needs*50
        ][detailed.type %in% c("Hydro","Pumped Storage"),startup.cost:=0
        ]
merged[detailed.type %in% c("Nuclear","Fuel Cell"),min.generation.pct:=90
        ][detailed.type %in% c("Biomass","Coal Steam","Combined Cycle",
                               "Municipal Solid Waste","Fossil Waste","Non-Fossil Waste",
                               "Landfill Gas") | (detailed.type=="Combustion Turbine" & gen.type.needs==''),min.generation.pct:=40
        ][detailed.type %in% c("Hydro","Pumped Storage"),min.generation.pct:=0
        ][(detailed.type=="Combustion Turbine" & gen.type.needs=='IC Engine') | detailed.type=="O/G Steam",min.generation.pct:=fifelse(fuel %like% "FO",0.25,0.40)]

library(stringr)
phorum.old[,oris:=as.numeric(word(`ORIS Plant Code`,1,sep=fixed("_")))]
zones <- unique(phorum.old[,.(oris,`Utility service area name`)])
zones.fips <- unique(phorum.old[,.(FIPS5,`Utility service area name`)])
zones.fips <- zones.fips[,freq:=.N,by=FIPS5][freq==1]
merged[zones[!is.na(oris)],zone1:=`Utility service area name`,on=.(plant.id=oris)]
merged[zones.fips,zone2:=`Utility service area name`,on=.(fips=FIPS5)]
merged[,zone:=fifelse(is.na(zone1),zone2,zone1)][,':='(zone1=NULL,zone2=NULL)]

merged[,no.load.cost:=0]

merged <- merged[egrid,':='(nox.rate=PLNOXRTO,
                            so2.rate=PLSO2RTA,
                            n2o.rate=PLN2ORTA,
                            co2.rate=PLCO2RTA,
                            co2eq.rate=PLC2ERTA),on=.(plant.id=ORISPL)]
merged.averages <- merged[,.(nox=mean(nox.rate,na.rm=TRUE),
                                   so2=mean(so2.rate,na.rm=TRUE),
                                   n2o=mean(n2o.rate,na.rm=TRUE),
                                   co2=mean(co2.rate,na.rm=TRUE),
                                   co2eq=mean(co2eq.rate,na.rm=TRUE)),
                                by=.(overall.type.needs)]
merged <- merged[merged.averages,':='(
        nox.rate=fifelse(is.na(nox.rate),nox,nox.rate),
        so2.rate=fifelse(is.na(so2.rate),so2,so2.rate),
        n2o.rate=fifelse(is.na(n2o.rate),n2o,n2o.rate),
        co2.rate=fifelse(is.na(co2.rate),co2,co2.rate),
        co2eq.rate=fifelse(is.na(co2eq.rate),co2eq,co2eq.rate)),
        on=.(overall.type.needs)]

merged[,':='(co.rate=as.numeric(NA),
             nh3.rate=as.numeric(NA),
             pm10.rate=as.numeric(NA),
             pm25.rate=as.numeric(NA),
             voc.rate=as.numeric(NA),
             nh3.md=as.numeric(NA),
             so2.md=as.numeric(NA),
             voc.md=as.numeric(NA),
             nox.md=as.numeric(NA),
             pm25.md=as.numeric(NA),
             pm10.md=as.numeric(NA),
             no.load.costs=as.numeric(NA)
)]

merged[damages[model=='AP3' & pollutant=='pm25'],pm25.md:=damage.acs.stacklevel*pm25.rate,on=.(fips=fips)]
merged[damages[model=='AP3' & pollutant=='so2'],so2.md:=damage.acs.stacklevel*so2.rate,on=.(fips=fips)]
merged[damages[model=='AP3' & pollutant=='voc'],voc.md:=damage.acs.stacklevel*voc.rate,on=.(fips=fips)]
merged[damages[model=='AP3' & pollutant=='nh3'],nh3.md:=damage.acs.stacklevel*nh3.rate,on=.(fips=fips)]
merged[damages[model=='AP3' & pollutant=='nox'],nox.md:=damage.acs.stacklevel*nox.rate,on=.(fips=fips)]

write.csv(merged,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\phorum_20210912.csv")
saveRDS(merged,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\phorum_20210912.rds")

eia860.rollup <- eia860.names[,.(capacity = sum(gen.capacity.eia,na.rm=TRUE),
                                 gens = .N),
                              by=.(plant.id,plant.name,combustion)]


merged <- eia860.rollup[needs.rollup,on=.(plant.id=plant.id,combustion=combustion)][,gen.diff:=ifelse(is.na(gens),0,gens)-i.gens][
     ,capacity.diff:=ifelse(is.na(capacity),0,capacity)-i.capacity
]











eia.one <- eia860.names[gen.count==1]
needs.one <- needs.names[gen.count==1]

needs.rollup.temp <- needs[,.N,by=.(`ORIS Plant Code`,`Plant Name`,`ORIS Plant Code`, PlantType)]


merged <- eia.one[needs.one,on=.(plant.id=plant.id)][length(overall.type.eia)>0][,
                                                                                 ':='(summer.ratio=gen.capacity.summer/gen.capacity.needs,
                                                                                      winter.ratio=gen.capacity.winter/gen.capacity.needs,
                                                                                      total.ratio=gen.capacity.eia/gen.capacity.needs)]

temp.1 <- needs.names[,sum(capacity,na.rm=TRUE), by=fuel]
temp.2 <- eia860.names[,sum(capacity,na.rm=TRUE), by=fuel]