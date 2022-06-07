derateHydro <- function(phorum_new, modelingYear){
     hydro_only <- phorum_new[fuel == "Hydro"]
     no_hydro <- phorum_new[fuel != "Hydro"]
     
     #derate hydro based on its capacity factor
     if(modelingYear==2019){
             egrid <- fread(paste0(gen_dir,"egrid2019_data.csv"))
     } else if(modelingYear %in% c(2025, 2035)){
             egrid <- fread(paste0(gen_dir,"egrid2020_data.csv"))
     }
     
     hydro_only[egrid,':='(CAPFAC=CAPFAC),on=.(plant.id=ORISPL)]
     avg.capfac = hydro_only[,weighted.mean(CAPFAC,gen.capacity.eia,na.rm=TRUE)]
     hydro_only <- hydro_only[CAPFAC!=0]
     hydro_only[is.na(CAPFAC),CAPFAC:=avg.capfac]
     hydro_only[,':='(gen.capacity.needs=gen.capacity.needs*CAPFAC,
                      gen.capacity.eia=gen.capacity.eia*CAPFAC,
                      CapacitySummer=CapacitySummer*CAPFAC,
                      CapacityWinter=CapacityWinter*CAPFAC)][,CAPFAC:=NULL]
     
     hydro_only <- hydro_only[,.(plant.id=min(plant.id,na.rm=TRUE),
                                 plant.name=paste0("Combined_Hydro_",TCR,"_",.N),
                                 gen.id="X",
                                 fips=min(fips,na.rm=TRUE),
                                 fips2=min(fips,na.rm=TRUE),
                                 PlantCode=paste0(min(plant.id),"_",unit.type,"_","X"),
                                 gen.capacity.needs=sum(gen.capacity.needs,na.rm=TRUE),
                                 gen.capacity.eia=sum(gen.capacity.eia,na.rm=TRUE),
                                 CapacitySummer=sum(CapacitySummer,na.rm=TRUE),
                                 CapacityWinter=sum(CapacityWinter,na.rm=TRUE),
                                 summer.factor=sum(CapacitySummer,na.rm=TRUE)/sum(gen.capacity.eia,na.rm=TRUE),
                                 winter.factor=sum(CapacityWinter,na.rm=TRUE)/sum(gen.capacity.eia,na.rm=TRUE),
                                 LAT=weighted.mean(LAT,gen.capacity.needs,na.rm=TRUE),
                                 LON=weighted.mean(LON,gen.capacity.needs,na.rm=TRUE)),
                              by=.(TCR,fuel,detailed.type,unit.type,gen.type.needs,
                                   overall.type.needs,HeatRate,gen.type.eia)]
     
     phorum_new <- rbindlist(list(hydro_only,no_hydro),use.names=TRUE)
     return(phorum_new)
}