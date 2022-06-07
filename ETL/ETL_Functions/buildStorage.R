buildStorage <- function(phorum_new,modelingYear)
{
     storage <- phorum_new[(fuel %in% c("Pumped Storage","Energy Storage") & gen.type.eia %in% c("BA","PS"))]
     storage <- storage[,.(CapacitySummer=sum(CapacitySummer,na.rm=TRUE),CapacityWinter=sum(CapacityWinter,na.rm=TRUE)),
                        by=.(plant.id,plant.name,gen.type.eia,fuel,TCR)]
     
     storage.eia860 <- fread(paste0(gen_dir,"eia860_2019_storage.csv"))
     storage.eia860 <- storage.eia860[,.(energy.capacity=sum(`Nameplate Energy Capacity (MWh)`,na.rm=TRUE)),by=`Plant Code`]
     storage[storage.eia860,':='(energy.capacity=energy.capacity),
             on=.(plant.id=`Plant Code`)]
     storage[plant.name=='Muddy Run',TCR:=3]
     storage <- storage[,.(PowerCapacity=sum(CapacitySummer,na.rm=TRUE),
                           EnergyCapacity=sum(energy.capacity,na.rm=TRUE)),
                        by=.(gen.type.eia,fuel,TCR)]
     storage[gen.type.eia=='PS',EnergyCapacity:=PowerCapacity*8] #8 hours' worth of storage assumed
     
     if(modelingYear %in% c(2025,2035,2037)){
             future_batteries <- fread(paste0(misc_dir,"future_storage.csv"))
             future_battery_mw <- future_batteries[year==modelingYear,sum(storage_mw,na.rm=TRUE)]
             today_battery_mw <- storage[gen.type.eia=='BA',sum(PowerCapacity,na.rm=TRUE)]
             multiplier <- future_battery_mw/today_battery_mw;
             storage[gen.type.eia=='BA',':='(PowerCapacity=PowerCapacity*multiplier,EnergyCapacity=EnergyCapacity*multiplier)]
     }
     
     
     storage[,PowerCapacity:=pmin(PowerCapacity,EnergyCapacity)] #1-hour resolution
     
     # roundtrip efficiency: 82% for batteries, 79% for pumped storage
     # from https://www.eia.gov/todayinenergy/detail.php?id=46756
     storage[gen.type.eia=='PS',ChargeEfficiency:=sqrt(0.79)][
          gen.type.eia=='BA',ChargeEfficiency:=sqrt(0.82)][,
                                                           DischargeEfficiency:=ChargeEfficiency][,
                                                                                                  VariableCost:=0]
     storage <- storage[!is.na(TCR)]
     write.csv(storage,paste0(out_dir,"storage_",modelingYear,".csv"))}