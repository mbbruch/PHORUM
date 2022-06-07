appendCapacities <- function(phorum_new, eia860){

     eia860.plant.averages <- eia860[,.(summer.factor=weighted.mean(summer.factor,gen.capacity.eia,na.rm=TRUE),
                                        winter.factor=weighted.mean(winter.factor,gen.capacity.eia,na.rm=TRUE)),
                                     by=plant.id]
     phorum_new[eia860.plant.averages,':='(gen.capacity.summer.1=summer.factor*gen.capacity.needs,
                                 gen.capacity.winter.1=winter.factor*gen.capacity.needs),
            on=.(plant.id=plant.id)]
     phorum_new[,':='(
          CapacitySummer=fifelse(!is.na(CapacitySummer),CapacitySummer,gen.capacity.summer.1),
          CapacityWinter=fifelse(!is.na(CapacityWinter),CapacityWinter,gen.capacity.winter.1)
     )]
     phorum_new.averages <- phorum_new[,.(mean.summer=weighted.mean(summer.factor,gen.capacity.needs,na.rm=TRUE),
                                          mean.winter=weighted.mean(winter.factor,gen.capacity.needs,na.rm=TRUE)),by=overall.type.needs]
     phorum_new <- phorum_new[phorum_new.averages,':='(
             CapacitySummer=fifelse(!is.na(CapacitySummer),CapacitySummer,mean.summer*gen.capacity.needs),
             CapacityWinter=fifelse(!is.na(CapacityWinter),CapacityWinter,mean.winter*gen.capacity.needs)),
             on=.(overall.type.needs=overall.type.needs)]
     
     phorum_new.averages <- phorum_new[,.(mean.summer=weighted.mean(summer.factor,gen.capacity.needs,na.rm=TRUE),
                                          mean.winter=weighted.mean(winter.factor,gen.capacity.needs,na.rm=TRUE)),by=detailed.type]
     phorum_new <- phorum_new[phorum_new.averages,':='(
             CapacitySummer=fifelse(!is.na(CapacitySummer),CapacitySummer,mean.summer*gen.capacity.needs),
             CapacityWinter=fifelse(!is.na(CapacityWinter),CapacityWinter,mean.winter*gen.capacity.needs)),
             on=.(detailed.type=detailed.type)]
     phorum_new[is.na(CapacitySummer),CapacitySummer:=fifelse(!is.na(gen.capacity.eia),gen.capacity.eia,gen.capacity.needs)]
     phorum_new[is.na(CapacityWinter),CapacityWinter:=fifelse(!is.na(gen.capacity.eia),gen.capacity.eia,gen.capacity.needs)]
     phorum_new[,':='(gen.capacity.summer.1=NULL,gen.capacity.winter.1=NULL)]
     return(phorum_new);
}