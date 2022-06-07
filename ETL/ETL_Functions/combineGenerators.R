combineGenerators <- function(phorum_new, modelingYear){
     size.threshold <- 0
     phorum_new[,small.gens:=sum(fifelse(gen.capacity.needs<size.threshold,1,0)),by=.(plant.id,plant.name,fuel,detailed.type,unit.type,gen.type.needs,
                                                                   overall.type.needs,fips,fips2,HeatRate,gen.type.eia)]
     small_gens <- phorum_new[small.gens>1][,small.gens:=NULL]
     no_small_gens <- phorum_new[small.gens<=1][,small.gens:=NULL]
     
     small_gens <- small_gens[,.(gen.id="X",
                                 PlantCode=paste0(plant.id,"_",unit.type,"_","X"),
                                 gen.capacity.needs=sum(gen.capacity.needs,na.rm=TRUE),
                                 gen.capacity.eia=sum(gen.capacity.eia,na.rm=TRUE),
                                 CapacitySummer=sum(CapacitySummer,na.rm=TRUE),
                                 CapacityWinter=sum(CapacityWinter,na.rm=TRUE),
                                 summer.factor=sum(CapacitySummer,na.rm=TRUE)/sum(gen.capacity.eia,na.rm=TRUE),
                                 winter.factor=sum(CapacityWinter,na.rm=TRUE)/sum(gen.capacity.eia,na.rm=TRUE)),
                              by=.(plant.id,plant.name,fuel,detailed.type,unit.type,gen.type.needs,
                                   overall.type.needs,fips,fips2,HeatRate,gen.type.eia,TCR,LAT,LON)]
     
     phorum_new <- rbindlist(list(no_small_gens,small_gens),use.names=TRUE)
     return(phorum_new)
}