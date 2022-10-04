retireCoal <- function(phorum_new, queue, retirePct, gasPct){
     if(retirePct == 0) return(phorum_new)
     coal_only <- phorum_new[fuel %like% "BIT" | fuel %like% "SUB" | fuel %like% "Coal"]     
     no_coal <- phorum_new[!plant.id %in% coal_only$plant.id]
     cc_only <- no_coal[fuel %like% "NG" & detailed.type %like% "Combined"]
     mwToRetire <- retirePct*coal_only[,sum(gen.capacity.needs,na.rm=TRUE)]
     mwToAdd <- mwToRetire*(gasPct/retirePct)
     coal_only <- coal_only[order(online.year,decreasing=FALSE)]
     coal_only[,runningMW:=cumsum(gen.capacity.needs)]
     coal_only <- coal_only[runningMW > min(ifelse(runningMW>mwToRetire,runningMW,Inf))]
     coal_only[,runningMW:=NULL]
     queue_ng <- queue[is.na(actual.service.year) & Fuel=="Natural Gas" & MFO >= 100,.(fips,TCR,MFO,LAT,LON)]
     set.seed(5)
     queue_ng <- queue_ng[sample(1:.N),]
     queue_ng[,runningMW:=cumsum(MFO)]
     queue_ng <- queue_ng[runningMW <= min(ifelse(runningMW>mwToAdd,runningMW,Inf))]
     queue_ng[,runningMW:=NULL]
     newHeatrate <- cc_only[online.year>=2020,mean(HeatRate,na.rm=TRUE)]
     max_plant_id <- max(c(max(coal_only$plant.id),max(no_coal$plant.id)))
     queue_ng[,plant.id:=max_plant_id+.I]
     
     new_cc <- data.table(
          TCR=queue_ng$TCR,
          fuel=cc_only[,max(fuel,na.rm=TRUE)],
          detailed.type=cc_only[,max(detailed.type,na.rm=TRUE)],
          unit.type=cc_only[,max(unit.type,na.rm=TRUE)],
          gen.type.needs=cc_only[,max(gen.type.needs,na.rm=TRUE)],
          overall.type.needs=cc_only[,max(overall.type.needs,na.rm=TRUE)],
          HeatRate=newHeatrate,
          gen.type.eia=cc_only[,max(gen.type.eia,na.rm=TRUE)],
          plant.id=queue_ng$plant.id,
          plant.name="",
          gen.id="X",
          online.year=2022,
          fips=queue_ng$fips,
          fips2=(queue_ng$fips-queue_ng$fips%%1000)/1000,
          PlantCode=paste0(queue_ng$plant.id,"_X"),
          gen.capacity.needs=queue_ng$MFO,
          gen.capacity.eia=as.numeric(NA),
          CapacitySummer=as.numeric(NA),
          CapacityWinter=as.numeric(NA),
          summer.factor=as.numeric(NA),
          winter.factor=as.numeric(NA),
          LAT=queue_ng$LAT,
          LON=queue_ng$LON
     )
     phorum_new <- rbindlist(list(coal_only,no_coal,new_cc),use.names=TRUE)
     return(phorum_new)
}