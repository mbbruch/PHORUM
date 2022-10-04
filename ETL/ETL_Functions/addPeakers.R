addPeakers <- function(phorum_new, peakerMWhPerTCR){
     heatRate <- phorum_new[!is.na(TCR) & fuel=="DFO" & detailed.type=="Combustion Turbine" & gen.capacity.needs>=5,
                            max(HeatRate,na.rm=TRUE)]
     max.plant.id <- phorum_new[,max(plant.id,na.rm=TRUE)]
     temp <- phorum_new[!is.na(TCR) & fuel=="DFO" & detailed.type=="Combustion Turbine" & gen.capacity.needs>=5, 
                        .SD[sample(x = .N, size = 10,replace=FALSE)], 
                        by = TCR]
     temp[,':='(
          plant.id=max.plant.id+.I,
          plant.name=paste0(plant.name, " Addon"),
          gen.id=paste0(gen.id,"_Addon"),
          PlantCode=paste0(PlantCode,"_Addon"),
          gen.type.needs="",
          gen.capacity.needs=peakerMWhPerTCR/10,gen.capacity.eia=peakerMWhPerTCR/10,
          CapacitySummer=peakerMWhPerTCR/10,CapacityWinter=peakerMWhPerTCR/10,
          summer.factor=1,winter.factor=1
     )]
     phorum_new <- rbindlist(list(phorum_new,temp),use.names=TRUE)
     #mincapacity=0.25
     #ramprate=capacity/3
     return(phorum_new)
}