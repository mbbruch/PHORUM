appendVarOMs <- function(phorum_new,phorum_old){
     
     VarOMs <- unique(phorum.old[,.(detailed.type,
                                    unit.type,
                                    gen.type.needs,
                                    fuel,
                                    VarOM)])
     VarOMs.single <- unique(VarOMs[,unique.vals:=uniqueN(VarOM),by=detailed.type][unique.vals==1][,.(detailed.type,VarOM)])
     VarOMs.single <- rbindlist(list(VarOMs.single,
                                     data.table(detailed.type="Non-Fossil Waste",VarOM=VarOMs.single[detailed.type=="Biomass",VarOM]),
                                     data.table(detailed.type="Tires",VarOM=VarOMs.single[detailed.type=="Biomass",VarOM])))
     phorum_new[VarOMs,VarOM.1:=VarOM, on=.(unit.type,
                                       gen.type.needs,
                                       fuel)]
     phorum_new[VarOMs.single,VarOM.2:=VarOM, on=.(detailed.type)]
     phorum_new[,VarOM:=ifelse(is.na(VarOM.1),VarOM.2,VarOM.1)][,':='(VarOM.1=NULL,VarOM.2=NULL)]
     return(phorum_new);
}