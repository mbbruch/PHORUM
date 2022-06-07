importQueue <- function(modelingYear){
     queue <- fread(paste0(misc_dir,"PlanningQueues.csv"))
     utilityTCRs <- fread(paste0(base_dir,"crosswalks\\","Utility_TCRs.csv"))
     queue[,actual.service.year:=as.numeric(substr(`Actual In Service Date`,
                                                   str_length(`Actual In Service Date`)-3,str_length(`Actual In Service Date`)))]
     queue[,planned.year:=as.numeric(substr(`Projected In Service Date`,
                                            str_length(`Projected In Service Date`)-3,str_length(`Projected In Service Date`)))]
     queue[,revised.year:=as.numeric(substr(`Revised In Service Date`,
                                            str_length(`Revised In Service Date`)-3,str_length(`Revised In Service Date`)))]
     queue[,planned.year:=fifelse(!is.na(actual.service.year),actual.service.year,
                                  fifelse(!is.na(revised.year),revised.year,planned.year))]
     queue[,`Transmission Owner`:=toupper(`Transmission Owner`)]
     queue <- queue[(is.na(actual.service.year) | actual.service.year > 2019) & 
                            !(`Transmission Owner`=='') &
                            !(`Status` %in% c('Withdrawn','Retracted','Suspended','Annulled','Deactivated')) & 
                            !(`Fuel` %in% c('','Other')) & 
                            planned.year > 2019 & 
                            planned.year <= modelingYear & 
                            MFO > 0.0 & 
                            !is.na(MFO)]
     queue[utilityTCRs,TCR:=TCR,on=.(`Transmission Owner`=Code)]
     return(queue)
}
