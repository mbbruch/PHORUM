importQueue <- function(modelingYear){
     queue <- fread(paste0(misc_dir,"PlanningQueues.csv"))
     utilityTCRs <- fread(paste0(base_dir,"crosswalks\\","Utility_TCRs.csv"))
     fipsNames <- fread(paste0(county_dir,"fips_countyname_state.csv"))
     fipsLatLon <- fread(paste0(base_dir,"crosswalks\\","2021_Gaz_counties_national.txt"))
     fipsNames[,name:=gsub(" County","",name)]
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
     queue[,County:=gsub(" County","",County)]
     queue[County %like% "City of ",County:=paste0(County," city")]
     queue[,County:=gsub("City of ","",County)]
     queue[,County:=gsub("Du Page","DuPage",County)]
     queue[,County:=gsub("Northhampton","Northampton",County)]
     queue[,County:=gsub("Lasalle","LaSalle",County)]
     queue[,County:=gsub("Tippecance","Tippecanoe",County)]
     queue[,County:=gsub("DeWitt","De Witt",County)]
     queue[,County:=gsub("Mc Kean","McKean",County)]
     queue[,County:=gsub("Raliegh","Raleigh",County)]
     queue[,County:=gsub("Tyrell","Tyrrell",County)]
     queue[fipsNames,fips:=fips,on=.(County=name,State=state)]
     queue[fipsLatLon,':='(LAT=INTPTLAT,LON=INTPTLONG),on=.(fips=GEOID)]
     return(queue)
}
