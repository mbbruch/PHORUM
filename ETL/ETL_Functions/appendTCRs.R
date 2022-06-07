appendTCRs <- function(phorum_new,phorum_old){
     orispl_latlong <- fread(paste0(gen_dir,"egrid2019_data.csv"))[!is.na(LAT),.(ORISPL,LAT,LON)]
     phorum_new <- phorum_new[orispl_latlong,':='(LAT=LAT,LON=LON),on=.(plant.id=ORISPL)]
     
     TCR.plants <- unique(phorum_old[,.(oris=as.numeric(word(`ORIS Plant Code`,1,sep=fixed("_"))),`Utility TCR`)])
     TCR.latlongs <- unique(phorum_old[!is.na(`Plant latitude`),
                                       .(`Plant latitude`,`Plant longitude`,`Utility TCR`)])
     phorum_new[TCR.plants[!is.na(oris)],TCR.plant:=`Utility TCR`,on=.(plant.id=oris)]
     index <- get.knnx(TCR.latlongs[,.(`Plant longitude`,`Plant latitude`)],phorum_new[!is.na(LON) & is.na(TCR.plant),.(LON,LAT)],1)$nn.index #TODO use lat-longs!
     phorum_new[,TCR.latlong:=as.numeric(NA)]
     phorum_new[!is.na(LON) & is.na(TCR.plant),TCR.latlong:=TCR.latlongs[index,]$`Utility TCR`]
     phorum_new[,TCR:=fifelse(is.na(TCR.plant),TCR.latlong,TCR.plant)][,':='(TCR.plant=NULL,TCR.latlong=NULL)]
     
     return(phorum_new)
}

appendWindSolarTCRs <- function(needs_2019,phorum_old){
        TCR.plants <- unique(phorum_old[,.(oris=as.numeric(word(`ORIS Plant Code`,1,sep=fixed("_"))),`Utility TCR`)])
        TCR.latlongs <- unique(phorum_old[!is.na(`Plant latitude`),
                                          .(`Plant latitude`,`Plant longitude`,`Utility TCR`)])
        index <- get.knnx(TCR.latlongs[,.(`Plant longitude`,`Plant latitude`)],needs_2019[!is.na(LON),.(LON,LAT)],1)$nn.index #TODO use lat-longs!
        needs_2019[,TCR.latlong:=as.numeric(NA)]
        needs_2019[!is.na(LON),TCR:=TCR.latlongs[index,]$`Utility TCR`]
        return(needs_2019)
}

