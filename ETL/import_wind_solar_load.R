library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)
wind <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\wind_gen.csv")
solar <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\solar_gen.csv")
load <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\hrl_load_metered.csv")
wind[area=="WEST",TCR:=1][
     area=="MIDATL",TCR:=3][
     area=="SOUTH",TCR:=5][
     area=="OTHER",TCR:=0]
solar[area=="WEST",TCR:=1][
     area=="MIDATL",TCR:=3][
     area=="SOUTH",TCR:=5][
     area=="OTHER",TCR:=0]
intermittents <- rbindlist(list(wind[!is.na(TCR),.(type="Wind",datetime=datetime_beginning_ept,TCR,MW=wind_generation_mw)],
                                solar[!is.na(TCR),.(type="Solar",datetime=datetime_beginning_ept,TCR,MW=solar_generation_mw)]))
intermittents = dcast(intermittents, datetime ~ type + TCR, value.var = c("MW"), fun.aggregate=sum)
intermittents <- intermittents[,datetime:=lubridate::parse_date_time(datetime, 
                                                                     '%m/%d/%Y %I:%M:%S %p')]
intermittents[,':='(date=date(datetime),
                   hour.ending=hour(datetime)+1)][,datetime:=NULL]
intermittents <- select(intermittents, date, hour.ending, everything())
intermittents <- rbindlist(list(intermittents,
                                data.table(date=as.Date('2019-03-10'),
                                           hour.ending=3)),
                           fill=TRUE)
intermittents[is.na(intermittents)] <- 0
intermittents <- intermittents[order(date,hour.ending)][,hour:=.I]
intermittents <- select(intermittents, date, hour.ending, hour, everything())
cols.to.fixup <- colnames(intermittents)[grep("^[A-Za-z]{4,5}_[0-9]{1}",colnames(intermittents))]
for(i in cols.to.fixup){
        intermittents[date=='2019-03-10' & hour.ending==3,c(i)] <- 
                (intermittents[date=='2019-03-10' & hour.ending==2,..i] +
                 intermittents[date=='2019-03-10' & hour.ending==4,..i])/2
}
saveRDS(intermittents,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\wind_solar_by_tcr.rds")
write.csv(intermittents,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\wind_solar_by_tcr.csv")

load = dcast(load, datetime_beginning_ept ~ load_area, value.var = c("mw"), fun.aggregate=sum)
load <- load[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                                     '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
load[,':='(date=date(datetime),
                    hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
load <- select(load, date, hour.ending, everything())
load <- rbindlist(list(load,
                                data.table(date=as.Date('2019-03-10'),
                                           hour.ending=3)),
                           fill=TRUE)
load <- load[order(date,hour.ending)][year(date)==2019][,hour:=.I]
load <- select(load, date, hour.ending, hour, everything())
load$RTO <- NULL
cols.to.fixup <- colnames(load)[grep("^[A-Z]{2,}",colnames(load))]
for(i in cols.to.fixup){
        load[date=='2019-03-10' & hour.ending==3,c(i)] <- 
                (load[date=='2019-03-10' & hour.ending==2,..i] +
                         load[date=='2019-03-10' & hour.ending==4,..i])/2
}
saveRDS(load,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\load_by_utility.rds")
write.csv(load,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\load_by_utility.csv")