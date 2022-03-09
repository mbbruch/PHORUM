library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)

txn <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\transfer_limits_and_flows.csv")

txn <- txn[!(transfer_limit_area %ilike% "Pre" | transfer_limit_area %ilike% "500")]
txn[transfer_limit_area %in% c("Bedington-BlackOak Post-Contingency",
               "AEP/DOM Post-Contingency",
               "AP-South Post-Contingency"),transfer_limit_area:="AP-SOUTH BBLACKOAK AEP-DOM"]
txn = dcast(txn, datetime_beginning_ept ~ transfer_limit_area, value.var = c("transfer_limit"), fun.aggregate=sum)
txn <- txn[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                   '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
txn[,':='(date=date(datetime),
           hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
txn <- select(txn, date, hour.ending, everything())
txn <- rbindlist(list(txn,
                       data.table(date=as.Date('2019-03-10'),
                                  hour.ending=3)),
                  fill=TRUE)
txn <- txn[order(date,hour.ending)][year(date)==2019][,hour:=.I]
txn <- select(txn, date, hour.ending, hour, everything())
cols.to.fixup <- colnames(txn)[grep("[A-Z]{1,}",colnames(txn))]
for(i in cols.to.fixup){
     txn[date=='2019-03-10' & hour.ending==3,c(i)] <- 
          (txn[date=='2019-03-10' & hour.ending==2,..i] +
                txn[date=='2019-03-10' & hour.ending==4,..i])/2
}
saveRDS(txn,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\txn_constraints.rds")
write.csv(txn,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\txn_constraints.csv")


imports <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\act_sch_interchange.csv")
imports = dcast(imports, datetime_beginning_ept ~ tie_line, value.var = c("actual_flow"), fun.aggregate=sum)
imports <- imports[,datetime:=lubridate::parse_date_time(datetime_beginning_ept, 
                                                 '%m/%d/%Y %I:%M:%S %p')][order(datetime)]
imports[,':='(date=date(datetime),
          hour.ending=hour(datetime)+1)][,datetime:=NULL][,datetime_beginning_ept:=NULL]
imports <- select(imports, date, hour.ending, everything())
imports <- rbindlist(list(imports,
                      data.table(date=as.Date('2019-03-10'),
                                 hour.ending=3)),
                 fill=TRUE)
imports <- imports[order(date,hour.ending)][year(date)==2019][,hour:=.I]
imports <- select(imports, date, hour.ending, hour, everything())
cols.to.fixup <- colnames(imports)[grep("[A-Z]{1,}",colnames(imports))]
for(i in cols.to.fixup){
     imports[date=='2019-03-10' & hour.ending==3,c(i)] <- 
          (imports[date=='2019-03-10' & hour.ending==2,..i] +
                imports[date=='2019-03-10' & hour.ending==4,..i])/2
}

saveRDS(imports,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\imports.rds")
write.csv(imports,"C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\imports.csv")
