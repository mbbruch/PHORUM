library(data.table)

eia923 <- fread("C:\\Users\\Matthew\\Desktop\\PJM\\updated_data\\eia923_2019_costs.csv")
eia923[,':='(
     heat.content=as.numeric(gsub("\\,", "", `Average Heat Content`)),
     quantity=as.numeric(gsub("\\,", "", QUANTITY)),
     fuel.cost=as.numeric(gsub("\\,", "", FUEL_COST)))
][,heat.content.purchased:=heat.content*quantity]

eia923.month.state.rollup <- eia923[,.(state.month.fuel.avg=sum(heat.content.purchased*fuel.cost,na.rm=TRUE)/sum(heat.content.purchased,na.rm=TRUE)),
                                    by=.(`Plant State`,MONTH,FUEL_GROUP)]
eia923.month.rollup <- eia923[,.(month.fuel.avg=sum(heat.content.purchased*fuel.cost,na.rm=TRUE)/sum(heat.content.purchased,na.rm=TRUE)),
                              by=.(MONTH,FUEL_GROUP)]

eia860[,energy.source.to.join:='NA']
eia860[,':='(fuelprice.jan=0,
             fuelprice.feb=0,
             fuelprice.mar=0,
             fuelprice.apr=0,
             fuelprice.may=0,
             fuelprice.jun=0,
             fuelprice.jul=0,
             fuelprice.aug=0,
             fuelprice.sep=0,
             fuelprice.oct=0,
             fuelprice.nov=0,
             fuelprice.dec=0)]
eia860[,nonzero.price:=FALSE]
eia860[`Energy Source 1` %in% c('BIT','LIG','SGC','SUB','RC','WC'),':='(energy.source.to.join='Coal',nonzero.price=TRUE)]
eia860[`Energy Source 1` %in% c('NG'),':='(energy.source.to.join='Natural Gas',nonzero.price=TRUE)]
eia860[`Energy Source 1` %in% c('DFO','RFO'),':='(energy.source.to.join='Petroleum',nonzero.price=TRUE)]
eia860[`Energy Source 1` %in% c('PC'),':='(energy.source.to.join='Petroleum Coke',nonzero.price=TRUE)]
#Merge on monthly average fuel prices by fuel group and state
eia860[eia923.month.state.rollup[MONTH==1],fuelprice.jan:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==2],fuelprice.feb:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==3],fuelprice.mar:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==4],fuelprice.apr:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==5],fuelprice.may:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==6],fuelprice.jun:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==7],fuelprice.jul:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==8],fuelprice.aug:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==9],fuelprice.sep:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==10],fuelprice.oct:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==11],fuelprice.nov:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
eia860[eia923.month.state.rollup[MONTH==12],fuelprice.dec:=state.month.fuel.avg,
       on=.(energy.source.to.join=FUEL_GROUP,State=`Plant State`)]
#IF there was no fuel data for that month for that state, instead use the national average
eia860[eia923.month.rollup[MONTH==1],fuelprice.jan:=fifelse(nonzero.price & fuelprice.jan==0,month.fuel.avg,fuelprice.jan),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==2],fuelprice.feb:=fifelse(nonzero.price & fuelprice.feb==0,month.fuel.avg,fuelprice.feb),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==3],fuelprice.mar:=fifelse(nonzero.price & fuelprice.mar==0,month.fuel.avg,fuelprice.mar),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==4],fuelprice.apr:=fifelse(nonzero.price & fuelprice.apr==0,month.fuel.avg,fuelprice.apr),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==5],fuelprice.may:=fifelse(nonzero.price & fuelprice.may==0,month.fuel.avg,fuelprice.may),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==6],fuelprice.jun:=fifelse(nonzero.price & fuelprice.jun==0,month.fuel.avg,fuelprice.jun),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==7],fuelprice.jul:=fifelse(nonzero.price & fuelprice.jul==0,month.fuel.avg,fuelprice.jul),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==8],fuelprice.aug:=fifelse(nonzero.price & fuelprice.aug==0,month.fuel.avg,fuelprice.aug),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==9],fuelprice.sep:=fifelse(nonzero.price & fuelprice.sep==0,month.fuel.avg,fuelprice.sep),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==10],fuelprice.oct:=fifelse(nonzero.price & fuelprice.oct==0,month.fuel.avg,fuelprice.oct),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==11],fuelprice.nov:=fifelse(nonzero.price & fuelprice.nov==0,month.fuel.avg,fuelprice.nov),
       on=.(energy.source.to.join=FUEL_GROUP)]
eia860[eia923.month.rollup[MONTH==12],fuelprice.dec:=fifelse(nonzero.price & fuelprice.dec==0,month.fuel.avg,fuelprice.dec),
       on=.(energy.source.to.join=FUEL_GROUP)]
