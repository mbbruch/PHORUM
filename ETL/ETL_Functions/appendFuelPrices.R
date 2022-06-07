appendFuelPrices <- function(phorum_new, modelingYear){    
     phorum_new[,fuel.type:=word(phorum_new$overall.type.needs,4,4,sep="_")]
     phorum_new[fuel.type %like% "NG",fuel_group:="Natural Gas"]
     phorum_new[fuel.type %like% "Petroleum Coke",fuel_group:="Petroleum Coke"]
     phorum_new[fuel.type %like% "BIT" | fuel.type %like% "SUB" | fuel.type %like% "Coal",fuel_group:="Coal"]
     phorum_new[fuel.type %like% "RFO" | fuel.type %like% "DFO",fuel_group:="Petroleum"]
     phorum_new[fuel.type %like% "MSW" | fuel.type %like% "Biomass" | fuel.type %like% "Landfill" | 
                 fuel.type %like% "Fossil Waste" | fuel.type %like% "NUC" | 
                 fuel.type %like% "Hydro", fuel_group:="Free"]
     
     month_numbers <- fread(paste0(base_dir,"crosswalks\\Month_Name_To_Number.csv"))
     state_latlong <- fread(paste0(gen_dir,"egrid2019_data.csv"))[,.(FIPSST,LAT,LON)]
     state_latlong <- state_latlong[,.(lat=mean(LAT,na.rm=TRUE),long=mean(LON,na.rm=TRUE)),by=FIPSST]
     state_info <- fread(paste0(base_dir,"crosswalks\\state_info.csv"))
     state_info[state_latlong,':='(lat=lat,long=long),on=.(fips2=FIPSST)]
     eia923 <- fread(paste0(fuel_dir,"f923_2019\\EIA923_Page5_2019.csv"))
     coal.region <- fread(paste0(fuel_dir,"CensusRegion_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_coal.csv"))
     natgas.region <- fread(paste0(fuel_dir,"CensusRegion_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_natural_gas.csv"))
     petcoke.region <- fread(paste0(fuel_dir,"CensusRegion_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_petroleum_coke.csv"))
     petliq.region <- fread(paste0(fuel_dir,"CensusRegion_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_petroleum_liquids.csv"))
     coal.state <- fread(paste0(fuel_dir,"State_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_coal.csv"))
     natgas.state <- fread(paste0(fuel_dir,"State_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_natural_gas.csv"))
     petcoke.state <- fread(paste0(fuel_dir,"State_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_petroleum_coke.csv"))
     petliq.state <- fread(paste0(fuel_dir,"State_cost_of_fossil_fuels_for_electricity_generation_(per_Btu)_for_petroleum_liquids.csv"))
     
     coal.state[,state.abbr:=word(`source key`,2,2,sep="-")][,`source key`:=NULL]
     natgas.state[,state.abbr:=word(`source key`,2,2,sep="-")][,`source key`:=NULL]
     petcoke.state[,state.abbr:=word(`source key`,2,2,sep="-")][,`source key`:=NULL]
     petliq.state[,state.abbr:=word(`source key`,2,2,sep="-")][,`source key`:=NULL]
     coal.region[,`source key`:=NULL]
     natgas.region[,`source key`:=NULL]
     petcoke.region[,`source key`:=NULL]
     petliq.region[,`source key`:=NULL]
     
     fixup_table <- function(table){
          months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
          months <- paste0(months,"-19")
          table = data.table::melt(table, measure.vars = patterns("-19",cols=names(table)), value.name = c("price"))
          table[,month:=substr(variable,1,3)][,variable:=NULL]
          table[month_numbers,month_number:=V2,on=.(month=V1)]
          table[,price:=parse_number(price, na = c("","W"," ","0","0.0","--","."))][price==0,price:=as.numeric(NA)]
          return(table)
     }
     
     coal.region <- fixup_table(coal.region)
     natgas.region <- fixup_table(natgas.region)
     petcoke.region <- fixup_table(petcoke.region)
     petliq.region <- fixup_table(petliq.region)
     coal.state <- fixup_table(coal.state)
     natgas.state <- fixup_table(natgas.state)
     petcoke.state <- fixup_table(petcoke.state)
     petliq.state <- fixup_table(petliq.state)
     coal.state[state_info,census_region:=census_region,on=.(state=state_abbr)]
     natgas.state[state_info,census_region:=census_region,on=.(state=state_abbr)]
     petcoke.state[state_info,census_region:=census_region,on=.(state=state_abbr)]
     petliq.state[state_info,census_region:=census_region,on=.(state=state_abbr)]
     coal.state[coal.region,price.region:=i.price,on=.(month_number,census_region)][state=='WV',price.region:=as.numeric(NA)]
     natgas.state[natgas.region,price.region:=i.price,on=.(month_number,census_region)][state=='WV',price.region:=as.numeric(NA)]
     petcoke.state[petcoke.region,price.region:=i.price,on=.(month_number,census_region)][state=='WV',price.region:=as.numeric(NA)]
     petliq.state[petliq.region,price.region:=i.price,on=.(month_number,census_region)][state=='WV',price.region:=as.numeric(NA)]
     fuel.prices.geo <- rbindlist(list(
          coal.state[,':='(fuel_group="Coal")],
          natgas.state[,':='(fuel_group="Natural Gas")],
          petcoke.state[,':='(fuel_group="Petroleum Coke")],
          petliq.state[,':='(fuel_group="Petroleum")]
     ))
     rm(coal.region); rm(natgas.region); rm(petcoke.region); rm(petliq.region);
     rm(coal.state); rm(natgas.state); rm(petcoke.state); rm(petliq.state);
     fuel.prices.geo[state_info,':='(fips2=fips2,lat=lat,long=long),on=.(state.abbr=state_abbr)]
     eia923[,':='(units=parse_number(QUANTITY),
                  mmbtu.per.unit=parse_number(`Average Heat Content`),
                  cost=parse_number(FUEL_COST))]
     eia923 <- eia923[!is.na(cost) & !is.na(units) & !is.na(mmbtu.per.unit)]
     eia923[,':='(mmbtu=units*mmbtu.per.unit,usd.per.mmbtu=cost/100)]
     plant.rollup <- eia923[,.(usd.per.mmbtu=weighted.mean(usd.per.mmbtu,w=mmbtu)),by=.(YEAR,MONTH,`Plant Id`,`Plant State`,FUEL_GROUP)]
     
     plant.month.combos <- data.table(crossing(unique(phorum_new[,.(plant.id,fips2,LAT,LON,fuel_group)]),month=c(1:12)))
     plant.month.combos[state_info,':='(LAT=fifelse(is.na(LAT),lat,LAT),LON=fifelse(is.na(LON),long,LON)),on=.(fips2=fips2)]
     plant.month.combos[plant.rollup,cost.plant:=usd.per.mmbtu,on=.(plant.id=`Plant Id`,month=MONTH)]
     plant.month.combos[fuel.prices.geo,':='(cost.state=price,cost.region=price.region),on=.(fips2=fips2,month=month_number,fuel_group=fuel_group)]
     plant.month.combos[fuel_group=="Free",':='(cost.plant=0,cost.state=0,cost.region=0)]
     plant.month.combos[,cost.nn:=as.numeric(NA)]
     fuel.prices.geo.withstatevalues = fuel.prices.geo[!is.na(price)]
     missing.rows <- plant.month.combos[is.na(cost.plant) & is.na(cost.state) & is.na(cost.region),which=TRUE]
     for(i in missing.rows){
          train.set <- fuel.prices.geo.withstatevalues[fuel_group==plant.month.combos[i,fuel_group] & 
                                                            month_number == plant.month.combos[i,month]]
          index <- get.knnx(train.set[,.(long,lat)],plant.month.combos[i,.(LON,LAT)],1)$nn.index
          plant.month.combos[i,cost.nn:=fuel.prices.geo.withstatevalues[index,]$price]
     }
     plant.month.combos[,cost.final:=(ifelse(!is.na(cost.plant),cost.plant,
                                              ifelse(!is.na(cost.state),cost.state,
                                                     ifelse(!is.na(cost.region),cost.region,cost.nn))))]
     plant.month.combos <- plant.month.combos[,.(plant.id,fuel_group,month,FuelCost=cost.final)]
     
     if(modelingYear %in% c(2025,2035)){
             future_fuelprices <- fread(paste0(misc_dir,"future_fuelprices.csv"))
             multiplier.gas <- future_fuelprices[year==2019,.(gas_usd_mmbtu/coal_usd_mmbtu)]/future_fuelprices[year==modelingYear,.(gas_usd_mmbtu/coal_usd_mmbtu)]
             multiplier.oil <- future_fuelprices[year==2019,.(avg_oil_usd_mmbtu/coal_usd_mmbtu)]/future_fuelprices[year==modelingYear,.(avg_oil_usd_mmbtu/coal_usd_mmbtu)]
             plant.month.combos[fuel_group=="Natural Gas",FuelCost:=FuelCost*multiplier.gas]
             plant.month.combos[fuel_group=="Petroleum",FuelCost:=FuelCost*multiplier.oil]
     }
     
     fuel_costs <- data.table::dcast(plant.month.combos, 
          plant.id + fuel_group ~ paste0("FuelPrice_", plant.month.combos[, str_pad(seq_len(.N),2,"0",side="left"), by=.(plant.id,fuel_group)]$V1),
          value.var="FuelCost")

     phorum_new <- phorum_new[fuel_costs,on=.(plant.id,fuel_group)]
     return(phorum_new);
}