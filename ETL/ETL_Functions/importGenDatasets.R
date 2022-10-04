importOldPHORUM <- function(){
     phorum_old <- fread(paste0(out_dir,"PHORUMdata2011.csv"))
     
     setnames(phorum_old,"Variable O&M Cost ($/MWh)", "VarOM")
     setnames(phorum_old,"Startup cost adder ($)", "StartupCost")
     setnames(phorum_old,"Boiler/Generator/Committed Unit","unit.type")
     setnames(phorum_old,"Combustion Turbine/IC Engine","gen.type.needs")
     setnames(phorum_old,"Modeled Fuels", "fuel")
     setnames(phorum_old,"Capacity (MW) (Summer)", "gen.capacity.summer")
     setnames(phorum_old,"Capacity (MW) (Winter)", "gen.capacity.winter")
     setnames(phorum_old,"Min uptime (hrs)","MinUp")
     setnames(phorum_old,"Min downtime (hrs)","MinDown")
     setnames(phorum_old,"Minimum Generation (% of max)","Min")
     setnames(phorum_old,"PlantType","detailed.type")
     
     #fix of a random typo in XLSX
     phorum_old[VarOM %in% c(4.25,4.29),VarOM:=4.29] 
     
     return(phorum_old);
}

importNEEDS <- function(modelingYear){
     if(modelingYear==2019){
             needs <- fread(paste0(gen_dir,"needs_20201006.csv"))
     } else if(modelingYear %in% c(2025, 2035)){
             needs <- fread(paste0(gen_dir,"needs_20220124_active.csv"))
             needs2 <- fread(paste0(gen_dir,"needs_20220124_new.csv"))
             needs2 <- needs2[!(UniqueID_Final %in% needs$UniqueID_Final)]
             needs <- rbindlist(list(needs,needs2),fill=TRUE)
     }
     needs <- needs[substr(`Region Name`,1,3)=='PJM'][ 
        (`On Line Year` <= (modelingYear) | `On Line Year`==9999) &
        (`Retirement Year` >= (modelingYear+1) | `Retirement Year`==9999)]

     setnames(needs,"Modeled Fuels", "fuel")
     needs[,fuel:=gsub("Natural Gas","NG", fuel)][
          ,fuel:=gsub("Distillate Fuel Oil","DFO", fuel)   
     ][,fuel:=gsub("Residual Fuel Oil","RFO", fuel) 
     ][,fuel:=gsub("Bituminous","BIT", fuel) 
     ][,fuel:=gsub("Subbituminous","SUB", fuel) 
     ][,fuel:=gsub("Nuclear Fuel","NUC", fuel)
     ]
     needs[fuel=="NG, DFO",fuel:="NG"][fuel=="NG, RFO",fuel:="NG"]
     needs <- needs[!(fuel %in% c("Wind","Solar"))]
     
     needs <- needs[,.(
             plant.id = `ORIS Plant Code`, 
             plant.name = `Plant Name`,
             gen.id = `Unit ID`,
             PlantCode = UniqueID_Final,
             online.year = `On Line Year`,
             fuel,
             detailed.type = PlantType,
             unit.type=`Boiler/Generator/Committed Unit`,
             gen.type.needs=`Combustion Turbine/IC Engine`,
             overall.type.needs = paste0(`Boiler/Generator/Committed Unit`,"_", substr(`Combustion Turbine/IC Engine`,1,2),"_",PlantType,"_",fuel),
             gen.capacity.needs=`Capacity (MW)`,
             fips=FIPS5,
             fips2=FIPS5 %/% 1000,
             HeatRate = `Heat Rate (Btu/kWh)`*1000/1000000 #convert from btu/kWh -> MMbtu/MWh
     )]
     return(needs);
}

importEIA860 <- function(modelingYear){
        if(modelingYear==2019){
                eia860 <- fread(paste0(gen_dir,"eia860_2019.csv"))
        } else if(modelingYear %in% c(2025, 2035)){
                eia860 <- fread(paste0(gen_dir,"eia860_2020_active.csv"))
                eia860_2 <- fread(paste0(gen_dir,"eia860_2020_new.csv"))
                eia860_2 <- eia860_2[!(paste0(`Plant Code`,"_",`Generator ID`) %in% eia860[,paste0(`Plant Code`,"_",`Generator ID`)])]
                eia860 <- rbindlist(list(eia860,eia860_2),fill=TRUE)
        }
     
     setnames(eia860,"Nameplate Capacity (MW)", "gen.capacity.eia")
     setnames(eia860,"Summer Capacity (MW)", "gen.capacity.summer")
     setnames(eia860,"Winter Capacity (MW)", "gen.capacity.winter")
     setnames(eia860,"Minimum Load (MW)", "min.load")
     
     eia860 <- eia860[,':='(
        plant.id = `Plant Code`,
        plant.name = `Plant Name`,
        gen.id = `Generator ID`, 
        gen.type.eia = `Prime Mover`, 
        tech = `Technology`,
        fuel = `Energy Source 1`,
        overall.type.eia = paste0(`Prime Mover`,"_", `Technology`,"_",`Energy Source 1`),
        gen.capacity.eia = parse_number(gen.capacity.eia),
        gen.capacity.summer = parse_number(gen.capacity.summer),
        gen.capacity.winter = parse_number(gen.capacity.winter),
        min.load = parse_number(min.load))]
     eia860[,':='(summer.factor = gen.capacity.summer/gen.capacity.eia,
                  winter.factor = gen.capacity.winter/gen.capacity.eia)]
     eia860[plant.id==2847 & gen.id %in% c('GT4','GT5','GT6','GT7'),':='(plant.id=55248)]
     #Note: date filter not needed; we use NEEDS to choose generators to include
     return(eia860);
}

