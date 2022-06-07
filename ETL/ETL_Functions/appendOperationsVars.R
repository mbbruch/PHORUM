appendOperationsVars <- function(phorum_new){
     
     phorum_new[detailed.type=="Combustion Turbine" | gen.type.eia=="IC" | fuel %in% "Landfill Gas",RampRate:=pmax(CapacitySummer,CapacityWinter)#gen.capacity.needs*0.33
     ][gen.type.eia=="GT",RampRate:=pmax(CapacitySummer,CapacityWinter)#gen.capacity.needs*0.34
     ][detailed.type %like% "Steam" | detailed.type %in% c("Nuclear","Fuel Cell") | gen.type.eia=="ST" | fuel %in% c("MSW","Biomass") | fuel %like% "Waste",RampRate:=gen.capacity.needs*0.14
     ][detailed.type=="Combined Cycle" | gen.type.eia %in% c("CA","CC","CS","CT"),RampRate:=gen.capacity.needs*0.22
     ]
     phorum_new[detailed.type %like% "Hydro" & ((!is.na(CapacitySummer) & CapacitySummer>0) | 
                                            (!is.na(CapacityWinter) & CapacityWinter>0)),RampRate:=30*as.numeric(parse_number(word(plant.name,-1,sep="_")))]
     
     phorum_new[detailed.type %in% c("Biomass","Combined Cycle","Landfill Gas","Municipal Solid Waste","LFG"),':='(MinUp=5,MinDown=4)
          ][detailed.type %in% c("Fossil Waste","Non-Fossil Waste"),':='(MinUp=10,MinDown=8)
          ][detailed.type == "Hydro",':='(MinUp=0,MinDown=0)
          ][detailed.type =="O/G Steam",':='(MinUp=9,MinDown=7)
          ][detailed.type =="Pumped Storage",':='(MinUp=0,MinDown=0)
          ][detailed.type %in% c("Nuclear","Fuel Cell"),':='(MinUp=20,MinDown=20)
          ][detailed.type=="Combustion Turbine",':='(MinUp=fifelse(gen.capacity.needs>70,3,2),MinDown=fifelse(gen.capacity.needs>70,2,1))
          ][detailed.type=="Coal Steam",':='(MinUp=fifelse(gen.capacity.needs>=150,15,6),MinDown=fifelse(gen.capacity.needs>70,12,6))]
     phorum_new[detailed.type %in% c("Nuclear","Fuel Cell"),StartupCost:=gen.capacity.needs*500
          ][detailed.type %in% c("Coal Steam","O/G Steam","Municipal Solid Waste","Fossil Waste","Non-Fossil Waste","Biomass"),StartupCost:=gen.capacity.needs*100
          ][detailed.type=="Combustion Turbine",StartupCost:=gen.capacity.needs*25
          ][detailed.type %in% c("Combined Cycle","Landfill Gas") | gen.type.eia %in% c("CA","CC","CS","CT"),StartupCost:=gen.capacity.needs*50
          ][detailed.type %in% c("Hydro","Pumped Storage"),StartupCost:=0
          ]
          
     phorum_new[detailed.type %in% c("Nuclear","Fuel Cell"),Min:=0.5
          ][detailed.type %in% c("Biomass","Coal Steam", "O/G Steam",
                                 "Municipal Solid Waste","Fossil Waste","Non-Fossil Waste"),Min:=0.3
          ][detailed.type == "Combined Cycle", Min:=0.2
          ][detailed.type %in% c("Hydro","Pumped Storage"),Min:=0.0
          ][(detailed.type=="Combustion Turbine" | gen.type.eia=="IC" | fuel %in% "Landfill Gas"),Min:=0.15]
          
     return(phorum_new);
}