library(data.table)
library(stringr)
library(readr)
library(FNN)
library(tidyr)
library(lubridate)
library(dplyr)

dataminer_dir <<- paste0(base_dir,"from_pjm_dataminer\\")
gen_dir <<- paste0(base_dir,"generators\\")
out_dir <<- paste0(base_dir,"outputs_for_matlab\\")
fuel_dir <<- paste0(base_dir,"fuel_prices\\")
misc_dir <<- paste0(base_dir,"misc\\")
county_dir <<- paste0(base_dir,"county_data\\")

cpi.2010 <- 214.738
cpi.2017 <- 245.120
cpi.2019 <- 255.538
cpi.2020 <- 258.811
income.2015 <- 51516/2080 #https://www.transportation.gov/office-policy/transportation-policy/revised-departmental-guidance-valuation-travel-time-economic
income.2019 <- 68703/2080
mi.per.km <- 1/1.60934
m.per.km <- 1E3
tonne.per.lb=0.907185/2000
g.per.tonne <- 1E6
y10.to.y19 <- cpi.2019/cpi.2010
y17.to.y19 <- cpi.2019/cpi.2017
y20.to.y19 <- cpi.2019/cpi.2020
vsl <- 10.9 #$M, 2019, https://www.transportation.gov/office-policy/transportation-policy/revised-departmental-guidance-on-valuation-of-a-statistical-life-in-economic-analysis
mpg <- 24.89 #see average_vehicle_age.R
mpg.greet <- 28
Greet.ICE.MPG <- mpg.greet
miles.greet <- 173151
km.greet <- miles.greet/mi.per.km
acs.to.pope2019 = -15.1 + 15.2*1.13 
carbon.price <- 51 #Ward: triangular distro, 15-51-149 in 2019 USD (mean 71+2/3, median 64.11212) per tonne
model.year <- 2019
suffix <- "2019"