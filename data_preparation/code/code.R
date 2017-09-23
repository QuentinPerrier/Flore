library(tidyverse); library(forcats); library(xlsx); library(stringr); library(lubridate)

# Clear previous outputs and generate the necessary directories
unlink("../outputs", recursive = TRUE)
dir.create("../outputs")
dir.create("../outputs/documentation")
dir.create("../tmp")

################
initial.year <- 2015 
end.year <- 2050
time.step <- 2 #in years
nb.week <- 6 #number of representative weeks (6 or 12)

# Import and export functions ---------------------------------------------

# in order to have consistent inputs
readInput <- function(file_name) {
  read.csv(file = paste0("../inputs/",file_name), sep = ";", comment.char = '#', dec = ",")
}

# in order to have consistent outputs
outputParameter <- function(data, file_name) {
  write.table(data, file = paste0("../outputs/",file_name), 
              sep = ",", dec = ".", 
              row.names = F, 
              col.names = F,
              quote = F)
  write.table(data, file = paste0("../tmp/",file_name), 
              sep = ";", dec = ",", 
              row.names = F, 
              col.names = F,
              quote = F)
}

outputTable <- function(data, file_name) {
  write.table(data, file = paste0("../outputs/",file_name), 
              sep = ",", dec = ".", 
              row.names = F, 
              quote = F)
  write.table(data, file = paste0("../tmp/",file_name), 
              sep = ";", dec = ",", 
              row.names = F, 
              quote = F)
}


# Tidy data ----------------------------------------------------

# RTE data on production and consumption
RTE <- read.table(paste0("../inputs/eCO2mix_RTE_Annuel-Definitif_",initial.year,".csv"), sep = ",", header = T, quote = "") %>%
  # remove rows with na (including the last line of disclaimer)
  drop_na() %>%
  # keep only one observations per hour (i.e. drop half-hours)
  separate(Heures, c("Hour", "Minute"), ":") %>%
  filter(Minute %in% c("00")) %>%
  select(-Minute) %>%
  mutate(Hour = Hour %>% as.numeric()) %>%
  # Build data per representative month
  mutate(Date = as.Date(Date),
         Day = wday(Date), #jour de la semaine, de 1 à 7. Le 1 est dimanche
         Month = month(Date, label = T),
         Year = year(Date)) %>% 
  # insert representative weeks
  mutate(Week = ceiling(match(Month, month.abb)/(12/nb.week))) #Gather 



# Initial capacity installed
capa0 <- readInput("Capacity2015.csv") %>%
  mutate(Capacity = Capacity / 1000) # to get GW

# Capacity per month
MonthlyCapa <- readInput("/MonthlyCapacities_2015.csv") 


# Extract load and net exports --------------------------------------------

Load <- RTE %>%
  group_by(Year, Week, Day, Hour) %>%
  summarise(Consumption = mean(Consommation), Exports = -mean(Ech..physiques)) %>%
  ungroup() %>%
  mutate(Load = Consumption + Exports) %>%
  # add the hour in the representative week (1 to 68)
  mutate(WeekHour = rep(1:168,nb.week)) %>%
  mutate_at(vars(Consumption, Exports, Load), funs(. / 1000)) #to get GWh as unit


# Compute load evolution
conso <- sum(RTE$Consommation) / 10^6
exports <- -sum(RTE$Ech..physiques) / 10^6
Load_evolution <- readInput(file_name = "Demand_DNTE.csv") %>%
  # on retranche les exports (supposés constants) pour n'avoir que la conso
  mutate_at(vars(DEC,DIV,SOB,EFF), funs(. - exports)) %>%
  # on calcule l'augmentation de cette conso
  mutate_at(vars(DEC,DIV,SOB,EFF), funs(. / .[1])) %>%
  # on divise DEC par deux car il est trop haut
  mutate(DEC = (DEC+1)/2)
  
#Check with graph
Load_evolution %>%
  mutate_at(vars(DEC,DIV,SOB,EFF), funs(.*conso + exports) ) %>%
  gather(key = Scenario, value = Demand, -Year) %>%
  ggplot(aes(x = Year, y = Demand, colour = Scenario)) + geom_line() + theme_bw()
ggsave(filename = "../outputs/demand_evolution.pdf", width = 4, height = 3.5)


# Extract solar profiles -----------------------------------------

WeeklyCapa <- MonthlyCapa %>% 
  mutate(Week = ceiling(match(Month, month.abb)/(12/nb.week))) %>% 
  select(Week, PV, Wind) %>% 
  group_by(Week) %>% 
  summarise(PV = mean(PV), Wind = mean(Wind)) %>%
  ungroup()

# Get production per month per unit of capacity and compute profile
PV <- RTE %>%
  # get solar production in MWh
  group_by(Year, Week, Day, Hour) %>%
  summarise(Prod = mean(Solaire)) %>%
  ungroup() %>%
  mutate(Prod = ifelse(Prod>0, Prod, 0)) %>% # because of negative values...
  mutate(WeekHour = rep(1:168,nb.week)) %>%
  # add capacity in MW
  left_join(., WeeklyCapa %>% select(Week, PV) %>% rename(Capacity = PV),
            by = "Week") %>%
  # compute profile per unit of GW for each representative week
  mutate(Profile = Prod / Capacity)

ggplot(PV %>% filter(Week ==1), aes(x = 1:168, y = Profile)) + geom_line()

# Note: by taking the average provides a good approximation of the output in terms of energy
# But it must be completed with reserves requirements
# Otherwise, taking a real week can be done with the following code
# PV <- RTE %>% 
#   group_by(Week) %>% 
#   slice(1:168) %>% 
#   ungroup() %>% 
#   select(Month,Week,Day,Hour,Solaire) %>% 
#   rename(Prod = Solaire) %>% 
#   mutate(Prod = ifelse(Prod<0,0,Prod)) %>% #Remove negative values...
#   left_join(., MonthlyCapa %>% select(Month,PV) %>% rename(Capacity = PV), by = c("Month")) %>% 
#   mutate(Profile = Prod / Capacity)

# Graphical check
#PV %>% filter(Week == 1) %>%
#  ggplot(aes(x = WeekHour, y = Profile)) + geom_line()

# Load factor check on original data
prodPV <- RTE %>% filter(Month == "Jan") %>% .$Solaire %>% (function(x) sum(x)/(12/nb.week))
loadFactor.PV <- prodPV / (MonthlyCapa$PV[1]*31*24); loadFactor.PV
#on retrouve bien le 6.2% comme Bernat Chabot : http://www.photovoltaique.info/IMG/pdf/9bpvfrance1-15.pdf
remove(prodPV, loadFactor.PV)

# Load factor check on final data
#prodPV <- sum(PV$Prod[1:168]*4) / (MonthlyCapa$PV[1]*28*24); prodPV
#remove(prodPV)


# Extract wind profiles -----------------------------------------

# Get production per month per unit of capacity and compute profile
Wind <- RTE %>%
  # get solar production in MWh
  group_by(Year, Week, Day, Hour) %>%
  summarise(Prod = mean(Eolien)) %>%
  ungroup() %>%
  mutate(WeekHour = rep(1:168,nb.week)) %>%
  # add capacity in MW
  left_join(., WeeklyCapa %>% 
              select(Week, Wind) %>% 
              rename(Capacity = Wind), 
            by = "Week") %>%
  # compute profile per unit of GW for each representative week
  mutate(Profile = Prod / Capacity)

# Graphical check
Wind %>% filter(Week == 1) %>%
  ggplot(aes(x = WeekHour, y = Profile)) + geom_line()

# Load factor check on original data
prodWind <- RTE %>% filter(Month == "Jan") %>% .$Eolien
loadFactor.Wind <- prodWind / mean(MonthlyCapa$Wind[1])
#plot(x = 1:744, y = loadFactor.Wind)
#on retrouve bien le graphique de RTE : http://www.rte-france.com/sites/default/files/apercu_energie_elec_2015_01.pdf, p6.
remove(prodWind, loadFactor.Wind)

# Load factor check on final data
loadFactor.Wind <- sum(Wind$Prod[1:168]*4) / (MonthlyCapa$Wind[1]*28*24); loadFactor.Wind
remove(loadFactor.Wind)



# Load factor for nuclear -------------------------------------------------

nucCapa <- capa0 %>% 
  filter(Technology == "nuc_hist") %>% 
  select(Capacity) %>% as.numeric()

nucFac <- RTE %>% 
  group_by(Year, Week, Day, Hour) %>% 
  summarise(Prod=mean(Nucléaire)) %>%
  ungroup() %>%
  mutate(Factor=Prod/(nucCapa*1000)) %>%
  mutate(WeekHour = rep(1:168,nb.week)) %>%
  group_by(Week) %>% summarise(factor = mean(Factor)) %>% ungroup()

#outputTable(nucFac %>% select(WeekHour, Factor), file_name = "Nuclear_Profile.csv")



# Nuclear plant age -------------------------------------------------------

ListeNuclearPlants <- readInput("ListeNuclearPlants.csv")

for (i in 2:4) {
  ListeNuclearPlants[,paste0("Visit",i)] <- ifelse(is.na(ListeNuclearPlants[,paste0("Visit",i)]),
                                         ListeNuclearPlants[,paste0("Visit",i-1)]+10, 
                                         ListeNuclearPlants[,paste0("Visit",i)])
}


  


# Capex annuities and LCOE ---------------------------------------------------------

# LCOE = annuity + fOM + (fuel + CO2price*CO2content)

capex <- readInput("Capex.csv") %>%
  gather(key = technology, value = capex, -year)

lifetimes <- readInput("Lifetimes.csv")

fOM <- readInput(file_name = "fixed_OM.csv") %>%
  gather(key = technology, value = fOM, -year)

fuel <- readInput("Fuel_costs.csv") %>%
  gather(key = technology, value = fuel, -year)

CO2price <- readInput("CO2_price.csv") %>%
  gather(key = scenario.CO2, value = CO2.price, -year)

CO2content <- readInput("CO2_content.csv") 

efficiency <- readInput("Efficiency.csv") %>%
  gather(key = technology, value = efficiency, -year)

load_factor <- readInput("Load_factors.csv") %>% 
  select(month, coal, CCGT, TAC, lake, step, nuc_new) %>%
  mutate(Week = ceiling(match(month, month.abb)/(12/nb.week))) %>% 
  select(-month) %>% 
  group_by(Week) %>% 
  summarise_all(mean) %>% 
  full_join(., nucFac, by = "Week") %>%
  rename(nuc_hist = factor) %>%
  mutate(nuc_retrofit = nuc_hist) %>%
  select(Week, coal, CCGT, TAC, lake, step, nuc_hist, nuc_retrofit, nuc_new) 

avg_load_factor <- load_factor %>% 
  gather(key = technology, value = load.factor, -Week) %>% 
  group_by(technology) %>% summarise(avg.load.factor = mean(load.factor)) %>% ungroup()

avg_load_factor[nrow(avg_load_factor)+1,1] <- "onshore"
avg_load_factor[nrow(avg_load_factor),2] <- sum(Wind$Profile)*365/(nb.week*7*8760)
  
avg_load_factor[nrow(avg_load_factor)+1,1] <- "PV"
avg_load_factor[nrow(avg_load_factor),2] <- sum(PV$Profile)*365/(nb.week*7*8760)


# interest rate 
r <- 0.08

LCOE <- list(capex, lifetimes, fOM, fuel, CO2price, CO2content, efficiency, avg_load_factor) %>%
  reduce(full_join) %>%
  mutate(annuity = capex * r / (1 - (1+r)^-lifetime),
         variable.CO2 = (fuel + CO2.content*CO2.price)/efficiency,
         variable.noCO2 = fuel/efficiency,
         generation.hours = 8760*avg.load.factor,
         LCOE.CO2 = variable.CO2 + 1000*(annuity + fOM ) / generation.hours,
         LCOE.noCO2 = variable.noCO2 + 1000*(annuity + fOM ) / generation.hours)

# Graph to check
LCOE %>% 
  filter(scenario.CO2 == "low" & technology %in% c("CCGT","onshore","PV","nuc_new")) %>%
  ggplot(aes(x = year, y = LCOE.noCO2, colour = technology)) + geom_point()



# Modifying LCOE ----------------------------------------------------------

# Function to compute capex to new value
getAnnuity <- function(LCOE, variable.cost, generation.hours, fOM) {
  # generation.hours = average number of hours of production in a year
  annuity <- (LCOE - variable.cost)*generation.hours/1000 - fOM
}

# Recalibrate new_nuc to a LCOE of 110
linearExtrapolation <- function(x, x1, x2, y1, y2) {
  y <- y1 + (x-x1)*(y2-y1)/(x2-x1)
}

# exponential extrapolation
exponentialExtrapolation <- function(x, x1, x2, y1, y2) {
  x0 <- x - x1
  a <- (y1 - y2) / (1 - exp(-(x2-x1)/10))
  b <- y1 - a
  out <- a*exp(-x0/10)+b
}

onshore_annuities <- tibble(year = initial.year:end.year,
                         technology = "onshore",
                         high = exponentialExtrapolation(year, initial.year, end.year, 82, 60),
                         low = exponentialExtrapolation(year, initial.year, end.year, 82, 40)) %>% 
  gather(key = scenario.enr, value = LCOE2, -c(year,technology)) %>% 
  left_join(., LCOE %>% 
              filter(technology == "onshore" & scenario.CO2 == "medium"),
            by = c("year", "technology")) %>% 
  mutate(annuity2 = (LCOE2 - variable.noCO2)*generation.hours/1000 - fOM) %>% 
  select(year, scenario.enr, annuity2)

PV_annuities <- tibble(year = initial.year:end.year,
                       technology = "PV",
                       high = exponentialExtrapolation(year, initial.year, end.year, 90, 50),
                       low = exponentialExtrapolation(year, initial.year, end.year, 90, 25)) %>% 
  gather(key = scenario.enr, value = LCOE2, -c(year,technology)) %>% 
  left_join(., LCOE %>% 
              filter(technology == "PV" & scenario.CO2 == "medium"),
            by = c("year", "technology")) %>% 
  mutate(annuity2 = (LCOE2 - variable.noCO2)*generation.hours/1000 - fOM) %>% 
  select(year, scenario.enr, annuity2)

#Nuclear new
nuc_new_annuities <- tibble(year = initial.year:end.year,
                       technology = "nuc_new",
                      low = linearExtrapolation(year, initial.year, end.year, 110, 70),
                      medium = linearExtrapolation(year, initial.year, end.year, 110, 90),
                      high = linearExtrapolation(year, initial.year, end.year, 110, 110)) %>%
  gather(key = scenario.nuc, value = LCOE2, -c(year,technology)) %>% 
  left_join(., LCOE %>% 
              filter(technology == "nuc_new" & scenario.CO2 == "medium"),
            by = c("year", "technology")) %>% 
  mutate(annuity2 = (LCOE2 - variable.noCO2)*generation.hours/1000 - fOM) %>% 
  select(year, scenario.nuc, annuity2)

# Nuclear retrofitted

nuc_retrofit_annuities <- tibble(year = initial.year:end.year,
                            technology = "nuc_retrofit")

for (i in seq(from=40,to=90,by=10)) {
  col <- rep(i,nrow(nuc_retrofit_annuities))
  nuc_retrofit_annuities <- cbind(nuc_retrofit_annuities, col)
  colnames(nuc_retrofit_annuities)[ncol(nuc_retrofit_annuities)] <- paste0(i)
}

nuc_retrofit_annuities <- nuc_retrofit_annuities %>% 
  gather(key = scenario.nuc_hist, value = LCOE2, -c(year,technology)) %>% 
  left_join(., LCOE %>% 
              filter(technology == "nuc_retrofit" & scenario.CO2 == "medium"),
            by = c("year", "technology")) %>% 
  mutate(annuity2 = (LCOE2 - variable.noCO2)*generation.hours/1000 - fOM) %>% 
  select(year, scenario.nuc_hist, annuity2)

# Historical nuke annuities
nuc_hist_annuities <- tibble(year = initial.year:end.year,
                             technology = "nuc_hist",
                             LCOE2 = 42 ) %>%
  left_join(., LCOE %>% 
              filter(technology == "nuc_hist" & scenario.CO2 == "medium"),
            by = c("year", "technology")) %>% 
  mutate(annuity2 = (LCOE2 - variable.noCO2)*generation.hours/1000 - fOM) %>% 
  select(year, annuity2)



# Hydro: run-of-river profile and lake inflows ----------------------------------------------------

river.capa <- capa0 %>% filter(Technology == "river") %>% select(Capacity) %>% as.numeric()

river <- RTE %>% 
  #on calcule d'avoir le facteur de charge pour chaque mois, heure et jour
  group_by(Week, Day, Hour) %>% summarise(prod = mean(Hydraulique...Fil.de.l.eau...éclusée)) %>% ungroup() %>% 
  mutate(WeekHour = rep(1:168,nb.week)) %>% 
  select(Week, WeekHour, prod) %>% 
  mutate(prod = prod / 1000)
# %>% 
#   # et ensuite on fait la moyenne mensuelle (sinon on a des problèmes avec le nombre de jours qui varie par mois)
#   group_by(Month) %>% summarise(profile = mean(profile)) %>%  ungroup()


lake_inflows <- RTE %>% 
  group_by(Week) %>% summarise(inflow = sum(Hydraulique...Lacs)) %>% ungroup() %>% 
  #convert to GWh
  mutate(inflow = inflow / 1000)

step.capa <- readInput("Step_evolution.csv") %>% 
  mutate(capa = evolution*( capa0 %>% filter(Technology == "step") %>% select(Capacity) %>% as.numeric())) %>% 
  select(-evolution)
  

# Write outputs --------------------------------------------------------

outputParameter(Load %>% 
                  select(Week, WeekHour, Consumption) %>%
                  rename(Hour = WeekHour),
                file_name = "consumption.csv")

outputParameter(Load %>% 
                  select(Week, WeekHour, Exports) %>%
                  rename(Hour = WeekHour),
                file_name = "exports.csv")

outputParameter(Load_evolution %>% gather(key = scenario, value = demand, -Year), 
            file_name = "load_evolution.csv")

outputParameter(lifetimes, file_name = "lifetimes.csv")

VRE <- full_join(Wind %>% select(Week, WeekHour, Profile) %>% rename(onshore = Profile),
                 PV %>% select(Week, WeekHour, Profile) %>% rename(PV = Profile),
                 by = c("Week","WeekHour")) %>% 
  gather(key = technology, value = value, onshore:PV) %>%
  select(technology, Week, WeekHour, value) %>%
  outputParameter(file_name = "vre_profiles.csv")


outputTable(load_factor, file_name = "load_factor.csv")

outputParameter(LCOE %>% filter(scenario.CO2 == "medium") %>% select(year, technology, annuity), file_name = "annuities.csv")

outputParameter(fOM, file_name = "fixed_OM.csv")

outputParameter(fuel, file_name = "fuel_costs.csv")

outputParameter(efficiency, file_name = "efficiency.csv")

outputParameter(CO2content, file_name = "CO2_content.csv")

outputParameter(CO2price, file_name = "CO2_price.csv")

outputParameter(river, file_name = "river.csv")

outputParameter(lake_inflows, file_name = "lake_inflows.csv")

exogenous_capa <- tibble(year = initial.year:2050) %>%
  mutate(nuc_hist = map_dbl(seq_along(initial.year:2050),
                            function(i) sum(ListeNuclearPlants$Net.power..MWe.[ListeNuclearPlants$Visit4 >= seq(initial.year,2050)[i]])
  )
  ) %>%
  mutate(nuc_hist = nuc_hist/1000) %>% 
  mutate(river = capa0 %>% filter(Technology == "river") %>% select(Capacity) %>% as.numeric(),
         lake = capa0 %>% filter(Technology == "lake") %>% select(Capacity) %>% as.numeric(),
         step = step.capa$capa) %>% 
  gather(technology, capacity,-year)
outputParameter(exogenous_capa, file_name = "exogenous_capacities.csv")

reserve_requirements <- readInput("Reserve_requirements.csv") 
outputParameter(reserve_requirements, file_name = "reserve_requirements.csv")

outputParameter(capa0, file_name = "initial_capacities.csv")

outputParameter(onshore_annuities %>% arrange(desc(scenario.enr), year), file_name = "onshore_annuity_scenarios.csv")

outputParameter(PV_annuities, file_name = "PV_annuity_scenarios.csv")

outputParameter(nuc_hist_annuities, file_name = "nuc_hist_annuities.csv")

outputParameter(nuc_retrofit_annuities, file_name = "nuc_retrofit_annuities.csv")

outputParameter(nuc_new_annuities, file_name = "nuc_new_annuities.csv")

deploy_speed <- readInput(file_name = "deploy_speed.csv")
outputParameter(deploy_speed, file_name = "deploy_speed.csv")

# Error forecast ----------------------------------------------------------
scalar1 <- function(x) {x / sqrt(sum(x^2, na.rm = T))}

# Wind 
forecast_w <- readInput("PrevisionEolienne_2015.csv") %>% 
  select(Date, Heure, Prevision.J.1) %>% 
  mutate(Heure = str_sub(Heure, end = -4),
         Heure = as.numeric(Heure),
         Date = as.Date(Date, "%d/%m/%Y")) %>% 
  rename(Hour = Heure) %>% 
  full_join(RTE %>% select(Date, Hour, Eolien), by = c("Date","Hour")) %>% 
  mutate(forecast_n = scalar1(Prevision.J.1),
         output = scalar1(Eolien),
         error = forecast_n - output,
         error_n = scalar1(error))
ggplot(data = forecast_w, aes(error_n)) + geom_density()

quantile(forecast_w$error_n, probs = 0.99, na.rm = T)

# Solar
forecast_pv <- readInput("PrevisionSolaire_2015.csv") %>% 
  select(Date, Heure, Prevision.J.1) %>% 
  mutate(Heure = str_sub(Heure, end = -4),
         Heure = as.numeric(Heure),
         Date = as.Date(Date, "%d/%m/%Y")) %>% 
  rename(Hour = Heure) %>% 
  full_join(RTE %>% select(Date, Hour, Solaire), by = c("Date","Hour")) %>% 
  mutate(forecast_n = scalar1(Prevision.J.1),
         output = scalar1(Solaire),
         error = forecast_n - output,
         error_n = scalar1(error))

ggplot(data = forecast_pv, aes(error_n)) + geom_density()

quantile(forecast_pv$error_n, probs = 0.99, na.rm = T)


# Plots for documentation -------------------------------------------------

# Nuclear cliff
g <- exogenous_capa %>% filter(technology == "nuc_hist") %>% 
  ggplot(aes(x = year, y = capacity)) + geom_bar(stat = "identity") + 
  labs(y = "Nuclear capacity before retrofit (GW)") +
  theme_bw()
ggsave(filename = "../outputs/nuclear_cliff.pdf", height = 7, width = 7)

# LCOE 
g <- LCOE %>% 
  filter(scenario.CO2 == "low") %>%
  ggplot(aes(x = year, y = LCOE.noCO2, colour = technology)) + geom_point() + theme_bw();g
ggsave(g, filename = "../outputs/calibration_LCOE.pdf", height = 7, width = 7)


# Tables for documentation ------------------------------------------------

docuTable <- function(df, file_name) {
  #table for documentation
  write.table(df, file = paste0("../outputs/documentation/",file_name),
              sep = ";", dec = ".", row.names = F)
}

docuTable(df = capex %>% 
             filter(year %in% c(2015,2020,2030,2040,2050)) %>% 
             spread(key = technology, value = capex),
          file_name = "capex.csv")

docuTable(df = fOM %>% 
            filter(year %in% c(2015,2020,2030,2040,2050)) %>% 
            spread(key = technology, value = fOM),
          file_name = "fOM.csv")

