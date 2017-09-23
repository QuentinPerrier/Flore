$Stitle Data

*Offlisting avoids printing the loaded data in the lst file
$Offlisting


Parameter  init_cap(tec)        'initial capacities'
/
$ondelim
$include ../inputs/initial_capacities.csv
$offdelim
/ ;

* Demand

Parameter initial_consumption(w,h) 'Initial consumption'
/
$ondelim
$include ../inputs/consumption.csv
$offdelim
/ ;

Parameter exports(w,h) 'Exports'
/
$ondelim
$include ../inputs/exports.csv
$offdelim
/ ;

Parameter load_evol(y_all,dem_scenario) 'Demand_Evolution'
/
$ondelim
$include ../inputs/load_evolution.csv
$offdelim
/
;

Parameter d_all(dem_scenario,y_all,w,h)  'Total Demand' ;
d_all(dem_scenario,y_all,w,h) = ( initial_consumption(w,h) * load_evol(y_all,dem_scenario) ) + exports(w,h)  ;



* Available capacity

Parameter  lifetime(tec)       lifetime of power plant (years)
/
$ondelim
$include ../inputs/lifetimes.csv
$offdelim
/ ;

Parameter capa_exo(y_all,tec_ex) 'Exogenous Capacities'
/
$ondelim
$include ../inputs/exogenous_capacities.csv
$offdelim
/;


Parameter pump_cap(y) 'pumping capacity in GWh';
pump_cap(y) = capa_exo(y,'step') ;


Parameter ramp_rate(tec_dispa)
/ coal          0.08, nuc_hist      0.05, nuc_retrofit   0.05, nuc_new       0.05  / ;


Parameter vre_profiles(tec_vre,w,h) 'Production profiles of VRE'
/
$ondelim
$include  ../inputs/vre_profiles.csv
$offdelim
/ ;


Table loadfactor(w,tec_dispa) 'Production profiles of dispatchable plants'
$ondelim
$include ../inputs/load_factor.csv
$offdelim


Parameter  deploy_speed(tec_end)        'initial capacities'
/
$ondelim
$include ../inputs/deploy_speed.csv
$offdelim
/ ;


* Costs

Parameter annuity(y_all,tec) 'CAPEX annuity payments in euro/kW - including Back-end (fuel recycling) and insurance costs'
/
$ondelim
$include ../inputs/annuities.csv
$offdelim
/;

Parameter fOM(y_all,tec) 'Fixed OM in euro/kW'
/
$ondelim
$include ../inputs/fixed_OM.csv
$offdelim
/;

Parameter Fuel_Costs(y_all,tec) 'Fuel Costs in euro/MWh (before conversion to electricity)'
/
$ondelim
$include ../inputs/fuel_costs.csv
$offdelim
/ ;

Parameter Efficiency(y_all,tec) 'Plant efficiency in producing electricity'
/
$ondelim
$include ../inputs/efficiency.csv
$offdelim
/;

Parameter CO2_content(tec) 'CO2 content in tCO2/MWh (CO2 content of energy before conversion)'
/
$ondelim
$include  ../inputs/CO2_content.csv
$offdelim
/ ;

Parameter CO2_price_all(y_all, CO2_scenario) 'CO2 price in euro/tCO2'
/
$ondelim
$include ../inputs/CO2_price.csv
$offdelim
/ ;

Parameter onshore_annuity(y_all,enr_scenario) 'annuities for wind in various scenarios'
/
$ondelim
$include ../inputs/onshore_annuity_scenarios.csv
$offdelim
/ ;

Parameter PV_annuity(y_all,enr_scenario ) 'annuities for wind in various scenarios'
/
$ondelim
$include ../inputs/PV_annuity_scenarios.csv
$offdelim
/ ;

Parameter nuc_hist_annuity(y_all) 'annuities for historical plants'
/
$ondelim
$include ../inputs/nuc_hist_annuities.csv
$offdelim
/ ;

annuity(y_all, "nuc_hist") = nuc_hist_annuity(y_all);

Parameter nuc_retrofit_annuity(y_all,retrofit_scenario) 'annuities for retrofitted plants'
/
$ondelim
$include ../inputs/nuc_retrofit_annuities.csv
$offdelim
/ ;

Parameter nuc_new_annuity(y_all,new_nuc_scenario) 'annuities for new nuclear'
/
$ondelim
$include ../inputs/nuc_new_annuities.csv
$offdelim
/ ;

*Hydropower

Parameter river(w,h) 'average river production in GWh'
/
$ondelim
$include  ../inputs/river.csv
$offdelim
/ ;

Parameter lake_inflows(w) 'lake inflows in GWh'
/
$ondelim
$include  ../inputs/lake_inflows.csv
$offdelim
/ ;

Parameter epsilon(tec_vre) 'reserve requirement per unit capacity of VRE'
/
$ondelim
$include  ../inputs/reserve_requirements.csv
$offdelim
/ ;


$Onlisting
