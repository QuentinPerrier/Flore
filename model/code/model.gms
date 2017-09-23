$Title The FLORE Model

*$offlisting
*GAMS Base Module 24.1.3 r41464 Released Jul 26, 2013 DEG Mac x86_64/Darwin

$Ontext
   This is an investment planning and dispatch model for the French power
   sector.
$Offtext




*==========================================================================
*  SETS
*==========================================================================

 Set  y_all                 all years                 / 2015*2050 /

      y(y_all)              years optimized           / 2015, 2017, 2019, 2021, 2023, 2025, 2027, 2029, 2031, 2033, 2035, 2037, 2039, 2041, 2043, 2045, 2047, 2049 /

      w                     Representative week       / 1*6 /

      h                     hour                      / 1*168 /

      tec                   plants and pomp           / onshore, PV, coal, CCGT, TAC, river, lake, step, nuc_hist, nuc_retrofit, nuc_new /

      tec_end(tec)          endogenous capacities     / onshore, PV, coal, CCGT, TAC, nuc_retrofit, nuc_new /

      tec_ex(tec)           exogenous capacities      / nuc_hist, river, lake, step /

      tec_vre(tec)          res profiled Technologies / onshore, PV /

      tec_dispa(tec)        dispatchable technologie  / coal, CCGT, TAC, lake, step, nuc_hist, nuc_retrofit, nuc_new /

      nuke(tec)             nuclear sources           / nuc_hist, nuc_retrofit, nuc_new /

      dem_scenario          demand scenarios          / DEC, DIV, EFF, SOB /

      enr_scenario          price scenario of enr     / low, high /

      CO2_scenario          CO2 price scenarios       / low, medium, high /

      retrofit_scenario     Retrofitting costs        / 40, 50, 60, 70, 80, 90 /

      new_nuc_scenario      New nuclear costs         / low, medium, high /

;


Alias(h,i);


*==========================================================================
*  LOADING DATA
*==========================================================================


Set labels for plant data /  lifetime     lifetime of power plants (in years)
                              deploy_speed deployment maximum speed
                              initcap      initial capacities  (MW) / ;

* Load all parameters and values
$include load_inputs.gms


* Choose scenario: demand, EnR cost, CO2 price, retrofit cost, new nuclear cost
* Nom des variables : demand, enr, retro, new, CO2

* Set demand evolution
$if not set demand $set demand EFF

Parameter d(y,w,h)  'Total Demand' ;
d(y,w,h) = ( initial_consumption(w,h) * load_evol(y,"%demand%") ) + exports(w,h)  ;
Display d;

* Set Enr cost
$if not set enr $set enr low
annuity(y_all, "onshore") = onshore_annuity(y_all,"%enr%") ;
annuity(y_all, "PV")      = PV_annuity(y_all,"%enr%") ;

*Set retrofitted nuclear
$if not set retro $set retro 40
annuity(y_all, "nuc_retrofit") = nuc_retrofit_annuity(y_all,"%retro%");

*Set new nuclear
$if not set new $set new high
annuity(y_all, "nuc_new") = nuc_new_annuity(y_all, "%new%");

* Set CO2 price
$if not set CO2 $set CO2 high

Parameter CO2_price(y) 'CO2 price in euro/tCO2';
CO2_price(y)=CO2_price_all(y,"%CO2%");


* Setting costs for the objective function

Parameter variable_costs(y,tec) 'Variable costs in euro/MWh' ;
variable_costs(y,tec)= ( Fuel_costs(y,tec) + CO2_content(tec)*CO2_price(y) ) / Efficiency(y,tec)  ;

Parameter fixed_Costs(y,tec) 'in euro/kW';
fixed_Costs(y,tec) = (annuity(y,tec)+fOM(y,tec)) ;


*==========================================================================
*  MODEL
*==========================================================================


 Variables
            INVE(tec_end,y)       investment in capacity i    (GW)
            CAPA(tec,y)           capacity i                  (GW)
            GENE(tec,y,w,h)       power generated             (GW)
            PUMP(y,w,h)           pumping                     (Gw)
            DECO(tec_end,y)       decommissing                (GW per year)
            RES(tec_dispa,y,w,h)  reserves demand             (Gw)
            COST                  total discounted cost       (million euros)

 Positive Variable INVE,CAPA,GENE,PUMP,DECO,RES ;


 Equations
            adequacy              supply meets demand

            capa_cons             constraint on production lower than real capacity at each moment
            capa_cons2            constraint on production lower than real capacity at each moment
            ramp_up               ramping rates up
            ramp_down             ramping rates down

            capa_evol             endogenous capacity evolution
            decom                 decomissioning
            retrofit_limit        retrofit cannot exceed previous capacity
            deploy_limit          deployment speed constraints

            pump_max              pumping constraint
            pump_water_balance    water balance betweeen pumping and turbining at the end of the week
            reservoir_cap_max     reservoir capacity constraint for step
            reservoir_pos         reserve cannot be negative (no advance in water)
            lake_res              constraint on water for lake reservoirs

            reserves_required     regulatory level of frr reserves

            obj                   total discounted cost   (million euros) ;




adequacy(y,w,h)..               sum(tec, GENE(tec,y,w,h))   =g=  d(y,w,h) + PUMP(y,w,h);

capa_cons(tec_dispa,y,w,h)..    GENE(tec_dispa,y,w,h)       =l=  CAPA(tec_dispa,y)*loadfactor(w,tec_dispa) - RES(tec_dispa,y,w,h);
capa_cons2(tec_vre,y,w,h)..     GENE(tec_vre,y,w,h)         =l=  CAPA(tec_vre,y)*vre_profiles(tec_vre,w,h) ;
ramp_up(tec_dispa,y,w,h+1)..    GENE(tec_dispa,y,w,h+1)     =l=  GENE(tec_dispa,y,w,h) + CAPA(tec_dispa,y)*ramp_rate(tec_dispa) ;
ramp_down(tec_dispa,y,w,h+1)..  GENE(tec_dispa,y,w,h+1)     =g=  GENE(tec_dispa,y,w,h) - CAPA(tec_dispa,y)*ramp_rate(tec_dispa) ;

capa_evol(tec_end,y+1)..        CAPA(tec_end,y+1)           =e=  CAPA(tec_end,y) + INVE(tec_end,y) - DECO(tec_end,y) ;
decom(tec_end,y)..              DECO(tec_end,y)             =e=  INVE(tec_end,y-floor(lifetime(tec_end)/2))$(ord(y)>(lifetime(tec_end)/2)) + (init_cap(tec_end)/20)$(ord(y) <= 20)   ;
retrofit_limit(y)..             INVE('nuc_retrofit',y)      =l=  CAPA('nuc_hist',y) - CAPA('nuc_hist',y+1) ;
deploy_limit(y, tec_end)..      INVE('onshore',y)           =l=  deploy_speed(tec_end) ;

lake_res(w,y)..                 lake_inflows(w)             =g=  sum(h,GENE('lake',y,w,h)) ;
pump_max(y,w,h)..               pump_cap(y)                 =g=  PUMP(y,w,h) ;
pump_water_balance(w,y)..       0.8*sum(h,PUMP(y,w,h))      =g=  sum(h,GENE('step',y,w,h)) ;
reservoir_cap_max(y,w,h)..      20*pump_cap(y)*(1/4)        =g=  sum(i$(ord(i)<ord(h)),0.8*PUMP(y,w,i)-GENE('step',y,w,i)) ;
reservoir_pos(y,w,h)..          20*pump_cap(y)*(3/4)        =g=  sum(i$(ord(i)<ord(h)),GENE('step',y,w,i)-0.8*PUMP(y,w,i)) ;

reserves_required(y,w,h)..      sum(tec_dispa, RES(tec_dispa, y,w,h) )  =e= sum(tec_vre, epsilon(tec_vre)*CAPA(tec_vre,y) );

obj..                           COST =e= ( sum((tec,y), CAPA(tec,y) * fixed_costs(y,tec) ) +
                                            sum((tec,y,w,h), GENE(tec,y,w,h) * variable_costs(y,tec) * (365/42) / 1000 ) ) / 1000000 ;


*==========================================================================
*  FIX INITIAL VALUES
*==========================================================================




*=====================
* Initial values
*=====================
CAPA.l(tec_end,y) = init_cap(tec_end) ;

GENE.l(tec_dispa,y,w,h) = CAPA.l(tec_dispa,y)*loadfactor(w,tec_dispa) ;
GENE.l(tec_vre,y,w,h)   = CAPA.l(tec_vre,y)*vre_profiles(tec_vre,w,h) ;

COST.l = ( sum((tec,y), CAPA.l(tec,y) * fixed_costs(y,tec))
                                         + sum((tec,y,w,h), GENE.l(tec,y,w,h)*variable_costs(y,tec)) )/1000 ;

CAPA.up(tec_end,y) = 200 ;

Display COST.l;

*=====================
* Exogenous values
*=====================

*Capacités exogènes
CAPA.fx(tec_ex,y) = capa_exo(y,tec_ex) ;

*Run-of-river
GENE.fx('river',y,w,h) = river(w,h) ;

*Capacités initiales
CAPA.fx(tec_end,y)$(ord(y)=1) = init_cap(tec_end) ;

*Investissement dans Flamanville
INVE.lo('nuc_new','2021') = 1.6 ;


*==========================================================================
*  MODEL OPTIONS
*==========================================================================

* Load the initial values (works only if a file exists)
$If exist flore_p.gdx execute_loadpoint 'flore_p';

MODEL flore /all/ ;

  option solvelink=2;
  option RESLIM = 1000000;
  option lp=cplex;
* Saves the results in a gdx for next iteration
  option Savepoint=1;
* Replace the results if some already exist
  option solveopt = replace ;
*  option iterLim = 0 ;

* Reduces the size of the .lst file
*This stops the echo print of the input
$offlisting
*This stops the print of a complete cros-reference list of symbols
$offsymxref
*This stops the print of the column listing
option limcol = 0;
*This stops the print of the equation listing
option limrow = 0;
*?
option SOLPRINT = OFF ;



*==========================================================================
*  SOLVE AND OUTPUTS
*==========================================================================

Solve flore using lp minimizing COST ;

Display INVE.l;

Execute_unload    '../outputs/%demand%_%enr%enr_%retro%ret_%new%new_%CO2%CO2';


