* referee2_replicate_main_results.do — REFEREE 2 INDEPENDENT REPLICATION
* =====================================================================
* Independent Stata replication of the author's diff1/diff2/diff3 DiD
* specifications, run on the panels exported from the author's .RData.
* Engine: reghdfe (absorb state + year^week, cluster on state, aweights
* = total_population). A third, independent engine alongside R (fixest)
* and Python (statsmodels).
*
* Run:  StataMP-64 -b do Code/replication/referee2_replicate_main_results.do

clear all
set more off

local exp "Code/replication/exported"

* Collect results across all specs into one CSV ------------------------------
tempname pf
tempfile results_dta
postfile `pf' str48 outcome double(coef se tval pval) long(n nclust) ///
    using "`results_dta'", replace

* ---------- program to run one spec and post the treat row -----------------
program define _runspec, rclass
    args dvexpr controls absorb cluster wt label
    * (kept simple: we call reghdfe inline below instead, see macros)
end

* Helper macros
local C1 "median_income percent_poverty cases initial_claims insured_unemployment_rate continued_claims puaIC"
local C2 "median_income cases initial_claims insured_unemployment_rate continued_claims puaIC"

* ======================= diff1 (weekCamp) ==================================
import delimited "`exp'/weekCamp.csv", clear varnames(1) case(preserve)
egen yw = group(year week)
encode state, gen(state_id)

foreach dv in campaigns donationsTotal amountTotal {
    capture drop ldv
    gen double ldv = log(`dv')
    reghdfe ldv treat `C1' [aweight=total_population], ///
        absorb(state_id yw) vce(cluster state_id)
    post `pf' ("diff1_log_`dv'") (_b[treat]) (_se[treat]) ///
        (_b[treat]/_se[treat]) (2*ttail(e(df_r), abs(_b[treat]/_se[treat]))) ///
        (e(N)) (e(N_clust))
}

* ======================= diff2 (weekLabel, log(x+1)) =======================
import delimited "`exp'/weekLabel.csv", clear varnames(1) case(preserve)
egen yw = group(year week)
encode state, gen(state_id)

foreach c in num_emergency num_medical num_memorial num_financial_emergency ///
             num_family num_volunteer num_community num_business num_education {
    capture drop ldv
    gen double ldv = log(`c' + 1)
    reghdfe ldv treat `C2' [aweight=total_population], ///
        absorb(state_id yw) vce(cluster state_id)
    post `pf' ("diff2_log_`c'_p1") (_b[treat]) (_se[treat]) ///
        (_b[treat]/_se[treat]) (2*ttail(e(df_r), abs(_b[treat]/_se[treat]))) ///
        (e(N)) (e(N_clust))
}

* ======================= diff3 (post-launch subset) ========================
preserve
keep if (week >= 40 & year == 2020) | (year == 2021)
capture drop ldv
gen double ldv = log(num_financial_emergency + 1)
reghdfe ldv treat `C2' [aweight=total_population], ///
    absorb(state_id yw) vce(cluster state_id)
post `pf' ("diff3_log_num_financial_emergency_p1_postlaunch") (_b[treat]) ///
    (_se[treat]) (_b[treat]/_se[treat]) ///
    (2*ttail(e(df_r), abs(_b[treat]/_se[treat]))) (e(N)) (e(N_clust))
restore

postclose `pf'

* Echo the collected results and write a real CSV
use "`results_dta'", clear
export delimited using "`exp'/referee_stata_results.csv", replace
list, clean noobs
display "STATA_REPLICATION_DONE"
