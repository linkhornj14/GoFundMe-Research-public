* dcdh_episodes.do
* =====================================================================
* de Chaisemartin-D'Haultfoeuille (did_multiplegt_dyn) robustness check
* for the non-absorbing composite `treat`, estimated episode by episode.
*
* `treat` switches on and off several times. Only three of those switches
* are staggered across states and therefore carry identifying variation:
*   E1  FPUC-$600 onset        2020 wk 13-19  (window 2020 wk 1-31)
*   E2  LWA onset              2020 wk 33-45  (window 2020 wk 32-52)
*   E3  FPUC-$300 early end    2021 wk 25-32  (window 2021 wk 2-36)
* The FPUC-$600 end (2020 wk 32) and FPUC-$300 start (2021 wk 1) are
* common to nearly all states and are absorbed by the time effects.
* E2 starts at wk 32 so its pre-period does not straddle the uniform
* FPUC-$600 end between wk 31 and wk 32.
*
* Each episode's treatment is built from that program's own dates.
* E3 treatment = "benefits terminated" (switch in), so its sign is the
* effect of LOSING benefits; E1/E2 are the effect of GAINING benefits.
*
* Specs: "none" (headline) matches the paper's clean specification, which
* drops the post-treatment claims controls; the only other baseline
* controls (median income, poverty) are constant within year and absorbed.
* "ctrl" adds the baseline time-varying controls as a sensitivity check.
*
* Inputs : Code/replication/exported/weekCamp.csv, weekLabel.csv
* Output : Code/replication/exported/dcdh_episodes.csv
*          (table built by Code/CARES/dcdh_table.R)
* Run    : StataMP-64 /e do Code/CARES/dcdh_episodes.do   (from project root;
*          /e avoids the Windows end-of-batch dialog that blocks /b)
*          Batch use only: the final line closes Stata.
* Needs  : ssc install did_multiplegt_dyn
* =====================================================================

clear all
set more off
version 18

local exp "Code/replication/exported"
local ctrls "cases initial_claims insured_unemployment_rate continued_claims puaIC"

* --- Build the weekly state panel ------------------------------------
import delimited "`exp'/weekLabel.csv", clear case(preserve)
keep state year week num_financial_emergency num_business
tempfile lab
save `lab'

import delimited "`exp'/weekCamp.csv", clear case(preserve)
merge 1:1 state year week using `lab'
assert _merge == 3
drop _merge
drop if state == "DC"                 // no program dates; 7 rows only
destring fWeek fWeek1End lWeekStart lWeekEnd fWeek2 fWeek2End `ctrls', ///
    replace force                     // R exports missing as "NA"

* Program dates must be present (SD has no LWA dates: it did not take LWA)
assert !missing(fWeek, fWeek2End)
assert !missing(lWeekEnd) if !missing(lWeekStart)
tab fWeek2End                         // non-early states end at wk 36

* Zero-campaign state-weeks are absent from the panel (not zero rows).
* Report any zero donation/amount weeks, which ln() drops as the baseline does.
count if campaigns == 0 | donationsTotal == 0 | amountTotal == 0

egen sid = group(state)
gen t = (year - 2020)*53 + week

gen y_campaigns = ln(campaigns)
gen y_donations = ln(donationsTotal)
gen y_amount    = ln(amountTotal)
gen y_finemerg  = ln(1 + num_financial_emergency)
gen y_business  = ln(1 + num_business)

* Episode treatments from program dates
gen D1 = (year == 2020) & (week >= fWeek)
gen D2 = (year == 2020) & !missing(lWeekStart) & (week >= lWeekStart) & (week <= lWeekEnd)
gen D3 = (year == 2021) & (week > fWeek2End)

gen byte w1 = (year == 2020) & inrange(week, 1, 31)
gen byte w2 = (year == 2020) & inrange(week, 32, 52)
gen byte w3 = (year == 2021) & inrange(week, 2, 36)

* --- Estimation loop -------------------------------------------------
tempname pf
postfile `pf' str3 episode str12 outcome str4 spec ///
    double(eff1 se_eff1 avg se_avg p_placebo) long(n_switch) ///
    using "`exp'/dcdh_episodes.dta", replace

local out1 "campaigns donations amount"
local out2 "campaigns donations amount"
local out3 "finemerg business campaigns donations amount"

forvalues e = 1/3 {
    foreach y of local out`e' {
        foreach spec in none ctrl {
            local c = cond("`spec'" == "ctrl", "controls(`ctrls')", "")
            di as txt _n "=== E`e' | `y' | `spec' ==="
            capture noisily did_multiplegt_dyn y_`y' sid t D`e' if w`e', ///
                effects(4) placebo(4) weight(total_population) cluster(sid) ///
                `c' graph_off save_sample
            if _rc == 0 {
                assert !missing(e(Effect_1), e(se_effect_1), e(Av_tot_effect))
                * unweighted count of switching states (e() counts are weighted)
                if `e' == 1 & "`y'" == "campaigns" & "`spec'" == "none" {
                    tab _did_sample, missing
                    di as txt "E1 states not used by the estimator:"
                    list state if w1 & week == 1 & missing(_did_sample), clean noobs
                }
                egen byte _tag = tag(sid) if !missing(_did_sample) & D`e' & w`e'
                qui count if _tag == 1
                local nsw = r(N)
                drop _tag _did_sample
                capture drop _effect
                post `pf' ("E`e'") ("`y'") ("`spec'") ///
                    (e(Effect_1)) (e(se_effect_1)) ///
                    (e(Av_tot_effect)) (e(se_avg_total_effect)) ///
                    (e(p_jointplacebo)) (`nsw')
            }
            else {
                capture drop _did_sample
                capture drop _effect
                post `pf' ("E`e'") ("`y'") ("`spec'") (.) (.) (.) (.) (.) (.)
            }
        }
    }
}
postclose `pf'

use "`exp'/dcdh_episodes.dta", clear
list, clean noobs
export delimited using "`exp'/dcdh_episodes.csv", replace
erase "`exp'/dcdh_episodes.dta"

exit, clear STATA
