"""
referee2_replicate_main_results.py — REFEREE 2 INDEPENDENT REPLICATION
=====================================================================
Independent Python replication of the author's main DiD specifications
(diff1 / diff2 / diff3 from Code/CARES/regressionsCARES.R), run on the
EXACT analytic panels exported from the author's .RData workspace.

Engine: statsmodels WLS with fixed effects entered as dummies and
cluster-robust SEs clustered on state. This is a deliberately DIFFERENT
engine from R's fixest (orthogonal implementation) so that any agreement
is meaningful and any disagreement flags a bug.

Specifications replicated:
  diff1: c(log(campaigns), log(donationsTotal), log(amountTotal))
         ~ treat + median_income + percent_poverty + cases + initial_claims
           + insured_unemployment_rate + continued_claims + puaIC
         | state + year^week        cluster=state  weights=total_population
  diff2: log(num_<cat> + 1) ~ treat + median_income + cases + initial_claims
           + insured_unemployment_rate + continued_claims + puaIC
         | state + year^week        (NOTE: no percent_poverty)
  diff3: log(num_financial_emergency + 1) on the post-launch subset
         (week>=40 & year==2020) | year==2021

Run:  python Code/replication/referee2_replicate_main_results.py
"""

import os
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf

HERE = os.path.dirname(os.path.abspath(__file__))
EXP = os.path.join(HERE, "exported")
OUT = os.path.join(EXP, "referee_python_results.csv")

DIFF1_CONTROLS = ["median_income", "percent_poverty", "cases", "initial_claims",
                  "insured_unemployment_rate", "continued_claims", "puaIC"]
DIFF2_CONTROLS = ["median_income", "cases", "initial_claims",
                  "insured_unemployment_rate", "continued_claims", "puaIC"]


def prep(df):
    """Build the state + year^week fixed-effect keys."""
    df = df.copy()
    # year^week interaction: one FE per (year, week) cell, exactly like fixest's year^week
    df["yw"] = df["year"].astype(str) + "_" + df["week"].astype(int).astype(str)
    df["state"] = df["state"].astype(str)
    return df


def run_fe_wls(df, dv_expr, controls, label):
    """WLS with state + yw dummies, cluster-robust SE on state."""
    d = prep(df)
    rhs = "treat + " + " + ".join(controls) + " + C(state) + C(yw)"
    formula = f"{dv_expr} ~ {rhs}"
    # weights = total_population (analytic weights, as in fixest weights=~total_population)
    model = smf.wls(formula, data=d, weights=d["total_population"])
    res = model.fit(cov_type="cluster", cov_kwds={"groups": d["state"]})
    return {
        "outcome": label,
        "coef": res.params.get("treat", np.nan),
        "se": res.bse.get("treat", np.nan),
        "tval": res.tvalues.get("treat", np.nan),
        "pval": res.pvalues.get("treat", np.nan),
        "n": int(res.nobs),
        "n_clusters": d["state"].nunique(),
    }


def main():
    weekCamp = pd.read_csv(os.path.join(EXP, "weekCamp.csv"))
    weekLabel = pd.read_csv(os.path.join(EXP, "weekLabel.csv"))

    rows = []

    # ---- diff1 -------------------------------------------------------------
    weekCamp = weekCamp.assign(
        log_campaigns=np.log(weekCamp["campaigns"]),
        log_donationsTotal=np.log(weekCamp["donationsTotal"]),
        log_amountTotal=np.log(weekCamp["amountTotal"]),
    )
    rows.append(run_fe_wls(weekCamp, "log_campaigns", DIFF1_CONTROLS, "diff1_log_campaigns"))
    rows.append(run_fe_wls(weekCamp, "log_donationsTotal", DIFF1_CONTROLS, "diff1_log_donationsTotal"))
    rows.append(run_fe_wls(weekCamp, "log_amountTotal", DIFF1_CONTROLS, "diff1_log_amountTotal"))

    # ---- diff2 (log(x+1), no percent_poverty) ------------------------------
    cats = ["num_emergency", "num_medical", "num_memorial", "num_financial_emergency",
            "num_family", "num_volunteer", "num_community", "num_business", "num_education"]
    for c in cats:
        dv = f"log_{c}_p1"
        weekLabel[dv] = np.log(weekLabel[c] + 1)
        rows.append(run_fe_wls(weekLabel, dv, DIFF2_CONTROLS, f"diff2_{dv}"))

    # ---- diff3 (post-launch subset) ----------------------------------------
    official = weekLabel[((weekLabel["week"] >= 40) & (weekLabel["year"] == 2020)) |
                         (weekLabel["year"] == 2021)].copy()
    official["log_num_financial_emergency_p1"] = np.log(official["num_financial_emergency"] + 1)
    rows.append(run_fe_wls(official, "log_num_financial_emergency_p1", DIFF2_CONTROLS,
                           "diff3_log_num_financial_emergency_p1_postlaunch"))

    out = pd.DataFrame(rows)
    pd.set_option("display.float_format", lambda x: f"{x:.8f}")
    print(out.to_string(index=False))
    out.to_csv(OUT, index=False)
    print(f"\nWrote {OUT}")


if __name__ == "__main__":
    main()
