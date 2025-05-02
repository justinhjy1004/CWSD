# -------------------------------------------------------------
# Author: Justin Ho
#
# Description: 
#   This script constructs directed acyclic graphs (DAGs) for
#   cross-over within-subject designs (CWSD) and sequential 
#   randomization scenarios, using the dagitty package in R.
#   For each DAG, we compute adjustment sets for estimating the 
#   causal effect of Z_t2 on Y_t2, and list all paths between 
#   treatment (Z_t2) and outcome (Y_t2).
#
# -------------------------------------------------------------

rm(list = ls())
library(dagitty)

##================================================
##    DAG of CWSD
##================================================

dag_cwsd <- dagitty("dag {
  Z_t1 -> Z_t2
  Z_t1 -> Y_t1
  Z_t1 -> X_t2
  Z_t1 -> Y_t2
  Z_t2 -> Y_t2
  X_t1 -> Y_t1
  X_t1 -> X_t2
  Y_t1 -> X_t2
  X_t2 -> Y_t2
}")

# All minimal sufficient adjustment sets for estimating the
# causal effect of Z_t2 on Y_t2 in the CWSD DAG
adjustmentSets(dag_cwsd, exposure = "Z_t2", outcome = "Y_t2")

# All causal paths from Z_t2 to Y_t2 (including direct and indirect)
# And if they are opened or closed
paths(dag_cwsd, from = "Z_t2", to = "Y_t2")


##================================================
##    DAG of CWSD without direct effects
##================================================

dag_cwsd_no_dir <- dagitty("dag {
  Z_t1 -> Z_t2
  Z_t1 -> Y_t1
  Z_t1 -> X_t2
  Z_t2 -> Y_t2
  X_t1 -> Y_t1
  X_t1 -> X_t2
  Y_t1 -> X_t2
  X_t2 -> Y_t2
}")

# All minimal sufficient adjustment sets
adjustmentSets(dag_cwsd_no_dir, exposure = "Z_t2", outcome = "Y_t2")
# All causal paths from Z_t2 to Y_t2
paths(dag_cwsd_no_dir, from = "Z_t2", to = "Y_t2")

##================================================
##    DAG of Sequential Randomization
##================================================

dag_sr <- dagitty("dag {
  Z_t1 -> Y_t1
  Z_t1 -> X_t2
  Z_t1 -> Y_t2
  Z_t2 -> Y_t2
  X_t1 -> Y_t1
  X_t1 -> X_t2
  Y_t1 -> X_t2
  X_t2 -> Y_t2
}")

# All minimal sufficient adjustment sets
adjustmentSets(dag_sr, exposure = "Z_t2", outcome = "Y_t2")
# All causal paths from Z_t2 to Y_t2
paths(dag_sr, from = "Z_t2", to = "Y_t2")


##============================================================
## DAG of Sequential Randomization (without direct effects)
##============================================================

dag_sr_no_dir <- dagitty("dag {
  Z_t1 -> Y_t1
  Z_t1 -> X_t2
  Z_t2 -> Y_t2
  X_t1 -> Y_t1
  X_t1 -> X_t2
  Y_t1 -> X_t2
  X_t2 -> Y_t2
}")

# All minimal sufficient adjustment sets
adjustmentSets(dag_sr_no_dir, exposure = "Z_t2", outcome = "Y_t2")
# All causal paths from Z_t2 to Y_t2
paths(dag_sr_no_dir, from = "Z_t2", to = "Y_t2")

