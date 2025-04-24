
library(dagitty)

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

adjustmentSets(dag_cwsd, exposure = "Z_t2", outcome = "Y_t2")

paths(dag_cwsd, from = "Z_t2", to = "Y_t2")

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

adjustmentSets(dag_cwsd_no_dir, exposure = "Z_t2", outcome = "Y_t2")

paths(dag_cwsd_no_dir, from = "Z_t2", to = "Y_t2")

## Sequential Randomization
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

adjustmentSets(dag_sr, exposure = "Z_t2", outcome = "Y_t2")

paths(dag_sr, from = "Z_t2", to = "Y_t2")

dag_sr_no_dir <- dagitty("dag {
  Z_t1 -> Y_t1
  Z_t1 -> X_t2
  Z_t2 -> Y_t2
  X_t1 -> Y_t1
  X_t1 -> X_t2
  Y_t1 -> X_t2
  X_t2 -> Y_t2
}")

adjustmentSets(dag_sr_no_dir, exposure = "Z_t2", outcome = "Y_t2")

paths(dag_sr_no_dir, from = "Z_t2", to = "Y_t2")

