## ---- include=FALSE------------------------------------------------------
library(assemblerr)

## ------------------------------------------------------------------------
m <- model() +
  prm_log_normal("emax", median = 10, var_log = 0.09) +
  prm_log_normal("ed50", median = 50, var_log = 0.09) +
  algebraic(effect~emax*dose/(ed50 + dose)) +
  obs_additive(~effect, var_add = 1)


## ------------------------------------------------------------------------
check(m)

## ------------------------------------------------------------------------
m <- m + 
  input_variable("dose")

## ------------------------------------------------------------------------
check(m)

## ------------------------------------------------------------------------
m <- m +
  prm_no_var("emax", value = 10)

## ------------------------------------------------------------------------
render(m)

## ------------------------------------------------------------------------
render(m)

## ---- eval=FALSE---------------------------------------------------------
#  setwd(tempdir())
#  render(m, filename = "run1.mod")

## ------------------------------------------------------------------------
render(
  m, 
  options = assemblerr_options(
    prm.use_mu_referencing = TRUE
  )
)

## ------------------------------------------------------------------------
render(m, 
       tasks = tsk_estimation(algorithm = "imp", se = TRUE))

## ------------------------------------------------------------------------
render(m, 
       tasks = tsk_estimation() + tsk_output(filename = "sdtab", variables = vars_prms()))

## ------------------------------------------------------------------------
m2 <- model() +
  prm_log_normal("v", median = 100, var_log = 0.09) +
  prm_log_normal("cl", median = 10, var_log = 0.09) +
  prm_log_normal("emax", median = 10, var_log = 0.09) +
  prm_log_normal("ec50", median = 1, var_log = 0.09) +
  compartment("central", volume = "v") +
  flow(from = "central", definition = ~cl*C) +
  obs_additive(effect~emax*C["central"]/(ec50 + C["central"]), var_add = 1)

## ------------------------------------------------------------------------
render(m2)

## ------------------------------------------------------------------------
pkm <- pk_model() +
  pk_absorption_fo() +
  pk_distribution_1cmp() +
  pk_elimination_linear() +
  obs_additive(conc~C["central"]) 

## ------------------------------------------------------------------------
render(
  model = pkm, 
  tasks = tsk_estimation("imp") + tsk_output("sdtab", variables = vars_prms()),
  options = assemblerr_options(prm.use_mu_referencing = TRUE)
)

## ------------------------------------------------------------------------
pkm <- pk_model() +
  pk_absorption_fo() +
  pk_distribution_1cmp(prm_vc = prm_no_var("v1", value = 100)) +
  pk_elimination_linear() +
  obs_additive(conc~C["central"]) 

render(pkm)

## ------------------------------------------------------------------------
pkm <- pk_model() +
  pk_absorption_fo() +
  pk_distribution_1cmp() +
  pk_elimination_linear() +
  prm_no_var("vc", value = 100) +
  obs_additive(conc~C["central"]) 
  
render(pkm)

