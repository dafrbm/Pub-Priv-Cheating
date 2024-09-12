# Preparación
rm(list = ls())

# Fijando directorios de trabajo

Data<-"C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/public and private cheating/power calculations/data"

Analysis<-"C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/public and private cheating/power calculations/analysis"

Results<-"C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/public and private cheating/power calculations/results"

#Paquetes necesarios
#rdss me da un problema para descargar, cualquier cosa desmarcar las dos
#líneas de abajo
#library(remotes)
#remotes::install_github("DeclareDesign/rdss")

library(DeclareDesign)
library(tidyverse)
library(dplyr)
library(haven)
library(scales)
library(rdss)
library(modelsummary)
library(knitr)

#Original approach (Declare Design guides)

M1 <-
  declare_model(
    N = 100,
    type = 
      rep(c("Always-Taker", "Never-Taker", "Complier", "Defier"),
          c(0.2, 0.2, 0.6, 0.0)*N),
    female = rbinom(N, 1, prob = 0.51),
    age = sample(18:80, N, replace = TRUE),
    income = sample(1:10, N, replace = TRUE),
    educ = sample(1:16, N, replace = TRUE),
    U = runif(N),
    corp_perc = (rnorm(N) + U +
                   0.008 *age  +
                   0.02 * female +
                   -0.1 * income +
                   0.05 * educ),
    # potential outcomes of Y with respect to D
    potential_outcomes(
      Y ~ case_when(
        type == "Always-Taker" ~ corp_perc - 0.025 - 0.05 * D,
        type == "Never-Taker" ~ corp_perc + 0.075 - 0.025 * D,
        type == "Complier" ~ corp_perc + 0.025 + 0.05 * D,
        type == "Defier" ~ corp_perc - 0.025 - 0.05 * D
      ),
      conditions = list(D = c(0, 1))
    ),
    # potential outcomes of D with respect to Z
    potential_outcomes(
      D ~ case_when(
        Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
        Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
        Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
        Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
      ),
      conditions = list(Z = c(0, 1))
    ),
    potential_outcomes(
      Y ~ case_when(
        Z == 1 & type == "Always-Taker" ~ likert_cut(corp_perc - 0.025 - 0.050 * 1),
        Z == 1 & type == "Never-Taker" ~ likert_cut(corp_perc + 0.075 - 0.025 * 0),
        Z == 1 & type == "Complier" ~ likert_cut(corp_perc + 0.025 + 0.050 * 1),
        Z == 1 & type == "Defier" ~ likert_cut(corp_perc - 0.025 - 0.050 * 0),
        Z == 0 & type == "Always-Taker" ~ likert_cut(corp_perc - 0.025 - 0.050 * 1),
        Z == 0 & type == "Never-Taker" ~ likert_cut(corp_perc + 0.075 - 0.025 * 0),
        Z == 0 & type == "Complier" ~ likert_cut(corp_perc + 0.025 + 0.050 * 0),
        Z == 0 & type == "Defier" ~ likert_cut(corp_perc - 0.025 - 0.050 * 1)
      ),
      conditions = list(Z = c(0, 1))
    )
  ) +
  declare_inquiry(
    ATE = mean(Y_D_1 - Y_D_0),
    CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"])) +
  declare_assignment(Z = conduct_ra(N = N)) +
  declare_measurement(D = reveal_outcomes(D ~ Z),
                      Y = reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    .method = iv_robust,
    inquiry = c("ATE", "CACE"),
    label = "Two stage least squares"
  ) +
  declare_estimator(
    Y ~ D,
    .method = lm_robust,
    inquiry = c("ATE", "CACE"),
    label = "As treated"
  ) +
  declare_estimator(
    Y ~ D,
    .method = lm_robust,
    inquiry = c("ATE", "CACE"),
    subset = D == Z,
    label = "Per protocol"
  )

#Diagnosis

diagnosis_M1 <-
  diagnose_design(
    M1,
    diagnosands = declare_diagnosands(
      bias = mean(estimate - estimand),
      true_se = sd(estimate),
      power = mean(p.value <= 0.05),
      coverage = mean(estimand <= conf.high &
                        estimand >= conf.low)
    )
  )

summary(diagnosis_M1)

#Simulaciones aumentando sample size

designs <- redesign(M1, N = seq(from=100, to=1000, by=50))
designs <- diagnose_designs(designs)

sum<-summary(designs)

write.table(summary(designs), file=paste0(Data, "/summary.txt"))

#Approach 2 (Códigos de Jake Bowers)

prob_comply <- .45
tau <- .2
N <- 100

the_pop <- declare_population(
  N = N,
  u0 = rnorm(N),
  u = ifelse(u0<=0,0,u0), ## truncated Normal
  type = sample(c("Always-Taker", "Never-Taker", "Complier", "Defier"), N,
                replace = TRUE,
                prob = c(.1, 1 - unique(prob_comply), unique(prob_comply), 0)
  )
)

d_po <- declare_potential_outcomes(
  D ~ case_when(
    Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
    Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
    Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1,
    Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1
  )
)

y_po_d <- declare_potential_outcomes(
  Y ~ tau * sd(u) * D + u, assignment_variables = "D" # c("D", "Z")
)

y_po_z <- declare_potential_outcomes(Y~(tau/2)*sd(u)*Z+u,assignment_variables=c("D","Z"))

the_assign <- declare_assignment(Z=conduct_ra(N=N,m=N/2))
## the_assign <- declare_assignment(assignment_variable = "Z")

# thereveal <- declare_reveal(Y, Z)
#d_reveal <- declare_reveal(D, assignment_variable = "Z")
#y_reveal <- declare_reveal(Y, assignment_variables = c("D", "Z"))

d_reveal <- declare_measurement(D=reveal_outcomes(D~Z))
y_reveal <- declare_measurement(Y=reveal_outcomes(Y~D+Z))

base_design <- the_pop + d_po + y_po_d + y_po_z + the_assign +  d_reveal + y_reveal

dat0 <- draw_data(base_design)

tempdat <- dat0[1:2, -1]
names(tempdat)[5] <- "pZ"
names(tempdat) <- gsub("_", "", names(tempdat))
set.seed(123455)
sample_n(dat0,15)

itt_y <- difference_in_means(Y ~ Z, data = dat0)
itt_y
itt_d <- difference_in_means(D ~ Z, data = dat0)
itt_d

cace_est <- iv_robust(Y ~ D | Z, data = dat0)
cace_est
## Notice same as below:
coef(itt_y)[["Z"]] / coef(itt_d)[["Z"]]

estimands <- declare_inquiry(
  CACE =     mean(Y_D_1[type=="Complier"] - Y_D_0[type=="Complier"]),
  ITT_y =    mean(  ( (Y_D_1_Z_1 + Y_D_0_Z_1)/2 )  - ( (Y_D_1_Z_0 + Y_D_0_Z_0)/2 ) ),
  ITT_d= mean(D_Z_1) - mean(D_Z_0))

estimator_cace <- declare_estimator(Y ~ D | Z, .method=iv_robust,inquiry=c("CACE"), label="iv_robust")
estimator_itt_y <- declare_estimator(Y ~ Z, inquiry = "ITT_y", .method = lm_robust, label =  "diff means ITT")
estimator_pp <- declare_estimator(Y ~ D, inquiry = "CACE", .method = lm_robust, label =  "per-protocol")
estimator_itt_d <- declare_estimator(D ~ Z, inquiry = "ITT_d", .method = lm_robust, label =  "diff means ITT_D")

full_design <- base_design + estimands+
  estimator_cace + estimator_itt_y + estimator_itt_d + estimator_pp

draw_estimands(full_design)
draw_estimates(full_design)[,c("estimator","term","estimate","std.error","outcome","inquiry")]

full_designs_by_size <-
  redesign(full_design,N=c(50,100,200,1000),prop_comply=c(.2,.5,.8))

dat_n20 <- draw_data(full_designs_by_size[["design_1"]])

my_diagnosands <-
  diagnose_design(
    full_design,
  declare_diagnosands(
    mean_estimand = mean(estimand),
    mean_estimate = mean(estimate),
    bias = mean(estimate - estimand),
    rmse = sqrt(mean((estimate - estimand) ^ 2)),
    power = mean(p.value <= alpha),
    coverage = mean(estimand <= conf.high & estimand >= conf.low),
    sd_estimate = sqrt(pop.var(estimate)),
    mean_se = mean(std.error)
  )
  )

summary(my_diagnosands)
