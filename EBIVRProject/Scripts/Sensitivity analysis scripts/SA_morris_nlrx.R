# Cara Gallagher
# February 7th, 2023
# Energy Budget with Individual Variation project
# Morris screening with nlrx package

#------------------------------------------------#
####  Packages: #### 
library(tidyverse)
library(nlrx, lib.loc= "/home/gallagher/R/x86_64-pc-linux-gnu-library/4.0/")

library(future)
library(future.batchtools)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)
library(readxl)

#------------------------------------------------#

#### Parameter prior values ####  

# specify netlogo, model, and output paths
netlogopath <- file.path("/srv/ag-jeltsch/netlogo/6.2.2")
modelpath <- file.path("/home/gallagher/Sensitivity/EBIVModelSA.nlogo")
outpath <- file.path("/import/ecoc9j/data-jeltsch/gallagher")

Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-1.11.0-openjdk-amd64")

parameter_combos <- read_csv("Data/Sensitivity analysis/Sensitivity_analysis.csv")


# initialize model
nl <- nl(nlversion = "6.2.2",
         nlpath = "",#netlogopath,
         modelpath = "",#modelpath,
         jvmmem = 3072)

# set up experiment
nl@experiment <- experiment(expname = "SA",
                            outpath = "",#outpath,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = "go", 
                            runtime = 38833,
                            metrics = c(
                                        "SA.BMadult", 
                                        "SA.BMemb", 
                                        "SA.BMwean", 
                                        "SA.age1stbirth", 
                                        "SA.LpY", 
                                        "SA.LSb", 
                                        "SA.dens"),
                            variables = list(
                              "perc.resource.cells" = list(min = parameter_combos[1,3], max = parameter_combos[1,7], step = (parameter_combos[1,7] - parameter_combos[1,3]) / 4, qfun="qunif"),
                              "max.resources.base" = list(min = parameter_combos[2,3], max = parameter_combos[2,7], step = (parameter_combos[2,7] - parameter_combos[2,3]) / 4, qfun="qunif"),
                              "r.growth.ts" = list(min = parameter_combos[3,3], max = parameter_combos[3,7], step = (parameter_combos[3,7] - parameter_combos[3,3]) / 4, qfun="qunif"),
                              "fragmentation.level" = list(min = parameter_combos[4,3], max = parameter_combos[4,7], step = (parameter_combos[4,7] - parameter_combos[4,3]) / 4, qfun="qunif"),
                              "n.animals" = list(min = parameter_combos[5,3], max = parameter_combos[5,7], step = (parameter_combos[5,7] - parameter_combos[5,3]) / 4, qfun="qunif"),
                              "speed.mean" = list(min = parameter_combos[6,3], max = parameter_combos[6,7], step = (parameter_combos[6,7] - parameter_combos[6,3]) / 4, qfun="qunif"),
                              "speed.max" = list(min = parameter_combos[7,3], max = parameter_combos[7,7], step = (parameter_combos[7,7] - parameter_combos[7,3]) / 4, qfun="qunif"),
                              "HR.r.min" = list(min = parameter_combos[8,3], max = parameter_combos[8,7], step = (parameter_combos[8,7] - parameter_combos[8,3]) / 4, qfun="qunif"),
                              "HR.r.max" = list(min = parameter_combos[9,3], max = parameter_combos[9,7], step = (parameter_combos[9,7] - parameter_combos[9,3]) / 4, qfun="qunif"),
                              "B.0" = list(min = parameter_combos[10,3], max = parameter_combos[10,7], step = (parameter_combos[10,7] - parameter_combos[10,3]) / 4, qfun="qunif"),
                              "gamma." = list(min = parameter_combos[11,3], max = parameter_combos[11,7], step = (parameter_combos[11,7] - parameter_combos[11,3]) / 4, qfun="qunif"),
                              "intercept.pcot" = list(min = parameter_combos[12,3], max = parameter_combos[12,7], step = (parameter_combos[12,7] - parameter_combos[12,3]) / 4, qfun="qunif"),
                              "slope.pcot" = list(min = parameter_combos[13,3], max = parameter_combos[13,7], step = (parameter_combos[13,7] - parameter_combos[13,3]) / 4, qfun="qunif"),
                              "intercept.icot" = list(min = parameter_combos[14,3], max = parameter_combos[14,7], step = (parameter_combos[14,7] - parameter_combos[14,3]) / 4, qfun="qunif"),
                              "slope.icot" = list(min = parameter_combos[15,3], max = parameter_combos[15,7], step = (parameter_combos[15,7] - parameter_combos[15,3]) / 4, qfun="qunif"),
                              "n.emb.mid" = list(min = parameter_combos[16,3], max = parameter_combos[16,7], step = (parameter_combos[16,7] - parameter_combos[16,3]) / 4, qfun="qunif"),
                              "emb.growth.c" = list(min = parameter_combos[17,3], max = parameter_combos[17,7], step = (parameter_combos[17,7] - parameter_combos[17,3]) / 4, qfun="qunif"),
                              "emb.mass.init" = list(min = parameter_combos[18,3], max = parameter_combos[18,7], step = (parameter_combos[18,7] - parameter_combos[18,3]) / 4, qfun="qunif"),
                              "emb.mass.inf" = list(min = parameter_combos[19,3], max = parameter_combos[19,7], step = (parameter_combos[19,7] - parameter_combos[19,3]) / 4, qfun="qunif"),
                              "percent.fat.emb" = list(min = parameter_combos[20,3], max = parameter_combos[20,7], step = (parameter_combos[20,7] - parameter_combos[20,3]) / 4, qfun="qunif"),
                              "percent.pro.emb" = list(min = parameter_combos[21,3], max = parameter_combos[21,7], step = (parameter_combos[21,7] - parameter_combos[21,3]) / 4, qfun="qunif"),
                              "ED.pl"= list(min = parameter_combos[22,3], max = parameter_combos[22,7], step = (parameter_combos[22,7] - parameter_combos[22,3]) / 4, qfun="qunif"),
                              "DE.pl" = list(min = parameter_combos[23,3], max = parameter_combos[23,7], step = (parameter_combos[23,7] - parameter_combos[23,3]) / 4, qfun="qunif"),
                              "preg.prob.const" = list(min = parameter_combos[24,3], max = parameter_combos[24,7], step = (parameter_combos[24,7] - parameter_combos[24,3]) / 4, qfun="qunif"),
                              "preg.prob.mid" = list(min = parameter_combos[25,3], max = parameter_combos[25,7], step = (parameter_combos[25,7] - parameter_combos[25,3]) / 4, qfun="qunif"),
                              "off.BMR.red" = list(min = parameter_combos[26,3], max = parameter_combos[26,7], step = (parameter_combos[26,7] - parameter_combos[26,3]) / 4, qfun="qunif"),
                              "off.growth.eff" = list(min = parameter_combos[27,3], max = parameter_combos[27,7], step = (parameter_combos[27,7] - parameter_combos[27,3]) / 4, qfun="qunif"),
                              "milk.prod.eff" = list(min = parameter_combos[28,3], max = parameter_combos[28,7], step = (parameter_combos[28,7] - parameter_combos[28,3]) / 4, qfun="qunif"),
                              "lact.prob.const" = list(min = parameter_combos[29,3], max = parameter_combos[29,7], step = (parameter_combos[29,7] - parameter_combos[29,3]) / 4, qfun="qunif"),
                              "lact.prob.mid" = list(min = parameter_combos[30,3], max = parameter_combos[30,7], step = (parameter_combos[30,7] - parameter_combos[30,3]) / 4, qfun="qunif"),
                              "ED.pro" = list(min = parameter_combos[31,3], max = parameter_combos[31,7], step = (parameter_combos[31,7] - parameter_combos[31,3]) / 4, qfun="qunif"),
                              "DE.fat" = list(min = parameter_combos[32,3], max = parameter_combos[32,7], step = (parameter_combos[32,7] - parameter_combos[32,3]) / 4, qfun="qunif"),
                              "DE.pro" = list(min = parameter_combos[33,3], max = parameter_combos[33,7], step = (parameter_combos[33,7] - parameter_combos[33,3]) / 4, qfun="qunif"),
                              "growth.lm.prob.const" = list(min = parameter_combos[34,3], max = parameter_combos[34,7], step = (parameter_combos[34,7] - parameter_combos[34,3]) / 4, qfun="qunif"),
                              "growth.lm.prob.mid" = list(min = parameter_combos[35,3], max = parameter_combos[35,7], step = (parameter_combos[35,7] - parameter_combos[35,3]) / 4, qfun="qunif"),
                              "growth.lm.inf" = list(min = parameter_combos[36,3], max = parameter_combos[36,7], step = (parameter_combos[36,7] - parameter_combos[36,3]) / 4, qfun="qunif"),
                              "growth.lm.k" = list(min = parameter_combos[37,3], max = parameter_combos[37,7], step = (parameter_combos[37,7] - parameter_combos[37,3]) / 4, qfun="qunif"),
                              "stomach.fill.perc" = list(min = parameter_combos[38,3], max = parameter_combos[38,7], step = (parameter_combos[38,7] - parameter_combos[38,3]) / 4, qfun="qunif"),
                              "AE.food" = list(min = parameter_combos[39,3], max = parameter_combos[39,7], step = (parameter_combos[39,7] - parameter_combos[39,3]) / 4, qfun="qunif"),
                              "H.I.F" = list(min = parameter_combos[40,3], max = parameter_combos[40,7], step = (parameter_combos[40,7] - parameter_combos[40,3]) / 4, qfun="qunif"),
                              "DM.food" = list(min = parameter_combos[41,3], max = parameter_combos[41,7], step = (parameter_combos[41,7] - parameter_combos[41,3]) / 4, qfun="qunif"),
                              "ED.food" = list(min = parameter_combos[42,3], max = parameter_combos[42,7], step = (parameter_combos[42,7] - parameter_combos[42,3]) / 4, qfun="qunif"),
                              "ED.fat" = list(min = parameter_combos[43,3], max = parameter_combos[43,7], step = (parameter_combos[43,7] - parameter_combos[43,3]) / 4, qfun="qunif"),
                              "EC.pro" = list(min = parameter_combos[44,3], max = parameter_combos[44,7], step = (parameter_combos[44,7] - parameter_combos[44,3]) / 4, qfun="qunif"),
                              "gamma.mobilize" = list(min = parameter_combos[45,3], max = parameter_combos[45,7], step = (parameter_combos[45,7] - parameter_combos[45,3]) / 4, qfun="qunif"),
                              "perc.water.adi" = list(min = parameter_combos[46,3], max = parameter_combos[46,7], step = (parameter_combos[46,7] - parameter_combos[46,3]) / 4, qfun="qunif"),
                              "max.SL" = list(min = parameter_combos[47,3], max = parameter_combos[47,7], step = (parameter_combos[47,7] - parameter_combos[47,3]) / 4, qfun="qunif"),
                              "t.mating.start" = list(min = parameter_combos[48,3], max = parameter_combos[48,7], step = (parameter_combos[48,7] - parameter_combos[48,3]) / 4, qfun="qunif"),
                              "t.mating.end" = list(min = parameter_combos[49,3], max = parameter_combos[49,7], step = (parameter_combos[49,7] - parameter_combos[49,3]) / 4, qfun="qunif"),
                              "t.mature" = list(min = parameter_combos[50,3], max = parameter_combos[50,7], step = (parameter_combos[50,7] - parameter_combos[50,3]) / 4, qfun="qunif"),
                              "prob.ovul" = list(min = parameter_combos[51,3], max = parameter_combos[51,7], step = (parameter_combos[51,7] - parameter_combos[51,3]) / 4, qfun="qunif"),
                              "t.0" = list(min = parameter_combos[52,3], max = parameter_combos[52,7], step = (parameter_combos[52,7] - parameter_combos[52,3]) / 4, qfun="qunif"),
                              "t.gest" = list(min = parameter_combos[53,3], max = parameter_combos[53,7], step = (parameter_combos[53,7] - parameter_combos[53,3]) / 4, qfun="qunif"),
                              "t.nurs" = list(min = parameter_combos[54,3], max = parameter_combos[54,7], step = (parameter_combos[54,7] - parameter_combos[54,3]) / 4, qfun="qunif"),
                              "t.max.age" = list(min = parameter_combos[55,3], max = parameter_combos[55,7], step = (parameter_combos[55,7] - parameter_combos[55,3]) / 4, qfun="qunif"),
                              "surv.prob.const" = list(min = parameter_combos[56,3], max = parameter_combos[56,7], step = (parameter_combos[56,7] - parameter_combos[56,3]) / 4, qfun="qunif"),
                              "surv.prob.mid" = list(min = parameter_combos[57,3], max = parameter_combos[57,7], step = (parameter_combos[57,7] - parameter_combos[57,3]) / 4, qfun="qunif"),
                              "surv.mod.emb" = list(min = parameter_combos[58,3], max = parameter_combos[58,7], step = (parameter_combos[58,7] - parameter_combos[58,3]) / 4, qfun="qunif"),
                              "surv.mod.off" = list(min = parameter_combos[59,3], max = parameter_combos[59,7], step = (parameter_combos[59,7] - parameter_combos[59,3]) / 4, qfun="qunif"),
                              "winter.surv.mid" = list(min = parameter_combos[60,3], max = parameter_combos[60,7], step = (parameter_combos[60,7] - parameter_combos[60,3]) / 4, qfun="qunif")),
                            constants = list("debug" = "Sens"))

# create morris sim design
nl@simdesign <- simdesign_morris(nl=nl,
                                 morristype="oat",
                                 morrislevels=5,
                                 morrisr=50,
                                 morrisgridjump=2,
                                 nseeds=1)
print(nl)
eval_variables_constants(nl)


# Define plan for future environment:
bsub <- tweak(batchtools_slurm, 
              template = "/home/gallagher/calibration/slurm_batchtools.tmpl", # define name of slurm tmeplate on HPC filesystem
              resources = list(ntasks = 1,
                               ncpus = 10, 
                               job.name = "SANLRX",
                               log.file = "SANLRX1",
                               walltime = 10000, # define walltime
                               memory = "3072")) # define memory per cpu   




#plan(multisession, workers = 16)  
#plan(list(sequential, multisession), workers = 5)
plan(list(bsub, multisession))

results <- run_nl_all(nl)

setsim(nl, "simoutput") <- results #attaching simpout to our NetLogo object

saveRDS(nl, file.path(nl@experiment@outpath, "morris.rds"))


