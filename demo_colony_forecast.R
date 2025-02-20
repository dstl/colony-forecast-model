#------------------------------------------------------------------------------
# (C) Crown Copyright 2025 Defence Science and Technology Laboratory UK
#
# Approval for wider use or release must be sought from:
#         Intellectual Property Department
#         Defence Science and Technology Laboratory
#         Porton Down, Salisbury, Wiltshire, SP4 0JQ.
#
# The software/data, including any associated documentation, is supplied without
# warranty of performance, and in particular Dstl does not warrant, guarantee
# or make any representations regarding the use or the results of the use of the
# software in terms of currentness, accuracy, reliability, correctness,
# merchantability, fitness for purpose or otherwise.
#------------------------------------------------------------------------------
# Script summary: This script is to demonstrate how to run the
#                 Marmoset Colony Stock Model
#
#------------------------------------------------------------------------------

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# WORKFLOW:
# - save the scripts into a single directory 
# - set simulation parameters
# - set parameters describing past data for the observation period
# - set parameters describing future plans for the forecast period
# - run the model for all scenarios of interest
# - analyse the outputs
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#------------------------------------------------------------------------------
# load scripts for running the model:

# USER INPUT: specify the full path of your working directory:

working_dir_path='/replace/with/your/working/directory/path'

setwd(working_dir_path)

# - assuming they are saved in the current working directory
source('./colony_forecast.R') # load functions to be called in model
source('./helper_functions.R') # load functions to be called in model

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# SIMULATION SET-UP
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

DEMO.START='2025-01-01' # start date for forecast (USER SPECIFIED)

NMONTH.DEMO=18 # number of months to run forecast (USER SPECIFIED)

NREPS.DEMO=1000 # number of replications of simulation to perform (USER SPECIFIED)

QUANTILES.DEMO=c(0, 25, 50, 75, 100) # quantiles/percentiles for output (USER SPECIFIED)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PARAMETERS DERIVED FROM PAST DATA FOR THE OBSERVATION PERIOD
#------------------------------------------------------------------------------
# - these parameters need to be determined from past colony data for an observation period
#     e.g. the previous year before the desired forecast period
# - derived by pre-processing data from the colony record-keeping system
# - the values below are dummy / illustrative / arbitrary
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# initial values for simulation:
I0.DEMO=100 # offspring (dummy value; would be taken from the state of the colony at the end of the observation period/ start of the forecast period)

A0.DEMO=100 # adults (dummy value; would be taken from the state of the colony at the end of the observation period/ start of the forecast period)

# - initial number of breeders is specified by 'breeding.pairs.planned' argument (see below)

# empirical distribution of litter sizes:
# - this would be extracted from real colony data for the observation period
LITTER.SIZE.DIST.DEMO=c(rep(1, 30), rep(2, 100), rep(3, 30)) # dummy values; mainly twins, but some singlets and triplets

# rate of transfers:
# - this would be calculated from data for the observation period
TRANSFER.RATE.DEMO=0.05 # dummy value

# per capita death rates (infants, breeders, adults)
# - these values would be derived from data for the observation period
DELTA.DEMO=c(delta.i=0.02, delta.a=0.01) # dummy values

# - N.B. by default, number of breeders is assumed constant, so delta.b is not included

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PARAMETERS DESCRIBING INTERVENTIONS DURING THE FORECAST PERIOD
#------------------------------------------------------------------------------
# - THE VALUES BELOW ARE DUMMY / ILLUSTRATIVE / ARBITRARY VALUES
# - you would need to determine values for these parameters from your own colony dataset
# - these parameters specify planned interventions in the colony, during the forecast period
#     e.g. changing the number of breeders; breeding control; issues; relocations
# - different cases/scenarios can be considered, to conduct 'what if?' analyses
#     e.g. compare the forecasts between maximal breeding vs. breeding control
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# number of breeding pairs:
BREEDING.PAIRS.DEMO.1=30 # single value --> constant number of breeding pairs maintained; dummy value

BREEDING.PAIRS.DEMO.2=30+2*c(0:11, 11:0)[1:NMONTH.DEMO] # vector provides planned number of breeding pairs during forecast period; dummy values

# breeding control (dummy values):
BREEDING.CONTROL.DEMO.1=1 # maximal; can be single value, model recycles value for each month

BREEDING.CONTROL.DEMO.2=c(1,1, rep(0.5,6), rep(0.1,16))[1:NMONTH.DEMO] # vector of length NMONTH.DEMO; breeding reduced to 50% in 5th month and 25% in 13th month; dummy values

# issues:
ISSUES.DEMO.1=0 # zero issues; can be single value, model recycles value for each month

ISSUES.DEMO.2=rep(c(0, 0, 0, 10), 6)[1:NMONTH.DEMO] # vector of length NMONTH.DEMO; 10 issues every 4 months; dummy values

# relocations:
RELOCATIONS.DEMO.1=0 # zero relocations; can be single value, model recycles value for each month

RELOCATIONS.DEMO.2=rep(c(0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0), 2)[1:NMONTH.DEMO] # vector of length NMONTH.DEMO; 20 relocations in 6th and 18th month; dummy values

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# RUN MODEL FOR TEST PARAMETERS
#------------------------------------------------------------------------------
# - below, the model is run for NREPS.DEMO replications, for a variety of 
#     illustrative scenarios, specified by combinations of parameters from above
# - the outputs are stored as a list, to make plotting slightly less laborious
# - brief summaries of each scenario are provided in comments in the list below
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

OUT.DEMO=list(
  # scenario 1
  # - constant number of breeding pairs; maximal breeding; no issues or relocations:
  colony_forecast(start.date=DEMO.START, nmonth=NMONTH.DEMO,
                  nreps=NREPS.DEMO,
                  I0=I0.DEMO, A0=A0.DEMO,
                  delta=DELTA.DEMO,
                  breeding.pairs.planned=BREEDING.PAIRS.DEMO.1, # constant number of breeding pairs
                  breeding.control=BREEDING.CONTROL.DEMO.1, # maximal breeding
                  lsz.dist=LITTER.SIZE.DIST.DEMO,
                  transfer.rate=TRANSFER.RATE.DEMO,
                  issues=ISSUES.DEMO.1, # no issues
                  relocations=RELOCATIONS.DEMO.1, # no relocations
                  quantiles=QUANTILES.DEMO,
                  write.log=FALSE
                  ),

  # scenario 2
  # - increasing number of breeding pairs; maximal breeding; no issues or relocations:
  colony_forecast(start.date=DEMO.START, nmonth=NMONTH.DEMO,
                  nreps=NREPS.DEMO,
                  I0=I0.DEMO, A0=A0.DEMO,
                  delta=DELTA.DEMO,
                  breeding.pairs.planned=BREEDING.PAIRS.DEMO.2, # increasing number of breeding pairs
                  breeding.control=BREEDING.CONTROL.DEMO.1, # maximal breeding
                  lsz.dist=LITTER.SIZE.DIST.DEMO,
                  transfer.rate=TRANSFER.RATE.DEMO,
                  issues=ISSUES.DEMO.1, # no issues
                  relocations=RELOCATIONS.DEMO.1, # no relocations
                  quantiles=QUANTILES.DEMO, 
                  write.log=FALSE),

  # scenario 3
  # constant number of breeding pairs; maximal breeding; with issues and relocations:
  colony_forecast(start.date=DEMO.START, nmonth=NMONTH.DEMO,
                  nreps=NREPS.DEMO,
                  I0=I0.DEMO, A0=A0.DEMO,
                  delta=DELTA.DEMO,
                  breeding.pairs.planned=BREEDING.PAIRS.DEMO.1, # constant number of breeding pairs
                  breeding.control=BREEDING.CONTROL.DEMO.1, # maximal breeding
                  lsz.dist=LITTER.SIZE.DIST.DEMO,
                  transfer.rate=TRANSFER.RATE.DEMO,
                  issues=ISSUES.DEMO.2, # with issues
                  relocations=RELOCATIONS.DEMO.2, # with relocations
                  quantiles=QUANTILES.DEMO, 
                  write.log=FALSE),

  # scenario 4
  # constant number of breeding pairs; with breeding control; no issues or relocations:
  colony_forecast(start.date=DEMO.START, nmonth=NMONTH.DEMO,
                  nreps=NREPS.DEMO,
                  I0=I0.DEMO, A0=A0.DEMO,
                  delta=DELTA.DEMO,
                  breeding.pairs.planned=BREEDING.PAIRS.DEMO.1, # constant number of breeding pairs
                  breeding.control=BREEDING.CONTROL.DEMO.2, # breeding control (100% --> 50% --> 25%)
                  lsz.dist=LITTER.SIZE.DIST.DEMO,
                  transfer.rate=TRANSFER.RATE.DEMO,
                  issues=ISSUES.DEMO.1, # no issues
                  relocations=RELOCATIONS.DEMO.1, # no relocations
                  quantiles=QUANTILES.DEMO, 
                  write.log=FALSE),

  # scenario 5
  # increasing number of breeding pairs; with breeding control; with issues and relocations:
  colony_forecast(start.date=DEMO.START, nmonth=NMONTH.DEMO,
                  nreps=NREPS.DEMO,
                  I0=I0.DEMO, A0=A0.DEMO,
                  delta=DELTA.DEMO,
                  breeding.pairs.planned=BREEDING.PAIRS.DEMO.2, # increasing number of breeding pairs
                  breeding.control=BREEDING.CONTROL.DEMO.2, # breeding control (100% --> 50% --> 25%)
                  lsz.dist=LITTER.SIZE.DIST.DEMO,
                  transfer.rate=TRANSFER.RATE.DEMO,
                  issues=ISSUES.DEMO.2, # with issues
                  relocations=RELOCATIONS.DEMO.2, # with relocations
                  quantiles=QUANTILES.DEMO, 
                  write.log=FALSE)

)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PLOTS TO COMPARE MODEL OUTPUTS FOR DIFFERENCE SCENARIOS
#------------------------------------------------------------------------------
# - the plots below compare scenario 1 above with scenarios 2, 3, 4 and 5 in turn
# - these results are based on DUMMY DATA, hence ILLUSTRATIVE
# - they demonstrate the ability of the model to incorporate many interventions 
#     into the forecasts
# - the rest of this script is simply the code for plotting these comparisons
# 
#------------------------------------------------------------------------------
# commentary on plots below:
# - in comparison 1, we see that increasing number of breeders noticeably
#   increases the offspring population over time. this becomes evident in the 
#   adult population after a lag period 
# - in comparison 2, we observe the effect of issues and relocations on the number
#   of adults; no significant effect on offspring
# - in comparison 3, the effects of breeding control (50% from month 3; 10% from month 9)
#   on the offspring population are clear; adult population affected after a lag
# - in comparison 4, all the above changes are implemented simultaneously, 
#   with obvious compound effects on both the offspring and adult populations  
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

{
  x11(width=32, height=16)
  # colours for each case:
  # (aside: the colours specified below are derived from the viridis package;
  #         they are given in hex here so that you don't need to install viridis)
  COLS.DEMO.MAX=c("#482576FF", "#375A8CFF", "#24868EFF", "#2CB17EFF", "#86D549FF") 
  COLS.DEMO.MID=c("#48257666", "#375A8C66", "#24868E66", "#2CB17E66", "#86D54966")
  COLS.DEMO.MIN=c("#48257633", "#375A8C33", "#24868E33", "#2CB17E33", "#86D54933")
  #
  par(mfrow=c(2,4))

  #----------------------------------------------------------------------------
  # COMPARE CONSTANT vs. CHANGING NUMBER OF BREEDERS
  #----------------------------------------------------------------------------
  # offspring:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Iq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 250), main='Comparison (1) \nConstant vs. Increasing number of breeders \nOffspring', xlab='Month', ylab='Number of offspring')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in 1:2) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[1,], rev(OUT.DEMO[[i]]$Iq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[2,], rev(OUT.DEMO[[i]]$Iq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Iq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[2]), legend=c('Constant breeders', 'Increasing breeders'), lty=c(1,1), lwd=2)

  # adults:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Aq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 300), main='Comparison (1) \nConstant vs. Increasing number of breeders \nAdults', xlab='Month', ylab='Number of adults')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in 1:2) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[1,], rev(OUT.DEMO[[i]]$Aq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[2,], rev(OUT.DEMO[[i]]$Aq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Aq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[2]), legend=c('Constant breeders', 'Increasing breeders'), lty=c(1,1), lwd=2)

  #----------------------------------------------------------------------------
  # COMPARE NO ISSUES AND RELOCATIONS vs. WITH ISSUES AND RELOCATIONS
  #----------------------------------------------------------------------------
  # offspring:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Iq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 250), main='Comparison (2) \nWith vs. Without issues and relocations \nOffspring', xlab='Month', ylab='Number of offspring')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,3)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[1,], rev(OUT.DEMO[[i]]$Iq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[2,], rev(OUT.DEMO[[i]]$Iq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Iq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[3]), legend=c('No issues or relocations', 'With issues and relocations'), lty=c(1,1), lwd=2)

  # adults:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Aq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 300), main='Comparison (2) \nWith vs. Without issues and relocations \nAdults', xlab='Month', ylab='Number of adults')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,3)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[1,], rev(OUT.DEMO[[i]]$Aq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[2,], rev(OUT.DEMO[[i]]$Aq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Aq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[3]), legend=c('No issues or relocations', 'With issues and relocations'), lty=c(1,1), lwd=2)

  #----------------------------------------------------------------------------
  # COMPARE MAXIMAL BREEDING vs. BREEDING CONTROL
  #----------------------------------------------------------------------------
  # offspring:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Iq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 250), main='Comparison (3) \nMaximal breeding vs. Breeding control \nOffspring', xlab='Month', ylab='Number of offspring')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,4)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[1,], rev(OUT.DEMO[[i]]$Iq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[2,], rev(OUT.DEMO[[i]]$Iq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Iq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[4]), legend=c('Maximal breeding', 'With breeding control'), lty=c(1,1), lwd=2)

  # adults:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Aq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 300), main='Comparison (3) \nMaximal breeding vs. Breeding control \nAdults', xlab='Month', ylab='Number of adults')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,4)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[1,], rev(OUT.DEMO[[i]]$Aq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[2,], rev(OUT.DEMO[[i]]$Aq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Aq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[4]), legend=c('Maximal breeding', 'With breeding control'), lty=c(1,1), lwd=2)

  #----------------------------------------------------------------------------
  # COMPARE CONSTANT BREEDING PAIRS + MAXIMAL BREEDING + NO ISSUES OR RELOCATIONS
  #   vs. CHANGING NUMBER OF BREEDING PAIRS + BREEDING CONTROL + ISSUES + RELOCATIONS
  #----------------------------------------------------------------------------
  # offspring:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Iq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 250), main='Comparison (4) \nNo changes vs. All changes \nOffspring', xlab='Month', ylab='Number of offspring')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,5)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[1,], rev(OUT.DEMO[[i]]$Iq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Iq[2,], rev(OUT.DEMO[[i]]$Iq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Iq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[5]), legend=c('No changes', 'All changes'), lty=c(1,1), lwd=2)

  # adults:
  plot(0:NMONTH.DEMO, OUT.DEMO[[1]]$Aq[2,], type='l', lwd=2, col='white', xaxt='n', cex.main=0.95,
       ylim=c(0, 300), main='Comparison (4) \nNo changes vs. All changes \nAdults', xlab='Month', ylab='Number of adults')
  axis(side=1, at=3*(0:8), labels=3*(0:8))

  for(i in c(1,5)) {
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[1,], rev(OUT.DEMO[[i]]$Aq[5,])), border=NA, col=COLS.DEMO.MIN[i]) # full range
    polygon(c(0:NMONTH.DEMO, rev(0:NMONTH.DEMO)),
            c(OUT.DEMO[[i]]$Aq[2,], rev(OUT.DEMO[[i]]$Aq[4,])), border=NA, col=COLS.DEMO.MID[i]) # interquartile range
    lines(0:NMONTH.DEMO, OUT.DEMO[[i]]$Aq[3,], lwd=3, col=COLS.DEMO.MAX[i]) # median
  }
  legend('bottomleft', col=c(COLS.DEMO.MAX[1], COLS.DEMO.MAX[5]), legend=c('No changes', 'All changes'), lty=c(1,1), lwd=2)

}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# END OF SCRIPT
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
