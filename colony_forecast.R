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
#'
#' Colony Forecast Model
#' 
#' This function simulates the monthly counts of offspring (denoted I, for 'infants', 
#' loosely speaking), and adults (A) in a captive colony over time
#'
#' The model is parametrised by both historic data from a past observation
#' period, and data describing future plans to be implemented during the
#' forecast period
#'
#' @param seed Random seed for simulations (optional; default=`NULL`)
#' @param start.date The start date of the forecast period
#' @param nmonth The number of months to forecast, beginning at `start.date`
#' @param nreps The number of replications of the simulation to be performed
#' @param I0 The initial number of offspring in the population at `start.date`
#' @param A0 The initial number of adults in the population at `start.date`
#' @param transfer.rate The monthly rate of transfer of offspring to adults
#' @param delta A vector of length 2, the mean monthly rates of death for
#' offspring and adults, per capita (I or A), respectively
#' @param lsz.dist The distribution of litter sizes from the observation period
#' @param breeding.pairs.planned The number of planned breeding pairs during
#' the forecast period; either (1) a single integer , or (2) a vector of
#' integers of length `nmonth`
#' @param breeding.control The proportion of the maximal breeding rate
#' implemented during the simulation (default 1); either (1) a single real
#' number between 0 and 1 (incl.), or (2) a vector of such numbers of length
#' `nmonth`
#' @param issues The planned number of animals to be issued to experiments
#' each month of the forecast; either (1) a single integer , or (2) a vector of
#' integers of length `nmonth`
#' @param relocations The planned number of animals to be relocated each month
#' of the forecast; either (1) a single integer , or (2) a vector of
#' integers of length `nmonth`
#' @param quantiles A vector of percentiles (between 0 and 100, inclusive) to
#' be used in summarising simulation results; default (0, 2.5, 50, 97.5, 100),
#' i.e. median, 95\% credible interval and range
#' @returns A data-frame of time series for (I,A,B) for each of the `nreps` runs,
#' a list of events for each of the `nreps` runs, and matrices of quantiles for
#' offspring, adults and breeders, across all runs
#'
colony_forecast <- function(seed=NULL, # random seed, for reproducibility
                            start.date, # start date of forecast
                            nmonth, # length of forecast (months)
                            nreps, # number of replications
                            I0, A0, # initial conditions (offspring, adults)
                            #--------------------------------------------------
                            # historic data:
                            transfer.rate, # transfer rate; single value
                            delta, # death rates; vector (offspring, breeders, adults)
                            lsz.dist, # litter size distribution
                            #--------------------------------------------------
                            # future plans:
                            breeding.pairs.planned, # intervention - number of breeding pairs each month
                            breeding.control, # intervention - breeding control each month
                            issues, # intervention - issues each month
                            relocations, # intervention - relocations each month
                            #--------------------------------------------------
                            # output:
                            quantiles=c(0, 2.5, 50, 97.5, 100),
                            write.log=FALSE
                            )
{
  #----------------------------------------------------------------------------
  # START INPUT CHECKS
  #----------------------------------------------------------------------------
  # check that no arguments are NULL, NA, NaN or '':
  stopifnot(# NULL:
            !is.null(start.date), !is.null(nmonth), !is.null(nreps),
            !is.null(I0), !is.null(A0), !is.null(transfer.rate),
            !is.null(delta), !is.null(lsz.dist),
            !is.null(breeding.pairs.planned), !is.null(breeding.control),
            !is.null(issues), !is.null(relocations), !is.null(quantiles),
            # NA:
            !is.na(start.date), !is.na(nmonth), !is.na(nreps),
            !is.na(I0), !is.na(A0), !is.na(transfer.rate),
            !is.na(delta), !is.na(lsz.dist),
            !is.na(breeding.pairs.planned), !is.na(breeding.control),
            !is.na(issues), !is.na(relocations), !is.na(quantiles),
            # NaN:
            !is.nan(start.date), !is.nan(nmonth), !is.nan(nreps),
            !is.nan(I0), !is.nan(A0), !is.nan(transfer.rate),
            !is.nan(delta), !is.nan(lsz.dist),
            !is.nan(breeding.pairs.planned), !is.nan(breeding.control),
            !is.nan(issues), !is.nan(relocations), !is.nan(quantiles),
            # empty:
            start.date!='', nmonth!='', nreps!='', I0!='', A0!='',
            transfer.rate!='', delta!='', lsz.dist!='',
            breeding.pairs.planned!='', breeding.control!='',
            issues!='', relocations!='', quantiles!='')

  #----------------------------------------------------------------------------
  # check that all function inputs are of correct type:
  # (all arguments numeric, except for start.date, which is a character string)
  stopifnot(is.character(start.date), is.double(nmonth), is.double(nreps),
            is.double(I0), is.double(A0), is.double(transfer.rate),
            is.double(delta), is.double(lsz.dist),
            is.double(breeding.pairs.planned), is.double(breeding.control),
            is.double(issues), is.double(relocations), is.double(quantiles))

  #----------------------------------------------------------------------------
  # check that none of the numerical arguments are negative:
  stopifnot(!any(nmonth<0, nreps<0, I0<0, A0<0, transfer.rate<0, any(delta<0),
                 any(lsz.dist<0), breeding.pairs.planned<0, any(breeding.control<0),
                 any(issues<0), any(relocations<0), any(quantiles<0)))

  #----------------------------------------------------------------------------
  # nreps must be positive:
  if(nreps==0) stop("nreps must be positive")

  # check start.date in correct format, YYYY-MM-DD:
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", start.date)) stop("start.date must be YYYY-MM-DD")

  # check length(delta)==2 (death rate for offspring, death rate for adults):
  if(length(delta)!=2) stop("delta must be a vector of length 2")

  # name delta if unnamed:
  if(is.null(names(delta))) names(delta)=c('delta.i', 'delta.a')

  # check that number of planned breeding pairs is greater than zero:
  # if(any(breeding.pairs.planned==0)) stop("Number of breeding pairs input into the model must be greater than zero")

  #----------------------------------------------------------------------------
  # END INPUT CHECKS
  #----------------------------------------------------------------------------
  # set random seed, if specified:
  if(!is.null(seed)) set.seed(seed)

  #----------------------------------------------------------------------------
  # set end date of forecast period:
  # (N.B. use base R methods rather than slightly simpler functions from lubridate, to avoid installing additional package)
  end.date=as.POSIXlt(start.date) # set end.date to start.date initially, then add nmonth
  end.date$mon=end.date$mon+nmonth # add nmonth
  end.date$mday=end.date$mday-1 # subtract 1 day to get to last day of nmonth

  #----------------------------------------------------------------------------
  # ensure issues and relocations are vectors of length nmonth:
  if(length(issues)!=1 & length(issues)!=nmonth) stop("issues must have length 1 or nmonth")

  if(length(relocations)!=1 & length(relocations)!=nmonth) stop("relocations must have length 1 or nmonth")

  if(length(issues)==1) issues=rep(issues, nmonth)

  if(length(relocations)==1) relocations=rep(relocations, nmonth)

  #----------------------------------------------------------------------------
  # set number of breeding dams (equal to the number of breeding pairs):

  D_i=D_i_set(breeding.pairs.planned, nmonth)

  # set breeding control parameter:

  f_i=f_i_set(breeding.control, nmonth)

  #----------------------------------------------------------------------------
  # simulate monthly counts of (I,B,A) for 'nmonths', for 'nreps' replicates
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ See Algorithms 1 and 2 in accompanying technical report for details ++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  runs=lapply(1:nreps, function(j) {
    #--------------------------------------------------------------------------
    # pre-allocate matrix to store numbers of events:

    events=matrix(data=NA, nrow=nmonth, ncol=8)

    colnames(events)=event.names=c('births', 'transfers', 'newbreeders',
                                   'deaths.I', 'deaths.A', 'deaths.B',
                                   'issues', 'relocations')

    # pre-allocate matrix to store state (rows for months; columns for (I,A,B)):
    # - Algorithm 1, step 1

    Xmat=matrix(data=NA, nrow=nmonth+1, ncol=3)

    # set first row of matrix to the initial conditions:
    # - Algorithm 1, step 2

    B0=2*D_i[1] # initial number of breeders is twice the initial number of planned breeding pairs

    Xmat[1,]=c(I0, A0, B0)

    # calculate number of breeding dams, taking breeding control into account:
    # - Algorithm 1, step 3

    D_i_hat=round(f_i*D_i)

    # time of first litter for each breeding dam:
    # - Algorithm 1, step 4
    # - if D_i_hat[1]==0 (i.e. no breeding pairs), then set time of first litter
    #     beyond the forecast period (nmonth+1), to ensure no litters in forecast

    if(D_i_hat[1]>0) t_lm=t_lm_set(D_i_hat) else t_lm=nmonth+1

    #--------------------------------------------------------------------------
    # loop through i from 1 to nmonth
    # - Algorithm 1, step 5

    for(i in 1:nmonth) {
      #------------------------------------------------------------------------
      # BIRTHS
      #------------------------------------------------------------------------
      # adjust for changes in number of breeding dams between months (i-1) and i (month 2 onwards):

      if(i>=2) t_lm=dams_adjust(i, D_i_hat, t_lm)

      #------------------------------------------------------------------------
      # determine dams which have litters in month i:
      # - Algorithm 1, step 6

      mu_i=which(floor(t_lm+1)==i)

      # new births in month i:
      # - Algorithm 1, steps 7-12 (inside function new_births)

      newbirths=new_births(lsz.dist, mu_i, t_lm)

      b_i=newbirths$b_i # new births during month i

      t_lm=newbirths$t_lm # updated time of next litter, for each dam

      #------------------------------------------------------------------------
      # current state of system at start of i'th month (i.e. values from month (i-1)):

      nI_i_1=Xmat[i,1] # number of offspring

      nA_i_1=Xmat[i,2] # number of adults

      nB_i_1=Xmat[i,3] # number of breeders (constant in model)

      #------------------------------------------------------------------------
      # TRANSFERS
      #------------------------------------------------------------------------

      w_i=transfers(transfer.rate, nI_i_1)

      #------------------------------------------------------------------------
      # DEATHS (offspring and adults)
      #------------------------------------------------------------------------
      # apply death rates, taking into account new births and transfers during month i:
      # Algorithm 1, step 13

      dI_i=deaths_infant(delta['delta.i'], nI_i_1, b_i, w_i) # number of infant deaths, based on new number of I

      dA_i=deaths_adult(delta['delta.a'], nA_i_1, w_i) # number of adult deaths, based on new number of A

      #------------------------------------------------------------------------
      # replacement of breeders
      # - not used, since it is assumed that number of breeders is constant
      # - included for completeness
      #------------------------------------------------------------------------

      dB_i=0 # set breeder deaths to zero

      r_i=0 # set rate of breeder replacement to zero

      #------------------------------------------------------------------------
      # ISSUES AND RELOCATIONS
      #------------------------------------------------------------------------

      if(is.null(issues)) u_i=0 else u_i=issues[i]

      if(is.null(relocations)) v_i=0 else v_i=relocations[i]

      #------------------------------------------------------------------------
      # UPDATE STATE VECTOR FOR MONTH i:
      #------------------------------------------------------------------------
      # change in number of offspring (births-deaths-weanings-replacements):
      # - Algorithm 1, step 14

      DI_i = b_i - dI_i - w_i - r_i

      # change in number of adults (weanings-deaths-issues):
      # - Algorithm 1, step 15

      DA_i = w_i - dA_i - u_i - v_i

      # change in number of breeders (-deaths+replacements)
      # - DB_i=0 by assumption (constant breeders)
      # - not included in Algorithm 1
      # - included here for completeness

      DB_i = r_i - dB_i

      # add new state for i'th month to results matrix (Algorithm 1, step 16)
      # - use pmax to catch any negative values and set to zero (Algorithm 1, step 17)

      Xmat[1+i,] = pmax(Xmat[i,] + c(DI_i, DA_i, DB_i), c(0,0,0)) # i'th month goes in the (i+1)'th row

      # capture rates for the i'th month:

      events[i,]=c(b_i, w_i, r_i, dI_i, dA_i, dB_i, u_i, v_i)

    }

    #--------------------------------------------------------------------------
    # assemble list of output of 'runs'
    # - Algorithm 1, step 18

    list(state=data.frame(cbind(Month=0:nmonth,
                                I=sapply(Xmat[,1], function(xI) max(0, xI)), # offspring
                                A=sapply(Xmat[,2], function(xA) max(0, xA)), # adults
                                B=sapply(Xmat[,3], function(xB) max(0, xB)))), # breeders
         events=events
    )
  }) # - Algorithm 1, steps 19, 20 (next i; end loop)

  #----------------------------------------------------------------------------
  # calculate quantiles across the simulations when nreps>1
  #----------------------------------------------------------------------------
  if(nreps>1) {
    #--------------------------------------------------------------------------
    # - Algorithm 2, steps 1 to 5: implemented above; 'runs' object contains
    #                              results of the realisations
    #--------------------------------------------------------------------------
    # assemble matrices of all realisations for I,A,B:
    # - Algorithm 2, step 6
    # assemble I columns for all runs into a single matrix:

    Imat=do.call('cbind', lapply(runs, function(x) x$state$I)) # offspring

    # assemble A columns for all runs into a single matrix:

    Amat=do.call('cbind', lapply(runs, function(x) x$state$A)) # adults

    # assemble B columns for all runs into a single matrix:

    Bmat=do.call('cbind', lapply(runs, function(x) x$state$B)) # breeders (columns will be identical)

    # calculate quantiles for results of simulations:
    # - Algorithm 2, step 7
    # - the percentiles are specified by function argument 'quantiles'

    Iquant=apply(Imat, 1, quantile, prob=quantiles/100) # offspring

    Aquant=apply(Amat, 1, quantile, prob=quantiles/100) # adults

    Bquant=apply(Bmat, 1, quantile, prob=quantiles/100) # breeders (will be constant)

  }

  gc() # garbage collection

  #----------------------------------------------------------------------------
  # function return:
  if(nreps>1) {

    #--------------------------------------------------------------------------
    # runs:

    Xdf.list=lapply(runs, function(x) data.frame(x$state))

    #--------------------------------------------------------------------------
    # events for all runs:

    events.list=lapply(runs, function(x) data.frame(x$events))

    #--------------------------------------------------------------------------
    # object to return:

    out=list(Xdf=Xdf.list,  # data.frames of individual runs (Xdf)
             events=events.list,
             Iq=Iquant, # quantiles for offspring (Iq)
             Aq=Aquant, # quantiles for adults (Aq)
             Bq=Bquant) # quantiles for breeders (Bq)
  }

  if(nreps==1) out=runs
  
  #-----------------------------------------------------------------------------
  # write log file
  #-----------------------------------------------------------------------------
  if(write.log) {
    # data to write to log file:
    log.data=list(
      seed=seed, start.date=start.date, nmonth=nmonth, nreps=nreps, I0=I0, A0=A0,
      transfer.rate=transfer.rate, delta=delta, lsz.dist=lsz.dist, 
      breeding.pairs.planned=breeding.pairs.planned, 
      breeding.control=breeding.control, issues=issues, relocations=relocations, 
      quantiles=quantiles, output=out
    )
    
    # timestamp for log file (YYYYMMDD_HHMMSS):
    log.timestamp=format(Sys.time(), format="%Y%m%d_%H%M%S")
    
    # initial id number for file:
    log.count=1
    
    # mitigate against overwriting files with same timestamp (very unlikely!)
    #   test for existence of filename including log.count
    #   if it already exists, then increment log.count until the filename is new:
    while(file.exists(paste0(log.timestamp, '-LogFile-', log.count, '.RData'))) {
      log.count=log.count+1
    }
    
    # finalise log file name:
    log.name=paste0(log.timestamp, '-LogFile-', log.count, '.RData')

    message(paste0('Writing log file containing inputs and outputs to: ', log.name))
    
    save(log.data, file=log.name)
      
  }
  #----------------------------------------------------------------------------
  
  # return:
  out
}
#------------------------------------------------------------------------------
# END OF SCRIPT
#------------------------------------------------------------------------------