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
#' Set Breeding Dam Numbers
#'
#' This function is used to set the number of breeding dams per month of the
#' simulation
#'
#' @param breeding.pairs.planned The number of planned breeding pairs during
#' the forecast period; either (1) a single integer , or (2) a vector of
#' integers of length `nmonth`
#' @param nmonth The number of months of the forecast
#' @returns A vector of the number of breeding dams per month, of length `nmonth`
#'
D_i_set <- function(breeding.pairs.planned, nmonth) {
  if(length(breeding.pairs.planned)==nmonth) {
    D_i=breeding.pairs.planned
  } else {
    if(length(breeding.pairs.planned)==1) {
      # - if a single number is specified, turn into a vector of length nmonth
      D_i=rep(breeding.pairs.planned[length(breeding.pairs.planned)], nmonth)
    } else {
      stop("breeding.pairs.planned must be of length 1 or nmonth")
    }
  }

  # return:
  D_i
}

#------------------------------------------------------------------------------
#' Set Breeding Control Parameter
#'
#' This function is used to set the breeding control parameter
#'
#' @param breeding.control The proportion of the maximal breeding rate
#' implemented during the simulation (default 1); either (1) a single real
#' number between 0 and 1 (incl.), or (2) a vector of such numbers of length
#' `nmonth`
#' @param nmonth The number of months of the forecast
#' @returns A vector of breeding controls, of length `nmonth`
#'
f_i_set <- function(breeding.control, nmonth) {
  if(length(breeding.control)==nmonth) {
    f_i=breeding.control
  } else {
    if(length(breeding.control)==1) {
      # - if a single number is specified, turn into a vector of length nmonth
      f_i=rep(breeding.control, nmonth)
    } else {
      stop("breeding.control must be of length 1 or nmonth")
    }
  }

  # return:
  f_i
}

#------------------------------------------------------------------------------
#' Set First Litter Time
#'
#' This function is used to set first litter time for each initial breeding dam
#'
#' Sets time of first litter (during the forecast period) for each breeding dam
#' in month `i`. Samples time from interval 0 to 5.4 months.
#'
#' @param D_i_hat A vector of length `nmonth`, of the number of breeding dams
#' planned for each month of the forecast period
#' @returns A vector of the times of the first litter for each breeding dam in
#' the first month, i.e. of length `D_i_hat[1]`
#'
t_lm_set <- function(D_i_hat) {
  if(length(D_i_hat)==0) stop('D_i_hat must have length greater than or equal to 1')
  t_lm=rep(NA, max(D_i_hat)) # set up (NA) vector to store month of next litter
  t_lm[1:D_i_hat[1]]=runif(D_i_hat[1], 0, 5.4) # time of first litter (months), for each of the initial number of breeding dams; to be updated in for loop below

  # return:
  t_lm
}

#------------------------------------------------------------------------------
#' Adjust Number Of Dams
#'
#' This function is used to adjust the time of next litter when the number of
#' breeding dams changes between months `(i-1)` and `i`
#'
#' @param i The month number (ranges from `1` to `nmonth`)
#' @param D_i_hat A vector of length `nmonth`, of the number of breeding dams
#' planned for each month of the forecast period
#' @param t_lm A vector of the times of the next litter, for each breeding dam
#' @param seed Random seed (optional; default `NULL`)
#' @returns Vector of length `D_i_hat[i]`, containing times of next litter for
#' each breeding dam in month `i`
#'
dams_adjust <- function(i, D_i_hat, t_lm, seed=NULL) {
  if(i==1) stop("i must be greater than or equal to 2")

  if(!is.null(seed)) set.seed(seed)

  ndiff=D_i_hat[i]-D_i_hat[i-1]

  # fewer dams in month i than (i-1):
  # - randomly select ndiff breeding dams to stop breeding
  if(ndiff<0) {
    not.na.ind=which(!is.na(t_lm)) # non-NA indices in t_lm
    new.na.ind=sample(x=not.na.ind, size=abs(ndiff), replace=FALSE) # indices of dams to switch off
    t_lm[new.na.ind]=NA # switch off ndiff breeding dams
  }

  # more dams in month i than (i-1):
  # - randomly select ndiff non-breeding dams to start breeding
  if(ndiff>0) {
    na.ind=which(is.na(t_lm)) # NA indices in t_lm
    if(length(na.ind)>0) {
      # in the sample function, if the 'x' argument is of length 1, then the
      #  function samples from 1:x, whereas here, if na.ind is of length 1,
      #  then we want new.ind=na.ind, we do not want to sample from 1:na.ind,
      #  as per the default behaviour of sample. So we have to consider the
      #  cases of length(na.ind)==1 and length(na.ind)>1 separately:
      if(length(na.ind)>1) {
        new.ind=sample(x=na.ind, size=ndiff, replace=FALSE)
      } else {
        new.ind=na.ind
      }
    } else {
      stop("There are no NA values in t_lm, i.e. no more dams for breeding")
    }
    t_lm[new.ind]=(i-1)+runif(ndiff, 5.0, 5.4) # sample next litter time for new breeders
    # -- subtract 1 to ensure correct mapping between exact next litter time and month number
    #    e.g. sampled numbers between integers j and (j+1) correspond to 'month (j+1)'
  }

  # return:
  t_lm
}

#------------------------------------------------------------------------------
#' New Births
#'
#' This function is used to calculate new births and update time to next litter
#'
#' For each dam giving birth to a litter during month `i`, a litter size is
#' sampled from `lsz.dist`. Time to next litter for each dam is sampled using
#' a uniform distribution reflecting inter-litter interval between 5 and 5.4
#' months.
#'
#' @param lsz.dist The distribution of litter sizes from the observation period
#' @param mu_i The number of litters for month `i`
#' @param t_lm A vector of the times of the next litter, for each breeding dam
#' @returns The total number of births during month `i`, and a vector of the
#' time to next litter for each dam
#'
new_births <- function(lsz.dist, mu_i, t_lm, seed=NULL) {

  if(!is.null(seed)) set.seed(seed)

  if(length(mu_i)>0) {
    # sample from litter size distribution:
    # - Algorithm 1, step 8
    s_i=sample(x=lsz.dist, size=length(mu_i), replace=TRUE)

    # update times of next litter for these dams:
    # - Algorithm 1, step 9
    t_lm[mu_i]=t_lm[mu_i] + runif(length(mu_i), 5.0, 5.4)

    # total new births for i'th month:
    # - Algorithm 1, step 10
    b_i=sum(s_i)

  } else { # - Algorithm 1, step 11
    # - Algorithm 1, step 12
    b_i=0 # no new births in month i
  }

  # return:
  list(b_i=b_i, t_lm=t_lm)
}

#------------------------------------------------------------------------------
#' Calculate Transfers
#'
#' This function is used to calculate transfers during month `i`
#'
#' A Poisson distribution is used to sample the number of transfers.
#'
#' @param transfer.rate The monthly rate of transfer of offspring to adults
#' @param nI_i_1 The number of offspring in the colony at the end of month `(i-1)`
#' @returns The number of offspring to be transferred to the adult compartment
#'
transfers <- function(transfer.rate, nI_i_1) rpois(1, transfer.rate*nI_i_1)

#------------------------------------------------------------------------------
#' Calculate Infant Deaths
#'
#' This function is used to calculate infant deaths during month `i`
#'
#' A Poisson distribution is used to sample the number of infant deaths.
#'
#' @param delta_i The mean monthly rates of death for offspring, per capita I
#' @param nI_i_1 The number of offspring in the colony at the end of month `(i-1)`
#' @param b_i The number of new births during month `i`
#' @param w_i The number of transfers occurring during month `i`
#' @returns The number of infant deaths during month `i`
#'
deaths_infant <- function(delta_i, nI_i_1, b_i, w_i) {

  if(w_i<=(nI_i_1+b_i)) out=rpois(1, delta_i*(nI_i_1 + b_i - w_i)) else out=0

  # return:
  out
}

#------------------------------------------------------------------------------
#' Calculate Adult Deaths
#'
#' This function is used to calculate adult deaths during month `i`
#'
#' A Poisson distribution is used to sample the number of adult deaths.
#'
#' @param delta_a The mean monthly rates of death for adults, per capita A
#' @param nA_i_1 The number of adults in the colony at the end of month `(i-1)`
#' @param w_i The number of transfers occurring during month `i`
#' @returns The number of adult deaths during month `i`
#'
deaths_adult <- function(delta_a, nA_i_1, w_i) rpois(1, delta_a*(nA_i_1 + w_i))

#-------------------------------------------------------------------------------
# END OF SCRIPT
#-------------------------------------------------------------------------------