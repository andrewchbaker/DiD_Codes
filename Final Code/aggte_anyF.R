## THIS IS A FUNCTION THAT COMBINES AGGREGATED TREATMENT EFFECTS FOR THE CS ESTIMATOR
## I MODIFY THIS TO ALLOW ME TO PICK THE YEARS TO COMBINE SO THAT I CAN DO PRE-TREATMENT 
## COMBINED EFFECTS - I.E. THE COMBINED EFFECTS OF RELATIVE TIME INDICATORS -5 to -1
compute.aggte.restricted <- function (MP, type = "group", balance_e = NULL, min_e = -Inf, 
                                      max_e = Inf, min_e2 = -Inf, max_e2 = Inf, na.rm = FALSE, bstrap = NULL, biters = NULL, 
                                      cband = NULL, alp = NULL, clustervars = NULL, call = NULL) 
{
  group <- MP$group
  t <- MP$t
  att <- MP$att
  dp <- MP$DIDparams
  inffunc1 <- MP$inffunc
  n <- MP$n
  gname <- dp$gname
  data <- dp$data
  tname <- dp$tname
  idname <- dp$idname
  if (is.null(clustervars)) {
    clustervars <- dp$clustervars
  }
  if (is.null(bstrap)) {
    bstrap <- dp$bstrap
  }
  if (is.null(biters)) {
    biters <- dp$biters
  }
  if (is.null(alp)) {
    alp <- dp$alp
  }
  if (is.null(cband)) {
    cband <- dp$cband
  }
  tlist <- dp$tlist
  glist <- dp$glist
  panel <- dp$panel
  MP$DIDparams$clustervars <- clustervars
  MP$DIDparams$bstrap <- bstrap
  MP$DIDparams$biters <- biters
  MP$DIDparams$alp <- alp
  MP$DIDparams$cband <- cband
  dp <- MP$DIDparams
  if (na.rm) {
    notna <- !is.na(att)
    group <- group[notna]
    t <- t[notna]
    att <- att[notna]
    inffunc1 <- inffunc1[, notna]
    glist <- sort(unique(group))
  }
  if ((na.rm == FALSE) && base::anyNA(att)) 
    stop("Missing values at att_gt found. If you want to remove these, set `na.rm = TRUE'.")
  ifelse(panel, dta <- data[data[, tname] == tlist[1], ], dta <- data)
  originalt <- t
  originalgroup <- group
  originalglist <- glist
  originaltlist <- tlist
  uniquet <- seq(1, length(unique(t)))
  t2orig <- function(t) {
    unique(c(originalt, 0))[which(c(uniquet, 0) == t)]
  }
  orig2t <- function(orig) {
    c(uniquet, 0)[which(unique(c(originalt, 0)) == orig)]
  }
  t <- sapply(originalt, orig2t)
  group <- sapply(originalgroup, orig2t)
  glist <- sapply(originalglist, orig2t)
  tlist <- unique(t)
  maxT <- max(t)
  weights.ind <- dta$w
  pg <- sapply(originalglist, function(g) mean(weights.ind * 
                                                 (dta[, gname] == g)))
  pgg <- pg
  pg <- pg[match(group, glist)]
  keepers <- which(group <= t)
  G <- unlist(lapply(dta[, gname], orig2t))
  
    eseq <- unique(originalt - originalgroup)
    eseq <- eseq[order(eseq)]
    include.balanced.gt <- rep(TRUE, length(originalgroup))
    if (!is.null(balance_e)) {
      eseq <- eseq[(eseq <= balance_e) & (eseq >= balance_e - 
                                            t2orig(maxT) + t2orig(1))]
      include.balanced.gt <- (t2orig(maxT) - originalgroup >= 
                                balance_e)
    }
    eseq <- eseq[(eseq >= min_e) & (eseq <= max_e)]
    dynamic.att.e <- sapply(eseq, function(e) {
      whiche <- which((originalt - originalgroup == e) & 
                        (include.balanced.gt))
      atte <- att[whiche]
      pge <- pg[whiche]/(sum(pg[whiche]))
      sum(atte * pge)
    })
    dynamic.se.inner <- lapply(eseq, function(e) {
      whiche <- which((originalt - originalgroup == e) & 
                        (include.balanced.gt))
      pge <- pg[whiche]/(sum(pg[whiche]))
      wif.e <- wif(whiche, pg, weights.ind, G, group)
      inf.func.e <- as.numeric(get_agg_inf_func(att = att, 
                                                inffunc1 = inffunc1, whichones = whiche, weights.agg = pge, 
                                                wif = wif.e))
      se.e <- getSE(inf.func.e, dp)
      list(inf.func = inf.func.e, se = se.e)
    })
    dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, 
                                                 "se"))
    dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, 
                                                               "inf.func"))
    dynamic.crit.val <- NULL
    if (dp$cband == TRUE) {
      dynamic.crit.val <- mboot(dynamic.inf.func.e, dp)$crit.val
    }
    ## THIS IS WHAT I ADD - RESTRICT THE TEST TO WHATEVER REGION YOU WANT WITH MIN_E2 and MAX_E2
    epos <- eseq %>% between(min_e2, max_e2)
    dynamic.att <- mean(dynamic.att.e[epos])
    dynamic.inf.func <- get_agg_inf_func(att = dynamic.att.e[epos], 
                                         inffunc1 = as.matrix(dynamic.inf.func.e[, epos]), 
                                         whichones = (1:sum(epos)), weights.agg = (rep(1/sum(epos), 
                                                                                       sum(epos))), wif = NULL)
    dynamic.se <- getSE(dynamic.inf.func, dp)
    return(AGGTEobj(overall.att = dynamic.att, overall.se = dynamic.se, 
                    type = type, egt = eseq, att.egt = dynamic.att.e, 
                    se.egt = dynamic.se.e, crit.val.egt = dynamic.crit.val, 
                    inf.function = list(dynamic.inf.func.e = dynamic.inf.func.e, 
                                        dynamic.inf.func = dynamic.inf.func), call = call, 
                    min_e = min_e, max_e = max_e, balance_e = balance_e, 
                    DIDparams = dp))
  }
#-----------------------------------------------------------------------------
# Internal functions for getteing standard errors
#-----------------------------------------------------------------------------

#' @title Compute extra term in influence function due to estimating weights
#'
#' @description A function to compute the extra term that shows up in the
#'  influence function for aggregated treatment effect parameters
#'  due to estimating the weights
#'
#' @param keepers a vector of indices for which group-time average
#'  treatment effects are used to compute a particular aggregated parameter
#' @param pg a vector with same length as total number of group-time average
#'  treatment effects that contains the probability of being in particular group
#' @param weights.ind additional sampling weights (nx1)
#' @param G vector containing which group a unit belongs to (nx1)
#' @param group vector of groups
#'
#' @return nxk influence function matrix
#'
#' @keywords internal
wif <- function(keepers, pg, weights.ind, G, group) {
  # note: weights are all of the form P(G=g|cond)/sum_cond(P(G=g|cond))
  # this is equal to P(G=g)/sum_cond(P(G=g)) which simplifies things here
  
  # effect of estimating weights in the numerator
  if1 <- sapply(keepers, function(k) {
    (weights.ind * 1*(G==group[k]) - pg[k]) /
      sum(pg[keepers])
  })
  # effect of estimating weights in the denominator
  if2 <- rowSums( sapply( keepers, function(k) {
    weights.ind*1*(G==group[k]) - pg[k]
  })) %*%
    t(pg[keepers]/(sum(pg[keepers])^2))
  
  # return the influence function for the weights
  if1 - if2
}


#' @title Get an influence function for particular aggregate parameters
#'
#' @title This is a generic internal function for combining influence
#'  functions across ATT(g,t)'s to return an influence function for
#'  various aggregated treatment effect parameters.
#'
#' @param att vector of group-time average treatment effects
#' @param inffunc1 influence function for all group-time average treatment effects
#'  (matrix)
#' @param whichones which elements of att will be used to compute the aggregated
#'  treatment effect parameter
#' @param weights.agg the weights to apply to each element of att[whichones];
#'  should have the same dimension as att[whichones]
#' @param wif extra influence function term coming from estimating the weights;
#'  should be n x k matrix where k is dimension of whichones
#'
#' @return nx1 influence function
#'
#' @keywords internal
get_agg_inf_func <- function(att, inffunc1, whichones, weights.agg, wif=NULL) {
  # enforce weights are in matrix form
  weights.agg <- as.matrix(weights.agg)
  
  # multiplies influence function times weights and sums to get vector of weighted IF (of length n)
  thisinffunc <- inffunc1[,whichones]%*%weights.agg
  
  # Incorporate influence function of the weights
  if (!is.null(wif)) {
    thisinffunc <- thisinffunc + wif%*%as.matrix(att[whichones])
  }
  
  # return influence function
  return(thisinffunc)
}


#' @title Take influence function and return standard errors
#'
#' @description Function to take an nx1 influence function and return
#'  a standard error
#'
#' @param thisinffunc An influence function
#' @inheritParams compute.aggte
#'
#' @return scalar standard error
#'
#' @keywords internal
getSE <- function(thisinffunc, DIDparams=NULL) {
  alp <- .05
  bstrap <- FALSE
  if (!is.null(DIDparams)) {
    bstrap <- DIDparams$bstrap
    alp <- DIDparams$alp
    cband <- DIDparams$cband
    n <- length(thisinffunc)
  }
  
  if (bstrap) {
    bout <- mboot(thisinffunc, DIDparams)
    return(bout$se)
  } else {
    return(sqrt( mean((thisinffunc)^2)/n ))
  }
}

aggte_anyF <- function(MP,
                       type = "group",
                       balance_e = NULL,
                       min_e = -Inf,
                       max_e = Inf,
                       min_e2 = -Inf,
                       max_e2 = Inf,
                       na.rm = FALSE,
                       bstrap = NULL,
                       biters = NULL,
                       cband = NULL,
                       alp = NULL,
                       clustervars = NULL
) {
  
  call <- match.call()
  
  compute.aggte.restricted(MP = MP,
                           type = type,
                           balance_e = balance_e,
                           min_e = min_e,
                           max_e = max_e, 
                           min_e2 = min_e2,
                           max_e2 = max_e2,
                           na.rm = na.rm,
                           bstrap = bstrap,
                           biters = biters,
                           cband = cband,
                           alp = alp,
                           clustervars = clustervars,
                           call = call)
}
