#' @title bc2
#'
#' @description Function for fitting Multivariate semi-continuous proportionally constrained two-part fixed effects model.
#'
#' @author Shahin Roshani
#'
#' @param logistic A one sided formula with only right hand side describing the predictors to be included in the logistic part of the model.
#' @param positive1 A formula describing the response and predictors to be used as the first positive part of the model.
#' @param positive2 A formula describing the response and predictors to be used as the second positive part of the model.
#' @param data Data containing responses and predictors.
#' @param search.space A named list containing usual parameters for controlling the optimization process of log-likelihood function.
#'
#' All combinations of the given values in the list will be considered. Valid arguments inside the list are:
#'
#' \itemize{
#' \item \code{coeffs.start}: Numeric vector containing values to be used as starting points of intercepts (a0, a1, a2), coefficients and constraints (b1 & b2) during optimization.
#' \item \code{sigmas.start}: Numeric vector containing values to be used as starting points of sigma1 & sigma2 during optimization.
#' \item \code{ro.start}: Numeric vector containing values to be used as starting points of ro during optimization.
#' \item \code{ro.start}: Optimization methods to be tested for optimizing the log-likelihood function.
#' \item \code{iterations}: Number of iterations to achieve the convergence in optimization process.
#' }
#'
#' Default is \code{NULL} where a pre-defined search space will be used. See part 1 in Details.
#' @param control.pars Additional control parameters used by optim function. Default is \code{NULL}. See Details section in \code{?optim}.
#' @param parallel Logical indicating whether optimization of different combinations in search.space must be done in parallel mode or not. Default is \code{FALSE}.
#' @param core.nums Number of CPU cores to be used for parallel computation. Default is \code{NULL} where half of the CPU cores will be used. See \code{?detectCores}.
#' @param g.funs A single function or a list of length two containing the functions to be used for transforming non-zero responses.
#'
#' If \code{g.funs} were specified by user, \code{$Data} part of return object will contain the un-transformed responses while structured data part will include transformed responses based on functions in \code{g.funs}.
#'
#' @return S3 print class of method \code{'bc2'} which is A list of length 3 containing fitting information of the logistic and the two positive parts.
#'
#' This list is a part of a more comprehensive return that can be accessed when the fit is saved in an object. The full return set contains:
#'
#' \itemize{
#' \item \code{$Data}: Cleaned data-set with original and un-transformed responses. Also see \code{g.funs} argument.
#' \item \code{$`Structured data`}: A list of divided and cleaned data to be used in the fitting process. If \code{g.funs} were specified, transformed responses will be included in this part.
#' \item \code{$`Optimization info`}: A tibble containing information about evaluation of different combinations that was given in search.space for optimization.
#' \item \code{$`Best combination`}: The best combination of parameters that results in the best set of estimations.
#' \item \code{$`Final tables`}: The default print output of class bc2 that was mentioned earlier.
#' }
#'
#' @details When search.space is \code{NULL}, a pre-defined search space will be used which is:
#'
#' \preformatted{
#' list(coeffs.start=seq(-1,1,1),
#'
#'      sigmas.start=c(.1,1),
#'
#'      ro.start=c(-.5,.5),
#'
#'      methods=c('Nelder-Mead','BFGS'),
#'
#'      iterations=c(100,500))
#' }
#'
#' @references \url{https://journals.sagepub.com/doi/abs/10.1177/0962280218807730}
#'
#' @seealso \url{https://shahin-roshani.github.io/BC2/}
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

bc2 <- function(logistic,positive1,positive2,data,search.space=NULL,

                control.pars=NULL,parallel=F,core.nums=NULL,g.funs=NULL){


  pos.resps <- list(positive1,positive2) %>%

    map_chr(~attr(terms(.,data=data),which='variables')[[2]] %>% as.character)

  preds <- attr(terms(logistic,data=data),which='term.labels')

  preds1 <- attr(terms(positive1,data=data),which='term.labels')

  preds2 <- attr(terms(positive2,data=data),which='term.labels')


  if ((!is.function(g.funs) & length(g.funs)==1) | (is.list(g.funs) & length(g.funs)!=2)){

    stop('g.funs must be a single function (used for transformation of both responses) or a list of length two containing the functions for transforming responses!',call.=F)

  }


  if (is.null(search.space)){

    search.space <- list(coeffs.start=seq(-1,1,1),

                         sigmas.start=c(.1,1),

                         ro.start=c(-.5,.5),

                         methods=c('Nelder-Mead','BFGS'),

                         iterations=c(100,500))

  } else{

    default_sp <- list(coeffs.start=seq(-1,1,1),

                       sigmas.start=c(.1,1),

                       ro.start=c(-.5,.5),

                       methods=c('Nelder-Mead','BFGS'),

                       iterations=c(100,500))

    if (any(!(names(search.space) %in% names(default_sp)))){

      stop('valid values must be specified under names: coeffs.start, sigmas.start, ro.start, methods & iterations in the search.space list!',call.=F)

    }

    user_sp <- search.space

    search.space <- user_sp %>% append(default_sp) %>% .[unique(names(.))] %>%

      .[names(default_sp)]

  }


  data %>% select(all_of(pos.resps)) %>%

    mutate(x=apply(.,1,function(x) (0 %in% x) & !all(x==0))) %>%

    mutate(index=1:nrow(.)) %>% filter(x==T) %>% .$index -> non_match_index

  if (!is_empty(non_match_index)){

    data %<>% .[-non_match_index,]

    warning(str_c('Removed line(s) ',str_c(non_match_index,collapse=', '),' of data due to non-matching zero responses!'),call.=F)

  }


  results <- list()

  results$Data <- data %>% select(all_of(pos.resps),everything())


  if(!is.null(g.funs)){

    if (!is.list(g.funs)){

      g.funs <- rep(list(g.funs),2)

      names(g.funs) <- pos.resps

    }

    if (is.null(names(g.funs))){

      names(g.funs) <- pos.resps

    }

    data %<>% mutate(zero=get(pos.resps[1])==0,index=1:nrow(.)) %>%

      split(.$zero) %>%

      map_at('FALSE',function(x){

        x[[pos.resps[1]]] %<>% g.funs[[pos.resps[1]]](.)

        x[[pos.resps[2]]] %<>% g.funs[[pos.resps[2]]](.)

        return(x)

      }) %>% suppressWarnings() %>% map(~select(.,-zero)) %>%

      reduce(rbind) %>% arrange(index) %>% select(-index)

  }

  check <- function(x) which(is.na(x) | is.nan(x) | is.infinite(x))

  removals <- data %>% select(all_of(pos.resps)) %>%

    map(check) %>% reduce(c) %>% unique %>% sort

  if (!is_empty(removals)){

    data %<>% .[-removals,]

    warning(str_c('Removed lines(s) ',str_c(removals,collapse=', '),' of data due to the inclusion of NA, NaN or infinite values in either or both of responses!'),call.=F)

  }

  data %<>% na.omit


  X <- model.matrix(str_c('~',str_c(c(preds,pos.resps),collapse='+')) %>%

                      as.formula,data=data)[,-1] %>% as.data.frame

  pos_index <- which(X[[pos.resps[1]]]!=0)

  y1 <- X[[pos.resps[1]]][pos_index]

  y2 <- X[[pos.resps[2]]][pos_index]

  X1 <- model.matrix(str_c('~',str_c(c(preds1,pos.resps),collapse='+')) %>%

                       as.formula,data=data)[,-1] %>% as.data.frame %>%

    slice(pos_index)

  X2 <- model.matrix(str_c('~',str_c(c(preds2,pos.resps),collapse='+')) %>%

                       as.formula,data=data)[,-1] %>% as.data.frame %>%

    slice(pos_index)

  tempp <- function(xx){

    return(

      xx %<>% as_tibble %>% select(-all_of(pos.resps)) %>% as.matrix

    )

  }

  X %<>% tempp ; X1 %<>% tempp ; X2 %<>% tempp


  results$`Structured data` <- list(X,X1,X2,y1,y2) %>%

    (function(x){

      names(x) <- c('X','X1','X2',pos.resps)

      return(x)

    })


  logistic_vars <- colnames(X)

  positive1_vars <- colnames(X1)

  positive2_vars <- colnames(X2)



  neg_loglik <- function(theta){

    names(theta) <- c('a0','a1','a2',

                      unique(c(logistic_vars,positive1_vars,positive2_vars)),

                      'b1','b2','sigma1','sigma2','ro')

    n1 <- nrow(X1)

    a0 <- theta['a0']

    a1 <- theta['a1']

    a2 <- theta['a2']

    beta <- theta[logistic_vars]

    beta1 <- theta[positive1_vars]

    beta2 <- theta[positive2_vars]

    xbeta <- X%*%beta

    x1beta1 <- X1%*%beta1

    x2beta2 <- X2%*%beta2

    b1 <- theta['b1']

    b2 <- theta['b2']

    sigma1 <- theta['sigma1']

    sigma2 <- theta['sigma2']

    ro <- theta['ro']


    loglik <-

      -(n1/2)*(log(sigma1^2)+log(sigma2^2)+log(1-ro^2))-

      sum(log(1+exp(a0+xbeta)))+n1*a0+sum(x1beta1)-

      (1/(2*(sigma1^2)*(1-ro^2)))*sum((y1-a1-b1*x1beta1)^2)-

      (1/(2*(sigma2^2)*(1-ro^2)))*sum((y2-a2-b2*x2beta2)^2)+

      (ro/(sigma1*sigma2*(1-ro^2)))*sum((y1-a1-b1*x1beta1)*(y2-a2-b2*x2beta2))

    return(-loglik)

  }


  combs <- expand.grid(coeffs=search.space$coeffs.start,

                       sigmas=search.space$sigmas.start,

                       ro=search.space$ro.start,

                       method=search.space$methods,

                       iters=search.space$iterations) %>%

    as_tibble %>% mutate_at('method',as.character)


  safe_optim <- possibly(.f=function(...){

    optim(...)[c('value','convergence')] %>% as_tibble

  },otherwise=NA)


  mmax <- list(X,X1,X2) %>% map_dbl(ncol) %>% max


  cat('\n1. Finding the best combination from the given search space...\n\n')

  if (parallel){

    if (is.null(core.nums)){

      core.nums <- future::availableCores()/2

    }

    future::plan(future::multisession(),workers=core.nums)

    start <- Sys.time()

    combs %>%

      furrr::future_pmap(~safe_optim(c(rep(..1,mmax+5),rep(..2,2),..3),

                                     neg_loglik,method=..4,hessian=F,

                                     control=list(maxit=..5) %>%

                                       append(control.pars) %>%

                                       (function(x) x[unique(names(x))]))) %>%

      suppressWarnings -> optim_info

    end <- Sys.time()


  } else{

    start <- Sys.time()

    combs %>% pmap(~safe_optim(c(rep(..1,mmax+5),rep(..2,2),..3),

                               neg_loglik,method=..4,hessian=F,

                               control=list(maxit=..5))) %>%

      suppressWarnings -> optim_info

    end <- Sys.time()

  }

  cat('   Done in:',(end-start) %>% as.character.POSIXt %>% str_c(.,'.\n\n'))


  if(all(is.na(optim_info))){

    stop('Estimations could not be obtained on any combination in the search.space!',call.=F)

  }


  combs %<>% slice(which(!is.na(optim_info))) %>%

    cbind(.,optim_info %>% .[!is.na(.)] %>% reduce(rbind))


  results$`Optimization info` <- combs %>% as_tibble


  combs %<>% filter(!is.na(value))

  if (!any(combs$convergence==0)){

    warning('Convergence was not achieved in any of the combinations in the search space!',call.=F)

    combs %<>% arrange(value,iters)

  } else{

    combs %<>% filter(convergence==0) %>% arrange(value,iters)

  }

  starts <- combs[1,]

  results$`Best combination` <- starts %>% as_tibble


  cat('2. Estimating parameters using the obtained best combination...\n\n')


  optim(c(rep(starts$coeffs,mmax+5),

          rep(starts$sigmas,2),

          starts$ro),

        neg_loglik,method=starts$method,

        hessian=T,control=list(maxit=starts$iters)) %>%

    suppressWarnings -> opt


  cov_mat <- solve(opt$hessian)


  if (any(diag(cov_mat)<0)){

    warning('Some Standard Errors cannot be calculated due to negative values in diagonal elements of hessian matrix that was caused by the inability of convergence in the given search.space!',call.=F)

  }


  names(opt$par) = colnames(cov_mat) = rownames(cov_mat) <-

    c('a0','a1','a2',

      unique(c(logistic_vars,positive1_vars,positive2_vars)),

      'b1','b2','sigma1','sigma2','ro')



  opt$par %>% as.data.frame %>% rownames_to_column() %>%

    rename('Estimate'='.','Coeff'='rowname') %>%

    mutate(Std.Err=sqrt(diag(cov_mat)),

           betas=Coeff %in% unique(c(logistic_vars,

                                     positive1_vars,

                                     positive2_vars))) %>%

    split(.$betas) %>% map(~select(.,-betas)) %>%

    (function(x){names(x) <- c('fixed','betas') ; return(x)}) %>%

    suppressWarnings -> base_set


  base_set %<>% map(function(x){

    row.names(x) <- 1:nrow(x)

    return(x)

  })


  base_set$betas %<>% mutate(lg=Coeff %in% logistic_vars,

                             p1=Coeff %in% positive1_vars,

                             p2=Coeff %in% positive2_vars) %>%

    gather('key','value',4:6) %>% split(.$key) %>%

    map(~filter(.,value) %>% select(-any_of(c('key','value')))) %>%

    .[c('lg','p1','p2')]


  for (i in 2:3){

    base_set$betas[[i]] %<>%

      mutate(x=ifelse(Coeff %in% base_set$betas[[1]]$Coeff,

                      base_set$fixed$Estimate[base_set$fixed$Coeff==str_c('b',i-1)],1)) %>%

      mutate(Estimate=Estimate*x)


    for (j in 1:nrow(base_set$betas[[i]])){

      if (base_set$betas[[i]]$x[j]!=1){

        base_set$betas[[i]]$Std.Err[j] <-

          msm::deltamethod(~x1*x2,

                           opt$par[c(base_set$betas[[i]]$Coeff[j],

                                     str_c('b',i-1))],

                           cov_mat[c(base_set$betas[[i]]$Coeff[j],

                                     str_c('b',i-1)),

                                   c(base_set$betas[[i]]$Coeff[j],

                                     str_c('b',i-1))])

      }

    }


  }

  base_set$betas %<>% map_at(-1,~select(.,-x))


  logistic_part <- rbind(base_set$fixed %>% filter(Coeff=='a0'),

                         base_set$betas$lg)


  positive_part1 <- rbind(base_set$fixed %>% filter(Coeff=='a1'),

                          base_set$betas$p1,

                          base_set$fixed %>%

                            filter(Coeff %in% c('b1','sigma1','ro')))


  positive_part2 <- rbind(base_set$fixed %>% filter(Coeff=='a2'),

                          base_set$betas$p2,

                          base_set$fixed %>%

                            filter(Coeff %in% c('b2','sigma2','ro')))


  list(logistic_part,positive_part1,positive_part2) %>%

    map(as_tibble) %>%

    map(function(x){

      row.names(x) <- NULL

      return(x)

    }) %>% map(~mutate(.,'Z.value'=Estimate/Std.Err,

                       'P.value'=1-pchisq(Z.value^2,1))) -> final_tabs

  names(final_tabs) <- c('Logistic part',pos.resps)


  results$`Final tables` <- final_tabs


  class(results) <- 'bc2'

  print.bc2 <<- function(x) print(x[[5]])


  cat('Done!\n\n')

  return(results)

}
