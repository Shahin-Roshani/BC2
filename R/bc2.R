#' @title bc2
#'
#' @description Function for fitting Multivariate semi-continuous proportionally constrained two-part fixed effects model.
#'
#' @author Shahin Roshani
#'
#' @param pos.resps A character vector of length two containing the names of model's two responses.
#' @param preds A character vector containing the names of predictors to be used in all parts of the model.
#' @param data Data containing responses and predictors.
#' @param search.space A named list containing usual parameters for controlling the optimization process of log-likelihood function.
#'
#' All combinations of the given values in the list will be considered. Valid arguments inside the list are:
#'
#' \itemize{
#' \item coeffs.start: Numeric vector containing values to be used as starting points of intercepts (a0, a1, a2), coefficients and constraints (b1 & b2) during optimization.
#' \item sigmas.start: Numeric vector containing values to be used as starting points of sigma1 & sigma2 during optimization.
#' \item ro.start: Numeric vector containing values to be used as starting points of ro during optimization.
#' \item methods: Optimization methods to be tested for optimizing the log-likelihood function.
#' \item iterations: Number of iterations to achieve the convergence in optimization process.
#' }
#'
#' Default is NULL where a pre-defined search space will be used. See part 1 in Details.
#' @param control.pars Additional control parameters used by optim function. Default is NULL. See Details section in \code{?optim}.
#' @param parallel Logical indicating whether optimization of different combinations in search.space must be done in parallel mode or not. Default is NULL.
#' @param core.nums Number of CPU cores to be used for parallel computation. Default is NULL where half of the CPU cores will be used. See \code{?detectCores}.
#' @param g.funs A single function or a list of length two containing the functions to be used for transforming non-zero responses.
#'
#' If \code{g.funs} were specified by user, $Data part of return object will contain the un-transformed responses while structured data part will include transformed responses based on functions in \code{g.funs}.
#'
#' @return S3 print class of method 'bc2' which is A list of length 3 containing fitting information of the logistic and the two positive parts.
#'
#' This list is a part of a more comprehensive return that can be accessed when the fit is saved in an object. The full return set contains:
#'
#' \itemize{
#' \item $Data: Cleaned data-set with original and un-transformed responses. Also see \code{g.funs} argument.
#' \item $`Structured data`: A list of divided and cleaned data to be used in the fitting process. If \code{g.funs} were specified, transformed responses will be included in this part.
#' \item $`Optimization info`: A tibble containing information about evaluation of different combinations that was given in search.space for optimization.
#' \item $`Best combination`: The best combination of parameters that results in the best set of estimations.
#' \item $`Final tables`: The default print output of class bc2 that was mentioned earlier.
#' }
#'
#' @details When search.space is NULL, a pre-defined search space will be used which is:
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
#' @import tidyverse magrittr msm parallel doParallel future furrr cowplot
#'
#' @export

bc2 <- function(pos.resps,preds,data,search.space=NULL,

                control.pars=NULL,parallel=F,core.nums=NULL,g.funs=NULL){


  if (!is.character(pos.resps) | length(pos.resps %>% unique)!=2){

    stop('pos.resps must be character vector of length two!',call.=F)

  }

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

  results$Data <- data %>% select(pos.resps,everything())


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


  X <- model.matrix(~.,data=data)[,-1] %>% as.data.frame

  pos_index <- which(X[[pos.resps[1]]]!=0)

  y1 <- X[[pos.resps[1]]][pos_index]

  y2 <- X[[pos.resps[2]]][pos_index]

  X1 <- as_tibble(X) %>% slice(pos_index) %>%

    select(-all_of(pos.resps)) %>% as.matrix

  X %<>% as_tibble %>% select(-all_of(pos.resps)) %>% as.matrix


  results$`Structured data` <- list(X,X1,y1,y2) %>%

    (function(x){

      names(x) <- c('X','X1',pos.resps)

      return(x)

    })


  neg_loglik <- function(theta){

    k <- ncol(X)

    n1 <- nrow(X1)

    a0 <- theta[1]

    a1 <- theta[2]

    a2 <- theta[3]

    beta <- theta[4:(k+3)]

    xbeta <- X%*%beta

    xbeta1 <- X1%*%beta

    b1 <- theta[k+4]

    b2 <- theta[k+5]

    sigma1 <- theta[k+6]

    sigma2 <- theta[k+7]

    ro <- theta[k+8]


    loglik <-

      -(n1/2)*(log(sigma1^2)+log(sigma2^2)+log(1-ro^2))-

      sum(log(1+exp(a0+xbeta)))+n1*a0+sum(xbeta1)-

      (1/(2*(sigma1^2)*(1-ro^2)))*sum((y1-a1-b1*xbeta1)^2)-

      (1/(2*(sigma2^2)*(1-ro^2)))*sum((y2-a2-b2*xbeta1)^2)+

      (ro/(sigma1*sigma2*(1-ro^2)))*sum((y1-a1-b1*xbeta1)*(y2-a2-b2*xbeta1))

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


  cat('\n1. Finding the best combination from the given search space...\n\n')


  if (parallel){

    if (is.null(core.nums)){

      core.nums <- parallel::detectCores()/2

    }

    cluster <- parallel::makeCluster(core.nums)

    doParallel::registerDoParallel(cluster)

    future::plan(multisession)

    start <- Sys.time()

    combs %>%

      furrr::future_pmap(~safe_optim(c(rep(..1,ncol(X)+5),rep(..2,2),..3),

                                     neg_loglik,method=..4,hessian=F,

                                     control=list(maxit=..5) %>%

                                       append(control.pars) %>%

                                       (function(x) x[unique(names(x))])),

                         scheduling=Inf) %>% suppressWarnings -> optim_info

    end <- Sys.time()

    parallel::stopCluster(cluster)


  } else{

    start <- Sys.time()

    combs %>% pmap(~safe_optim(c(rep(..1,ncol(X)+5),rep(..2,2),..3),

                               neg_loglik,method=..4,hessian=F,

                               control=list(maxit=..5) %>%

                                 append(control.pars) %>%

                                 (function(x) x[unique(names(x))]))) %>%

      suppressWarnings -> optim_info

    end <- Sys.time()

  }

  cat('   Done in:',(end-start) %>% as.character.POSIXt %>% str_c(.,'.\n\n'))


  combs %<>% cbind(.,optim_info %>% reduce(rbind))


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


  optim(c(rep(starts$coeffs,ncol(X)+5),

          rep(starts$sigmas,2),

          starts$ro),

        neg_loglik,method=starts$method,

        hessian=T,control=list(maxit=starts$iters)) %>%

    suppressWarnings -> opt

  cov_mat <- solve(opt$hessian)

  names(opt$par) = colnames(cov_mat) = rownames(cov_mat) <-

    c('a0','a1','a2',colnames(X),'b1','b2','sigma1','sigma2','ro')


  opt$par %>% as.data.frame %>% rownames_to_column() %>%

    rename('Estimate'='.','Coeff'='rowname') %>%

    mutate(Std.Err=sqrt(diag(cov_mat)),betas=Coeff %in% colnames(X)) %>%

    split(.$betas) %>% map(~select(.,-betas)) %>%

    (function(x){names(x) <- c('fixed','betas') ; return(x)}) -> base_set


  b1_est <- base_set$fixed$Estimate[base_set$fixed$Coeff=='b1']

  b2_est <- base_set$fixed$Estimate[base_set$fixed$Coeff=='b2']


  logistic_part <- rbind(base_set$fixed %>% filter(Coeff=='a0'),base_set$betas)


  pexpand <- function(x,b_est,b_char){

    x %<>% mutate_at('Estimate',~.*b_est)

    x$Std.Err <- as.list(x$Coeff) %>%

      map_dbl(~msm::deltamethod(~x1*x2,

                                opt$par[c(.,b_char)],

                                cov_mat[c(.,b_char),c(.,b_char)]))

    return(x)

  }

  positive_part1 <- rbind(base_set$fixed %>% filter(Coeff=='a1'),

                          base_set$betas %>% pexpand(.,b1_est,'b1'),

                          base_set$fixed %>%

                            filter(Coeff %in% c('b1','sigma1','ro')))


  positive_part2 <- rbind(base_set$fixed %>% filter(Coeff=='a2'),

                          base_set$betas %>% pexpand(.,b2_est,'b2'),

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

