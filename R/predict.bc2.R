#' @title predict.bc2
#'
#' @description predict method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#' @param newdata Data (tibble or data.frame) containing predictor information of new records.
#' @param interval Type of interval for predictions. Valid inputs are \code{'confidence'} or \code{'prediction'}. Default is \code{'prediction'}.
#' @param alpha alpha value for creating the interval(s) of new prediction(s). Default is \emph{0.05}.
#' @param ... Other predict arguments (Not being used by objects of class \code{bc2}).
#'
#' @return A list of length 2. Each slot of this list contains a tibble with the information of predictions and their related intervals for each response.
#'
#' @seealso \url{https://shahin-roshani.github.io/BC2/}
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

predict.bc2 <- function(object,newdata,interval='prediction',alpha=.05,...){

  if (!(interval %in% c('prediction','confidence'))){

    stop('interval must be prediction or confidence!',call.=F)

  }

  mats <- object$`Structured data`[c('X1','X2')] %>% map(~cbind(1,.))

  betas <- map2(.x=mats %>% map(ncol),

                .y=object$`Final tables`[-1] %>% map(~.$Estimate),

                .f=~.y[1:.x])

  newdata %<>% (function(x){

    if (any(names(x) %in% names(object$Data)[1:2])){

      x %<>% select(-which(names(x) %in% names(object$Data)[1:2]))

    }

    return(x)

  })

  preds_mats <- map2(.x=list(model.matrix(~.,data=newdata)) %>% rep(.,2),

                     .y=mats %>% map(~colnames(.) %>% .[-which(.=='')]),

                     .f=~.x[,.y]) %>% map(~cbind(1,.))


  preds <- map2(.x=preds_mats,.y=betas,.f=~.x%*%.y) %>%

    map(~as.data.frame(.) %>% as_tibble %>% rename('prediction'='V1'))

  inside_vals <- map2(.x=mats,

                      .y=preds_mats,

                      .f=~.y%*%solve(t(.x)%*%.x)%*%t(.y)) %>% map(diag)


  if (interval=='prediction'){

    inside_vals %<>% map(~.+1)

  }

  sigmas <- object$`Final tables`[-1] %>% map(~.$Estimate[nrow(.)-1])

  preds <-

    pmap(.l=list(mats,preds,inside_vals,sigmas),

         .f=~mutate(..2,

                    lower=prediction-

                      ..4*qt(1-alpha/2,nrow(..1)-ncol(..1)-1)*sqrt(..3),

                    upper=prediction+

                      ..4*qt(1-alpha/2,nrow(..1)-ncol(..1)-1)*sqrt(..3))) %>%

    map(~rename_at(.,-1,~str_c(.,' (%',(1-alpha)*100,')')))

  names(preds) <- object$`Final tables`[-1] %>% names

  return(preds)

}
