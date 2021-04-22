#' @title predict.bc2
#'
#' @description predict method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#' @param newdata Data (tibble or data.frame) containing predictor information of new records.
#' @param interval Type of interval for predictions. Valid inputs are 'confidence' or 'interval'. Default is 'interval'.
#' @param alpha alpha value for creating the interval(s) of new prediction(s). Default is 0.05.
#'
#' @return A list of length 2. Each slot of this list contains a tibble with the information of predictions and their related intervals for each response.
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

predict.bc2 <- function(object,newdata,interval='prediction',alpha=.05){

  x <- object

  if (!(interval %in% c('prediction','confidence'))){

    stop('interval must be prediction or confidence!',call.=F)

  }

  x0 <- newdata

  newdata <- model.matrix(~.,data=newdata)

  fitted.betas <- x$`Final tables` %>% .[-1] %>%

    map(~.$Estimate[1:ncol(newdata)] %>% as.matrix(ncol=1))

  res <- fitted.betas %>% map(~newdata%*%.) %>%

    map(~as.data.frame(.) %>% rename('prediction'='V1')) %>%

    map(~cbind(x0,.))

  res <- map2(res,

              x$`Final tables`[-1] %>% map(~.$Estimate[nrow(.)-1]),

              ~mutate(.x,sigma=.y))

  res <- map2(names(res) %>% as.list,

              res,

              ~mutate(.y,response=.x)) %>% reduce(rbind)


  Xmat <- x$`Structured data`$X1

  resX <- res %>% select(-c('prediction','sigma','response'))

  for (i in 1:nrow(res)){

    x0 <- resX[i,] %>% as.matrix %>% t

    res$pure_inside[i] <- t(x0)%*%solve(t(Xmat)%*%Xmat)%*%x0

  }

  if (interval=='prediction'){

    res %<>% mutate_at('pure_inside',~.+1)

  }

  mydt <- qt(1-alpha/2,nrow(Xmat)-ncol(Xmat)-1)

  res %<>% mutate(lower=prediction-mydt*sigma*sqrt(pure_inside),

                  upper=prediction+mydt*sigma*sqrt(pure_inside)) %>%

    select(prediction,lower,upper,response) %>%

    mutate_at('response',~as.factor(.) %>% fct_inorder) %>%

    split(.$response) %>%

    map(~select(.,-response) %>% as_tibble %>%

          rename_at(2:3,~str_c(.,' (%',(1-alpha)*100,')')))

  return(res)

}

