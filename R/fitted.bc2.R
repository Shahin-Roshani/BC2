#' @title  fitted.bc2
#'
#' @description fitted method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#' @param ... Other fitted arguments (Not being used by objects of class \code{bc2}).
#'
#' @return A tibble with 2 columns containing fitted values of each response.
#'
#' @seealso \url{https://shahin-roshani.github.io/BC2/}
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

fitted.bc2 <- function(object,...){

  mats <- object$`Structured data`[c('X1','X2')] %>% map(~cbind(1,.))

  betas <- map2(.x=mats %>% map(ncol),

                .y=object$`Final tables`[-1] %>% map(~.$Estimate),

                .f=~.y[1:.x])

  names(mats) = names(betas) <- object$`Final tables`[-1] %>% names

  return(

    map2(.x=mats,.y=betas,.f=~.x%*%.y) %>% as.data.frame %>% as_tibble

  )

}
