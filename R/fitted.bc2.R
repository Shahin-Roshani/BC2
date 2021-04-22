#' @title  fitted.bc2
#'
#' @description fitted method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#'
#' @return A tibble with 2 columns containing fitted values of each response.
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

fitted.bc2 <- function(object){

  x <- object

  fitted.X1 <- x$`Structured data`$X1 %>% cbind(1,.)

  fitted.betas <- x$`Final tables` %>% .[-1] %>%

    map(~.$Estimate[1:ncol(fitted.X1)] %>% as.matrix(ncol=1))

  return(

    fitted.betas %>% map(~fitted.X1%*%.) %>% as.data.frame %>% as_tibble

  )

}

