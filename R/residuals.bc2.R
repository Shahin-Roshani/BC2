#' @title residuals.bc2
#'
#' @description residuals (resid) method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#'
#' @return A tibble with 2 columns containing residuals related to each response.
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

residuals.bc2 <- function(object){

  x <- object

  ((x$`Structured data`[3:4] %>% as_tibble)-fitted(x)) %>% as_tibble

}

