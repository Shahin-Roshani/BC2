#' @title residuals.bc2
#'
#' @description residuals (resid) method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#' @param ... Other residuals arguments (Not being used by objects of class \code{bc2}).
#'
#' @return A tibble with 2 columns containing residuals related to each response.
#'
#' @seealso \url{https://shahin-roshani.github.io/BC2/}
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

residuals.bc2 <- function(object,...){

  real <- object$`Structured data`[names(object$Data)[1:2]] %>% as_tibble

  return(

    (real-fitted(object)) %>% as_tibble

  )

}
