#' @title plot.bc2
#'
#' @description plot method for bc2 fits.
#'
#' @author Shahin Roshani
#'
#' @param object a fitted object of class inheriting from "bc2".
#' @param type Type of plot to be drawn. Valid inputs are \code{'responses'}, \code{'residuals'} or \code{'fit'}. Default is \code{'residuals'}.
#'
#' @return When \code{type='responses'}, density plots of original and un-transformed responses (non-zero) will be returned (unless transformation was done directly on non-zero responses and not through the \code{g.funs} arguemt).
#'
#' When \code{type='residuals'}, density plot of residuals plus residuals vs fitted values plot for each response will be returned.
#'
#' For \code{type='fit'}, actual vs fitted values of each response will be returned.
#'
#' @seealso \url{https://shahin-roshani.github.io/BC2/}
#'
#' @import tidyverse magrittr msm future furrr cowplot
#'
#' @export

plot.bc2 <- function(object,type='residuals'){

  x <- object

  if (!(type %in% c('responses','residuals','fit'))){

    stop('type must be responses, residuals or fit',call.=F)

  }

  if (type=='responses'){

    p <- ggplot(data=x$Data[,1:2] %>% gather %>% filter(value!=0),

                aes(x=value)) +

      geom_density(color='steelblue4',fill='steelblue3',alpha=.5,size=.75) +

      facet_wrap(~key,scales='free') + theme_minimal() +

      labs(x='Values',y='Density',title='Density of original responses:') +

      theme(panel.grid = element_line(color='grey80'),

            text = element_text(family='serif'))

  }

  if (type=='residuals'){

    residu <- resid(x) %>% gather(value='resid') %>%

      mutate_at('key',~as.factor(.) %>% fct_inorder)

    p1 <- ggplot(data=list(fitted=fitted(x) %>% gather(value='fitted'),

                           resid=residu %>% .[,-1]) %>% reduce(cbind) %>%

                   mutate_at('key',~as.factor(.) %>% fct_inorder)) +

      geom_point(aes(x=fitted,y=resid),color='steelblue4',size=2) +

      facet_wrap(~key,scales='free') + theme_minimal() +

      labs(x='Fitted values',y='Residuals',title='Residuals vs fitted values:') +

      theme(panel.grid = element_line(color='grey80'),

            text = element_text(family='serif'))

    p2 <- ggplot(data=residu,aes(x=resid)) +

      geom_density(color='steelblue4',fill='steelblue3',alpha=.5,size=.75) +

      facet_wrap(~key,scales='free') + theme_minimal() +

      labs(x='Residuals',y='Density',title='Density of residuals:') +

      theme(panel.grid = element_line(color='grey80'),

            text = element_text(family='serif'))

    p <- cowplot::plot_grid(p2,p1,nrow=2)

  }

  if (type=='fit'){

    p <- ggplot(data=list(x$`Structured data`[3:4] %>% as_tibble %>%

                            gather(value='y'),

                          fitted(x) %>% gather(value='yhat') %>% .[,-1]) %>%

                  reduce(cbind) %>%

                  mutate_at('key',~as.factor(.) %>% fct_inorder),

                aes(x=yhat,y=y)) +

      geom_point(color='steelblue4',size=2) +

      geom_abline(intercept=0,slope=1,color='firebrick4',size=.75) +

      facet_wrap(~key,scales='free') + theme_minimal() +

      labs(x='Fitted values',y='Actual values',

           title='Actual vs fitted values:') +

      theme(panel.grid = element_line(color='grey80'),

            text = element_text(family='serif'))

  }

  return(p)

}

