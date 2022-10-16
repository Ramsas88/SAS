
#' Analysis for Continous Variables
#'
#' @param data a data frame
#' @param grp group variable name
#' @param var summary variables
#' @param var_label the display label
#' @param decimal the display decimal
#' @param total whether to calculate total
#' @param blank_row whether to add a blank row
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats na.omit
#' @export
continous_by <- function(data, grp, var,
                         var_label = var,
                         decimal = 1,
                         total = TRUE,
                         blank_row = FALSE){

  data <- data %>% rename(grp = !! grp, var = !! var)
  if(!is.numeric(data$grp)){
    coding <- levels( factor(data$grp) )
    data <- data %>% mutate(grp = as.numeric(factor(grp)))
  }#do not use factor for numeric grp, should avoid char grp input

  res <- data %>% select(grp, var) %>% na.omit() %>%
    group_by(grp) %>%
    summarise(`Participants with data` = n(),
              Mean =   formatC( mean(var),   digits = decimal, format = "f", flag = "0"),
              SD =     formatC( stats::sd(var)  ,   digits = decimal, format = "f", flag = "0"),
              Median = formatC( stats::median(var), digits = decimal, format = "f", flag = "0"),
              Range =  paste( range(var), collapse = " to ") )

  if(total){

    res_tot <- data %>% select(grp, var) %>% na.omit() %>%
      summarise(`Participants with data` = n(),
                Mean =   formatC( mean(var),   digits = decimal, format = "f", flag = "0"),
                SD =     formatC( stats::sd(var)  ,   digits = decimal, format = "f", flag = "0"),
                Median = formatC( stats::median(var), digits = decimal, format = "f", flag = "0"),
                Range =  paste( range(var), collapse = " to ") ) %>%
      mutate(grp = 9999)
    res <- bind_rows(res, res_tot)
  }


  res <- res %>% gather("key", "value", - grp) %>%
   mutate(key = factor(key, levels = c("Participants with data", "Mean", "SD", "Median", "Range")) ) %>%
    spread(grp, value) %>%
    mutate(var_label = var_label) %>%
    mutate(key = as.character(key)) %>%
    rename(var = key)

  if(blank_row){
    res <- bind_rows(tibble(var_label = var_label), res)
  }

  res
}
