#' Count categorical variables
#'
#' @param data a data frame
#' @param grp group variable name
#' @param var summary variables
#' @param id id variable, if not null then will count distinct number of records by id
#' @param var_label the display label
#' @param decimal the display decimal
#' @param total whether to calculate total
#'
#' @export
count_by <- function(data, grp,
                     var = NULL,
                     var_label = var,
                     id = "USUBJID",
                     decimal = 1,
                     total = TRUE
){
  if(is.null(id)){
    rowid <- data.frame(rowid = as.numeric(rownames(data)))
    data <- bind_cols(data, rowid)
    id <- "rowid"
  }

  if(is.null(var)){
    data <- mutate(data, dum = "1")
    var <- "dum"
    if(var_label == var){
      var_label <- id
    }
  }

  data <- data %>% rename(grp = !! grp, var = !! var, id = !!id)

  if(!is.numeric(data$grp)){
    coding <- levels( factor(data$grp) )
    data <- data %>% mutate(grp = as.numeric(factor(grp)))
  }#do not use factor for numeric grp, should avoid char grp input

  data_total <- data %>% mutate(grp = 9999) #add total rows into calculation
  data <- rbind(data, data_total)

  res <- data %>%
    group_by(grp, var) %>%
    summarise(n = n_distinct(id))

  res_total <- data %>%
    group_by(grp) %>%
    summarise(tot = n_distinct(id)) #total within each grp


  res <- res %>%
    left_join(res_total, by = "grp") %>%
    mutate(pct = formatC(100*n/tot, digits = decimal, format = "f", flag = "0")) #calculate pct

  dum_pct <- formatC(0,digits = decimal, format = "f", flag = "0")

  res <- res %>%
    mutate(n = as.character(n)) %>%
    pivot_wider(id_cols = var,
                names_from = grp,
                values_from = c(n, pct),
                values_fill = list(n = "0", pct = dum_pct)
    ) %>%
    mutate(var_label = var_label)

  if(total != TRUE){
    res <- res %>% select(-c(n_9999,pct_9999))
  }

  if(var != "dum"){
    res <- res %>%
      mutate(var = as.character(var))
  } else{
    res <- res %>%
      select(c(grep("n",names(res)),var_label))  #removing pct from counting unique per ID since it's irrelevant
  }

  names(res) <- gsub("n_", "", names(res), fixed = TRUE)

  if(exists("coding")){
    attr(res, "coding") <- coding
  }

  res
}
