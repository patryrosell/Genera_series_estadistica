remove_outliers <- function(df, var,stdv) {

  dF= df %>% 
    group_by(month(Date)) %>% 
    mutate(up_lim=mean(get(var))+stdv*sd(get(var)),lo_lim=mean(get(var))-stdv*sd(get(var)))
  
  df2 = df %>% dplyr::filter(get(var) < dF$up_lim & get(var) > dF$lo_lim)

  df2
}




