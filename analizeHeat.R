#' @title Analize Heat
#' 
#' @description Define heat stress through the heat_treshold parameter and count the number of days
#' above the treshold, the number of consecutive days of heat stress and calulate the penaltyes for heat days 
#' on earlier stages of corn development.
#' @parameters 
#' wt: wheather data. 
#' heat treshold

analizeHeat <- function(wt, heat_treshold) {
  #Defining heat stress

  wt_heat = wt[which(wt$TMAX > heat_treshold), ]

  if (nrow(wt_heat)) {
    #agregating the total number of stress days
    wt_heat_days = aggregate(wt_heat[, "ENV_ID"], by = list(wt_heat$ENV_ID), FUN = "length")
    environ = unique(wt_heat[, "ENV_ID"])
    
    #calculating penalties for the heatdays acording to the development fase
    wt_heat_days_penalties = aggregate(wt_heat[, "ENV_ID"],
                                       by = list(wt_heat$ENV_ID, wt_heat$GROWTH_FASE),
                                       FUN = "length")
    wt_heat_days_penalties$WEIGHTED_HEAT = as.numeric(wt_heat_days_penalties$Group.2) * wt_heat_days_penalties$x
    wt_heat_days_penalties = aggregate(
      wt_heat_days_penalties[, "WEIGHTED_HEAT"],
      by = list(wt_heat_days_penalties$Group.1),
      FUN = "mean"
    )
    
    #summing up the total number of days where the stress persisted for more the 1 day in a row
    wt_heat_total_consec_days = data.frame()
    for (i in environ) {
      x = diff(wt_heat[wt_heat$ENV_ID == i, "DAY_NUM"])
      wt_heat_total_consec_days[i, 1] = length(x[x == 1])
    }
    
    #counting the maximum number of days where the temperature was above the treshold in a row
    x = data.frame()
    heat_count = data.frame()
    for (i in environ) {
      x = as.numeric(wt_heat[wt_heat$ENV_ID == i, "DAY_NUM"])
      n <- length(x)
      y <- x[-1L] != x[-n] + 1
      j <- c(which(y | is.na(y)), n)
      lengths = diff(c(0L, j))
      
      x = as.data.frame(cbind(sequence(lengths), i))
      heat_count = rbind(heat_count, x)
    }
    
    #Organizing the columns
    heat_count$V1 = as.numeric(heat_count$V1)
    wt_heat_max_consec_days = aggregate(heat_count$V1,
                                        by = list(heat_count$i),
                                        FUN = max)
    
    colnames(wt_heat_days) = c("ENV_ID", "TOTAL_HEAT_DAYS")
    wt_heat_total_consec_days$ENV_ID = rownames(wt_heat_total_consec_days) #this variable comes without the row ENV_ID
    colnames(wt_heat_total_consec_days) = c("TOTAL_HEAT_CONSEC_DAYS", "ENV_ID")
    colnames(wt_heat_max_consec_days) = c("ENV_ID", "MAX_CONSEC_HEAT_DAYS")
    colnames(wt_heat_days_penalties) = c("ENV_ID", "WEIGHTED_HEAT")
    
    x = merge(wt_heat_days, wt_heat_total_consec_days , by = "ENV_ID")
    y = merge(x, wt_heat_max_consec_days, by = "ENV_ID")
    z = merge(y, wt_heat_days_penalties, by = "ENV_ID")
    
    wt_merged = merge(wt, z, all = TRUE)
    wt_merged[is.na(wt_merged)] = 0
    
    
    return(wt_merged)
    
  } else {
    wt_merged = data.frame(
      wt,
      TOTAL_HEAT_DAYS = 0,
      TOTAL_HEAT_CONSEC_DAYS = 0,
      MAX_CONSEC_HEAT_DAYS = 0,
      WEIGHTED_HEAT = 0
    )
   
    return(wt_merged)
  }
}
