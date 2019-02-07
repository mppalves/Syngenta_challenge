#' @title Creating features
#' @Objective:
#' Create features based on weather data and production the dataset for features that can correlate with heat and drought stress.
#' features created: "TOTAL_HEAT_DAYS", "TOTAL_HEAT_CONSEC_DAYS", "MAX_CONSEC_HEAT_DAYS", "WEIGHTED_HEAT".
#' @Function_parameters:
#' pr: production database
#' wt: Weather database
#' temp_treshold: min temperature to consider for stress
#' stages: number of stages in which the corn stages will be divided
#' Soil_groups: number of groups in which the soil charactheristics will be divided
#' #' @author Marcos Paulo Pedrosa Alves

require(plyr)
require(car)
source("analizeHeat.R")
set.seed(100)

createFeatures = function(pr, wt, hybrid, temp_treshold, stages = 4, soil_groups = 3) {

  
  #Correcting irrigation data
  pr[pr$IRRIGATION == "NONE", "IRRIGATION"] = "DRY"

  #Creating a database with the selected hybrid
  pr_hyb = pr[pr$HYBRID_ID == hybrid, ]
  
  #tranforming plant and harvest date in 365 days
  pr_hyb$PLANT_DATE = as.numeric(strftime(pr_hyb$PLANT_DATE, format = "%j"))
  pr_hyb$HARVEST_DATE = as.numeric(strftime(pr_hyb$HARVEST_DATE, format = "%j"))

  #grouping soil types
  pr_num_soil_type = c("SAND", "KSAT")
  #"AWC", "OM", "CEC",
  fitKmeans = kmeans(pr_hyb[, pr_num_soil_type], soil_groups)
  
  
  pr_hyb = data.frame(pr_hyb, fitKmeans$cluster)
  colnames(pr_hyb)[which(colnames(pr_hyb) == "fitKmeans.cluster")] = "SOILGROUP"
  
  soil_groups = aggregate(pr_hyb$YIELD, list(pr_hyb$SOILGROUP), mean)
  ordered_soil = soil_groups[order(-soil_groups$x),]
  ordered_soil = cbind(ordered_soil,seq(from=max(soil_groups$Group.1), to=min(soil_groups$Group.1)))
  
  for (i in ordered_soil[[1]]) {
    pr_hyb$SOILNEWGROUP[pr_hyb$SOILGROUP == i] = ordered_soil[ordered_soil$Group.1 == i,3]
  }
  
  soil_groups_new = aggregate(pr_hyb$YIELD, list(pr_hyb$SOILNEWGROUP), mean)
  #print(soil_groups_new)
  
  #grouping by average
  pr_hyb = aggregate(
    pr_hyb[, c(
      "YIELD",
      "LAT",
      "LONG",
      "PLANT_DATE",
      "HARVEST_DATE",
      "ENV_YIELD_MEAN",
      "ENV_YIELD_STD",
      "ELEVATION",
      "CLAY",
      "SILT",
      "SAND",
      "AWC",
      "PH",
      "OM",
      "CEC",
      "KSAT"
    )],
    by = list(pr_hyb$ENV_ID, pr_hyb$IRRIGATION, pr_hyb$SOILNEWGROUP),
    FUN = mean
  )
 
  #correcting gouping names
  colnames(pr_hyb)[c(
    which(colnames(pr_hyb) == "Group.1"),
    which(colnames(pr_hyb) == "Group.2"),
    which(colnames(pr_hyb) == "Group.3")
  )] = c("ENV_ID", "IRRIGATION", "SOILGROUP")
  
  #subsetting wt database to have only hyb data
  env_hyb = unique(pr_hyb$ENV_ID)
  wt_hyb = wt[wt$ENV_ID %in% env_hyb, ]
  
  #Selecting only the data relavant in the wt table
  y = data.frame()
  for (i in env_hyb) {
    z = pr_hyb[pr_hyb$ENV_ID == i, c("PLANT_DATE", "HARVEST_DATE")]
    x = wt_hyb[which(
      wt_hyb$ENV_ID == i &
        wt_hyb$DAY_NUM >= as.numeric(z[1]) &
        wt_hyb$DAY_NUM <= as.numeric(z[2])
    ), ]
    y = rbind(y, x)
  }
  wt_hyb = y
  

  #adding a weight for the period with heat
  steps = NULL
  for (i in wt_hyb$ENV_ID){
    
    x = wt_hyb[wt_hyb$ENV_ID == i,"DAY_NUM"]
    y = min(x)
    z = max(x)
    m = (z - y)/stages
    
    fact = stages^2
    
    for (j in 1:stages) {
      steps[j] = y + m*j
    }
    wt_hyb[wt_hyb$ENV_ID == i, "GROWTH_FASE"] = as.numeric(cut(x, c(0,steps),stages:1))
  }
  
  #define the number of days with heat above the treshold
  wt_hyb = analizeHeat(wt_hyb, temp_treshold)
  
  #adding the column with the average temperature
  wt_hyb$TAVG = as.numeric(rowMeans(wt_hyb[, c("TMAX", "TMIN")]))
  
  #summarizing with column means
  nums <- unlist(lapply(wt_hyb, is.numeric))
  wt_hyb = aggregate(wt_hyb[, nums], list(wt_hyb$ENV_ID), mean)
  colnames(wt_hyb)[which(colnames(wt_hyb) == "Group.1")] = "ENV_ID"
  
  wt_hyb[wt_hyb[, "TOTAL_HEAT_DAYS"]==0, c("TOTAL_HEAT_DAYS", "TOTAL_HEAT_CONSEC_DAYS", "MAX_CONSEC_HEAT_DAYS", "WEIGHTED_HEAT")] = NA
  

  #final table adjusted per strain, with climate averaged by the real plant and harvest dates
  wt_pr_hyb = merge(pr_hyb, wt_hyb, by = "ENV_ID")
  return(wt_pr_hyb)
  
}
