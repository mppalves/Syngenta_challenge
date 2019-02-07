#' @title Testing heat features
#' @Objectives the function below evaluate the results of a single strain by simple linear regression and scatterplots of two 
#' features with the greatest potential to explain the variation in the hybrids.
#' Additional lm are offered on the page bottom.
#' @author Marcos Paulo Pedrosa Alves

require(car)
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#function to teste data features with linear regression models.
testing_heat_features = function(hybrid, pr, wt, temp_treshold, stages, soil_groups ) {

 out = tryCatch( {
    Df_features = createFeatures(pr, wt, hybrid, temp_treshold, stages, soil_groups)
    Df_features_dry = Df_features[which(Df_features$IRRIGATION == "DRY"),]
   
    a = tryCatch({lm(YIELD ~ WEIGHTED_HEAT, data = Df_features_dry)}, 
                 error = function(cond) {
                   message(paste0(as.character(hybrid)," WEIGHTED_HEAT not possible to analysis"))
                 })
    b = tryCatch({lm(YIELD ~ MAX_CONSEC_HEAT_DAYS, data = Df_features_dry)},
                 error = function(cond) {
                   message(paste0(as.character(hybrid)," MAX_CONSEC_HEAT_DAYS not possible to analysis"))
                 })
    c = tryCatch({lm(YIELD ~ TOTAL_HEAT_CONSEC_DAYS, data = Df_features_dry)},
                 error = function(cond) {
                   message(paste0(as.character(hybrid)," TOTAL_HEAT_CONSEC_DAYS not possible to analysis"))
                 })
    d = tryCatch({lm(YIELD ~ TOTAL_HEAT_DAYS, data = Df_features_dry)},
                 error = function(cond) {
                   message(paste0(as.character(hybrid)," TOTAL_HEAT_DAYS not possible to analysis"))
                 })
    e = tryCatch({lm(YIELD ~ TMAX, data = Df_features_dry)},
                 error = function(cond) {
                   message(paste0(as.character(hybrid)," TMAX not possible to analysis"))
                 })
    aa = cbind(rownames(summary(a)$coefficients), summary(a)$coefficients,summary(a)$r.squared,lmp(a),as.character(hybrid))
    bb = cbind(rownames(summary(b)$coefficients), summary(b)$coefficients,summary(b)$r.squared,lmp(b),as.character(hybrid))
    cc = cbind(rownames(summary(c)$coefficients), summary(c)$coefficients,summary(c)$r.squared,lmp(c),as.character(hybrid))
    dd = cbind(rownames(summary(d)$coefficients), summary(d)$coefficients,summary(d)$r.squared,lmp(d),as.character(hybrid))
    ee = cbind(rownames(summary(e)$coefficients), summary(e)$coefficients,summary(e)$r.squared,lmp(e),as.character(hybrid))
    exp_final = rbind(aa,bb,cc,dd,ee)
    colnames(exp_final)[c(1,6,7,8)] = c("coefficient name","r.squared","P all", "Hybrid")
    return(exp_final)
    
  }, error =  function(cond) {
    er = matrix(c(NA,NA,NA,NA,NA,NA,NA,as.character(hybrid)), nrow = 1, ncol = 8)
    colnames(er) =  c("Estimate", "Std. Error", "t value", "Pr(>|t|)",'coefficient name','r.squared','P all', 'Hybrid')
    return(er)
  } )
 return(out)
}

#Optional models to test other engeneered features.
# a = lm(YIELD ~ I(WEIGHTED_HEAT^2) * WEIGHTED_HEAT * SOILGROUP * PREC, data = Df_features_dry)
# b = lm(YIELD ~ I(MAX_CONSEC_HEAT_DAYS^2) * MAX_CONSEC_HEAT_DAYS * SOILGROUP * PREC, data = Df_features_dry)
# c = lm(YIELD ~ I(TOTAL_HEAT_CONSEC_DAYS^2) * TOTAL_HEAT_CONSEC_DAYS * SOILGROUP * PREC, data = Df_features_dry)
# d = lm(YIELD ~ I(TOTAL_HEAT_DAYS^2) * TOTAL_HEAT_DAYS * SOILGROUP * PREC, data = Df_features_dry)
# e = lm(YIELD ~ I(TMAX^2) * TMAX * SOILGROUP * PREC, data = Df_features_dry)