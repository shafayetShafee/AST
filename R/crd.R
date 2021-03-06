#' Anova analysis for given AST204 problem
#' anova_crd gets the anova table for given dataset whose treatments are given
#' rowwise and replications will be columnwise
#'
#' @param data
#' @param info
#'
#' @return dataframe
#' @export
#' @examples
#' obs = c(25,35,21,37,40,52,39,58,27,34,20,31)
#' a = 3
#' b = 4
#' N = 12
#' mat = matrix(obs, nrow = a, ncol = b, byrow = T)
#' anova_crd(mat,info="All")
anova_crd= function(data,info=c("anova","All")){

  N = dim(data)[1]*dim(data)[2]
  a = dim(data)[1]
  n = dim(data)[2]

  treat_totals = rowSums(data)
  grand_totals = sum(data)

  SST_crd = sum(data^2) - (grand_totals^2)/N
  SS_treat_crd = (sum(treat_totals^2))/n - (grand_totals^2)/N
  SS_error_crd = SST_crd - SS_treat_crd

  MS_treat_crd = SS_treat_crd/ (a-1)
  MS_error_crd = SS_error_crd / (N-a)

  F_ratio_crd = MS_treat_crd/MS_error_crd

  if(info=="anova"){
    result = t(data.frame("SST"=SST_crd, "SS_treat" = SS_treat_crd, "SSE" = SS_error_crd,
                          "MS_treatment" = MS_treat_crd, "MSE" = MS_error_crd,
                          "F_ratio" = F_ratio_crd))
    colnames(result) = "values"
    return(result)
  }
  else if (info=="All"){
    result = t(data.frame( "grand"=sum(data),
                           "Data_squared" = sum(data^2),
                           "treatSquared_by_n"=(sum(treat_totals^2))/n,
                           "grandSquared_by_N" = (grand_totals^2)/N,
                           "SST"=SST_crd, "SS_treat" = SS_treat_crd, "SSE" = SS_error_crd,
                           "MS_treatment" = MS_treat_crd, "MSE" = MS_error_crd,
                           "F_ratio" = F_ratio_crd))
    colnames(result) = "values"
    return(result)
  }
}
