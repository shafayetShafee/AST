#' Anova analysis for given AST204 problem
#' anova_rcbd gets the anova table for given dataset whose treatments are given
#' rowwise and blocks will be columnwise
#'
#' @param data
#'
#' @return dataframe
#' @export
#'
#' @examples
#' obs = c(25,35,21,37,40,52,39,58,27,34,20,31)
#' a = 3
#' b = 4
#' N = 12
#' mat = matrix(obs, nrow = a, ncol = b, byrow = T)
#' anova_rcbd(mat)


anova_rcbd= function(data){

  N = dim(data)[1]*dim(data)[2]
  a = dim(data)[1]
  b = dim(data)[2]

  treat_totals = rowSums(data)
  block_totals = colSums(data)
  grand_totals = sum(data)

  SST = sum(data^2) - (grand_totals^2)/N
  SS_treat = sum(treat_totals^2)/b - (grand_totals^2)/N
  SS_blocks = sum(block_totals^2)/a-(grand_totals^2)/N
  SS_error = SST - SS_treat - SS_blocks

  MS_treat = SS_treat/(a-1)
  MS_error = SS_error/((a-1)*(b-1))
  MS_blocks = SS_blocks/(b-1)

  F_ratio = MS_treat/MS_error

  result = t(data.frame(grand"=sum(data),
                         "Data_squared" = sum(data^2),
                         "treatSquared_by_b"=(sum(treat_totals^2))/b,
                         "blockSquared_by_a"=sum(block_totals^2)/a,
                         "grandSquared_by_N" = (grand_totals^2)/N,
                         "SST"=SST, "SS_treat" = SS_treat, "SSB" = SS_blocks,
                        "SSE" = SS_error,"MS_treatment" = MS_treat,
                        "MSE" = MS_error,"F_ratio" = F_ratio))
  colnames(result) = "values"
  return(result)
}

