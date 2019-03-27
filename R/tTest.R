#' T-test for two given treatments vector
#'
#'
#' @param y1
#' @param y2
#'
#' @return dataframe
#'
#' @export
#' @examples
# data = c(24,28,37,30,31,28,37,44,31,35,36,39,42,47,52,38,44,50,58,55,63,70,68,60)
#
# a = 4
# n = 6
# N = 24
#
# obs = matrix(data, nrow = a, ncol = n, byrow = T)
#
# t_test(obs[1,],obs[4,])

t_test = function(y1,y2){
  n1=length(y1)
  n2= length(y2)
  n = n1
  s1_sq = (sum(y1^2)- n1*((mean(y1)^2))) / (n1-1)
  s4_sq = (sum(y2^2)- n2*((mean(y2)^2))) / (n2-1)

  s_pooled = (((n1-1)*s1_sq) + ((n2-1)*s4_sq)) / (n1+n2-2)

  t_stat = (mean(y1) - mean(y2))/ sqrt(s_pooled*(2/n))
  p = t(data.frame("pooled_s"=s_pooled,
                   "test_statistics"=t_stat
  ))
  colnames(p)="values"
  return(p)
}
